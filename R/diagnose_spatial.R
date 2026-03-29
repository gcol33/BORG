# Spatial autocorrelation diagnostics for borg_diagnose()

#' Diagnose Spatial Autocorrelation (Internal)
#' @noRd
diagnose_spatial <- function(data, coords, y, alpha, verbose, crs = NULL) {
  x_coord <- data[[coords[1]]]
  y_coord <- data[[coords[2]]]
  n <- nrow(data)

  # Remove missing coordinates
  complete <- complete.cases(x_coord, y_coord)
  if (!is.null(y)) complete <- complete & complete.cases(y)

  x_coord <- x_coord[complete]
  y_coord <- y_coord[complete]
  if (!is.null(y)) y <- y[complete]
  n_complete <- length(x_coord)

  if (n_complete < 10) {
    return(list(detected = FALSE, reason = "Insufficient complete observations"))
  }

  # Compute distance matrix (using only subset for large datasets)
  max_for_full <- 2000
  if (n_complete > max_for_full) {
    # Sample for distance computation
    idx <- sample(n_complete, max_for_full)
    x_sub <- x_coord[idx]
    y_sub <- y_coord[idx]
    y_sub_val <- if (!is.null(y)) y[idx] else NULL
  } else {
    x_sub <- x_coord
    y_sub <- y_coord
    y_sub_val <- y
  }

  # Compute pairwise distances (Haversine for geographic CRS, Euclidean otherwise)
  dist_mat <- compute_distance_matrix_geo(x_sub, y_sub, crs = crs)

  # If no target, use coordinates to test for clustering
  if (is.null(y_sub_val)) {
    # Just check if points are clustered vs uniform
    nn_dists <- apply(dist_mat + diag(Inf, nrow(dist_mat)), 1, min)
    median_nn <- median(nn_dists)
    max_dist <- max(dist_mat)

    # Heuristic: if median NN distance is < 5% of max extent, likely clustered
    clustering_ratio <- median_nn / max_dist
    detected <- clustering_ratio < 0.05

    return(list(
      detected = detected,
      morans_i = NA_real_,
      morans_p = NA_real_,
      range_estimate = if (detected) median_nn * 10 else NA_real_,
      effective_n = if (detected) n_complete * clustering_ratio * 10 else as.numeric(n_complete),
      coords_used = coords
    ))
  }

  # Compute Moran's I
  morans <- compute_morans_i(y_sub_val, dist_mat)

  # Compute variogram and estimate spatial range
  variogram <- compute_empirical_variogram(y_sub_val, dist_mat)
  range_estimate <- estimate_spatial_range(y_sub_val, dist_mat)
  sill <- if (!is.null(variogram)) max(variogram$semivariance) else NA_real_

  # Compute effective sample size
  effective_n <- compute_effective_n_spatial(n_complete, morans$I, range_estimate,
                                             max(dist_mat))

  detected <- morans$p < alpha && morans$I > 0.1

  list(
    detected = detected,
    morans_i = morans$I,
    morans_p = morans$p,
    range_estimate = range_estimate,
    effective_n = effective_n,
    coords_used = coords,
    variogram = variogram,
    sill = sill
  )
}


#' Compute Distance Matrix (Internal)
#' @noRd
compute_distance_matrix <- function(x, y) {
  n <- length(x)
  # Euclidean distance
  outer(1:n, 1:n, function(i, j) {
    sqrt((x[i] - x[j])^2 + (y[i] - y[j])^2)
  })
}


#' Compute Moran's I (Internal)
#' @noRd
compute_morans_i <- function(y, dist_mat) {
  n <- length(y)
  y_centered <- y - mean(y)

  # Inverse distance weights (with cutoff to avoid huge weights)
  W <- 1 / (dist_mat + 1e-10)
  diag(W) <- 0

  # Row-standardize
  row_sums <- rowSums(W)
  row_sums[row_sums == 0] <- 1
  W <- W / row_sums

  # Moran's I
  numerator <- sum(W * outer(y_centered, y_centered))
  denominator <- sum(y_centered^2)

  I <- (n / sum(W)) * (numerator / denominator)

  # Expected value and variance under null
  E_I <- -1 / (n - 1)

  # Simplified variance (assuming randomization)
  S1 <- 0.5 * sum((W + t(W))^2)
  S2 <- sum((rowSums(W) + colSums(W))^2)
  S0 <- sum(W)

  k <- (sum(y_centered^4) / n) / (sum(y_centered^2) / n)^2

  var_I <- (n * ((n^2 - 3*n + 3) * S1 - n * S2 + 3 * S0^2) -
              k * (n * (n - 1) * S1 - 2*n*S2 + 6*S0^2)) /
    ((n - 1) * (n - 2) * (n - 3) * S0^2) - E_I^2

  var_I <- max(var_I, 1e-10)  # Ensure positive

  # Z-score and p-value
  z <- (I - E_I) / sqrt(var_I)
  p <- 2 * stats::pnorm(-abs(z))

  list(I = I, E_I = E_I, var_I = var_I, z = z, p = p)
}


#' Compute Empirical Variogram (Internal)
#'
#' Bins pairwise semivariance by distance for spatial autocorrelation analysis.
#'
#' @return A data.frame with columns: distance, semivariance, n_pairs.
#'   NULL if insufficient data.
#' @noRd
compute_empirical_variogram <- function(y, dist_mat, n_bins = 15) {
  upper_idx <- upper.tri(dist_mat)
  distances <- dist_mat[upper_idx]
  semivar <- 0.5 * outer(y, y, function(a, b) (a - b)^2)[upper_idx]

  n_bins <- min(n_bins, floor(length(distances) / 50))
  if (n_bins < 3) return(NULL)

  breaks <- quantile(distances, probs = seq(0, 1, length.out = n_bins + 1))
  breaks <- unique(breaks)

  bins <- cut(distances, breaks, include.lowest = TRUE)
  bin_distance <- tapply(distances, bins, mean)
  bin_semivariance <- tapply(semivar, bins, mean)
  bin_n_pairs <- tapply(distances, bins, length)

  valid <- !is.na(bin_distance) & !is.na(bin_semivariance)

  data.frame(
    distance = as.numeric(bin_distance[valid]),
    semivariance = as.numeric(bin_semivariance[valid]),
    n_pairs = as.integer(bin_n_pairs[valid]),
    stringsAsFactors = FALSE
  )
}


#' Estimate Spatial Range from Variogram (Internal)
#' @noRd
estimate_spatial_range <- function(y, dist_mat) {
  vario <- compute_empirical_variogram(y, dist_mat)
  if (is.null(vario) || nrow(vario) < 3) return(max(dist_mat) / 3)

  sill_estimate <- max(vario$semivariance)
  threshold <- 0.95 * sill_estimate

  range_idx <- which(vario$semivariance >= threshold)[1]
  if (is.na(range_idx)) max(vario$distance) else vario$distance[range_idx]
}


#' Compute Effective Sample Size for Spatial Data (Internal)
#' @noRd
compute_effective_n_spatial <- function(n, morans_i, range, max_dist) {
  if (is.na(morans_i) || is.na(range)) return(as.numeric(n))

  # Rough approximation: effective n decreases with autocorrelation

  # Based on Griffith (2005) effective sample size formula
  rho <- max(0, min(1, morans_i))  # Bound to [0, 1]

  if (rho < 0.05) return(as.numeric(n))

  # Simple approximation
  effective_n <- n * (1 - rho^2) / (1 + rho^2)
  max(10, effective_n)
}
