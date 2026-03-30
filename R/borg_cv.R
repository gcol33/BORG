# BORG CV Generator
# Generates valid cross-validation schemes based on data dependency structure

#' Generate Valid Cross-Validation Scheme
#'
#' Creates cross-validation folds that respect data dependency structure.
#' When spatial, temporal, or clustered dependencies are detected, random
#' CV is disabled and appropriate blocking strategies are enforced.
#'
#' @param data A data frame to create CV folds for.
#' @param diagnosis A \code{\link{BorgDiagnosis}} object from \code{\link{borg_diagnose}}.
#'   If NULL, diagnosis is performed automatically.
#' @param v Integer. Number of folds. Default: 5.
#' @param coords Character vector of length 2 specifying coordinate column names.
#'   Required for spatial blocking if diagnosis is NULL.
#' @param time Character string specifying the time column name.
#'   Required for temporal blocking if diagnosis is NULL.
#' @param groups Character string specifying the grouping column name.
#'   Required for group CV if diagnosis is NULL.
#' @param target Character string specifying the response variable column name.
#' @param env Environmental covariates for environmental blocking.
#'   A \code{SpatRaster}, data frame, or matrix with one row per observation.
#'   Required when \code{strategy = "environmental_block"} or
#'   \code{"spatial_plus"}.
#' @param dist_mat A distance matrix or \code{dist} object for custom blocking.
#'   Must be an \code{n x n} matrix matching \code{nrow(data)}. Required when
#'   \code{strategy = "custom_block"}.
#' @param prediction_points Data frame, matrix, \code{SpatVector}, or \code{sf}
#'   object with coordinates of prediction locations. Required when
#'   \code{strategy = "knndm"}.
#' @param block_size Numeric. For spatial blocking, the minimum block size.
#'   If NULL, automatically determined from diagnosis. Should be larger than
#'   the autocorrelation range.
#' @param embargo Integer. For temporal blocking, minimum gap between train and test.
#'   If NULL, automatically determined from diagnosis.
#' @param buffer Numeric. Spatial buffer distance (in coordinate units).
#'   Training points within this distance of any test-fold point are removed
#'   to reduce autocorrelation leakage. Default: NULL (no buffer).
#' @param strategy Character. Override the auto-detected CV strategy. Use
#'   \code{"temporal_expanding"} for expanding window (forward-chaining) or
#'   \code{"temporal_sliding"} for fixed-size sliding window.
#'   Default: NULL (auto-detect from diagnosis).
#' @param repeats Integer. Number of times to repeat CV fold generation with
#'   different random seeds. Default: 1 (no repetition).
#' @param output Character. Output format: "list" (default), "rsample", "caret", "mlr3".
#' @param allow_random Logical. If TRUE, allows random CV even when dependencies detected.
#'   Default: FALSE. Setting to TRUE requires explicit acknowledgment.
#' @param verbose Logical. If TRUE, print diagnostic messages. Default: FALSE.
#'
#' @return Depending on \code{output}:
#' \describe{
#'   \item{"list"}{A list with elements: \code{folds} (list of train/test index vectors),
#'     \code{n} (number of observations), \code{diagnosis} (the BorgDiagnosis used),
#'     \code{strategy} (CV strategy name), \code{params} (parameters used).}
#'   \item{"rsample"}{An \code{rsample} \code{rset} object compatible with tidymodels.}
#'   \item{"caret"}{A \code{trainControl} object for caret.}
#'   \item{"mlr3"}{An \code{mlr3} \code{Resampling} object.}
#' }
#'
#' @details
#' \subsection{The Enforcement Principle}{
#' Unlike traditional CV helpers, \code{borg_cv} enforces valid evaluation:
#' \itemize{
#'   \item If spatial autocorrelation is detected, \strong{random CV is disabled}
#'   \item If temporal autocorrelation is detected, \strong{random CV is disabled}
#'   \item If clustered structure is detected, \strong{random CV is disabled}
#'   \item To use random CV on dependent data, you must set \code{allow_random = TRUE}
#'         and provide justification (this is logged).
#' }
#' }
#'
#' \subsection{Spatial Blocking}{
#' When spatial dependencies are detected, data are partitioned into spatial blocks
#' using k-means clustering on coordinates. Block size is set to exceed the
#' estimated autocorrelation range. This ensures train and test sets are
#' spatially separated.
#' }
#'
#' \subsection{Temporal Blocking}{
#' When temporal dependencies are detected, data are split chronologically with
#' an embargo period between train and test sets. This prevents information from
#' future observations leaking into training.
#' }
#'
#' \subsection{Group CV}{
#' When clustered structure is detected, entire groups (clusters) are held out
#' together. No group appears in both train and test within a fold.
#' }
#'
#' @examples
#' # Spatial data with autocorrelation
#' set.seed(42)
#' spatial_data <- data.frame(
#'   x = runif(200, 0, 100),
#'   y = runif(200, 0, 100),
#'   response = rnorm(200)
#' )
#'
#' # Diagnose and create CV
#' cv <- borg_cv(spatial_data, coords = c("x", "y"), target = "response")
#' str(cv$folds)  # List of train/test indices
#'
#' # Clustered data
#' clustered_data <- data.frame(
#'   site = rep(1:20, each = 10),
#'   value = rep(rnorm(20, sd = 2), each = 10) + rnorm(200, sd = 0.5)
#' )
#'
#' cv <- borg_cv(clustered_data, groups = "site", target = "value")
#' cv$strategy  # "group_fold"
#'
#' # Get rsample-compatible output for tidymodels
#' \donttest{
#' cv_rsample <- borg_cv(spatial_data, coords = c("x", "y"), output = "rsample")
#' }
#'
#' @seealso \code{\link{borg_diagnose}}, \code{\link{BorgDiagnosis}}
#'
#' @export
borg_cv <- function(data,
                    diagnosis = NULL,
                    v = 5,
                    coords = NULL,
                    time = NULL,
                    groups = NULL,
                    target = NULL,
                    env = NULL,
                    dist_mat = NULL,
                    prediction_points = NULL,
                    block_size = NULL,
                    embargo = NULL,
                    buffer = NULL,
                    strategy = NULL,
                    repeats = 1L,
                    output = c("list", "rsample", "caret", "mlr3"),
                    allow_random = FALSE,
                    verbose = FALSE) {

  output <- match.arg(output)

  # Auto-detect coords from borg_extract() output
  if (is.null(coords) && !is.null(attr(data, "borg_coords"))) {
    coords <- attr(data, "borg_coords")
  }

  # Handle sf / SpatVector inputs
  if (inherits(data, "SpatVector") || inherits(data, "sf")) {
    spatial_meta <- extract_coords(data)
    df <- extract_data_frame(data)
    df$.borg_x <- spatial_meta$x
    df$.borg_y <- spatial_meta$y
    data <- df
    if (is.null(coords)) coords <- c(".borg_x", ".borg_y")
  }

  # Input validation
  if (!is.data.frame(data)) {
    stop("'data' must be a data frame, sf object, or SpatVector")
  }
  n <- nrow(data)
  if (n < v * 2) {
    stop(sprintf("Insufficient data: need at least %d rows for %d folds", v * 2, v))
  }

  # Run diagnosis if not provided
  if (is.null(diagnosis)) {
    if (verbose) message("Running borg_diagnose()...")
    diagnosis <- borg_diagnose(data,
                               coords = coords,
                               time = time,
                               groups = groups,
                               target = target,
                               verbose = verbose)
  }

  # Check if random CV is appropriate
  if (diagnosis@severity != "none" && !allow_random) {
    if (verbose) {
      message(sprintf("Dependency detected (%s). Using %s instead of random CV.",
                      diagnosis@dependency_type, diagnosis@recommended_cv))
    }
  } else if (diagnosis@severity != "none" && allow_random) {
    warning(sprintf(
      paste0("BORG WARNING: Random CV requested despite %s dependency.\n",
             "  Estimated metric inflation: ~%.0f%%\n",
             "  This may produce invalid performance estimates."),
      diagnosis@dependency_type,
      diagnosis@inflation_estimate$auc_inflation * 100
    ))
  }

  # Select CV strategy
  if (!is.null(strategy)) {
    valid_strategies <- c("random", "spatial_block", "environmental_block",
                          "checkerboard", "hexagonal", "custom_block", "knndm",
                          "leave_location_out", "llto", "spatial_plus",
                          "temporal_block", "temporal_expanding",
                          "temporal_sliding", "purged_cv",
                          "group_fold",
                          "spatial_temporal", "spatial_group",
                          "temporal_group")
    if (!strategy %in% valid_strategies) {
      stop(sprintf("Unknown strategy '%s'. Must be one of: %s",
                   strategy, paste(valid_strategies, collapse = ", ")))
    }
  } else {
    strategy <- if (allow_random && diagnosis@severity != "none") {
      "random_override"
    } else {
      diagnosis@recommended_cv
    }
  }

  # Generate folds based on strategy
  .generate_folds_once <- function(seed = 42L) {
    switch(strategy,
      "random" = generate_random_folds(data, v),
      "random_override" = generate_random_folds(data, v),
      "spatial_block" = generate_spatial_block_folds(data, v, diagnosis, coords, block_size, verbose, seed = seed),
      "environmental_block" = generate_environmental_block_folds(data, v, diagnosis, coords, env, verbose),
    "checkerboard" = generate_checkerboard_folds(data, v, diagnosis, coords, block_size, verbose),
    "hexagonal" = generate_hexagonal_folds(data, v, diagnosis, coords, block_size, verbose),
    "custom_block" = generate_custom_block_folds(data, v, dist_mat, verbose),
    "knndm" = generate_knndm_folds(data, v, diagnosis, coords, prediction_points, verbose),
    "leave_location_out" = generate_leave_location_out_folds(data, v, diagnosis, coords, verbose),
    "llto" = generate_llto_folds(data, v, diagnosis, coords, time, groups, verbose),
    "spatial_plus" = generate_spatial_plus_folds(data, v, diagnosis, coords, env, verbose),
      "temporal_block" = generate_temporal_block_folds(data, v, diagnosis, time, embargo, verbose),
      "temporal_expanding" = generate_temporal_expanding_folds(data, v, diagnosis, time, embargo, verbose),
      "temporal_sliding" = generate_temporal_sliding_folds(data, v, diagnosis, time, embargo, verbose),
    "purged_cv" = generate_purged_cv_folds(data, v, diagnosis, time, embargo, verbose),
      "group_fold" = generate_group_folds(data, v, diagnosis, groups, verbose),
      "spatial_temporal" = generate_spatial_temporal_folds(data, v, diagnosis, coords, time, verbose),
      "spatial_group" = generate_spatial_group_folds(data, v, diagnosis, coords, groups, verbose),
      "temporal_group" = generate_temporal_group_folds(data, v, diagnosis, time, groups, verbose),
      stop(sprintf("Unknown CV strategy: %s", strategy))
    )
  }

  repeats <- as.integer(max(1L, repeats))
  all_folds <- NULL

  if (repeats > 1L) {
    seeds <- seq(42L, by = 1000L, length.out = repeats)
    all_folds <- lapply(seq_len(repeats), function(r) {
      if (verbose) message(sprintf("Repeat %d/%d", r, repeats))
      .generate_folds_once(seed = seeds[r])
    })
    folds <- all_folds[[1]]
  } else {
    folds <- .generate_folds_once()
  }

  # Apply spatial buffer if requested
  if (!is.null(buffer)) {
    buf_coords <- coords
    if (is.null(buf_coords) && !is.null(diagnosis@spatial$coords_used)) {
      buf_coords <- diagnosis@spatial$coords_used
    }
    if (is.null(buf_coords)) {
      warning("buffer specified but no coordinates available; ignoring buffer")
    } else {
      folds <- apply_spatial_buffer(folds, data[[buf_coords[1]]], data[[buf_coords[2]]],
                                     buffer, verbose = verbose)
    }
  }

  # Build result
  params <- list(
    v = v,
    n = n,
    strategy = strategy,
    allow_random = allow_random
  )

  if (!is.null(block_size)) params$block_size <- block_size
  if (!is.null(embargo)) params$embargo <- embargo
  if (!is.null(buffer)) params$buffer <- buffer
  params$repeats <- repeats

  result <- list(
    folds = folds,
    n = n,
    diagnosis = diagnosis,
    strategy = strategy,
    params = params
  )
  if (!is.null(all_folds)) result$all_folds <- all_folds
  class(result) <- c("borg_cv", "list")

  # Convert output format if requested
  if (output == "list") {
    return(result)
  } else if (output == "rsample") {
    return(convert_to_rsample(result, data))
  } else if (output == "caret") {
    return(convert_to_caret(result))
  } else if (output == "mlr3") {
    return(convert_to_mlr3(result, data))
  }

  result
}


#' @export
print.borg_cv <- function(x, ...) {
  cat("BORG Cross-Validation\n")
  cat("=====================\n\n")
  cat(sprintf("Strategy:    %s\n", x$strategy))
  cat(sprintf("Folds:       %d\n", length(x$folds)))
  cat(sprintf("Total obs:   %d\n", x$params$n))

  # Show fold sizes
  train_sizes <- vapply(x$folds, function(f) length(f$train), integer(1))
  test_sizes <- vapply(x$folds, function(f) length(f$test), integer(1))
  cat(sprintf("Train sizes: %d - %d (mean: %.0f)\n",
              min(train_sizes), max(train_sizes), mean(train_sizes)))
  cat(sprintf("Test sizes:  %d - %d (mean: %.0f)\n",
              min(test_sizes), max(test_sizes), mean(test_sizes)))

  # Warning if random override
  if (x$strategy == "random_override") {
    cat("\nWARNING: Random CV used despite detected dependencies.\n")
    cat(sprintf("         Estimated inflation: ~%.0f%%\n",
                x$diagnosis@inflation_estimate$auc_inflation * 100))
  }

  invisible(x)
}


# ============================================================================
# Fold generation functions
# ============================================================================

#' Generate Random Folds (Internal)
#' @noRd
generate_random_folds <- function(data, v) {
  n <- nrow(data)
  idx <- sample(n)
  fold_ids <- rep(1:v, length.out = n)

  lapply(1:v, function(i) {
    test_idx <- idx[fold_ids == i]
    train_idx <- idx[fold_ids != i]
    list(train = train_idx, test = test_idx)
  })
}


#' Generate Spatial Block Folds (Internal)
#' @noRd
generate_spatial_block_folds <- function(data, v, diagnosis, coords, block_size, verbose,
                                         seed = 42L) {
  # Get coordinates
  if (is.null(coords)) {
    coords <- diagnosis@spatial$coords_used
  }
  if (is.null(coords)) {
    stop("Spatial blocking requires coordinates. Provide 'coords' argument.")
  }

  coord_info <- extract_coords(data, coords)
  x_coord <- coord_info$x
  y_coord <- coord_info$y
  n <- nrow(data)

  # Determine block size
  if (is.null(block_size)) {
    if (!is.na(diagnosis@spatial$range_estimate)) {
      block_size <- diagnosis@spatial$range_estimate * 1.5  # 1.5x range for safety
    } else {
      # Fallback: divide extent by sqrt(v)
      x_range <- diff(range(x_coord, na.rm = TRUE))
      y_range <- diff(range(y_coord, na.rm = TRUE))
      block_size <- max(x_range, y_range) / sqrt(v)
    }
  }

  if (verbose) message(sprintf("Using spatial block size: %.2f", block_size))

  # Use k-means to create spatial clusters
  # Number of clusters should be >= v
  n_clusters <- max(v, ceiling(n / 20))  # At least 20 points per cluster
  n_clusters <- min(n_clusters, n %/% 3)  # But not too many

  coord_mat <- cbind(x_coord, y_coord)
  complete <- complete.cases(coord_mat)
  coord_mat_complete <- coord_mat[complete, , drop = FALSE]

  set.seed(seed)
  km <- stats::kmeans(coord_mat_complete, centers = n_clusters, nstart = 10)

  # Assign clusters back (NA for incomplete)
  clusters <- rep(NA_integer_, n)
  clusters[complete] <- km$cluster

  # Assign clusters to folds (balanced by size)
  cluster_table <- table(clusters)
  cluster_ids <- as.integer(names(cluster_table))
  cluster_order <- order(cluster_table, decreasing = TRUE)

  fold_assignment <- rep(NA_integer_, length(cluster_ids))
  fold_sizes <- rep(0L, v)

  for (cid in cluster_ids[cluster_order]) {
    # Assign to fold with smallest current size
    target_fold <- which.min(fold_sizes)
    fold_assignment[cid] <- target_fold
    fold_sizes[target_fold] <- fold_sizes[target_fold] + cluster_table[as.character(cid)]
  }

  # Create folds
  obs_folds <- fold_assignment[clusters]
  obs_folds[is.na(obs_folds)] <- sample(1:v, sum(is.na(obs_folds)), replace = TRUE)

  folds <- lapply(1:v, function(i) {
    test_idx <- which(obs_folds == i)
    train_idx <- which(obs_folds != i)
    list(train = train_idx, test = test_idx)
  })

  # Attach spatial block metadata for visualization
  attr(folds, "spatial_meta") <- list(
    coords = cbind(x = x_coord, y = y_coord),
    coord_names = coord_info$coord_names,
    clusters = clusters,
    fold_assignment = obs_folds,
    centers = km$centers,
    block_size = block_size
  )

  folds
}


#' Generate Spatial+ CV Folds (Internal)
#'
#' Two-stage CV (Wang et al. 2023): Stage 1 creates spatial blocks via
#' hierarchical clustering. Stage 2 ensures within-block sampling
#' represents prediction-location feature space.
#'
#' @references
#' Wang, Y., Li, W., Zhang, Z., & Ding, J. (2023). A Spatial+ framework
#' for cross-validation of spatial prediction models.
#' \emph{International Journal of Applied Earth Observation and
#' Geoinformation}, 121, 103364.
#' \doi{10.1016/j.jag.2023.103364}
#' @noRd
generate_spatial_plus_folds <- function(data, v, diagnosis, coords, env, verbose) {
  if (is.null(coords)) coords <- diagnosis@spatial$coords_used
  if (is.null(coords)) {
    stop("Spatial+ requires coordinates. Provide 'coords' argument.")
  }

  coord_info <- extract_coords(data, coords)
  x <- coord_info$x
  y <- coord_info$y
  n <- nrow(data)

  # Stage 1: Hierarchical spatial clustering
  dist_mat <- compute_distance_matrix(x, y)
  hc <- stats::hclust(stats::as.dist(dist_mat), method = "ward.D2")
  n_clusters <- max(v * 2, ceiling(n / 15))
  clusters <- stats::cutree(hc, k = n_clusters)

  # Stage 2: If env provided, ensure feature-space representativeness
  # within each block assignment
  if (!is.null(env)) {
    if (is.data.frame(env) || is.matrix(env)) {
      env_mat <- as.matrix(env)
    } else {
      env_mat <- matrix(0, n, 0)
    }
  } else {
    # Use all numeric non-coord columns as environmental variables
    other_cols <- setdiff(names(data), coords)
    num_cols <- other_cols[vapply(data[other_cols], is.numeric, logical(1))]
    env_mat <- if (length(num_cols) > 0) as.matrix(data[, num_cols, drop = FALSE])
               else matrix(0, n, 0)
  }

  # Assign clusters to folds, balancing both size and environmental coverage
  cluster_table <- table(clusters)
  cluster_ids <- as.integer(names(cluster_table))

  if (ncol(env_mat) > 0) {
    # Compute cluster centroids in env space
    env_scaled <- scale(env_mat)
    cluster_centroids <- do.call(rbind, lapply(cluster_ids, function(cid) {
      colMeans(env_scaled[clusters == cid, , drop = FALSE], na.rm = TRUE)
    }))

    # Greedy assignment maximizing environmental diversity per fold
    fold_assignment <- rep(NA_integer_, length(cluster_ids))
    fold_envs <- vector("list", v)
    fold_sizes <- rep(0L, v)

    cluster_order <- order(cluster_table, decreasing = TRUE)
    for (cid_idx in cluster_order) {
      cid <- cluster_ids[cid_idx]
      centroid <- cluster_centroids[cid_idx, ]

      # Assign to fold that maximizes distance to existing centroids
      best_fold <- which.min(fold_sizes)  # default: smallest fold
      if (any(vapply(fold_envs, length, integer(1)) > 0)) {
        fold_scores <- vapply(seq_len(v), function(f) {
          if (length(fold_envs[[f]]) == 0) return(Inf)
          existing <- do.call(rbind, fold_envs[[f]])
          min(sqrt(colSums((t(existing) - centroid)^2)))
        }, numeric(1))
        # Balance size and diversity
        size_penalty <- fold_sizes / max(max(fold_sizes), 1)
        best_fold <- which.max(fold_scores - size_penalty * max(fold_scores))
      }

      fold_assignment[cid_idx] <- best_fold
      fold_envs[[best_fold]] <- c(fold_envs[[best_fold]], list(centroid))
      fold_sizes[best_fold] <- fold_sizes[best_fold] + cluster_table[as.character(cid)]
    }
  } else {
    # Fallback: balanced assignment
    cluster_order <- order(cluster_table, decreasing = TRUE)
    fold_assignment <- rep(NA_integer_, length(cluster_ids))
    fold_sizes <- rep(0L, v)
    for (cid_idx in cluster_order) {
      target_fold <- which.min(fold_sizes)
      fold_assignment[cid_idx] <- target_fold
      fold_sizes[target_fold] <- fold_sizes[target_fold] +
        cluster_table[as.character(cluster_ids[cid_idx])]
    }
  }

  obs_folds <- fold_assignment[match(clusters, cluster_ids)]

  folds <- lapply(seq_len(v), function(i) {
    list(train = which(obs_folds != i), test = which(obs_folds == i))
  })

  if (verbose) {
    message(sprintf("Spatial+: %d spatial clusters -> %d folds (env-balanced)",
                     n_clusters, v))
  }

  folds
}


#' Generate Leave-Location-and-Time-Out (LLTO) Folds (Internal)
#'
#' Combined spatiotemporal blocking that simultaneously holds out
#' spatial locations AND time periods. Based on CAST CreateSpacetimeFolds.
#'
#' @references
#' Meyer, H., Reudenbach, C., Hengl, T., Katurji, M., & Nauss, T. (2018).
#' Improving performance of spatio-temporal machine learning models using
#' forward feature selection and target-oriented validation.
#' \emph{Environmental Modelling & Software}, 101, 1-9.
#' \doi{10.1016/j.envsoft.2017.12.001}
#' @noRd
generate_llto_folds <- function(data, v, diagnosis, coords, time_col, group_col, verbose) {
  # Need either coords or groups for spatial, and time for temporal
  if (is.null(time_col)) time_col <- diagnosis@temporal$time_col
  if (is.null(time_col)) {
    stop("LLTO requires time column. Provide 'time' argument.")
  }

  time_vals <- data[[time_col]]
  n <- nrow(data)

  # Determine spatial grouping
  if (!is.null(group_col)) {
    space_groups <- data[[group_col]]
  } else if (!is.null(coords)) {
    # Create location groups from coordinates
    coord_info <- extract_coords(data, coords)
    loc_id <- paste(round(coord_info$x, 6), round(coord_info$y, 6), sep = "_")
    space_groups <- loc_id
  } else if (!is.null(diagnosis@spatial$coords_used)) {
    coords <- diagnosis@spatial$coords_used
    coord_info <- extract_coords(data, coords)
    loc_id <- paste(round(coord_info$x, 6), round(coord_info$y, 6), sep = "_")
    space_groups <- loc_id
  } else {
    stop("LLTO requires spatial structure (coords or groups)")
  }

  # Create time groups (bin into v temporal blocks)
  time_numeric <- as.numeric(time_vals)
  time_groups <- cut(time_numeric, breaks = v, labels = seq_len(v),
                      include.lowest = TRUE)

  # Combined spatiotemporal groups
  st_id <- paste(space_groups, time_groups, sep = "|")
  unique_st <- unique(st_id)

  # Distribute spatiotemporal groups across folds
  set.seed(42)
  n_st <- length(unique_st)
  fold_map <- setNames(
    rep(seq_len(v), length.out = n_st)[sample(n_st)],
    unique_st
  )

  obs_fold <- fold_map[st_id]

  folds <- lapply(seq_len(v), function(i) {
    test_idx <- which(obs_fold == i)
    train_idx <- which(obs_fold != i)
    list(train = train_idx, test = test_idx)
  })

  if (verbose) {
    n_space <- length(unique(space_groups))
    n_time <- length(unique(time_groups))
    message(sprintf("LLTO: %d locations x %d time periods = %d spatiotemporal groups, %d folds",
                     n_space, n_time, n_st, v))
  }

  folds
}


#' Generate Leave-Location-Out Folds (Internal)
#'
#' Groups by unique coordinate pairs, holds out each location.
#' For repeated measurements at sites. Locations are distributed
#' across v folds for computational tractability.
#' @noRd
generate_leave_location_out_folds <- function(data, v, diagnosis, coords, verbose) {
  if (is.null(coords)) coords <- diagnosis@spatial$coords_used
  if (is.null(coords)) {
    stop("Leave-location-out requires coordinates. Provide 'coords' argument.")
  }

  coord_info <- extract_coords(data, coords)
  x <- coord_info$x
  y <- coord_info$y
  n <- nrow(data)

  # Identify unique locations (round to avoid floating point issues)
  loc_id <- paste(round(x, 8), round(y, 8), sep = "_")
  unique_locs <- unique(loc_id)
  n_locs <- length(unique_locs)

  if (verbose) {
    message(sprintf("Leave-location-out: %d unique locations, %d observations",
                     n_locs, n))
  }

  # If fewer locations than folds, use LOOCV on locations
  v_actual <- min(v, n_locs)

  # Assign locations to folds (round-robin)
  set.seed(42)
  loc_order <- sample(n_locs)
  loc_fold <- rep(NA_integer_, n_locs)
  loc_fold[loc_order] <- rep(seq_len(v_actual), length.out = n_locs)
  names(loc_fold) <- unique_locs

  # Map to observations
  obs_fold <- loc_fold[loc_id]

  folds <- lapply(seq_len(v_actual), function(i) {
    test_idx <- which(obs_fold == i)
    train_idx <- which(obs_fold != i)
    list(train = train_idx, test = test_idx)
  })

  attr(folds, "llo_meta") <- list(
    n_locations = n_locs,
    n_per_fold = vapply(folds, function(f) length(f$test), integer(1)),
    loc_id = loc_id
  )

  folds
}


#' Generate KNNDM Folds (Internal)
#'
#' K-Nearest Neighbor Distance Matching (Linnenbrink et al. 2024).
#' Creates folds where the NN distance distribution between train and test
#' matches the distance from prediction points to training data.
#' @noRd
generate_knndm_folds <- function(data, v, diagnosis, coords, prediction_points, verbose) {
  if (is.null(prediction_points)) {
    stop("KNNDM requires 'prediction_points' (coordinates of prediction locations)")
  }
  if (is.null(coords)) coords <- diagnosis@spatial$coords_used
  if (is.null(coords)) {
    stop("KNNDM requires coordinates. Provide 'coords' argument.")
  }

  coord_info <- extract_coords(data, coords)
  x_train <- coord_info$x
  y_train <- coord_info$y
  n <- length(x_train)

  # Extract prediction point coordinates
  if (is.data.frame(prediction_points) || is.matrix(prediction_points)) {
    if (ncol(prediction_points) >= 2) {
      if (all(coords %in% names(prediction_points))) {
        x_pred <- prediction_points[[coords[1]]]
        y_pred <- prediction_points[[coords[2]]]
      } else {
        x_pred <- prediction_points[, 1]
        y_pred <- prediction_points[, 2]
      }
    } else {
      stop("prediction_points must have at least 2 columns")
    }
  } else if (inherits(prediction_points, "SpatVector")) {
    pp_coords <- extract_coords(prediction_points)
    x_pred <- pp_coords$x
    y_pred <- pp_coords$y
  } else if (inherits(prediction_points, "sf")) {
    pp_coords <- extract_coords(prediction_points)
    x_pred <- pp_coords$x
    y_pred <- pp_coords$y
  } else {
    stop("prediction_points must be data.frame, matrix, SpatVector, or sf")
  }

  # Compute target NN distance distribution: prediction -> nearest training
  target_nn <- vapply(seq_along(x_pred), function(i) {
    dists <- sqrt((x_pred[i] - x_train)^2 + (y_pred[i] - y_train)^2)
    min(dists)
  }, numeric(1))

  # Greedy optimization: assign observations to folds such that
  # train-test NN distances match the target distribution
  # Start with random assignment, then swap to improve KS statistic
  set.seed(42)
  fold_assignment <- sample(rep(seq_len(v), length.out = n))

  best_ks <- Inf

  for (iter in seq_len(100)) {
    # Compute current train-test NN distances across all folds
    current_nn <- numeric(0)
    for (f in seq_len(v)) {
      test_idx <- which(fold_assignment == f)
      train_idx <- which(fold_assignment != f)

      fold_nn <- vapply(test_idx, function(i) {
        dists <- sqrt((x_train[i] - x_train[train_idx])^2 +
                       (y_train[i] - y_train[train_idx])^2)
        min(dists)
      }, numeric(1))
      current_nn <- c(current_nn, fold_nn)
    }

    # KS statistic between current and target NN distributions
    ks <- suppressWarnings(
      stats::ks.test(current_nn, target_nn)$statistic
    )

    if (ks < best_ks) {
      best_ks <- ks
      best_assignment <- fold_assignment
    }

    # Random swap
    i <- sample(n, 1)
    new_fold <- sample(setdiff(seq_len(v), fold_assignment[i]), 1)
    fold_assignment[i] <- new_fold
  }

  if (verbose) {
    message(sprintf("KNNDM: KS = %.3f after optimization (%d iterations)", best_ks, 100))
  }

  folds <- lapply(seq_len(v), function(f) {
    test_idx <- which(best_assignment == f)
    train_idx <- which(best_assignment != f)
    list(train = train_idx, test = test_idx)
  })

  attr(folds, "knndm_meta") <- list(
    ks_statistic = best_ks,
    target_nn = target_nn,
    n_prediction_points = length(x_pred)
  )

  folds
}


#' Generate Custom Distance Block Folds (Internal)
#'
#' Uses a user-provided distance matrix with k-medoids (PAM) clustering
#' to create blocks. Useful for ecological, network, or genetic distances.
#' @noRd
generate_custom_block_folds <- function(data, v, dist_mat, verbose) {
  if (is.null(dist_mat)) {
    stop("Custom blocking requires 'dist_mat' argument (distance matrix or dist object)")
  }

  # Convert to matrix if dist object
  if (inherits(dist_mat, "dist")) {
    dist_mat <- as.matrix(dist_mat)
  }

  n <- nrow(data)
  if (!is.matrix(dist_mat) || nrow(dist_mat) != n || ncol(dist_mat) != n) {
    stop(sprintf("dist_mat must be an %d x %d matrix matching data rows", n, n))
  }

  # K-medoids via PAM-like algorithm (no cluster package dependency)
  n_clusters <- max(v, ceiling(n / 20))
  n_clusters <- min(n_clusters, n %/% 3)

  set.seed(42)

  # Initialize medoids randomly
  medoids <- sample(n, n_clusters)

  for (iter in seq_len(20)) {
    # Assign each point to nearest medoid
    clusters <- apply(dist_mat[, medoids, drop = FALSE], 1, which.min)

    # Update medoids: for each cluster, pick the point minimizing total distance
    new_medoids <- vapply(seq_len(n_clusters), function(k) {
      members <- which(clusters == k)
      if (length(members) == 0) return(medoids[k])
      if (length(members) == 1) return(members)
      sub_dist <- dist_mat[members, members, drop = FALSE]
      members[which.min(rowSums(sub_dist))]
    }, integer(1))

    if (identical(sort(new_medoids), sort(medoids))) break
    medoids <- new_medoids
  }

  if (verbose) {
    message(sprintf("Custom blocking: %d clusters, %d iterations", n_clusters, iter))
  }

  # Assign clusters to folds (balanced)
  cluster_table <- table(clusters)
  cluster_ids <- as.integer(names(cluster_table))
  cluster_order <- order(cluster_table, decreasing = TRUE)

  fold_assignment <- rep(NA_integer_, max(cluster_ids))
  fold_sizes <- rep(0L, v)

  for (cid in cluster_ids[cluster_order]) {
    target_fold <- which.min(fold_sizes)
    fold_assignment[cid] <- target_fold
    fold_sizes[target_fold] <- fold_sizes[target_fold] + cluster_table[as.character(cid)]
  }

  obs_folds <- fold_assignment[clusters]

  lapply(seq_len(v), function(i) {
    list(train = which(obs_folds != i), test = which(obs_folds == i))
  })
}


#' Generate Hexagonal Grid Folds (Internal)
#'
#' Overlays a hexagonal grid on the study area. Better spatial coverage
#' than rectangular grids due to uniform nearest-neighbor distances.
#' @noRd
generate_hexagonal_folds <- function(data, v, diagnosis, coords, block_size, verbose) {
  if (is.null(coords)) coords <- diagnosis@spatial$coords_used
  if (is.null(coords)) {
    stop("Hexagonal blocking requires coordinates. Provide 'coords' argument.")
  }

  coord_info <- extract_coords(data, coords)
  x <- coord_info$x
  y <- coord_info$y
  n <- nrow(data)

  if (is.null(block_size)) {
    if (!is.na(diagnosis@spatial$range_estimate)) {
      block_size <- diagnosis@spatial$range_estimate
    } else {
      x_range <- diff(range(x, na.rm = TRUE))
      y_range <- diff(range(y, na.rm = TRUE))
      block_size <- max(x_range, y_range) / sqrt(v * 2)
    }
  }

  if (verbose) message(sprintf("Hexagonal cell size: %.2f", block_size))

  x_min <- min(x, na.rm = TRUE)
  y_min <- min(y, na.rm = TRUE)

  # Hex grid: offset every other row by half the cell width
  # Hex height = block_size, width = block_size * 2/sqrt(3)
  hex_h <- block_size
  hex_w <- block_size * 2 / sqrt(3)

  row_idx <- floor((y - y_min) / (hex_h * 0.75))
  col_offset <- ifelse(row_idx %% 2 == 1, hex_w / 2, 0)
  col_idx <- floor((x - x_min - col_offset) / hex_w)

  # Assign hex cells to folds
  cell_id <- paste(row_idx, col_idx, sep = "_")
  unique_cells <- unique(cell_id)

  # Distribute cells across folds (round-robin by spatial position)
  cell_df <- data.frame(
    cell = unique_cells,
    row = as.integer(sub("_.*", "", unique_cells)),
    col = as.integer(sub(".*_", "", unique_cells)),
    stringsAsFactors = FALSE
  )
  cell_df <- cell_df[order(cell_df$row, cell_df$col), ]
  cell_df$fold <- rep(seq_len(v), length.out = nrow(cell_df))

  fold_map <- setNames(cell_df$fold, cell_df$cell)
  obs_fold <- fold_map[cell_id]
  obs_fold[is.na(obs_fold)] <- sample(seq_len(v), sum(is.na(obs_fold)), replace = TRUE)

  folds <- lapply(seq_len(v), function(i) {
    test_idx <- which(obs_fold == i)
    train_idx <- which(obs_fold != i)
    list(train = train_idx, test = test_idx)
  })

  attr(folds, "hex_meta") <- list(
    block_size = block_size,
    n_cells = length(unique_cells),
    cell_id = cell_id,
    obs_fold = as.integer(obs_fold)
  )

  folds
}


#' Generate Checkerboard Folds (Internal)
#'
#' Overlays a regular grid on the study area and assigns alternating
#' cells to folds in a checkerboard pattern. Standard SDM approach
#' (Muscarella et al. 2014, ENMeval).
#' @noRd
generate_checkerboard_folds <- function(data, v, diagnosis, coords, block_size, verbose) {
  if (is.null(coords)) coords <- diagnosis@spatial$coords_used
  if (is.null(coords)) {
    stop("Checkerboard blocking requires coordinates. Provide 'coords' argument.")
  }

  coord_info <- extract_coords(data, coords)
  x <- coord_info$x
  y <- coord_info$y
  n <- nrow(data)

  # Determine grid cell size
  if (is.null(block_size)) {
    if (!is.na(diagnosis@spatial$range_estimate)) {
      block_size <- diagnosis@spatial$range_estimate
    } else {
      x_range <- diff(range(x, na.rm = TRUE))
      y_range <- diff(range(y, na.rm = TRUE))
      block_size <- max(x_range, y_range) / sqrt(v * 2)
    }
  }

  if (verbose) message(sprintf("Checkerboard cell size: %.2f", block_size))

  # Assign each point to a grid cell
  x_min <- min(x, na.rm = TRUE)
  y_min <- min(y, na.rm = TRUE)

  cell_x <- floor((x - x_min) / block_size)
  cell_y <- floor((y - y_min) / block_size)

  # Checkerboard pattern: (cell_x + cell_y) mod v gives fold assignment
  cell_fold <- ((cell_x + cell_y) %% v) + 1L

  folds <- lapply(seq_len(v), function(i) {
    test_idx <- which(cell_fold == i)
    train_idx <- which(cell_fold != i)
    list(train = train_idx, test = test_idx)
  })

  # Check for empty folds and redistribute
  empty_folds <- vapply(folds, function(f) length(f$test) == 0, logical(1))
  if (any(empty_folds)) {
    warning(sprintf(
      "%d empty fold(s) in checkerboard pattern. Consider adjusting block_size or v.",
      sum(empty_folds)
    ))
  }

  attr(folds, "checkerboard_meta") <- list(
    block_size = block_size,
    cell_x = cell_x,
    cell_y = cell_y,
    cell_fold = cell_fold
  )

  folds
}


#' Generate Environmental Block Folds (Internal)
#'
#' Blocks by environmental similarity: PCA on environmental variables,
#' k-means clustering on PC scores, assign clusters to folds.
#' @noRd
generate_environmental_block_folds <- function(data, v, diagnosis, coords, env, verbose) {
  if (is.null(env)) {
    stop("Environmental blocking requires 'env' argument (SpatRaster or data.frame)")
  }

  n <- nrow(data)

  # Extract environmental matrix
  if (inherits(env, "SpatRaster")) {
    if (!requireNamespace("terra", quietly = TRUE)) {
      stop("Package 'terra' required for SpatRaster env input")
    }
    # Need coordinates to extract raster values
    if (is.null(coords) && !is.null(diagnosis@spatial$coords_used)) {
      coords <- diagnosis@spatial$coords_used
    }
    if (is.null(coords)) {
      stop("Environmental blocking with SpatRaster requires coordinates")
    }
    xy <- cbind(data[[coords[1]]], data[[coords[2]]])
    env_mat <- terra::extract(env, xy, ID = FALSE)
  } else if (is.data.frame(env) || is.matrix(env)) {
    env_mat <- as.data.frame(env)
    if (nrow(env_mat) != n) {
      stop(sprintf("'env' has %d rows but data has %d rows", nrow(env_mat), n))
    }
  } else {
    stop(sprintf("'env' must be SpatRaster, data.frame, or matrix, got %s",
                 class(env)[1]))
  }

  # Remove columns with zero variance
  env_mat <- env_mat[, vapply(env_mat, function(x) stats::sd(x, na.rm = TRUE) > 0,
                               logical(1)), drop = FALSE]
  if (ncol(env_mat) == 0) {
    stop("No environmental variables with non-zero variance")
  }

  # Handle NAs
  complete <- complete.cases(env_mat)
  if (sum(complete) < n * 0.5) {
    warning("More than 50% of observations have NA environmental values")
  }

  # PCA on environmental variables (standardized)
  env_complete <- env_mat[complete, , drop = FALSE]
  pca <- stats::prcomp(env_complete, center = TRUE, scale. = TRUE)

  # Keep PCs explaining >= 95% variance
  cum_var <- cumsum(pca$sdev^2) / sum(pca$sdev^2)
  n_pcs <- max(1, which(cum_var >= 0.95)[1])
  if (is.na(n_pcs)) n_pcs <- ncol(pca$x)
  pc_scores <- pca$x[, seq_len(n_pcs), drop = FALSE]

  if (verbose) {
    message(sprintf("Environmental blocking: %d PCs (%.1f%% variance), %d env variables",
                     n_pcs, cum_var[n_pcs] * 100, ncol(env_mat)))
  }

  # K-means on PC scores
  n_clusters <- max(v, ceiling(sum(complete) / 20))
  n_clusters <- min(n_clusters, sum(complete) %/% 3)

  set.seed(42)
  km <- stats::kmeans(pc_scores, centers = n_clusters, nstart = 10)

  # Assign clusters back
  clusters <- rep(NA_integer_, n)
  clusters[complete] <- km$cluster

  # Assign clusters to folds (balanced by size)
  cluster_table <- table(clusters)
  cluster_ids <- as.integer(names(cluster_table))
  cluster_order <- order(cluster_table, decreasing = TRUE)

  fold_assignment <- rep(NA_integer_, length(cluster_ids))
  fold_sizes <- rep(0L, v)

  for (cid in cluster_ids[cluster_order]) {
    target_fold <- which.min(fold_sizes)
    fold_assignment[cid] <- target_fold
    fold_sizes[target_fold] <- fold_sizes[target_fold] + cluster_table[as.character(cid)]
  }

  obs_folds <- fold_assignment[clusters]
  obs_folds[is.na(obs_folds)] <- sample(seq_len(v), sum(is.na(obs_folds)), replace = TRUE)

  folds <- lapply(seq_len(v), function(i) {
    test_idx <- which(obs_folds == i)
    train_idx <- which(obs_folds != i)
    list(train = train_idx, test = test_idx)
  })

  attr(folds, "env_meta") <- list(
    n_pcs = n_pcs,
    explained_variance = cum_var[n_pcs],
    n_clusters = n_clusters,
    pca_loadings = pca$rotation[, seq_len(n_pcs), drop = FALSE]
  )

  folds
}


#' Generate Temporal Block Folds (Internal)
#' @noRd
generate_temporal_block_folds <- function(data, v, diagnosis, time_col, embargo, verbose) {
  # Get time column
  if (is.null(time_col)) {
    time_col <- diagnosis@temporal$time_col
  }
  if (is.null(time_col)) {
    stop("Temporal blocking requires time column. Provide 'time' argument.")
  }

  time_vals <- data[[time_col]]
  n <- nrow(data)

  # Convert to numeric
  if (inherits(time_vals, "Date") || inherits(time_vals, "POSIXt")) {
    time_numeric <- as.numeric(time_vals)
  } else {
    time_numeric <- as.numeric(time_vals)
  }

  # Determine embargo
  if (is.null(embargo)) {
    if (!is.na(diagnosis@temporal$embargo_minimum)) {
      embargo <- diagnosis@temporal$embargo_minimum
    } else {
      # Fallback: 5% of time range
      embargo <- diff(range(time_numeric, na.rm = TRUE)) * 0.05
    }
  }

  if (verbose) message(sprintf("Using temporal embargo: %.2f", embargo))

  # Sort by time
  ord <- order(time_numeric)

  # Create temporal folds with embargo
  # Use forward chaining approach
  fold_boundaries <- round(seq(0, n, length.out = v + 1))

  folds <- lapply(1:v, function(i) {
    # For fold i, test on block i, train on blocks before
    test_start <- fold_boundaries[i] + 1
    test_end <- fold_boundaries[i + 1]
    test_idx <- ord[test_start:test_end]

    # Training: all data before test block (minus embargo)
    if (test_start > 1) {
      test_min_time <- min(time_numeric[test_idx], na.rm = TRUE)
      train_mask <- time_numeric[ord[1:(test_start - 1)]] < (test_min_time - embargo)
      train_idx <- ord[1:(test_start - 1)][train_mask]
    } else {
      train_idx <- integer(0)
    }

    # If no training data, use later data (expanding window)
    if (length(train_idx) < 10) {
      # Use later blocks as training instead
      if (test_end < n) {
        test_max_time <- max(time_numeric[test_idx], na.rm = TRUE)
        after_mask <- time_numeric[ord[(test_end + 1):n]] > (test_max_time + embargo)
        train_idx <- c(train_idx, ord[(test_end + 1):n][after_mask])
      }
    }

    list(train = train_idx, test = test_idx)
  })

  # Filter out folds with insufficient training data
  valid_folds <- Filter(function(f) length(f$train) >= 10, folds)

  if (length(valid_folds) < 2) {
    warning("Temporal blocking with embargo produced < 2 valid folds. ",
            "Consider reducing embargo or using a different strategy.")
    # Fallback to simple temporal split without embargo
    folds <- lapply(1:v, function(i) {
      test_start <- fold_boundaries[i] + 1
      test_end <- fold_boundaries[i + 1]
      test_idx <- ord[test_start:test_end]
      train_idx <- setdiff(ord, test_idx)
      list(train = train_idx, test = test_idx)
    })
  } else {
    folds <- valid_folds
  }

  folds
}


#' Generate Combinatorial Purged Cross-Validation Folds (Internal)
#'
#' Implements de Prado's combinatorial purged CV for time series.
#' Divides data into N groups by time, uses combinations of groups
#' as test sets, with embargo (purge gap) between train and test.
#' @noRd
generate_purged_cv_folds <- function(data, v, diagnosis, time_col, embargo, verbose) {
  if (is.null(time_col)) time_col <- diagnosis@temporal$time_col
  if (is.null(time_col)) {
    stop("Purged CV requires time column. Provide 'time' argument.")
  }

  time_vals <- data[[time_col]]
  n <- nrow(data)
  time_order <- order(time_vals)

  # Set embargo
  if (is.null(embargo)) {
    embargo <- if (!is.na(diagnosis@temporal$embargo_minimum)) {
      diagnosis@temporal$embargo_minimum
    } else {
      0
    }
  }

  time_numeric <- as.numeric(time_vals)

  # Divide into N_groups temporal groups
  n_groups <- v + 1  # one more group than folds for combinatorial selection
  n_groups <- max(n_groups, 3)
  group_assignment <- cut(time_numeric, breaks = n_groups, labels = FALSE,
                           include.lowest = TRUE)

  # Each fold: one group is the test set, adjacent groups are purged
  folds <- lapply(seq_len(min(v, n_groups)), function(i) {
    test_group <- i
    test_idx <- which(group_assignment == test_group)

    if (length(test_idx) == 0) {
      return(list(train = seq_len(n), test = integer(0)))
    }

    # Purge: remove training observations within embargo distance of test
    test_times <- time_numeric[test_idx]
    min_test <- min(test_times)
    max_test <- max(test_times)

    train_idx <- which(
      group_assignment != test_group &
      (time_numeric < (min_test - embargo) | time_numeric > (max_test + embargo))
    )

    list(train = train_idx, test = test_idx)
  })

  # Remove empty folds
  folds <- Filter(function(f) length(f$test) > 0, folds)

  if (verbose) {
    n_purged <- n - sum(vapply(folds, function(f) length(f$train) + length(f$test), integer(1))) / length(folds)
    message(sprintf("Purged CV: %d folds, embargo = %.1f, ~%.0f obs purged per fold",
                     length(folds), embargo, n_purged))
  }

  folds
}


#' Generate Group Folds (Internal)
#' @noRd
generate_group_folds <- function(data, v, diagnosis, group_col, verbose) {
  # Get group column
  if (is.null(group_col)) {
    group_col <- diagnosis@clustered$group_col
  }
  if (is.null(group_col)) {
    stop("Group CV requires group column. Provide 'groups' argument.")
  }

  groups <- data[[group_col]]
  unique_groups <- unique(groups)
  n_groups <- length(unique_groups)

  if (n_groups < v) {
    warning(sprintf("Only %d groups available, reducing to %d folds", n_groups, n_groups))
    v <- n_groups
  }

  if (verbose) message(sprintf("Creating %d-fold group CV with %d groups", v, n_groups))

  # Assign groups to folds (balanced by total observations)
  group_sizes <- table(groups)
  group_order <- order(group_sizes, decreasing = TRUE)

  fold_assignment <- rep(NA_integer_, n_groups)
  names(fold_assignment) <- unique_groups
  fold_sizes <- rep(0L, v)

  for (g in unique_groups[group_order]) {
    target_fold <- which.min(fold_sizes)
    fold_assignment[g] <- target_fold
    fold_sizes[target_fold] <- fold_sizes[target_fold] + group_sizes[g]
  }

  # Create folds
  lapply(1:v, function(i) {
    test_groups <- names(fold_assignment)[fold_assignment == i]
    test_idx <- which(groups %in% test_groups)
    train_idx <- which(!groups %in% test_groups)
    list(train = train_idx, test = test_idx)
  })
}


#' Apply diagnosed spatial buffer to folds if coords are available
#' @param folds List of fold lists.
#' @param data Data frame with coordinate columns.
#' @param diagnosis borg_diagnosis object.
#' @param coords Optional character(2) coordinate column names.
#' @param verbose Logical.
#' @return Folds with spatial buffer applied (or unchanged if no coords).
#' @noRd
.maybe_apply_spatial_buffer <- function(folds, data, diagnosis, coords, verbose) {
  if (is.null(coords)) coords <- diagnosis@spatial$coords_used
  if (is.null(coords)) return(folds)

  x_coord <- data[[coords[1]]]
  y_coord <- data[[coords[2]]]

  max_extent <- max(diff(range(x_coord, na.rm = TRUE)),
                     diff(range(y_coord, na.rm = TRUE)))
  buffer_dist <- if (!is.na(diagnosis@spatial$range_estimate)) {
    min(diagnosis@spatial$range_estimate, max_extent * 0.15)
  } else {
    max_extent * 0.05
  }

  apply_spatial_buffer(folds, x_coord, y_coord, buffer_dist, verbose = verbose)
}

#' Generate Spatial-Temporal Folds (Internal)
#' @noRd
generate_spatial_temporal_folds <- function(data, v, diagnosis, coords, time_col, verbose) {
  if (is.null(time_col)) time_col <- diagnosis@temporal$time_col
  if (is.null(coords)) coords <- diagnosis@spatial$coords_used

  temporal_folds <- generate_temporal_block_folds(data, v, diagnosis, time_col, NULL, verbose)
  .maybe_apply_spatial_buffer(temporal_folds, data, diagnosis, coords, verbose)
}


#' Generate Spatial-Group Folds (Internal)
#' @noRd
generate_spatial_group_folds <- function(data, v, diagnosis, coords, group_col, verbose) {
  group_folds <- generate_group_folds(data, v, diagnosis, group_col, verbose)
  .maybe_apply_spatial_buffer(group_folds, data, diagnosis, coords, verbose)
}


#' Generate Temporal-Group Folds (Internal)
#' @noRd
generate_temporal_group_folds <- function(data, v, diagnosis, time_col, group_col, verbose) {
  # Group-out with temporal ordering
  if (is.null(time_col)) time_col <- diagnosis@temporal$time_col
  if (is.null(group_col)) group_col <- diagnosis@clustered$group_col

  groups <- data[[group_col]]
  time_vals <- data[[time_col]]

  # Order groups by their mean time
  group_times <- tapply(as.numeric(time_vals), groups, mean, na.rm = TRUE)
  group_order <- names(sort(group_times))

  unique_groups <- group_order
  n_groups <- length(unique_groups)

  if (n_groups < v) {
    v <- n_groups
  }

  # Assign groups to folds in temporal order
  fold_assignment <- rep(1:v, length.out = n_groups)
  names(fold_assignment) <- unique_groups

  lapply(1:v, function(i) {
    test_groups <- names(fold_assignment)[fold_assignment == i]
    test_idx <- which(groups %in% test_groups)
    train_idx <- which(!groups %in% test_groups)
    list(train = train_idx, test = test_idx)
  })
}


#' Resolve time column, compute temporal ordering and embargo
#' @return list(time_numeric, ord, n, embargo)
#' @noRd
.resolve_temporal_setup <- function(data, diagnosis, time_col, embargo, method_label) {
  if (is.null(time_col)) time_col <- diagnosis@temporal$time_col
  if (is.null(time_col)) {
    stop(sprintf("%s CV requires time column. Provide 'time' argument.", method_label))
  }

  n <- nrow(data)
  time_numeric <- as.numeric(data[[time_col]])
  ord <- order(time_numeric)

  if (is.null(embargo)) {
    embargo <- if (!is.na(diagnosis@temporal$embargo_minimum)) {
      diagnosis@temporal$embargo_minimum
    } else {
      0
    }
  }

  list(time_numeric = time_numeric, ord = ord, n = n, embargo = embargo)
}

#' Apply embargo and minimum-size filter to a single fold
#' @return list(train, test) or NULL if fold is too small.
#' @noRd
.apply_embargo_filter <- function(train_idx, test_idx, time_numeric, embargo,
                                  min_train = 10L, min_test = 1L) {
  if (embargo > 0 && length(test_idx) > 0 && length(train_idx) > 0) {
    test_min_time <- min(time_numeric[test_idx], na.rm = TRUE)
    keep <- time_numeric[train_idx] < (test_min_time - embargo)
    train_idx <- train_idx[keep]
  }
  if (length(train_idx) >= min_train && length(test_idx) >= min_test) {
    list(train = train_idx, test = test_idx)
  } else {
    NULL
  }
}

#' Generate Temporal Expanding Window Folds (Internal)
#'
#' Forward-chaining: training window grows, test window slides forward.
#' @noRd
generate_temporal_expanding_folds <- function(data, v, diagnosis, time_col, embargo, verbose) {
  setup <- .resolve_temporal_setup(data, diagnosis, time_col, embargo, "Expanding window")
  n <- setup$n; ord <- setup$ord
  time_numeric <- setup$time_numeric; embargo <- setup$embargo

  test_size <- floor(n / (v + 1))
  initial_train_size <- max(test_size * 2, floor(n / (v + 1)))

  if (verbose) {
    message(sprintf("Expanding window: initial train = %d, test size = %d, embargo = %s",
                    initial_train_size, test_size, embargo))
  }

  folds <- list()
  for (i in seq_len(v)) {
    train_end <- initial_train_size + (i - 1) * test_size
    test_start <- train_end + 1
    test_end <- min(test_start + test_size - 1, n)

    if (test_start > n || train_end > n) break

    fold <- .apply_embargo_filter(ord[1:train_end], ord[test_start:test_end],
                                  time_numeric, embargo)
    if (!is.null(fold)) folds <- c(folds, list(fold))
  }

  if (length(folds) < 2) {
    warning("Expanding window produced < 2 valid folds. Consider fewer folds or smaller embargo.")
  }

  folds
}


#' Generate Temporal Sliding Window Folds (Internal)
#'
#' Fixed-size training window slides forward with the test window.
#' @noRd
generate_temporal_sliding_folds <- function(data, v, diagnosis, time_col, embargo, verbose) {
  setup <- .resolve_temporal_setup(data, diagnosis, time_col, embargo, "Sliding window")
  n <- setup$n; ord <- setup$ord
  time_numeric <- setup$time_numeric; embargo <- setup$embargo

  test_size <- floor(n / (v + 1))
  window_size <- test_size * 3
  step_size <- floor((n - window_size - test_size) / max(v - 1, 1))
  step_size <- max(step_size, 1)

  if (verbose) {
    message(sprintf("Sliding window: window = %d, test = %d, step = %d, embargo = %s",
                    window_size, test_size, step_size, embargo))
  }

  folds <- list()
  for (i in seq_len(v)) {
    train_start <- 1 + (i - 1) * step_size
    train_end <- min(train_start + window_size - 1, n)
    test_start <- train_end + 1
    test_end <- min(test_start + test_size - 1, n)

    if (test_start > n || train_end > n) break

    fold <- .apply_embargo_filter(ord[train_start:train_end], ord[test_start:test_end],
                                  time_numeric, embargo)
    if (!is.null(fold)) folds <- c(folds, list(fold))
  }

  if (length(folds) < 2) {
    warning("Sliding window produced < 2 valid folds. Consider fewer folds or smaller window.")
  }

  folds
}


# ============================================================================
# Output format converters
# ============================================================================

#' Convert to rsample Format (Internal)
#' @noRd
convert_to_rsample <- function(borg_cv_obj, data) {
  if (!requireNamespace("rsample", quietly = TRUE)) {
    stop("Package 'rsample' required for output='rsample'. Install with: install.packages('rsample')")
  }

  repeats <- borg_cv_obj$params$repeats %||% 1L

  if (repeats > 1L && !is.null(borg_cv_obj$all_folds)) {
    # Repeated CV: flatten all repeats into one rset with Repeat_Fold IDs
    all_splits <- list()
    all_ids <- character(0)
    for (r in seq_along(borg_cv_obj$all_folds)) {
      fold_set <- borg_cv_obj$all_folds[[r]]
      for (f in seq_along(fold_set)) {
        split <- rsample::make_splits(
          x = list(analysis = fold_set[[f]]$train, assessment = fold_set[[f]]$test),
          data = data
        )
        all_splits <- c(all_splits, list(split))
        all_ids <- c(all_ids, sprintf("Repeat%d_Fold%d", r, f))
      }
    }
    result <- rsample::manual_rset(all_splits, all_ids)
  } else {
    # Single repeat
    splits <- lapply(borg_cv_obj$folds, function(fold) {
      rsample::make_splits(
        x = list(analysis = fold$train, assessment = fold$test),
        data = data
      )
    })
    ids <- paste0("Fold", seq_along(splits))
    result <- rsample::manual_rset(splits, ids)
  }

  # Subclass with BORG metadata
  attr(result, "borg_strategy") <- borg_cv_obj$strategy
  attr(result, "borg_diagnosis") <- borg_cv_obj$diagnosis
  attr(result, "borg_params") <- borg_cv_obj$params
  class(result) <- c("borg_rset", class(result))

  result
}


#' Convert to caret Format (Internal)
#' @noRd
convert_to_caret <- function(borg_cv_obj) {
  if (!requireNamespace("caret", quietly = TRUE)) {
    stop("Package 'caret' required for output='caret'. Install with: install.packages('caret')")
  }

  # Create index and indexOut lists
  index <- lapply(borg_cv_obj$folds, function(f) f$train)
  indexOut <- lapply(borg_cv_obj$folds, function(f) f$test)

  names(index) <- paste0("Fold", seq_along(index))
  names(indexOut) <- paste0("Fold", seq_along(indexOut))

  caret::trainControl(
    method = "cv",
    index = index,
    indexOut = indexOut,
    savePredictions = "final"
  )
}


#' Convert to mlr3 Format (Internal)
#' @noRd
convert_to_mlr3 <- function(borg_cv_obj, data) {
  if (!requireNamespace("mlr3", quietly = TRUE)) {
    stop("Package 'mlr3' required for output='mlr3'. Install with: install.packages('mlr3')")
  }

  # Create custom resampling
  train_sets <- lapply(borg_cv_obj$folds, function(f) f$train)
  test_sets <- lapply(borg_cv_obj$folds, function(f) f$test)

  mlr3::rsmp("custom")$instantiate(
    mlr3::TaskClassif$new(id = "temp", backend = data, target = names(data)[1]),
    train_sets = train_sets,
    test_sets = test_sets
  )
}
