# Temporal and clustered structure diagnostics for borg_diagnose()

#' Diagnose Temporal Autocorrelation (Internal)
#' @noRd
diagnose_temporal <- function(data, time_col, y, alpha, verbose) {
  time_vals <- data[[time_col]]
  n <- length(time_vals)

  # Convert to numeric if needed
  if (inherits(time_vals, "Date")) {
    time_numeric <- as.numeric(time_vals)
  } else if (inherits(time_vals, "POSIXt")) {
    time_numeric <- as.numeric(time_vals)
  } else {
    time_numeric <- as.numeric(time_vals)
  }

  # Sort by time
  ord <- order(time_numeric)
  time_numeric <- time_numeric[ord]
  if (!is.null(y)) y <- y[ord]

  # Remove NA
  complete <- complete.cases(time_numeric)
  if (!is.null(y)) complete <- complete & complete.cases(y)

  time_numeric <- time_numeric[complete]
  if (!is.null(y)) y <- y[complete]
  n_complete <- length(time_numeric)

  if (n_complete < 20) {
    return(list(detected = FALSE, reason = "Insufficient observations for temporal analysis"))
  }

  # If no target, check for temporal clustering in observations
  if (is.null(y)) {
    # Check if observations are evenly spaced or clustered
    time_diffs <- diff(time_numeric)
    cv_diffs <- sd(time_diffs) / mean(time_diffs)
    detected <- cv_diffs > 1  # High CV indicates clustering

    return(list(
      detected = detected,
      acf_lag1 = NA_real_,
      ljung_box_p = NA_real_,
      decorrelation_lag = NA_integer_,
      embargo_minimum = if (detected) as.integer(ceiling(median(time_diffs) * 3)) else 1L,
      time_col = time_col
    ))
  }

  # Compute ACF
  max_lag <- min(n_complete - 1, 20)
  acf_vals <- stats::acf(y, lag.max = max_lag, plot = FALSE)$acf[-1]  # Remove lag 0

  # Ljung-Box test
  lb_test <- tryCatch(
    stats::Box.test(y, lag = min(10, max_lag), type = "Ljung-Box"),
    error = function(e) list(p.value = 1)
  )

  # Find decorrelation lag (first lag where |ACF| < threshold)
  threshold <- 2 / sqrt(n_complete)  # Standard significance threshold
  decorr_lag <- which(abs(acf_vals) < threshold)[1]
  if (is.na(decorr_lag)) decorr_lag <- max_lag

  # Compute embargo minimum based on time scale
  time_step <- median(diff(time_numeric))
  embargo_minimum <- as.integer(ceiling(decorr_lag * time_step))

  detected <- lb_test$p.value < alpha && abs(acf_vals[1]) > 0.2

  list(
    detected = detected,
    acf_lag1 = acf_vals[1],
    ljung_box_p = lb_test$p.value,
    decorrelation_lag = as.integer(decorr_lag),
    embargo_minimum = embargo_minimum,
    time_col = time_col
  )
}


#' Diagnose Clustered Structure (Internal)
#' @noRd
diagnose_clustered <- function(data, group_col, y, alpha, verbose) {
  groups <- data[[group_col]]
  n <- length(groups)

  # Get cluster info
  cluster_table <- table(groups)
  n_clusters <- length(cluster_table)
  cluster_sizes <- as.numeric(cluster_table)
  mean_cluster_size <- mean(cluster_sizes)

  if (n_clusters < 3) {
    return(list(
      detected = TRUE,  # With < 3 clusters, definitely need group CV
      icc = NA_real_,
      n_clusters = as.integer(n_clusters),
      cluster_sizes = cluster_sizes,
      design_effect = NA_real_,
      group_col = group_col,
      reason = "Too few clusters to estimate ICC, but group CV required"
    ))
  }

  if (n_clusters == n) {
    # Each observation is its own cluster - no clustering
    return(list(
      detected = FALSE,
      icc = 0,
      n_clusters = as.integer(n_clusters),
      cluster_sizes = cluster_sizes,
      design_effect = 1,
      group_col = group_col
    ))
  }

  # Compute ICC
  if (!is.null(y)) {
    icc <- compute_icc(y, groups)
  } else {
    # If no target, estimate ICC from all numeric columns
    numeric_cols <- names(data)[vapply(data, is.numeric, logical(1))]
    numeric_cols <- setdiff(numeric_cols, group_col)

    if (length(numeric_cols) == 0) {
      icc <- NA_real_
    } else {
      iccs <- vapply(numeric_cols, function(col) {
        compute_icc(data[[col]], groups)
      }, numeric(1))
      icc <- mean(iccs, na.rm = TRUE)
    }
  }

  # Design effect: DEFF = 1 + (m - 1) * ICC
  if (!is.na(icc)) {
    deff <- 1 + (mean_cluster_size - 1) * icc
  } else {
    deff <- NA_real_
  }

  # ICC > 0.05 is meaningful clustering
  detected <- !is.na(icc) && icc > 0.05

  list(
    detected = detected,
    icc = icc,
    n_clusters = as.integer(n_clusters),
    cluster_sizes = cluster_sizes,
    design_effect = deff,
    group_col = group_col
  )
}


#' Compute Intraclass Correlation Coefficient (Internal)
#' @noRd
compute_icc <- function(y, groups) {
  if (length(unique(groups)) < 2) return(NA_real_)

  # ANOVA-based ICC(1)
  # ICC = (MSB - MSW) / (MSB + (k-1)*MSW)
  # where k is average group size

  group_means <- tapply(y, groups, mean, na.rm = TRUE)
  grand_mean <- mean(y, na.rm = TRUE)

  group_sizes <- table(groups)
  k <- length(group_sizes)
  n <- length(y)
  m <- n / k  # Average group size

  # Between-group sum of squares
  SSB <- sum(group_sizes * (group_means - grand_mean)^2)
  dfB <- k - 1

  # Within-group sum of squares
  SSW <- sum((y - group_means[as.character(groups)])^2, na.rm = TRUE)
  dfW <- n - k

  if (dfB == 0 || dfW == 0) return(NA_real_)

  MSB <- SSB / dfB
  MSW <- SSW / dfW

  if (MSB + (m - 1) * MSW == 0) return(NA_real_)

  icc <- (MSB - MSW) / (MSB + (m - 1) * MSW)

  # Bound to [0, 1]
  max(0, min(1, icc))
}
