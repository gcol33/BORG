# BORG Diagnosis Module
# Detects data dependency structure: spatial, temporal, clustered

#' @title BorgDiagnosis S4 Class
#'
#' @description
#' Holds the result of \code{\link{borg_diagnose}}: a structured assessment
#' of data dependency patterns that affect cross-validation validity.
#'
#' @name BorgDiagnosis
#' @docType class
#' @aliases BorgDiagnosis-class
#'
#' @slot dependency_type Character. Primary dependency type detected:
#'   "none", "spatial", "temporal", "clustered", or "mixed".
#' @slot severity Character. Overall severity: "none", "moderate", "severe".
#' @slot recommended_cv Character. Recommended CV strategy:
#'   "random", "spatial_block", "temporal_block", "group_fold", "spatial_temporal".
#' @slot spatial List. Spatial autocorrelation diagnostics with elements:
#'   detected (logical), morans_i (numeric), morans_p (numeric),
#'   range_estimate (numeric), effective_n (numeric), coords_used (character).
#' @slot temporal List. Temporal autocorrelation diagnostics with elements:
#'   detected (logical), acf_lag1 (numeric), ljung_box_p (numeric),
#'   decorrelation_lag (integer), embargo_minimum (integer), time_col (character).
#' @slot clustered List. Clustered structure diagnostics with elements:
#'   detected (logical), icc (numeric), n_clusters (integer),
#'   cluster_sizes (numeric), design_effect (numeric), group_col (character).
#' @slot inflation_estimate List. Estimated metric inflation from random CV with
#'   elements: auc_inflation (numeric, proportion), rmse_deflation (numeric),
#'   confidence (character: "low"/"medium"/"high"), basis (character).
#' @slot n_obs Integer. Number of observations in the dataset.
#' @slot timestamp POSIXct. When the diagnosis was performed.
#' @slot call Language object. The original call that triggered diagnosis.
#'
#' @seealso \code{\link{borg_diagnose}}, \code{\link{borg_cv}}
#'
#' @importFrom methods new
#' @importFrom stats quantile
#' @export
setClass(
  "BorgDiagnosis",
 slots = list(
   dependency_type     = "character",
   severity            = "character",
   recommended_cv      = "character",
   spatial             = "list",
   temporal            = "list",
   clustered           = "list",
   inflation_estimate  = "list",
   n_obs               = "integer",
   timestamp           = "POSIXct",
   call                = "language"
 ),
 validity = function(object) {
   valid_deps <- c("none", "spatial", "temporal", "clustered", "mixed")
   if (!object@dependency_type %in% valid_deps) {
     return(sprintf("dependency_type must be one of: %s", paste(valid_deps, collapse = ", ")))
   }

   valid_severity <- c("none", "moderate", "severe")
   if (!object@severity %in% valid_severity) {
     return(sprintf("severity must be one of: %s", paste(valid_severity, collapse = ", ")))
   }

   valid_cv <- c("random", "spatial_block", "temporal_block", "group_fold",
                 "spatial_temporal", "spatial_group", "temporal_group")
   if (!object@recommended_cv %in% valid_cv) {
     return(sprintf("recommended_cv must be one of: %s", paste(valid_cv, collapse = ", ")))
   }

   TRUE
 }
)


#' @rdname BorgDiagnosis
#' @aliases show,BorgDiagnosis-method
#' @importMethodsFrom methods show
#' @exportMethod show
#' @param object A \code{BorgDiagnosis} object to be printed.
setMethod("show", "BorgDiagnosis", function(object) {
  cat("BORG Dependency Diagnosis\n")
  cat("=========================\n\n")

  # Header summary
  severity_color <- switch(object@severity,
    "none" = "NONE",
    "moderate" = "MODERATE",
    "severe" = "SEVERE"
  )
  cat(sprintf("Dependency Type: %s\n", toupper(object@dependency_type)))
  cat(sprintf("Severity:        %s\n", severity_color))
  cat(sprintf("Recommended CV:  %s\n", object@recommended_cv))
  cat(sprintf("Observations:    %d\n", object@n_obs))

  # Spatial details
  if (object@spatial$detected) {
    cat("\n--- Spatial Autocorrelation ---\n")
    cat(sprintf("  Moran's I:     %.3f (p = %.4f)\n",
                object@spatial$morans_i, object@spatial$morans_p))
    cat(sprintf("  Range:         %.2f (data units)\n", object@spatial$range_estimate))
    cat(sprintf("  Effective N:   %.1f (vs %d actual)\n",
                object@spatial$effective_n, object@n_obs))
    cat(sprintf("  Coordinates:   %s\n", paste(object@spatial$coords_used, collapse = ", ")))
  }

  # Temporal details
  if (object@temporal$detected) {
    cat("\n--- Temporal Autocorrelation ---\n")
    cat(sprintf("  ACF lag-1:     %.3f\n", object@temporal$acf_lag1))
    cat(sprintf("  Ljung-Box p:   %.4f\n", object@temporal$ljung_box_p))
    cat(sprintf("  Decorr. lag:   %d\n", object@temporal$decorrelation_lag))
    cat(sprintf("  Min embargo:   %d time units\n", object@temporal$embargo_minimum))
    cat(sprintf("  Time column:   %s\n", object@temporal$time_col))
  }

  # Clustered details
  if (object@clustered$detected) {
    cat("\n--- Clustered Structure ---\n")
    cat(sprintf("  ICC:           %.3f\n", object@clustered$icc))
    cat(sprintf("  Clusters:      %d\n", object@clustered$n_clusters))
    cat(sprintf("  Design effect: %.2f\n", object@clustered$design_effect))
    cat(sprintf("  Group column:  %s\n", object@clustered$group_col))
  }

  # Inflation estimate
  if (!is.null(object@inflation_estimate$auc_inflation)) {
    cat("\n--- Estimated Random CV Bias ---\n")
    if (!is.na(object@inflation_estimate$auc_inflation)) {
      cat(sprintf("  AUC inflation:  ~%.0f%%\n",
                  object@inflation_estimate$auc_inflation * 100))
    }
    if (!is.na(object@inflation_estimate$rmse_deflation)) {
      cat(sprintf("  RMSE deflation: ~%.0f%%\n",
                  object@inflation_estimate$rmse_deflation * 100))
    }
    cat(sprintf("  Confidence:     %s\n", object@inflation_estimate$confidence))
  }

  # Warning for severe cases
  if (object@severity == "severe") {
    cat("\n")
    cat("WARNING: Random CV would produce invalid performance estimates.\n")
    cat(sprintf("         Use %s instead.\
", object@recommended_cv))
  }

  invisible(NULL)
})


#' Diagnose Data Dependency Structure
#'
#' Automatically detects spatial autocorrelation, temporal autocorrelation,
#' and clustered structure in data. Returns a diagnosis object that specifies
#' appropriate cross-validation strategies.
#'
#' @param data A data frame to diagnose.
#' @param coords Character vector of length 2 specifying coordinate column names
#'   (e.g., \code{c("lon", "lat")} or \code{c("x", "y")}). If NULL, spatial
#'   autocorrelation is not tested.
#' @param time Character string specifying the time column name. Can be Date,
#'   POSIXct, or numeric. If NULL, temporal autocorrelation is not tested.
#' @param groups Character string specifying the grouping column name
#'   (e.g., "site_id", "patient_id"). If NULL, clustered structure is not tested.
#' @param target Character string specifying the response variable column name.
#'   Used for more accurate autocorrelation diagnostics on residuals. Optional.
#' @param alpha Numeric. Significance level for autocorrelation tests.
#'   Default: 0.05.
#' @param verbose Logical. If TRUE, print diagnostic progress. Default: FALSE.
#'
#' @return A \code{\link{BorgDiagnosis}} object containing:
#' \itemize{
#'   \item Detected dependency type(s)
#'   \item Severity assessment
#'   \item Recommended CV strategy
#'   \item Detailed diagnostics for each dependency type
#'   \item Estimated metric inflation from using random CV
#' }
#'
#' @details
#' \subsection{Spatial Autocorrelation}{
#' Detected using Moran's I test on the target variable (or first numeric column).
#' The autocorrelation range is estimated from the empirical variogram.
#' Effective sample size is computed as \eqn{n_{eff} = n / DEFF} where
#' DEFF is the design effect.
#' }
#'
#' \subsection{Temporal Autocorrelation}{
#' Detected using the Ljung-Box test on the target variable. The decorrelation
#' lag is the first lag where ACF drops below the significance threshold.
#' Minimum embargo period is set to the decorrelation lag.
#' }
#'
#' \subsection{Clustered Structure}{
#' Detected by computing the intraclass correlation coefficient (ICC).
#' An ICC > 0.05 indicates meaningful clustering. The design effect
#' (DEFF) quantifies variance inflation: \eqn{DEFF = 1 + (m-1) \times ICC}
#' where m is the average cluster size.
#' }
#'
#' @examples
#' # Spatial data example
#' set.seed(42)
#' spatial_data <- data.frame(
#'   x = runif(100, 0, 100),
#'   y = runif(100, 0, 100),
#'   response = rnorm(100)
#' )
#' # Add spatial autocorrelation (nearby points are similar)
#' for (i in 2:100) {
#'   nearest <- which.min((spatial_data$x[1:(i-1)] - spatial_data$x[i])^2 +
#'                        (spatial_data$y[1:(i-1)] - spatial_data$y[i])^2)
#'   spatial_data$response[i] <- 0.7 * spatial_data$response[nearest] +
#'                               0.3 * rnorm(1)
#' }
#'
#' diagnosis <- borg_diagnose(spatial_data, coords = c("x", "y"),
#'                            target = "response")
#' print(diagnosis)
#'
#' # Clustered data example
#' clustered_data <- data.frame(
#'   site = rep(1:10, each = 20),
#'   value = rep(rnorm(10, sd = 2), each = 20) + rnorm(200, sd = 0.5)
#' )
#'
#' diagnosis <- borg_diagnose(clustered_data, groups = "site", target = "value")
#' print(diagnosis)
#'
#' @export
borg_diagnose <- function(data,
                          coords = NULL,
                          time = NULL,
                          groups = NULL,
                          target = NULL,
                          alpha = 0.05,
                          verbose = FALSE) {
  call <- match.call()

  # Input validation
  if (!is.data.frame(data)) {
    stop("'data' must be a data frame")
  }
  n <- nrow(data)
  if (n < 10) {
    stop("'data' must have at least 10 observations for diagnosis")
  }

  # Validate coords
  if (!is.null(coords)) {
    if (length(coords) != 2) {
      stop("'coords' must be a character vector of length 2")
    }
    if (!all(coords %in% names(data))) {
      stop(sprintf("Coordinate columns not found: %s",
                   paste(setdiff(coords, names(data)), collapse = ", ")))
    }
  }

  # Validate time
  if (!is.null(time)) {
    if (!time %in% names(data)) {
      stop(sprintf("Time column '%s' not found in data", time))
    }
  }

  # Validate groups
  if (!is.null(groups)) {
    if (!groups %in% names(data)) {
      stop(sprintf("Group column '%s' not found in data", groups))
    }
  }

  # Find target variable if not specified
  if (is.null(target)) {
    numeric_cols <- names(data)[vapply(data, is.numeric, logical(1))]
    numeric_cols <- setdiff(numeric_cols, c(coords, time))
    if (length(numeric_cols) > 0) {
      target <- numeric_cols[1]
      if (verbose) message(sprintf("Using '%s' as target variable", target))
    }
  } else {
    if (!target %in% names(data)) {
      stop(sprintf("Target column '%s' not found in data", target))
    }
  }

  # Get target values
  y <- if (!is.null(target)) data[[target]] else NULL

  # Initialize results
  spatial_result <- list(detected = FALSE)
  temporal_result <- list(detected = FALSE)
  clustered_result <- list(detected = FALSE)

  # ===== SPATIAL AUTOCORRELATION =====
  if (!is.null(coords)) {
    if (verbose) message("Testing spatial autocorrelation...")
    spatial_result <- diagnose_spatial(data, coords, y, alpha, verbose)
  }

  # ===== TEMPORAL AUTOCORRELATION =====
  if (!is.null(time)) {
    if (verbose) message("Testing temporal autocorrelation...")
    temporal_result <- diagnose_temporal(data, time, y, alpha, verbose)
  }

  # ===== CLUSTERED STRUCTURE =====
  if (!is.null(groups)) {
    if (verbose) message("Testing clustered structure...")
    clustered_result <- diagnose_clustered(data, groups, y, alpha, verbose)
  }

  # Determine overall dependency type and severity
  dep_types <- c()
  if (spatial_result$detected) dep_types <- c(dep_types, "spatial")
  if (temporal_result$detected) dep_types <- c(dep_types, "temporal")
  if (clustered_result$detected) dep_types <- c(dep_types, "clustered")

  if (length(dep_types) == 0) {
    dependency_type <- "none"
  } else if (length(dep_types) == 1) {
    dependency_type <- dep_types[1]
  } else {
    dependency_type <- "mixed"
  }

  # Determine severity
  severity <- determine_severity(spatial_result, temporal_result, clustered_result)

  # Determine recommended CV strategy
  recommended_cv <- recommend_cv_strategy(spatial_result, temporal_result,
                                          clustered_result)

  # Estimate inflation
  inflation_estimate <- estimate_inflation(spatial_result, temporal_result,
                                           clustered_result, n)

  # Create diagnosis object
  new("BorgDiagnosis",
      dependency_type = dependency_type,
      severity = severity,
      recommended_cv = recommended_cv,
      spatial = spatial_result,
      temporal = temporal_result,
      clustered = clustered_result,
      inflation_estimate = inflation_estimate,
      n_obs = as.integer(n),
      timestamp = Sys.time(),
      call = call)
}


# ============================================================================
# Internal diagnostic functions
# ============================================================================

#' Diagnose Spatial Autocorrelation (Internal)
#' @noRd
diagnose_spatial <- function(data, coords, y, alpha, verbose) {
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

  # Compute pairwise distances
  dist_mat <- compute_distance_matrix(x_sub, y_sub)

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

  # Estimate spatial range from variogram
  range_estimate <- estimate_spatial_range(y_sub_val, dist_mat)

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
    coords_used = coords
  )
}


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


#' Determine Overall Severity (Internal)
#' @noRd
determine_severity <- function(spatial, temporal, clustered) {
  severities <- c()

  # Spatial severity

  if (spatial$detected) {
    if (!is.na(spatial$morans_i) && spatial$morans_i > 0.5) {
      severities <- c(severities, "severe")
    } else if (!is.na(spatial$morans_i) && spatial$morans_i > 0.2) {
      severities <- c(severities, "moderate")
    } else if (spatial$detected) {
      severities <- c(severities, "moderate")
    }
  }

  # Temporal severity
  if (temporal$detected) {
    if (!is.na(temporal$acf_lag1) && abs(temporal$acf_lag1) > 0.7) {
      severities <- c(severities, "severe")
    } else if (!is.na(temporal$acf_lag1) && abs(temporal$acf_lag1) > 0.4) {
      severities <- c(severities, "moderate")
    } else if (temporal$detected) {
      severities <- c(severities, "moderate")
    }
  }

  # Clustered severity
  if (clustered$detected) {
    if (!is.na(clustered$icc) && clustered$icc > 0.3) {
      severities <- c(severities, "severe")
    } else if (!is.na(clustered$icc) && clustered$icc > 0.1) {
      severities <- c(severities, "moderate")
    } else if (clustered$detected) {
      severities <- c(severities, "moderate")
    }
  }

  if (length(severities) == 0) return("none")
  if ("severe" %in% severities) return("severe")
  if ("moderate" %in% severities) return("moderate")
  "none"
}


#' Recommend CV Strategy (Internal)
#' @noRd
recommend_cv_strategy <- function(spatial, temporal, clustered) {
  has_spatial <- spatial$detected
  has_temporal <- temporal$detected
  has_clustered <- clustered$detected

  # Priority order for combinations
  if (has_spatial && has_temporal) {
    return("spatial_temporal")
  }
  if (has_spatial && has_clustered) {
    return("spatial_group")
  }
  if (has_temporal && has_clustered) {
    return("temporal_group")
  }
  if (has_spatial) {
    return("spatial_block")
  }
  if (has_temporal) {
    return("temporal_block")
  }
  if (has_clustered) {
    return("group_fold")
  }

  "random"
}


#' Estimate Inflation from Random CV (Internal)
#' @noRd
estimate_inflation <- function(spatial, temporal, clustered, n) {
  # Base inflation estimates from literature
  # Roberts et al. (2017), Valavi et al. (2019), Meyer et al. (2019)

  inflation_factors <- c()
  confidence <- "high"

  if (spatial$detected) {
    if (!is.na(spatial$effective_n)) {
      # Inflation proportional to effective sample size reduction
      eff_ratio <- spatial$effective_n / n
      spatial_inflation <- max(0, 1 - eff_ratio) * 0.5  # Up to 50% from spatial
      inflation_factors <- c(inflation_factors, spatial_inflation)
    } else if (!is.na(spatial$morans_i)) {
      # Rough estimate from Moran's I
      spatial_inflation <- spatial$morans_i * 0.4
      inflation_factors <- c(inflation_factors, spatial_inflation)
      confidence <- "medium"
    }
  }

  if (temporal$detected) {
    if (!is.na(temporal$acf_lag1)) {
      # ACF-based inflation estimate
      temporal_inflation <- abs(temporal$acf_lag1) * 0.35
      inflation_factors <- c(inflation_factors, temporal_inflation)
    }
  }

  if (clustered$detected) {
    if (!is.na(clustered$design_effect)) {
      # DEFF-based inflation
      deff <- clustered$design_effect
      clustered_inflation <- max(0, (deff - 1) / deff) * 0.4
      inflation_factors <- c(inflation_factors, clustered_inflation)
    } else if (!is.na(clustered$icc)) {
      clustered_inflation <- clustered$icc * 0.3
      inflation_factors <- c(inflation_factors, clustered_inflation)
      confidence <- "medium"
    }
  }

  if (length(inflation_factors) == 0) {
    return(list(
      auc_inflation = NA_real_,
      rmse_deflation = NA_real_,
      confidence = "high",
      basis = "no_dependency_detected"
    ))
  }

  # Combine inflation factors (not simply additive)
  # Use max + fraction of others
  total_inflation <- max(inflation_factors) +
    sum(inflation_factors[-which.max(inflation_factors)]) * 0.5

  total_inflation <- min(total_inflation, 0.6)  # Cap at 60%

  list(
    auc_inflation = total_inflation,
    rmse_deflation = total_inflation * 0.8,  # RMSE typically less affected
    confidence = confidence,
    basis = paste(c(
      if (spatial$detected) "spatial_autocorrelation",
      if (temporal$detected) "temporal_autocorrelation",
      if (clustered$detected) "clustered_structure"
    ), collapse = "+")
  )
}


# ============================================================================
# Helper functions for statistical computations
# ============================================================================

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


#' Estimate Spatial Range from Variogram (Internal)
#' @noRd
estimate_spatial_range <- function(y, dist_mat) {
  n <- length(y)

  # Get upper triangle (unique pairs)
  upper_idx <- upper.tri(dist_mat)
  distances <- dist_mat[upper_idx]
  semivar <- 0.5 * outer(y, y, function(a, b) (a - b)^2)[upper_idx]

  # Bin by distance
  n_bins <- min(15, floor(length(distances) / 50))
  if (n_bins < 3) return(max(dist_mat) / 3)  # Fallback

  breaks <- quantile(distances, probs = seq(0, 1, length.out = n_bins + 1))
  breaks <- unique(breaks)

  bins <- cut(distances, breaks, include.lowest = TRUE)
  bin_means <- tapply(distances, bins, mean)
  bin_semivar <- tapply(semivar, bins, mean)

  # Remove NA
  valid <- !is.na(bin_means) & !is.na(bin_semivar)
  bin_means <- bin_means[valid]
  bin_semivar <- bin_semivar[valid]

  if (length(bin_means) < 3) return(max(dist_mat) / 3)

  # Find range: distance at which semivariance reaches ~95% of sill
  sill_estimate <- max(bin_semivar)
  threshold <- 0.95 * sill_estimate

  range_idx <- which(bin_semivar >= threshold)[1]
  if (is.na(range_idx)) {
    range_estimate <- max(bin_means)
  } else {
    range_estimate <- bin_means[range_idx]
  }

  range_estimate
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
