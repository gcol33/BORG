# BORG Diagnosis Module
# Detects data dependency structure: spatial, temporal, clustered
#
# Internal helpers split into:
#   diagnose_spatial.R  — diagnose_spatial(), compute_distance_matrix(),
#                         compute_morans_i(), estimate_spatial_range(),
#                         compute_effective_n_spatial()
#   diagnose_temporal.R — diagnose_temporal(), diagnose_clustered(), compute_icc()
#   diagnose_inference.R — determine_severity(), recommend_cv_strategy(),
#                          estimate_inflation()

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

   valid_cv <- c("random", "spatial_block", "temporal_block",
                 "temporal_expanding", "temporal_sliding",
                 "group_fold",
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


#' Coerce BorgDiagnosis to Data Frame
#'
#' @description Converts a \code{BorgDiagnosis} object into a one-row data frame
#' of diagnostic results for programmatic access.
#'
#' @name as.data.frame.BorgDiagnosis
#' @method as.data.frame BorgDiagnosis
#' @export
#'
#' @param x A \code{BorgDiagnosis} object.
#' @param row.names Optional row names for the output data frame.
#' @param optional Logical. Passed to \code{data.frame()}.
#' @param ... Additional arguments passed to \code{data.frame()}.
#'
#' @return A one-row data frame with columns: \code{dependency_type},
#'   \code{severity}, \code{recommended_cv}, \code{n_obs},
#'   \code{spatial_detected}, \code{morans_i}, \code{temporal_detected},
#'   \code{acf_lag1}, \code{clustered_detected}, \code{icc}.
#'
#' @seealso \code{\link{BorgDiagnosis}}
as.data.frame.BorgDiagnosis <- function(x, row.names = NULL, optional = FALSE, ...) {
  df <- data.frame(
    dependency_type = x@dependency_type,
    severity = x@severity,
    recommended_cv = x@recommended_cv,
    n_obs = x@n_obs,
    spatial_detected = x@spatial$detected %||% FALSE,
    morans_i = x@spatial$morans_i %||% NA_real_,
    temporal_detected = x@temporal$detected %||% FALSE,
    acf_lag1 = x@temporal$acf_lag1 %||% NA_real_,
    clustered_detected = x@clustered$detected %||% FALSE,
    icc = x@clustered$icc %||% NA_real_,
    stringsAsFactors = FALSE
  )

  if (!is.null(row.names)) {
    rownames(df) <- row.names
  }

  df
}


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
