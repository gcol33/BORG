#' Inspect R Objects for Evaluation Risks
#'
#' `borg_inspect()` examines R objects for signals of information reuse that
#' would invalidate model evaluation. It returns a structured assessment of
#' detected risks.
#'
#' @param object An R object to inspect. Supported types include:
#'   \itemize{
#'     \item Preprocessing: \code{preProcess}, \code{recipe}, \code{prcomp}
#'     \item CV objects: \code{trainControl}, \code{rsplit}, \code{vfold_cv}
#'     \item Model objects: \code{train}, \code{lm}, \code{glm}
#'     \item Data frames with train/test split information
#'   }
#' @param train_idx Integer vector of training row indices. Required for
#'   data-level inspection.
#' @param test_idx Integer vector of test row indices. Required for
#'   data-level inspection.
#' @param data Optional data frame. Required when inspecting preprocessing
#'   objects to compare parameters against train-only statistics.
#' @param target_col Optional name of the target/outcome column. If provided,
#'   checks for target leakage (features highly correlated with target).
#' @param spatial_cols Optional character vector of coordinate column names.
#'   If provided, checks spatial separation between train and test.
#' @param ... Additional arguments passed to type-specific inspectors.
#'
#' @return A \code{\link{BorgRisk}} object containing:
#'   \describe{
#'     \item{risks}{List of detected risk objects}
#'     \item{n_hard}{Count of hard violations}
#'     \item{n_soft}{Count of soft inflation warnings}
#'     \item{is_valid}{TRUE if no hard violations detected}
#'   }
#'
#' @details
#' `borg_inspect()` dispatches to type-specific inspectors based on the class
#' of the input object. Each inspector looks for specific leakage patterns:
#'
#' \describe{
#'   \item{Preprocessing objects}{Checks if parameters (mean, sd, loadings)
#'     were computed on data that includes test indices}
#'   \item{CV objects}{Validates that train/test indices do not overlap and
#'     that grouping structure is respected}
#'   \item{Feature engineering}{Checks if encodings, embeddings, or derived
#'     features used test data during computation}
#' }
#'
#' @seealso
#' \code{\link{borg_validate}} for complete workflow validation,
#' \code{\link{borg_guard}} for automated enforcement during evaluation.
#'
#' @examples
#' # Inspect a preprocessing object
#' data(mtcars)
#' train_idx <- 1:25
#' test_idx <- 26:32
#'
#' # BAD: preProcess fitted on full data (will detect leak)
#' pp_bad <- scale(mtcars[, -1])
#'
#' # GOOD: preProcess fitted on train only
#' pp_good <- scale(mtcars[train_idx, -1])
#'
#' @export
borg_inspect <- function(object, train_idx = NULL, test_idx = NULL, data = NULL,
                         target_col = NULL, spatial_cols = NULL, ...) {

 # ===========================================================================
 # Input validation
 # ===========================================================================

 if (missing(object)) {
   stop("'object' is required")
 }

 # Validate indices if provided
 if (!is.null(train_idx)) {
   if (!is.numeric(train_idx) && !is.integer(train_idx)) {
     stop("'train_idx' must be an integer vector")
   }
   train_idx <- as.integer(train_idx)
 }

 if (!is.null(test_idx)) {
   if (!is.numeric(test_idx) && !is.integer(test_idx)) {
     stop("'test_idx' must be an integer vector")
   }
   test_idx <- as.integer(test_idx)
 }

 # Check for index overlap (immediate hard violation)
 # Use C++ backend for fast set intersection
 if (!is.null(train_idx) && !is.null(test_idx)) {
   overlap_result <- tryCatch(
     checkIndexOverlap(train_idx, test_idx),
     error = function(e) {
       # Fallback to R if C++ not available
       overlap <- intersect(train_idx, test_idx)
       list(has_overlap = length(overlap) > 0,
            n_overlap = length(overlap),
            overlap_indices = overlap)
     }
   )

   if (overlap_result$has_overlap) {
     overlap_risk <- list(
       type = "index_overlap",
       severity = "hard_violation",
       description = sprintf(
         "Train and test indices overlap (%d shared indices). This invalidates evaluation.",
         overlap_result$n_overlap
       ),
       affected_indices = overlap_result$overlap_indices,
       source_object = "train_idx/test_idx"
     )

     return(new("BorgRisk",
       risks = list(overlap_risk),
       n_hard = 1L,
       n_soft = 0L,
       is_valid = FALSE,
       train_indices = train_idx,
       test_indices = test_idx,
       timestamp = Sys.time(),
       call = match.call()
     ))
   }
 }

 # ===========================================================================
 # Dispatch to type-specific inspector
 # ===========================================================================

 risks <- list()

 # Dispatch based on object class and structure
 # Some packages (like caret) don't use proper S3/S4 classes
 obj_class <- class(object)[1]

 inspector_result <- if (obj_class == "preProcess") {
   .inspect_preProcess(object, train_idx, test_idx, data, ...)
 } else if (inherits(object, "recipe")) {
   .inspect_recipe(object, train_idx, test_idx, data, ...)
 } else if (obj_class == "prcomp") {
   .inspect_prcomp(object, train_idx, test_idx, data, ...)
 } else if (inherits(object, "rsplit")) {
   .inspect_rsplit(object, train_idx, test_idx, ...)
 } else if (inherits(object, "vfold_cv") || inherits(object, "rset")) {
   .inspect_vfold_cv(object, train_idx, test_idx, ...)
 } else if (inherits(object, "train")) {
   .inspect_caret_train(object, train_idx, test_idx, ...)
 } else if (obj_class == "lm") {
   .inspect_lm(object, train_idx, test_idx, data, ...)
 } else if (obj_class == "glm") {
   .inspect_glm(object, train_idx, test_idx, data, ...)
 } else if (inherits(object, "ranger")) {
   .inspect_ranger(object, train_idx, test_idx, data, ...)
 } else if (inherits(object, "xgb.Booster")) {
   .inspect_xgboost(object, train_idx, test_idx, data, ...)
 } else if (inherits(object, "lgb.Booster")) {
   .inspect_lightgbm(object, train_idx, test_idx, data, ...)
 } else if (inherits(object, "model_fit")) {
   .inspect_parsnip(object, train_idx, test_idx, data, ...)
 } else if (inherits(object, "workflow")) {
   .inspect_workflow(object, train_idx, test_idx, data, ...)
 } else if (inherits(object, "tune_results")) {
   .inspect_tune_results(object, train_idx, test_idx, data, ...)
 } else if (is.data.frame(object)) {
   .inspect_data_frame(object, train_idx, test_idx,
                       target_col = target_col, spatial_cols = spatial_cols, ...)
 } else if (.is_trainControl(object)) {
   # caret::trainControl returns a list, detect by structure
   .inspect_trainControl(object, train_idx, test_idx, ...)
 } else {
   .inspect_generic(object, train_idx, test_idx, data, ...)
 }

 risks <- c(risks, inspector_result)

 # ===========================================================================
 # Build result object
 # ===========================================================================

 n_hard <- sum(vapply(risks, function(r) r$severity == "hard_violation", logical(1)))
 n_soft <- sum(vapply(risks, function(r) r$severity == "soft_inflation", logical(1)))

 new("BorgRisk",
   risks = risks,
   n_hard = as.integer(n_hard),
   n_soft = as.integer(n_soft),
   is_valid = n_hard == 0L,
   train_indices = train_idx %||% integer(0),
   test_indices = test_idx %||% integer(0),
   timestamp = Sys.time(),
   call = match.call()
 )
}


# ===========================================================================
# Type-specific inspectors (stubs for initial skeleton)
# ===========================================================================

#' @noRd
.inspect_preProcess <- function(object, train_idx, test_idx, data, ...) {
  risks <- list()

  # Need data and indices to detect leakage

  if (is.null(data) || is.null(train_idx) || is.null(test_idx)) {
    return(risks)
  }

  # caret::preProcess stores:
  #   $mean - column means used for centering
  #   $std  - column sds used for scaling
  #   $method - methods applied
  #   $numComp - for PCA

  # Strategy: recompute statistics on train-only and compare
  # If they differ significantly, the object was fitted on more than train

  methods_used <- object$method
  if (is.null(methods_used)) {
    return(risks)
  }

  # caret preProcess stores method as a list where names are method names
  # e.g., list(center = c("x1", "x2"), scale = c("x1", "x2"))
  method_names <- names(methods_used)

  # Get numeric columns that preProcess would have used
  # May be stored in $mean (centering) or $std (scaling)
  numeric_cols <- names(object$mean)
  if (is.null(numeric_cols) || length(numeric_cols) == 0) {
    numeric_cols <- names(object$std)
  }
  if (is.null(numeric_cols) || length(numeric_cols) == 0) {
    return(risks)
  }

  # Check centering (mean)
  if ("center" %in% method_names && !is.null(object$mean)) {
    train_data <- data[train_idx, numeric_cols, drop = FALSE]

    # Compute train-only means
    train_means <- colMeans(train_data, na.rm = TRUE)

    # Compare to stored means
    stored_means <- object$mean[numeric_cols]

    # Tolerance for floating point comparison
    # If preProcess was fitted on train only, these should match
    mean_diff <- abs(train_means - stored_means)
    max_diff <- max(mean_diff, na.rm = TRUE)

    # Use relative tolerance based on data scale
    data_scale <- max(abs(stored_means), na.rm = TRUE)
    rel_tol <- if (data_scale > 0) 1e-10 * data_scale else 1e-10

    if (max_diff > rel_tol) {
      # Means don't match - preprocessing used more than train data
      risks <- c(risks, list(list(
        type = "preprocessing_leak",
        severity = "hard_violation",
        description = sprintf(
          "preProcess centering parameters were computed on data beyond training set (mean difference: %.6g)",
          max_diff
        ),
        affected_indices = test_idx,
        source_object = "preProcess"
      )))
    }
  }

  # Check scaling (sd)
  if ("scale" %in% method_names && !is.null(object$std)) {
    train_data <- data[train_idx, numeric_cols, drop = FALSE]

    # Compute train-only sds
    train_sds <- apply(train_data, 2, sd, na.rm = TRUE)

    # Compare to stored sds
    stored_sds <- object$std[numeric_cols]

    sd_diff <- abs(train_sds - stored_sds)
    max_diff <- max(sd_diff, na.rm = TRUE)

    data_scale <- max(abs(stored_sds), na.rm = TRUE)
    rel_tol <- if (data_scale > 0) 1e-10 * data_scale else 1e-10

    if (max_diff > rel_tol) {
      risks <- c(risks, list(list(
        type = "preprocessing_leak",
        severity = "hard_violation",
        description = sprintf(
          "preProcess scaling parameters were computed on data beyond training set (sd difference: %.6g)",
          max_diff
        ),
        affected_indices = test_idx,
        source_object = "preProcess"
      )))
    }
  }

  risks
}

#' @noRd
.inspect_recipe <- function(object, train_idx, test_idx, data, ...) {
  risks <- list()

  # recipes store training data info in several places:
  #   $template - the original data structure used to create the recipe
  #   $tr_info$nrows - number of rows used during prep()
  #   $steps[[i]]$means, $steps[[i]]$sds - step-specific learned parameters

  # Check if recipe has been prepped
  # Recipe prep status is determined by step$trained, not recipe$trained
  is_prepped <- length(object$steps) > 0 &&
    any(vapply(object$steps, function(s) isTRUE(s$trained), logical(1)))

  if (!is_prepped) {
    # Unprepped recipe - no leakage possible yet
    return(risks)
  }

  # Check 1: Compare prep() row count to expected train-only count
  tr_info <- object$tr_info
  if (!is.null(tr_info) && !is.null(tr_info$nrows)) {
    prep_nrows <- tr_info$nrows
    expected_nrows <- length(train_idx)

    if (!is.null(train_idx) && prep_nrows != expected_nrows) {
      # Recipe was prepped on different number of rows than train set
      risks <- c(risks, list(list(
        type = "preprocessing_leak",
        severity = "hard_violation",
        description = sprintf(
          "Recipe was prepped on %d rows, but train set has %d rows. Preprocessing may include test data.",
          prep_nrows, expected_nrows
        ),
        affected_indices = test_idx,
        source_object = "recipe"
      )))
    }
  }

  # Check 2: Inspect step-specific parameters if data is provided
  if (!is.null(data) && !is.null(train_idx) && !is.null(test_idx)) {
    # Check normalization steps (step_center, step_scale, step_normalize)
    for (i in seq_along(object$steps)) {
      step <- object$steps[[i]]
      step_class <- class(step)[1]

      # step_center stores means
      if (step_class == "step_center" && !is.null(step$means)) {
        cols <- names(step$means)
        cols <- intersect(cols, names(data))

        if (length(cols) > 0) {
          train_data <- data[train_idx, cols, drop = FALSE]
          train_means <- colMeans(train_data, na.rm = TRUE)
          stored_means <- step$means[cols]

          mean_diff <- abs(train_means - stored_means)
          max_diff <- max(mean_diff, na.rm = TRUE)

          data_scale <- max(abs(stored_means), na.rm = TRUE)
          rel_tol <- if (data_scale > 0) 1e-10 * data_scale else 1e-10

          if (max_diff > rel_tol) {
            risks <- c(risks, list(list(
              type = "preprocessing_leak",
              severity = "hard_violation",
              description = sprintf(
                "step_center parameters were computed on data beyond training set (mean difference: %.6g)",
                max_diff
              ),
              affected_indices = test_idx,
              source_object = sprintf("recipe$steps[[%d]] (step_center)", i)
            )))
          }
        }
      }

      # step_scale stores sds
      if (step_class == "step_scale" && !is.null(step$sds)) {
        cols <- names(step$sds)
        cols <- intersect(cols, names(data))

        if (length(cols) > 0) {
          train_data <- data[train_idx, cols, drop = FALSE]
          train_sds <- apply(train_data, 2, sd, na.rm = TRUE)
          stored_sds <- step$sds[cols]

          sd_diff <- abs(train_sds - stored_sds)
          max_diff <- max(sd_diff, na.rm = TRUE)

          data_scale <- max(abs(stored_sds), na.rm = TRUE)
          rel_tol <- if (data_scale > 0) 1e-10 * data_scale else 1e-10

          if (max_diff > rel_tol) {
            risks <- c(risks, list(list(
              type = "preprocessing_leak",
              severity = "hard_violation",
              description = sprintf(
                "step_scale parameters were computed on data beyond training set (sd difference: %.6g)",
                max_diff
              ),
              affected_indices = test_idx,
              source_object = sprintf("recipe$steps[[%d]] (step_scale)", i)
            )))
          }
        }
      }

      # step_normalize combines center and scale
      if (step_class == "step_normalize") {
        if (!is.null(step$means)) {
          cols <- names(step$means)
          cols <- intersect(cols, names(data))

          if (length(cols) > 0) {
            train_data <- data[train_idx, cols, drop = FALSE]
            train_means <- colMeans(train_data, na.rm = TRUE)
            stored_means <- step$means[cols]

            mean_diff <- abs(train_means - stored_means)
            max_diff <- max(mean_diff, na.rm = TRUE)

            data_scale <- max(abs(stored_means), na.rm = TRUE)
            rel_tol <- if (data_scale > 0) 1e-10 * data_scale else 1e-10

            if (max_diff > rel_tol) {
              risks <- c(risks, list(list(
                type = "preprocessing_leak",
                severity = "hard_violation",
                description = sprintf(
                  "step_normalize centering parameters were computed on data beyond training set (mean difference: %.6g)",
                  max_diff
                ),
                affected_indices = test_idx,
                source_object = sprintf("recipe$steps[[%d]] (step_normalize)", i)
              )))
            }
          }
        }

        if (!is.null(step$sds)) {
          cols <- names(step$sds)
          cols <- intersect(cols, names(data))

          if (length(cols) > 0) {
            train_data <- data[train_idx, cols, drop = FALSE]
            train_sds <- apply(train_data, 2, sd, na.rm = TRUE)
            stored_sds <- step$sds[cols]

            sd_diff <- abs(train_sds - stored_sds)
            max_diff <- max(sd_diff, na.rm = TRUE)

            data_scale <- max(abs(stored_sds), na.rm = TRUE)
            rel_tol <- if (data_scale > 0) 1e-10 * data_scale else 1e-10

            if (max_diff > rel_tol) {
              risks <- c(risks, list(list(
                type = "preprocessing_leak",
                severity = "hard_violation",
                description = sprintf(
                  "step_normalize scaling parameters were computed on data beyond training set (sd difference: %.6g)",
                  max_diff
                ),
                affected_indices = test_idx,
                source_object = sprintf("recipe$steps[[%d]] (step_normalize)", i)
              )))
            }
          }
        }
      }

      # step_pca stores rotation matrix
      if (step_class == "step_pca" && !is.null(step$res)) {
        # PCA results are in step$res (prcomp-like object)
        # We can delegate to prcomp inspection
        pca_risks <- .inspect_prcomp(step$res, train_idx, test_idx, data, ...)
        for (r in pca_risks) {
          r$source_object <- sprintf("recipe$steps[[%d]] (step_pca)", i)
          risks <- c(risks, list(r))
        }
      }
    }
  }

  risks
}

#' @noRd
.inspect_prcomp <- function(object, train_idx, test_idx, data, ...) {
  risks <- list()

  # prcomp stores:
  #   $center - means used for centering (if center=TRUE)
  #   $scale  - sds used for scaling (if scale=TRUE)
  #   $rotation - loadings matrix (eigenvectors)
  #   $sdev - standard deviations of principal components
  #   $x - scores (if retx=TRUE)

  if (is.null(data) || is.null(train_idx) || is.null(test_idx)) {
    return(risks)
  }

  # Determine which columns were used in PCA
  # prcomp uses row names of $rotation as column names
  pca_cols <- rownames(object$rotation)
  if (is.null(pca_cols)) {
    return(risks)
  }

  # Filter to columns present in data
  pca_cols <- intersect(pca_cols, names(data))
  if (length(pca_cols) == 0) {
    return(risks)
  }

  train_data <- data[train_idx, pca_cols, drop = FALSE]

  # Check centering
  if (!is.null(object$center) && !isFALSE(object$center)) {
    # object$center is either FALSE or a named numeric vector
    if (is.numeric(object$center)) {
      stored_center <- object$center[pca_cols]
      train_means <- colMeans(train_data, na.rm = TRUE)

      center_diff <- abs(train_means - stored_center)
      max_diff <- max(center_diff, na.rm = TRUE)

      data_scale <- max(abs(stored_center), na.rm = TRUE)
      rel_tol <- if (data_scale > 0) 1e-10 * data_scale else 1e-10

      if (max_diff > rel_tol) {
        risks <- c(risks, list(list(
          type = "preprocessing_leak",
          severity = "hard_violation",
          description = sprintf(
            "PCA centering was computed on data beyond training set (mean difference: %.6g)",
            max_diff
          ),
          affected_indices = test_idx,
          source_object = "prcomp"
        )))
      }
    }
  }

  # Check scaling
  if (!is.null(object$scale) && !isFALSE(object$scale)) {
    if (is.numeric(object$scale)) {
      stored_scale <- object$scale[pca_cols]
      train_sds <- apply(train_data, 2, sd, na.rm = TRUE)

      scale_diff <- abs(train_sds - stored_scale)
      max_diff <- max(scale_diff, na.rm = TRUE)

      data_scale <- max(abs(stored_scale), na.rm = TRUE)
      rel_tol <- if (data_scale > 0) 1e-10 * data_scale else 1e-10

      if (max_diff > rel_tol) {
        risks <- c(risks, list(list(
          type = "preprocessing_leak",
          severity = "hard_violation",
          description = sprintf(
            "PCA scaling was computed on data beyond training set (sd difference: %.6g)",
            max_diff
          ),
          affected_indices = test_idx,
          source_object = "prcomp"
        )))
      }
    }
  }

  # Check rotation matrix (loadings) - this is the core PCA leak
  # If PCA was computed on full data, the loadings encode test data variance
  # We can detect this by recomputing PCA on train-only and comparing loadings
  if (!is.null(object$rotation)) {
    # Recompute PCA on train-only data
    train_centered <- scale(train_data,
                            center = !isFALSE(object$center),
                            scale = !isFALSE(object$scale))

    # SVD to get loadings
    train_svd <- tryCatch(
      svd(train_centered, nu = 0, nv = ncol(train_centered)),
      error = function(e) NULL
    )

    if (!is.null(train_svd)) {
      # Compare loadings - they should match (up to sign flip)
      # PCA loadings are unique up to sign, so compare absolute values
      stored_rotation <- object$rotation[pca_cols, , drop = FALSE]
      n_components <- min(ncol(stored_rotation), ncol(train_svd$v))

      if (n_components > 0) {
        stored_abs <- abs(stored_rotation[, 1:n_components, drop = FALSE])
        train_abs <- abs(train_svd$v[, 1:n_components, drop = FALSE])

        # Compare first few components (most variance, most detectable)
        n_check <- min(3, n_components)
        rotation_diff <- abs(stored_abs[, 1:n_check, drop = FALSE] -
                            train_abs[, 1:n_check, drop = FALSE])
        max_diff <- max(rotation_diff, na.rm = TRUE)

        # Loadings should be very close if computed on same data
        # Use a somewhat larger tolerance since SVD can have numerical differences
        if (max_diff > 0.01) {
          risks <- c(risks, list(list(
            type = "preprocessing_leak",
            severity = "hard_violation",
            description = sprintf(
              "PCA loadings were computed on data beyond training set (max loading difference: %.4f)",
              max_diff
            ),
            affected_indices = test_idx,
            source_object = "prcomp"
          )))
        }
      }
    }
  }

  risks
}

#' @noRd
.inspect_trainControl <- function(object, train_idx, test_idx, ...) {
  risks <- list()

  # caret::trainControl stores:
  #   $index - list of training indices for each fold
  #   $indexOut - list of holdout indices for each fold
  #   $method - CV method (cv, repeatedcv, LOOCV, etc.)

  if (is.null(train_idx) || is.null(test_idx)) {
    return(risks)
  }

  # Check if CV folds leak test data into training
  if (!is.null(object$index)) {
    for (i in seq_along(object$index)) {
      fold_train <- object$index[[i]]

      # Check if any test indices appear in CV training folds
      leaked_test <- intersect(fold_train, test_idx)
      if (length(leaked_test) > 0) {
        risks <- c(risks, list(list(
          type = "cv_leak",
          severity = "hard_violation",
          description = sprintf(
            "CV fold %d training set contains %d test indices. Test data is being used in CV.",
            i, length(leaked_test)
          ),
          affected_indices = leaked_test,
          source_object = sprintf("trainControl$index[[%d]]", i)
        )))
      }
    }
  }

  # Check indexOut (holdout indices) - less critical but worth flagging
  if (!is.null(object$indexOut)) {
    for (i in seq_along(object$indexOut)) {
      fold_out <- object$indexOut[[i]]

      # If test indices appear in CV holdout, that's suspicious but not always wrong
      # (could be intentional nested CV design)
      leaked_test <- intersect(fold_out, test_idx)
      if (length(leaked_test) > 0 && length(leaked_test) < length(test_idx)) {
        # Partial overlap is suspicious - full overlap might be intentional
        risks <- c(risks, list(list(
          type = "cv_leak",
          severity = "soft_inflation",
          description = sprintf(
            "CV fold %d holdout contains %d test indices (partial overlap).",
            i, length(leaked_test)
          ),
          affected_indices = leaked_test,
          source_object = sprintf("trainControl$indexOut[[%d]]", i)
        )))
      }
    }
  }

  risks
}

#' @noRd
.inspect_rsplit <- function(object, train_idx, test_idx, ...) {
  risks <- list()

  # rsample::rsplit stores:
  #   $in_id - integer vector of analysis (training) row indices
  #   $out_id - integer vector of assessment (holdout) row indices
  #   $data - reference to original data

  if (is.null(train_idx) || is.null(test_idx)) {
    return(risks)
  }

  # Get analysis (training) indices from rsplit
  # rsample uses in_id for training rows
  analysis_idx <- NULL
  if (!is.null(object$in_id)) {
    analysis_idx <- object$in_id
  }

  if (!is.null(analysis_idx)) {
    # Check if test indices appear in analysis set
    leaked_test <- intersect(analysis_idx, test_idx)
    if (length(leaked_test) > 0) {
      risks <- c(risks, list(list(
        type = "cv_leak",
        severity = "hard_violation",
        description = sprintf(
          "rsplit analysis set contains %d test indices. Test data is being used in model training.",
          length(leaked_test)
        ),
        affected_indices = leaked_test,
        source_object = "rsplit$in_id"
      )))
    }
  }

  # Check assessment indices
  assessment_idx <- NULL
  if (!is.null(object$out_id)) {
    assessment_idx <- object$out_id
  }

  if (!is.null(assessment_idx)) {
    # Check if train indices appear in assessment set (information flow issue)
    train_in_assessment <- intersect(assessment_idx, train_idx)
    if (length(train_in_assessment) > 0) {
      # This indicates the rsplit doesn't align with expected train/test boundary
      risks <- c(risks, list(list(
        type = "split_misalignment",
        severity = "soft_inflation",
        description = sprintf(
          "rsplit assessment set contains %d expected training indices. Split boundaries may be misaligned.",
          length(train_in_assessment)
        ),
        affected_indices = train_in_assessment,
        source_object = "rsplit$out_id"
      )))
    }
  }

  risks
}

#' @noRd
.inspect_vfold_cv <- function(object, train_idx, test_idx, ...) {
  risks <- list()

  # rsample::vfold_cv returns an rset object (tibble with splits column)
  # Each row contains an rsplit object
  # The object also has attributes: v (number of folds), repeats, strata

  if (is.null(train_idx) || is.null(test_idx)) {
    return(risks)
  }

  # vfold_cv is a tibble with a "splits" column containing rsplit objects
  if (!"splits" %in% names(object)) {
    return(risks)
  }

  # Check each fold's rsplit
  for (i in seq_len(nrow(object))) {
    split <- object$splits[[i]]
    fold_id <- if ("id" %in% names(object)) object$id[i] else sprintf("Fold%d", i)

    # Get analysis indices
    analysis_idx <- NULL
    if (!is.null(split$in_id)) {
      analysis_idx <- split$in_id
    }

    if (!is.null(analysis_idx)) {
      leaked_test <- intersect(analysis_idx, test_idx)
      if (length(leaked_test) > 0) {
        risks <- c(risks, list(list(
          type = "cv_leak",
          severity = "hard_violation",
          description = sprintf(
            "vfold_cv %s analysis set contains %d test indices.",
            fold_id, length(leaked_test)
          ),
          affected_indices = leaked_test,
          source_object = sprintf("vfold_cv$splits[[%d]]", i)
        )))
      }
    }
  }

  # Check if CV was created on data that includes test indices

  # Get total observations from the first split's underlying data
  n_total <- NULL
  if (nrow(object) > 0 && "splits" %in% names(object)) {
    first_split <- object$splits[[1]]
    if (!is.null(first_split) && !is.null(first_split$data)) {
      n_total <- nrow(first_split$data)
    }
  }

  if (!is.null(n_total) && length(n_total) == 1) {
    expected_n <- length(train_idx)
    if (n_total > expected_n) {
      # CV was created on more data than just training set
      risks <- c(risks, list(list(
        type = "cv_scope",
        severity = "hard_violation",
        description = sprintf(
          "vfold_cv was created on %d observations, but training set has only %d. CV includes non-training data.",
          n_total, expected_n
        ),
        affected_indices = test_idx,
        source_object = "vfold_cv"
      )))
    }
  }

  risks
}

#' @noRd
.inspect_rset <- function(object, train_idx, test_idx, ...) {
  # rset is the base class for rsample resampling objects

  # vfold_cv, bootstraps, mc_cv, etc. all inherit from rset
  # Delegate to vfold_cv inspector which handles the common structure
  .inspect_vfold_cv(object, train_idx, test_idx, ...)
}

#' @noRd
.inspect_caret_train <- function(object, train_idx, test_idx, ...) {
  risks <- list()

  # caret::train objects store:
  #   $control - trainControl object used
  #   $preProcess - preprocessing applied (if any)
  #   $trainingData - the actual training data used
  #   $resample - resampling results
  #   $pred - predictions on holdout folds

  if (is.null(train_idx) || is.null(test_idx)) {
    return(risks)
  }

  # Check 1: Inspect the embedded trainControl
  if (!is.null(object$control)) {
    ctrl_risks <- .inspect_trainControl(object$control, train_idx, test_idx, ...)
    for (r in ctrl_risks) {
      r$source_object <- paste0("train$control/", r$source_object)
      risks <- c(risks, list(r))
    }
  }

  # Check 2: Verify training data row count
  if (!is.null(object$trainingData)) {
    n_train_used <- nrow(object$trainingData)
    n_expected <- length(train_idx)

    if (n_train_used != n_expected) {
      if (n_train_used > n_expected) {
        risks <- c(risks, list(list(
          type = "data_scope",
          severity = "hard_violation",
          description = sprintf(
            "Model was trained on %d rows, but expected training set has %d. Non-training data may be included.",
            n_train_used, n_expected
          ),
          affected_indices = test_idx,
          source_object = "train$trainingData"
        )))
      } else {
        # Fewer rows than expected - might be OK (e.g., NA removal) but worth noting
        risks <- c(risks, list(list(
          type = "data_scope",
          severity = "soft_inflation",
          description = sprintf(
            "Model was trained on %d rows, but expected %d. Some training data may have been excluded.",
            n_train_used, n_expected
          ),
          affected_indices = integer(0),
          source_object = "train$trainingData"
        )))
      }
    }
  }

  # Check 3: If preProcess is embedded, inspect it
  if (!is.null(object$preProcess)) {
    # caret stores preProcess object when preProc is used in train()
    # Need original data to check, which we may not have
    # Flag as potential risk
    pp_methods <- object$preProcess$method
    if (!is.null(pp_methods) && length(pp_methods) > 0) {
      # Note: Without original data, we can only warn about presence
      # The actual check would need data to be passed
    }
  }

  # Check 4: Examine resampling indices used
  if (!is.null(object$control$index)) {
    # Already covered by trainControl inspection above
  }

  risks
}

#' @noRd
.inspect_data_frame <- function(object, train_idx, test_idx,
                                target_col = NULL, spatial_cols = NULL, ...) {
  risks <- list()

  # Check for duplicate rows between train and test
  if (!is.null(train_idx) && !is.null(test_idx)) {
    # Check bounds
    max_idx <- max(c(train_idx, test_idx))
    if (max_idx > nrow(object)) {
      risks <- c(risks, list(list(
        type = "invalid_indices",
        severity = "hard_violation",
        description = sprintf(
          "Indices exceed data dimensions (max index %d > nrow %d)",
          max_idx, nrow(object)
        ),
        affected_indices = integer(0),
        source_object = "data.frame"
      )))
      return(risks)
    }

    # Check for duplicate rows using C++ backend if available
    # Get numeric columns for comparison
    numeric_cols <- vapply(object, is.numeric, logical(1))

    if (any(numeric_cols)) {
      # Use C++ backend for numeric data
      numeric_data <- as.matrix(object[, numeric_cols, drop = FALSE])

      dup_result <- tryCatch(
        checkDuplicateRows(numeric_data, train_idx, test_idx),
        error = function(e) NULL
      )

      if (!is.null(dup_result) && dup_result$has_duplicates) {
        risks <- c(risks, list(list(
          type = "duplicate_rows",
          severity = "hard_violation",
          description = sprintf(
            "Test set contains %d rows identical to training rows (memorization risk)",
            dup_result$n_duplicates
          ),
          affected_indices = dup_result$duplicate_indices,
          source_object = "data.frame"
        )))
      }
    } else {
      # Fallback to R-based comparison for non-numeric data
      train_data <- object[train_idx, , drop = FALSE]
      test_data <- object[test_idx, , drop = FALSE]

      train_hashes <- apply(train_data, 1, function(r) paste(r, collapse = "|"))
      test_hashes <- apply(test_data, 1, function(r) paste(r, collapse = "|"))

      duplicates <- which(test_hashes %in% train_hashes)
      if (length(duplicates) > 0) {
        risks <- c(risks, list(list(
          type = "duplicate_rows",
          severity = "hard_violation",
          description = sprintf(
            "Test set contains %d rows identical to training rows (memorization risk)",
            length(duplicates)
          ),
          affected_indices = test_idx[duplicates],
          source_object = "data.frame"
        )))
      }
    }
  }

  # =========================================================================
  # Target leakage detection
  # =========================================================================

  if (!is.null(target_col) && target_col %in% names(object)) {
    target <- object[[target_col]]

    # Only check numeric targets
    if (is.numeric(target)) {
      # Get numeric feature columns (exclude target)
      feature_cols <- setdiff(names(object), target_col)
      numeric_features <- feature_cols[vapply(object[feature_cols], is.numeric, logical(1))]

      if (length(numeric_features) > 0) {
        # Compute correlations with target (use training data only to avoid bias)
        if (!is.null(train_idx)) {
          train_target <- target[train_idx]
          train_features <- object[train_idx, numeric_features, drop = FALSE]
        } else {
          train_target <- target
          train_features <- object[, numeric_features, drop = FALSE]
        }

        # Compute correlations, handling NAs
        correlations <- vapply(train_features, function(feat) {
          if (sd(feat, na.rm = TRUE) == 0 || sd(train_target, na.rm = TRUE) == 0) {
            return(NA_real_)
          }
          cor(feat, train_target, use = "pairwise.complete.obs")
        }, numeric(1))

        # Check for target leakage (|cor| > 0.99)
        abs_cor <- abs(correlations)
        direct_leakage <- which(abs_cor > 0.99 & !is.na(abs_cor))

        if (length(direct_leakage) > 0) {
          leaked_features <- numeric_features[direct_leakage]
          leaked_cors <- correlations[direct_leakage]

          for (i in seq_along(leaked_features)) {
            risks <- c(risks, list(list(
              type = "target_leakage_direct",
              severity = "hard_violation",
              description = sprintf(
                "Feature '%s' has correlation %.3f with target '%s'. Likely derived from outcome.",
                leaked_features[i], leaked_cors[i], target_col
              ),
              affected_indices = integer(0),
              source_object = sprintf("data.frame$%s", leaked_features[i])
            )))
          }
        }

        # Check for proxy leakage (|cor| 0.95-0.99)
        proxy_leakage <- which(abs_cor >= 0.95 & abs_cor <= 0.99 & !is.na(abs_cor))

        if (length(proxy_leakage) > 0) {
          proxy_features <- numeric_features[proxy_leakage]
          proxy_cors <- correlations[proxy_leakage]

          for (i in seq_along(proxy_features)) {
            risks <- c(risks, list(list(
              type = "target_leakage_proxy",
              severity = "soft_inflation",
              description = sprintf(
                "Feature '%s' has correlation %.3f with target '%s'. May be a proxy for outcome.",
                proxy_features[i], proxy_cors[i], target_col
              ),
              affected_indices = integer(0),
              source_object = sprintf("data.frame$%s", proxy_features[i])
            )))
          }
        }
      }
    }
  }

  # =========================================================================
  # Spatial separation check
  # =========================================================================

  if (!is.null(spatial_cols) && length(spatial_cols) >= 2 &&
      !is.null(train_idx) && !is.null(test_idx)) {

    # Check that spatial columns exist and are numeric
    spatial_cols_valid <- spatial_cols[spatial_cols %in% names(object)]
    spatial_cols_valid <- spatial_cols_valid[
      vapply(object[spatial_cols_valid], is.numeric, logical(1))
    ]

    if (length(spatial_cols_valid) >= 2) {
      # Use first two columns as coordinates
      coord_cols <- spatial_cols_valid[1:2]

      train_coords <- as.matrix(object[train_idx, coord_cols, drop = FALSE])
      test_coords <- as.matrix(object[test_idx, coord_cols, drop = FALSE])

      # Compute minimum distance from each test point to any train point
      # This is O(n*m) but acceptable for moderate dataset sizes
      min_distances <- vapply(seq_len(nrow(test_coords)), function(i) {
        test_point <- test_coords[i, ]
        dists <- sqrt(rowSums((train_coords - matrix(test_point, nrow = nrow(train_coords),
                                                      ncol = 2, byrow = TRUE))^2))
        min(dists, na.rm = TRUE)
      }, numeric(1))

      # Estimate spatial scale from training data spread
      train_spread <- sqrt(sum(apply(train_coords, 2, var, na.rm = TRUE)))

      # Flag if test points are very close to train points (< 1% of spread)
      close_threshold <- train_spread * 0.01
      very_close <- which(min_distances < close_threshold)

      if (length(very_close) > 0) {
        risks <- c(risks, list(list(
          type = "spatial_proximity",
          severity = "soft_inflation",
          description = sprintf(
            "%d test points are very close to training points (< 1%% of spatial spread). Spatial autocorrelation may inflate performance.",
            length(very_close)
          ),
          affected_indices = test_idx[very_close],
          source_object = "data.frame"
        )))
      }

      # Also check if train and test regions overlap substantially
      # Use convex hull overlap as a proxy
      train_hull <- tryCatch(chull(train_coords), error = function(e) NULL)
      test_hull <- tryCatch(chull(test_coords), error = function(e) NULL)

      if (!is.null(train_hull) && !is.null(test_hull)) {
        # Check how many test points fall inside training hull
        # Simple point-in-polygon check
        n_test_in_train <- sum(vapply(seq_len(nrow(test_coords)), function(i) {
          .point_in_polygon(test_coords[i, ], train_coords[train_hull, ])
        }, logical(1)))

        overlap_pct <- n_test_in_train / nrow(test_coords) * 100

        if (overlap_pct > 50) {
          risks <- c(risks, list(list(
            type = "spatial_overlap",
            severity = "soft_inflation",
            description = sprintf(
              "%.0f%% of test points fall within the training region convex hull. Consider spatial blocking.",
              overlap_pct
            ),
            affected_indices = test_idx,
            source_object = "data.frame"
          )))
        }
      }
    }
  }

  risks
}

#' Point in polygon test (ray casting)
#' @noRd
.point_in_polygon <- function(point, polygon) {
  x <- point[1]
  y <- point[2]
  n <- nrow(polygon)
  inside <- FALSE

  j <- n
  for (i in seq_len(n)) {
    xi <- polygon[i, 1]
    yi <- polygon[i, 2]
    xj <- polygon[j, 1]
    yj <- polygon[j, 2]

    if (((yi > y) != (yj > y)) &&
        (x < (xj - xi) * (y - yi) / (yj - yi) + xi)) {
      inside <- !inside
    }
    j <- i
  }

  inside
}

# ===========================================================================
# Model Object Inspectors
# ===========================================================================

#' @noRd
.inspect_lm <- function(object, train_idx, test_idx, data, ...) {
  risks <- list()

  if (is.null(train_idx)) return(risks)

  # Check 1: Model was fitted on correct number of observations
  n_model <- length(object$fitted.values)
  n_expected <- length(train_idx)

  if (n_model != n_expected) {
    risks <- c(risks, list(list(
      type = "model_scope",
      severity = "hard_violation",
      description = sprintf(
        "Model was fitted on %d observations, but training set has %d. Possible data leakage.",
        n_model, n_expected
      ),
      affected_indices = test_idx,
      source_object = "lm"
    )))
  }

  # Check 2: If we have the original data, verify row alignment
 if (!is.null(data) && n_model == n_expected) {
    # Get model's training data from model frame
    model_data <- tryCatch(model.frame(object), error = function(e) NULL)

    if (!is.null(model_data) && nrow(model_data) == n_expected) {
      # Compare row names if available
      model_rows <- as.integer(rownames(model_data))
      if (!any(is.na(model_rows))) {
        unexpected <- setdiff(model_rows, train_idx)
        if (length(unexpected) > 0) {
          risks <- c(risks, list(list(
            type = "model_data_leak",
            severity = "hard_violation",
            description = sprintf(
              "Model was fitted on rows not in training set (%d unexpected rows)",
              length(unexpected)
            ),
            affected_indices = unexpected,
            source_object = "lm"
          )))
        }
      }
    }
  }

  risks
}

#' @noRd
.inspect_glm <- function(object, train_idx, test_idx, data, ...) {
  # GLM inspection is similar to LM
  risks <- .inspect_lm(object, train_idx, test_idx, data, ...)

  # Update source_object references
  for (i in seq_along(risks)) {
    risks[[i]]$source_object <- "glm"
  }

  risks
}

#' @noRd
.inspect_ranger <- function(object, train_idx, test_idx, data, ...) {
  risks <- list()

  if (is.null(train_idx)) return(risks)

  # Check 1: Number of training observations
  n_model <- object$num.samples
  n_expected <- length(train_idx)

  if (n_model != n_expected) {
    risks <- c(risks, list(list(
      type = "model_scope",
      severity = "hard_violation",
      description = sprintf(
        "Ranger model was trained on %d observations, but training set has %d. Possible data leakage.",
        n_model, n_expected
      ),
      affected_indices = test_idx,
      source_object = "ranger"
    )))
  }

  # Check 2: OOB predictions should not exist for test indices
  # (if model stored predictions)
  if (!is.null(object$predictions) && length(object$predictions) > 0) {
    # OOB predictions are for training data only
    if (length(object$predictions) > n_expected) {
      risks <- c(risks, list(list(
        type = "model_scope",
        severity = "soft_inflation",
        description = "Ranger OOB predictions exceed training set size",
        affected_indices = test_idx,
        source_object = "ranger"
      )))
    }
  }

  risks
}

#' @noRd
.inspect_xgboost <- function(object, train_idx, test_idx, data, ...) {
  risks <- list()

  if (is.null(train_idx)) return(risks)

  # xgboost models store niter (number of boosting rounds) but not training size directly
 # We can check the training log if available

  # Check 1: If evaluation log exists, check for test data in training
  if (!is.null(object$evaluation_log)) {
    eval_log <- object$evaluation_log

    # If there's a "test" metric that was used during training, that's suspicious
    # unless it was for early stopping with proper validation set
    test_cols <- grep("^test_", names(eval_log), value = TRUE)

    if (length(test_cols) > 0 && is.null(object$params$early_stopping_rounds)) {
      risks <- c(risks, list(list(
        type = "hpo_test_usage",
        severity = "soft_inflation",
        description = "XGBoost model has test metrics in evaluation log without early stopping - possible test data leakage during training",
        affected_indices = test_idx,
        source_object = "xgb.Booster"
      )))
    }
  }

  # Check 2: Early stopping on test data is a common leak
  if (!is.null(object$best_iteration) && !is.null(object$params$early_stopping_rounds)) {
    # This is often fine if using validation set, but warn if suspicious
    risks <- c(risks, list(list(
      type = "early_stopping_check",
      severity = "soft_inflation",
      description = sprintf(
        "XGBoost used early stopping (best iteration: %d). Verify validation set does not overlap with final test set.",
        object$best_iteration
      ),
      affected_indices = test_idx,
      source_object = "xgb.Booster"
    )))
  }

  risks
}

#' @noRd
.inspect_lightgbm <- function(object, train_idx, test_idx, data, ...) {
  risks <- list()

  if (is.null(train_idx)) return(risks)

  # LightGBM models have similar structure to xgboost
  # Check for early stopping issues

  if (!is.null(object$best_iter) && object$best_iter > 0) {
    risks <- c(risks, list(list(
      type = "early_stopping_check",
      severity = "soft_inflation",
      description = sprintf(
        "LightGBM used early stopping (best iteration: %d). Verify validation set does not overlap with final test set.",
        object$best_iter
      ),
      affected_indices = test_idx,
      source_object = "lgb.Booster"
    )))
  }

  risks
}

#' @noRd
.inspect_parsnip <- function(object, train_idx, test_idx, data, ...) {
  risks <- list()

  if (is.null(train_idx)) return(risks)

  # parsnip model_fit objects wrap underlying engines
  # Extract the underlying fit and dispatch to appropriate inspector

  if (!is.null(object$fit)) {
    underlying <- object$fit
    underlying_class <- class(underlying)[1]

    # Dispatch to underlying model inspector
    underlying_risks <- if (underlying_class == "lm") {
      .inspect_lm(underlying, train_idx, test_idx, data, ...)
    } else if (underlying_class == "glm") {
      .inspect_glm(underlying, train_idx, test_idx, data, ...)
    } else if (inherits(underlying, "ranger")) {
      .inspect_ranger(underlying, train_idx, test_idx, data, ...)
    } else if (inherits(underlying, "xgb.Booster")) {
      .inspect_xgboost(underlying, train_idx, test_idx, data, ...)
    } else if (inherits(underlying, "lgb.Booster")) {
      .inspect_lightgbm(underlying, train_idx, test_idx, data, ...)
    } else {
      list()
    }

    # Update source to indicate parsnip wrapper
    for (i in seq_along(underlying_risks)) {
      underlying_risks[[i]]$source_object <- paste0(
        "parsnip(", underlying_risks[[i]]$source_object, ")"
      )
    }

    risks <- c(risks, underlying_risks)
  }

  # Check parsnip-specific issues
  # Spec should match what was trained
  if (!is.null(object$spec) && !is.null(object$fit)) {
    # Could check mode, engine consistency
  }

  risks
}

#' @noRd
.inspect_workflow <- function(object, train_idx, test_idx, data, ...) {
  risks <- list()

  if (is.null(train_idx)) return(risks)

  # Workflows combine preprocessor + model
  # Check both components

  # Check 1: Preprocessor (recipe)
  if (!is.null(object$pre$actions$recipe$recipe)) {
    recipe_obj <- object$pre$actions$recipe$recipe
    if (inherits(recipe_obj, "recipe")) {
      recipe_risks <- .inspect_recipe(recipe_obj, train_idx, test_idx, data, ...)
      for (i in seq_along(recipe_risks)) {
        recipe_risks[[i]]$source_object <- paste0(
          "workflow$pre$recipe: ", recipe_risks[[i]]$source_object
        )
      }
      risks <- c(risks, recipe_risks)
    }
  }

  # Check 2: Fitted model
  if (!is.null(object$fit$fit)) {
    model_fit <- object$fit$fit
    if (inherits(model_fit, "model_fit")) {
      model_risks <- .inspect_parsnip(model_fit, train_idx, test_idx, data, ...)
      for (i in seq_along(model_risks)) {
        model_risks[[i]]$source_object <- paste0(
          "workflow$fit: ", model_risks[[i]]$source_object
        )
      }
      risks <- c(risks, model_risks)
    }
  }

  # Check 3: Workflow was fitted on correct data
  if (!is.null(object$fit$fit$fit)) {
    # Try to get training row count from underlying model
  }

  risks
}

#' @noRd
.inspect_tune_results <- function(object, train_idx, test_idx, data, ...) {
  risks <- list()

  if (is.null(train_idx)) return(risks)

  # tune_results from tidymodels tune package contains:
  # - splits: resampling splits used during tuning
  # - .metrics: performance metrics
  # - .notes: any warnings/errors

  # Check 1: Verify splits don't include test data
  if ("splits" %in% names(object)) {
    splits <- object$splits

    for (i in seq_along(splits)) {
      split <- splits[[i]]

      # Get analysis (training) indices for this resample
      if (!is.null(split$in_id)) {
        analysis_idx <- split$in_id

        # Check if any test indices are in the analysis set
        leaked_test <- intersect(analysis_idx, test_idx)

        if (length(leaked_test) > 0) {
          risks <- c(risks, list(list(
            type = "tune_test_in_resamples",
            severity = "hard_violation",
            description = sprintf(
              "Tuning resample %d uses %d test indices in training fold. HPO is using test data.",
              i, length(leaked_test)
            ),
            affected_indices = leaked_test,
            source_object = "tune_results"
          )))
        }
      }
    }
  }

  # Check 2: Verify the object was created from train data only
  # tune_results stores the original data size
  if (".config" %in% names(attributes(object))) {
    # Config may contain info about data used
  }

  # Check 3: Look for suspiciously good metrics (potential overfitting to CV)
  if (".metrics" %in% names(object)) {
    # Could check if best metrics are implausibly good
  }

  # Check 4: Verify resamples are nested within train set
  n_train <- length(train_idx)
  n_test <- length(test_idx)
  n_total <- max(c(train_idx, test_idx))

  # If tune was done on full data, the split indices will span beyond train
  if ("splits" %in% names(object) && length(object$splits) > 0) {
    first_split <- object$splits[[1]]

    # Check the data attribute of the split
    if (!is.null(first_split$data) && is.data.frame(first_split$data)) {
      split_nrow <- nrow(first_split$data)

      if (split_nrow == n_total) {
        risks <- c(risks, list(list(
          type = "tune_on_full_data",
          severity = "hard_violation",
          description = sprintf(
            "Tuning was performed on %d observations (full dataset). Should use only training data (%d observations).",
            split_nrow, n_train
          ),
          affected_indices = test_idx,
          source_object = "tune_results"
        )))
      }
    }
  }

  risks
}

#' @noRd
.inspect_generic <- function(object, train_idx, test_idx, data, ...) {
 # Generic fallback: no specific inspection available
 list()
}

# Null-coalescing operator (internal)
`%||%` <- function(x, y) if (is.null(x)) y else x

#' Check if object is a caret trainControl
#' @noRd
.is_trainControl <- function(object) {
  # trainControl is a list with specific fields
  if (!is.list(object)) return(FALSE)

  # Must have these fields to be a trainControl
  required_fields <- c("method", "number", "repeats")
  has_required <- all(required_fields %in% names(object))

  # Method should be a character string for CV type
  valid_method <- has_required &&
    is.character(object$method) &&
    length(object$method) == 1

  has_required && valid_method
}
