#' Automatically Rewrite Leaky Evaluation Pipelines
#'
#' `borg_rewrite()` attempts to automatically fix detected evaluation risks
#' by restructuring the pipeline to avoid information leakage.
#'
#' @param workflow A list containing the evaluation workflow (same structure
#'   as \code{\link{borg_validate}}).
#' @param risks Optional \code{\link{BorgRisk}} object from a previous
#'   inspection. If NULL, `borg_validate()` is called first.
#' @param fix Character vector specifying which risk types to attempt to fix.
#'   Default: \code{"all"} attempts all rewritable violations.
#'   Other options: \code{"preprocessing"}, \code{"feature_engineering"},
#'   \code{"thresholds"}.
#'
#' @return A list containing:
#'   \describe{
#'     \item{workflow}{The rewritten workflow (modified in place where possible)}
#'     \item{fixed}{Character vector of risk types that were successfully fixed}
#'     \item{unfixable}{Character vector of risk types that could not be fixed}
#'     \item{report}{\code{BorgRisk} object from post-rewrite validation}
#'   }
#'
#' @details
#' `borg_rewrite()` can automatically fix certain types of leakage:
#'
#' \describe{
#'   \item{Preprocessing on full data}{Refits preprocessing objects using
#'     only training indices}
#'   \item{Feature engineering leaks}{Recomputes target encodings, embeddings,
#'     and derived features using train-only data}
#'   \item{Threshold optimization}{Moves threshold selection to training/validation
#'     data}
#' }
#'
#' Some violations cannot be automatically fixed:
#' \itemize{
#'   \item Train-test index overlap (requires new split)
#'   \item Target leakage in original features (requires domain intervention)
#'   \item Temporal look-ahead in features (requires feature re-engineering)
#' }
#'
#' @seealso
#' \code{\link{borg_validate}} for validation without rewriting,
#' \code{\link{borg}} for proactive enforcement.
#'
#' @examples
#' \dontrun{
#' # Attempt to fix a leaky workflow
#' result <- borg_rewrite(my_workflow)
#'
#' if (length(result$unfixable) > 0) {
#'   warning("Some risks could not be automatically fixed")
#'   print(result$unfixable)
#' }
#'
#' # Use the fixed workflow
#' fixed_workflow <- result$workflow
#' }
#'
#' @export
borg_rewrite <- function(
 workflow,
 risks = NULL,
 fix = "all"
) {

 # ===========================================================================
 # Input validation
 # ===========================================================================

 if (!is.list(workflow)) {
   stop("'workflow' must be a list")
 }

 # Validate if no risks provided
 if (is.null(risks)) {
   risks <- borg_validate(workflow, strict = FALSE)
 }

 if (!inherits(risks, "BorgRisk")) {
   stop("'risks' must be a BorgRisk object")
 }

 # ===========================================================================
 # Categorize risks by fixability
 # ===========================================================================

 rewritable_types <- c(
   "preprocessing_leak",
   "normalization_leak",
   "imputation_leak",
   "pca_leak",
   "encoding_leak",
   "threshold_leak"
 )

 unfixable_types <- c(
   "index_overlap",
   "duplicate_rows",
   "target_leakage_direct",
   "temporal_lookahead",
   "group_overlap"
 )

 fixed <- character(0)
 unfixable <- character(0)

 # ===========================================================================
 # Attempt fixes
 # ===========================================================================

 workflow_modified <- workflow

 for (risk in risks@risks) {
   risk_type <- risk$type

   if (risk_type %in% unfixable_types) {
     unfixable <- c(unfixable, risk_type)
     next
   }

   if (fix != "all" && !risk_type %in% fix) {
     next
   }

   # Dispatch to type-specific rewriters
   fix_result <- switch(risk_type,
     "preprocessing_leak" = .rewrite_preprocessing(workflow_modified, risk),
     "normalization_leak" = .rewrite_normalization(workflow_modified, risk),
     "imputation_leak"    = .rewrite_imputation(workflow_modified, risk),
     "pca_leak"           = .rewrite_pca(workflow_modified, risk),
     "encoding_leak"      = .rewrite_encoding(workflow_modified, risk),
     "threshold_leak"     = .rewrite_threshold(workflow_modified, risk),
     NULL
   )

   if (!is.null(fix_result)) {
     workflow_modified <- fix_result$workflow
     if (fix_result$success) {
       fixed <- c(fixed, risk_type)
     } else {
       unfixable <- c(unfixable, risk_type)
     }
   }
 }

 # ===========================================================================
 # Re-validate after rewriting
 # ===========================================================================

 post_risks <- borg_validate(workflow_modified, strict = FALSE)

 list(
   workflow = workflow_modified,
   fixed = unique(fixed),
   unfixable = unique(unfixable),
   report = post_risks
 )
}


# ===========================================================================
# Type-specific rewriters
# ===========================================================================

#' Extract train-only data from workflow
#' @noRd
.get_train_data <- function(workflow) {
  data <- workflow$data
  train_idx <- workflow$train_idx
  if (is.null(data) || is.null(train_idx)) return(NULL)
  data[train_idx, , drop = FALSE]
}

#' Replace preprocessing object(s) in workflow
#' @noRd
.replace_preprocess <- function(workflow, new_pp) {
  pp <- workflow$preprocess
  if (is.null(pp)) return(workflow)

  if (is.list(pp) && !inherits(pp, "preProcess") && !inherits(pp, "recipe") &&
      !inherits(pp, "prcomp")) {
    # List of preprocessing objects - find and replace matching type
    for (i in seq_along(pp)) {
      if (identical(class(pp[[i]]), class(new_pp))) {
        pp[[i]] <- new_pp
        workflow$preprocess <- pp
        return(workflow)
      }
    }
    # No match found, replace first element
    pp[[1]] <- new_pp
    workflow$preprocess <- pp
  } else {
    workflow$preprocess <- new_pp
  }
  workflow
}


#' @noRd
.rewrite_preprocessing <- function(workflow, risk) {
  train_data <- .get_train_data(workflow)
  if (is.null(train_data)) {
    return(list(workflow = workflow, success = FALSE))
  }

  pp <- workflow$preprocess

  # Delegate to type-specific rewriters for non-preProcess objects
  if (inherits(pp, "prcomp")) {
    return(.rewrite_pca(workflow, risk))
  }
  if (inherits(pp, "recipe")) {
    return(.rewrite_recipe_on_train(workflow, train_data))
  }

  if (!requireNamespace("caret", quietly = TRUE)) {
    return(list(workflow = workflow, success = FALSE))
  }

  old_pp <- if (inherits(pp, "preProcess")) {
    pp
  } else if (is.list(pp) && !inherits(pp, "preProcess")) {
    matches <- Filter(function(x) inherits(x, "preProcess"), pp)
    if (length(matches) > 0) matches[[1]] else NULL
  } else {
    NULL
  }

  if (is.null(old_pp) || !inherits(old_pp, "preProcess")) {
    return(list(workflow = workflow, success = FALSE))
  }

  # Extract the method used and refit on train data only
  # Filter out "ignore" entries - caret::preProcess warns about them
  method <- old_pp$method
  method <- method[names(method) != "ignore"]
  numeric_cols <- names(train_data)[vapply(train_data, is.numeric, logical(1))]
  if (length(numeric_cols) == 0) {
    return(list(workflow = workflow, success = FALSE))
  }

  new_pp <- tryCatch(
    caret::preProcess(train_data[, numeric_cols, drop = FALSE], method = method),
    error = function(e) NULL
  )

  if (is.null(new_pp)) {
    return(list(workflow = workflow, success = FALSE))
  }

  workflow <- .replace_preprocess(workflow, new_pp)
  list(workflow = workflow, success = TRUE)
}


#' @noRd
.rewrite_normalization <- function(workflow, risk) {
  # Refit normalization (center/scale) on training data only
  # Handles both caret preProcess and base R scale() stored as matrix
  train_data <- .get_train_data(workflow)
  if (is.null(train_data)) {
    return(list(workflow = workflow, success = FALSE))
  }

  # If the workflow has a preProcess object with center/scale, defer to
  # .rewrite_preprocessing which handles caret objects

  pp <- workflow$preprocess
  if (!is.null(pp) && (inherits(pp, "preProcess") ||
      (is.list(pp) && any(vapply(pp, function(x) inherits(x, "preProcess"), logical(1)))))) {
    return(.rewrite_preprocessing(workflow, risk))
  }

  # Handle case where data itself was scaled (matrix with center/scale attributes)
  data <- workflow$data
  if (is.matrix(data) && !is.null(attr(data, "scaled:center"))) {
    numeric_train <- data[workflow$train_idx, , drop = FALSE]
    # Recompute on original unscaled data if possible, otherwise re-scale
    centers <- colMeans(numeric_train, na.rm = TRUE)
    scales <- apply(numeric_train, 2, sd, na.rm = TRUE)
    scales[scales == 0] <- 1

    rescaled <- scale(data, center = centers, scale = scales)
    workflow$data <- rescaled
    return(list(workflow = workflow, success = TRUE))
  }

  list(workflow = workflow, success = FALSE)
}


#' @noRd
.rewrite_imputation <- function(workflow, risk) {
  # Refit imputation on training data only
  train_data <- .get_train_data(workflow)
  if (is.null(train_data)) {
    return(list(workflow = workflow, success = FALSE))
  }

  # Handle caret preProcess with imputation methods
  if (requireNamespace("caret", quietly = TRUE)) {
    pp <- workflow$preprocess
    old_pp <- if (inherits(pp, "preProcess")) pp
              else if (is.list(pp)) Filter(function(x) inherits(x, "preProcess"), pp)[[1]]
              else NULL

    if (!is.null(old_pp) && inherits(old_pp, "preProcess")) {
      method <- old_pp$method
      impute_methods <- c("knnImpute", "bagImpute", "medianImpute")
      if (any(method %in% impute_methods)) {
        numeric_cols <- names(train_data)[vapply(train_data, is.numeric, logical(1))]
        if (length(numeric_cols) > 0) {
          new_pp <- tryCatch(
            caret::preProcess(train_data[, numeric_cols, drop = FALSE], method = method),
            error = function(e) NULL
          )
          if (!is.null(new_pp)) {
            workflow <- .replace_preprocess(workflow, new_pp)
            return(list(workflow = workflow, success = TRUE))
          }
        }
      }
    }
  }

  # Handle recipes with imputation steps
  if (requireNamespace("recipes", quietly = TRUE)) {
    pp <- workflow$preprocess
    if (inherits(pp, "recipe")) {
      return(.rewrite_recipe_on_train(workflow, train_data))
    }
  }

  list(workflow = workflow, success = FALSE)
}


#' @noRd
.rewrite_pca <- function(workflow, risk) {
  # Refit PCA on training data only
  train_data <- .get_train_data(workflow)
  if (is.null(train_data)) {
    return(list(workflow = workflow, success = FALSE))
  }

  # Handle base R prcomp
  pp <- workflow$preprocess
  if (inherits(pp, "prcomp")) {
    numeric_cols <- names(train_data)[vapply(train_data, is.numeric, logical(1))]
    if (length(numeric_cols) == 0) {
      return(list(workflow = workflow, success = FALSE))
    }

    new_pca <- tryCatch(
      prcomp(train_data[, numeric_cols, drop = FALSE],
             center = pp$center[1] != 0 || is.logical(pp$center),
             scale. = !is.null(pp$scale) && (pp$scale[1] != 1 || is.logical(pp$scale))),
      error = function(e) NULL
    )

    if (!is.null(new_pca)) {
      workflow$preprocess <- new_pca
      return(list(workflow = workflow, success = TRUE))
    }
  }

  # Handle caret preProcess with PCA
  if (requireNamespace("caret", quietly = TRUE)) {
    old_pp <- if (inherits(pp, "preProcess")) pp
              else if (is.list(pp)) {
                matches <- Filter(function(x) inherits(x, "preProcess"), pp)
                if (length(matches) > 0) matches[[1]] else NULL
              } else NULL

    if (!is.null(old_pp) && "pca" %in% old_pp$method) {
      numeric_cols <- names(train_data)[vapply(train_data, is.numeric, logical(1))]
      if (length(numeric_cols) > 0) {
        new_pp <- tryCatch(
          caret::preProcess(train_data[, numeric_cols, drop = FALSE],
                            method = old_pp$method),
          error = function(e) NULL
        )
        if (!is.null(new_pp)) {
          workflow <- .replace_preprocess(workflow, new_pp)
          return(list(workflow = workflow, success = TRUE))
        }
      }
    }
  }

  list(workflow = workflow, success = FALSE)
}


#' @noRd
.rewrite_encoding <- function(workflow, risk) {
  # Refit encoding (target encoding, one-hot, etc.) on training data only
  train_data <- .get_train_data(workflow)
  if (is.null(train_data)) {
    return(list(workflow = workflow, success = FALSE))
  }

  # Handle recipes with encoding steps
  if (requireNamespace("recipes", quietly = TRUE)) {
    pp <- workflow$preprocess
    if (inherits(pp, "recipe")) {
      return(.rewrite_recipe_on_train(workflow, train_data))
    }
  }

  # Handle caret preProcess with dummy variables
  if (requireNamespace("caret", quietly = TRUE)) {
    pp <- workflow$preprocess
    old_pp <- if (inherits(pp, "preProcess")) pp
              else if (is.list(pp)) {
                matches <- Filter(function(x) inherits(x, "preProcess"), pp)
                if (length(matches) > 0) matches[[1]] else NULL
              } else NULL

    if (!is.null(old_pp) && inherits(old_pp, "preProcess")) {
      method <- old_pp$method
      if (any(method %in% c("dummy", "zv", "nzv"))) {
        numeric_cols <- names(train_data)[vapply(train_data, is.numeric, logical(1))]
        if (length(numeric_cols) > 0) {
          new_pp <- tryCatch(
            caret::preProcess(train_data[, numeric_cols, drop = FALSE], method = method),
            error = function(e) NULL
          )
          if (!is.null(new_pp)) {
            workflow <- .replace_preprocess(workflow, new_pp)
            return(list(workflow = workflow, success = TRUE))
          }
        }
      }
    }
  }

  list(workflow = workflow, success = FALSE)
}


#' @noRd
.rewrite_threshold <- function(workflow, risk) {
  # Move threshold optimization to training/validation data
  train_data <- .get_train_data(workflow)
  if (is.null(train_data)) {
    return(list(workflow = workflow, success = FALSE))
  }

  thresholds <- workflow$thresholds
  if (is.null(thresholds) || !is.list(thresholds)) {
    return(list(workflow = workflow, success = FALSE))
  }

  # If model and target are available, re-optimize threshold on train predictions
  model <- workflow$model
  target_col <- workflow$target_col

  if (!is.null(model) && !is.null(target_col) && target_col %in% names(train_data)) {
    train_preds <- tryCatch(
      predict(model, newdata = train_data, type = "response"),
      error = function(e) {
        tryCatch(predict(model, newdata = train_data),
                 error = function(e2) NULL)
      }
    )

    if (!is.null(train_preds) && is.numeric(train_preds)) {
      train_actual <- train_data[[target_col]]

      # Re-optimize threshold on training predictions
      if (is.factor(train_actual) || is.logical(train_actual) ||
          length(unique(train_actual)) == 2) {
        train_actual_num <- as.numeric(as.factor(train_actual)) - 1

        # Simple threshold optimization: maximize Youden's J
        thresholds_to_try <- sort(unique(train_preds))
        if (length(thresholds_to_try) > 100) {
          thresholds_to_try <- quantile(train_preds,
                                        probs = seq(0, 1, length.out = 101),
                                        na.rm = TRUE)
        }

        best_j <- -Inf
        best_thresh <- 0.5

        for (thresh in thresholds_to_try) {
          pred_class <- as.integer(train_preds >= thresh)
          tp <- sum(pred_class == 1 & train_actual_num == 1, na.rm = TRUE)
          tn <- sum(pred_class == 0 & train_actual_num == 0, na.rm = TRUE)
          fp <- sum(pred_class == 1 & train_actual_num == 0, na.rm = TRUE)
          fn <- sum(pred_class == 0 & train_actual_num == 1, na.rm = TRUE)

          sens <- if ((tp + fn) > 0) tp / (tp + fn) else 0
          spec <- if ((tn + fp) > 0) tn / (tn + fp) else 0
          j <- sens + spec - 1

          if (j > best_j) {
            best_j <- j
            best_thresh <- thresh
          }
        }

        workflow$thresholds$value <- best_thresh
        workflow$thresholds$optimized_on <- "train"
        workflow$thresholds$used_test_predictions <- FALSE
        workflow$thresholds$adjusted_after_test <- FALSE

        return(list(workflow = workflow, success = TRUE))
      }
    }
  }

  # Fallback: just mark threshold as needing train-only optimization
  list(workflow = workflow, success = FALSE)
}


#' Refit a recipe on training data (shared helper)
#' @noRd
.rewrite_recipe_on_train <- function(workflow, train_data) {
  if (!requireNamespace("recipes", quietly = TRUE)) {
    return(list(workflow = workflow, success = FALSE))
  }

  old_recipe <- workflow$preprocess

  # Extract the unprepped recipe if available
  unprepped <- if (!is.null(old_recipe$template)) {
    # A prepped recipe - try to get the original steps
    tryCatch({
      # Rebuild recipe from template and steps
      new_rec <- recipes::recipe(old_recipe$var_info$variable[old_recipe$var_info$role == "outcome"] ~
                                   ., data = train_data)
      # Re-add each step from the original
      for (step in old_recipe$steps) {
        step$trained <- FALSE
        new_rec$steps <- c(new_rec$steps, list(step))
      }
      new_rec
    }, error = function(e) NULL)
  } else {
    old_recipe
  }

  if (is.null(unprepped)) {
    return(list(workflow = workflow, success = FALSE))
  }

  new_prepped <- tryCatch(
    recipes::prep(unprepped, training = train_data),
    error = function(e) NULL
  )

  if (!is.null(new_prepped)) {
    workflow$preprocess <- new_prepped
    return(list(workflow = workflow, success = TRUE))
  }

  list(workflow = workflow, success = FALSE)
}
