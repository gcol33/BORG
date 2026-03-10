# Model object inspectors: lm, glm, ranger, xgboost, lightgbm, parsnip,
# workflow, tune_results

#' @noRd
.inspect_lm <- function(object, train_idx, test_idx, data, ...) {
  risks <- list()

  if (is.null(train_idx)) return(risks)

  # Check 1: Model was fitted on correct number of observations
  n_model <- length(object$fitted.values)
  n_expected <- length(train_idx)

  if (n_model != n_expected) {
    risks <- c(risks, list(.new_risk(
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
          risks <- c(risks, list(.new_risk(
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
    risks <- c(risks, list(.new_risk(
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
      risks <- c(risks, list(.new_risk(
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
      risks <- c(risks, list(.new_risk(
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
    risks <- c(risks, list(.new_risk(
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
    risks <- c(risks, list(.new_risk(
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
          risks <- c(risks, list(.new_risk(
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
        risks <- c(risks, list(.new_risk(
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
