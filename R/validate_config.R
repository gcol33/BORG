# Threshold selection and HPO validation checks for borg_validate()

#' Check for threshold selection issues
#' @noRd
.check_threshold_selection <- function(workflow) {
  risks <- list()

  thresholds <- workflow$thresholds

  if (!is.list(thresholds)) {
    return(risks)
  }

  # Check if threshold was optimized on test data
  if (!is.null(thresholds$optimized_on)) {
    if (thresholds$optimized_on == "test") {
      risks <- c(risks, list(.new_risk(
        type = "threshold_leak",
        severity = "hard_violation",
        description = "Classification threshold was optimized on test data",
        affected_indices = workflow$test_idx,
        source_object = "workflow$thresholds"
      )))
    } else if (thresholds$optimized_on == "full") {
      risks <- c(risks, list(.new_risk(
        type = "threshold_leak",
        severity = "hard_violation",
        description = "Classification threshold was optimized on full data (includes test)",
        affected_indices = workflow$test_idx,
        source_object = "workflow$thresholds"
      )))
    }
  }

  # Check if threshold selection used information from predictions on test
  if (!is.null(thresholds$selection_method)) {
    if (thresholds$selection_method %in% c("youden", "f1_max", "roc_optimal")) {
      # These methods require predictions - check if they used test predictions
      if (isTRUE(thresholds$used_test_predictions)) {
        risks <- c(risks, list(.new_risk(
          type = "threshold_leak",
          severity = "hard_violation",
          description = sprintf(
            "Threshold selection method '%s' used test set predictions",
            thresholds$selection_method
          ),
          affected_indices = workflow$test_idx,
          source_object = "workflow$thresholds"
        )))
      }
    }
  }

  # Check for post-hoc threshold adjustment based on test performance
  if (!is.null(thresholds$adjusted_after_test) && isTRUE(thresholds$adjusted_after_test)) {
    risks <- c(risks, list(.new_risk(
      type = "threshold_leak",
      severity = "hard_violation",
      description = "Threshold was adjusted after seeing test set performance",
      affected_indices = workflow$test_idx,
      source_object = "workflow$thresholds"
    )))
  }

  risks
}


#' Check for hyperparameter optimization validation issues
#' @noRd
.check_hpo_validation <- function(workflow) {
  risks <- list()

  hpo <- workflow$hpo

  if (!is.list(hpo)) {
    return(risks)
  }

  # ===========================================================================
  # 1. Check if HPO used test data
  # ===========================================================================

  if (!is.null(hpo$used_test_data) && isTRUE(hpo$used_test_data)) {
    risks <- c(risks, list(.new_risk(
      type = "hpo_test_leak",
      severity = "hard_violation",
      description = "Hyperparameter optimization used test data for tuning",
      affected_indices = workflow$test_idx,
      source_object = "workflow$hpo"
    )))
  }

  # ===========================================================================
  # 2. Check if final model selection used test performance
  # ===========================================================================

  if (!is.null(hpo$model_selected_on)) {
    if (hpo$model_selected_on == "test") {
      risks <- c(risks, list(.new_risk(
        type = "hpo_selection_leak",
        severity = "hard_violation",
        description = "Final model was selected based on test set performance",
        affected_indices = workflow$test_idx,
        source_object = "workflow$hpo"
      )))
    } else if (hpo$model_selected_on == "full") {
      risks <- c(risks, list(.new_risk(
        type = "hpo_selection_leak",
        severity = "hard_violation",
        description = "Final model was selected based on full data performance",
        affected_indices = workflow$test_idx,
        source_object = "workflow$hpo"
      )))
    }
  }

  # ===========================================================================
  # 3. Check for proper nested CV structure
  # ===========================================================================

  if (!is.null(hpo$cv_type)) {
    if (hpo$cv_type == "simple" && !isTRUE(hpo$nested)) {
      # Simple CV without nesting can cause optimistic bias
      risks <- c(risks, list(.new_risk(
        type = "hpo_cv_bias",
        severity = "soft_inflation",
        description = "HPO uses simple (non-nested) CV which may cause optimistic bias in performance estimates",
        affected_indices = integer(0),
        source_object = "workflow$hpo"
      )))
    }
  }

  # ===========================================================================
  # 4. Check for excessive number of configurations tested
  # ===========================================================================

  if (!is.null(hpo$n_configurations)) {
    n_train <- length(workflow$train_idx)
    n_configs <- hpo$n_configurations

    # Rule of thumb: more configurations than training samples is suspicious
    if (n_configs > n_train) {
      risks <- c(risks, list(.new_risk(
        type = "hpo_overfit_risk",
        severity = "soft_inflation",
        description = sprintf(
          "HPO tested %d configurations with only %d training samples - high risk of overfitting to validation set",
          n_configs, n_train
        ),
        affected_indices = integer(0),
        source_object = "workflow$hpo"
      )))
    }
  }

  # ===========================================================================
  # 5. Check for data snooping in feature selection during HPO
  # ===========================================================================

  if (!is.null(hpo$feature_selection)) {
    if (isTRUE(hpo$feature_selection$used_full_data)) {
      risks <- c(risks, list(.new_risk(
        type = "hpo_feature_leak",
        severity = "hard_violation",
        description = "Feature selection during HPO used full data instead of CV folds",
        affected_indices = workflow$test_idx,
        source_object = "workflow$hpo"
      )))
    }
  }

  # ===========================================================================
  # 6. Check for early stopping based on test data
  # ===========================================================================

  if (!is.null(hpo$early_stopping)) {
    if (isTRUE(hpo$early_stopping$used_test_data)) {
      risks <- c(risks, list(.new_risk(
        type = "hpo_early_stopping_leak",
        severity = "hard_violation",
        description = "Early stopping during HPO was based on test set performance",
        affected_indices = workflow$test_idx,
        source_object = "workflow$hpo"
      )))
    }
  }

  risks
}
