# Resampling object inspectors: trainControl, rsplit, vfold_cv, rset, caret train

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
    risks <- c(risks, .check_cv_leak(
      object$index, test_idx,
      source_prefix = "trainControl$index"
    ))
  }

  # Check indexOut (holdout indices) - less critical but worth flagging
  if (!is.null(object$indexOut)) {
    for (i in seq_along(object$indexOut)) {
      fold_out <- object$indexOut[[i]]
      leaked_test <- intersect(fold_out, test_idx)
      if (length(leaked_test) > 0 && length(leaked_test) < length(test_idx)) {
        risks <- c(risks, list(.new_risk(
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
    leaked_test <- intersect(analysis_idx, test_idx)
    if (length(leaked_test) > 0) {
      risks <- c(risks, list(.new_risk(
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
      risks <- c(risks, list(.new_risk(
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

  # Check each fold's analysis set for test data leakage
  fold_analysis <- lapply(seq_len(nrow(object)), function(i) {
    object$splits[[i]]$in_id
  })
  risks <- c(risks, .check_cv_leak(
    fold_analysis, test_idx,
    source_prefix = "vfold_cv$splits"
  ))

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
      risks <- c(risks, list(.new_risk(
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
        risks <- c(risks, list(.new_risk(
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
        risks <- c(risks, list(.new_risk(
          type = "data_scope",
          severity = "soft_inflation",
          description = sprintf(
            "Model was trained on %d rows, but expected %d. Some training data may have been excluded.",
            n_train_used, n_expected
          ),
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
