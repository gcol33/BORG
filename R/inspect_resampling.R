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
    indexout_leaks <- .check_cv_leak(
      object$indexOut, test_idx,
      source_prefix = "trainControl$indexOut",
      severity = "soft_inflation",
      describe = function(prefix, i, n) sprintf(
        "CV fold %d holdout contains %d test indices (partial overlap).", i, n
      )
    )
    # A holdout that IS the full test set is the fold working as intended,
    # not a leak -- only flag partial overlap.
    indexout_leaks <- Filter(
      function(r) length(r$affected_indices) < length(test_idx), indexout_leaks
    )
    risks <- c(risks, indexout_leaks)
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
    risks <- c(risks, .check_cv_leak(
      list(analysis_idx), test_idx,
      source_prefix = "rsplit$in_id",
      describe = function(prefix, i, n) sprintf(
        "rsplit analysis set contains %d test indices. Test data is being used in model training.", n
      ),
      source_object = function(prefix, i) prefix
    ))
  }

  # Check assessment indices
  assessment_idx <- NULL
  if (!is.null(object$out_id)) {
    assessment_idx <- object$out_id
  }

  if (!is.null(assessment_idx)) {
    # Check if train indices appear in assessment set (information flow issue)
    risks <- c(risks, .check_cv_leak(
      list(assessment_idx), train_idx,
      source_prefix = "rsplit$out_id",
      severity = "soft_inflation",
      type = "split_misalignment",
      describe = function(prefix, i, n) sprintf(
        "rsplit assessment set contains %d expected training indices. Split boundaries may be misaligned.", n
      ),
      source_object = function(prefix, i) prefix
    ))
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
    risks <- c(risks, .check_train_scope(
      n_total, train_idx, test_idx, "vfold_cv",
      type = "cv_scope",
      check_under = FALSE,
      describe_over = function(label, n, expected) sprintf(
        "vfold_cv was created on %d observations, but training set has only %d. CV includes non-training data.",
        n, expected
      )
    ))
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
    risks <- c(risks, .check_train_scope(
      nrow(object$trainingData), train_idx, test_idx, "train$trainingData",
      type = "data_scope",
      severity_under = "soft_inflation",
      affected_indices_under = integer(0),
      describe_over = function(label, n, expected) sprintf(
        "Model was trained on %d rows, but expected training set has %d. Non-training data may be included.",
        n, expected
      ),
      describe_under = function(label, n, expected) sprintf(
        "Model was trained on %d rows, but expected %d. Some training data may have been excluded.",
        n, expected
      )
    ))
  }

  risks
}
