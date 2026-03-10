# mlr3 framework inspectors: Learner, Resampling, Task, ResampleResult

#' Inspect mlr3 Learner for leakage
#' @noRd
.inspect_mlr3_learner <- function(object, train_idx, test_idx, data, ...) {
  risks <- list()

  # Check if learner has been trained
  if (is.null(object$model)) {
    return(risks)
  }

  # Check training set size vs expected
  if (!is.null(object$state$train_task)) {
    n_train_used <- object$state$train_task$nrow
    n_train_expected <- length(train_idx)

    if (!is.null(n_train_used) && !is.null(n_train_expected)) {
      n_total <- if (!is.null(data)) nrow(data) else max(c(train_idx, test_idx))

      if (n_train_used == n_total && n_train_used != n_train_expected) {
        risks <- c(risks, list(.new_risk(
          type = "model_trained_on_full_data",
          severity = "hard_violation",
          description = sprintf(
            "mlr3 Learner (%s) trained on %d observations (full dataset). Expected %d (training only).",
            object$id, n_train_used, n_train_expected
          ),
          affected_indices = test_idx,
          source_object = object$id
        )))
      } else if (n_train_used > n_train_expected) {
        risks <- c(risks, list(.new_risk(
          type = "model_training_size_mismatch",
          severity = "soft_inflation",
          description = sprintf(
            "mlr3 Learner (%s) trained on %d observations. Expected %d (training set size).",
            object$id, n_train_used, n_train_expected
          ),
          affected_indices = integer(0),
          source_object = object$id
        )))
      }
    }
  }

  risks
}

#' Inspect mlr3 Resampling for leakage
#' @noRd
.inspect_mlr3_resampling <- function(object, train_idx, test_idx, data, ...) {
  risks <- list()

  if (!object$is_instantiated) {
    return(risks)
  }

  n_iters <- object$iters

  for (i in seq_len(n_iters)) {
    fold_train <- tryCatch(object$train_set(i), error = function(e) NULL)
    fold_test <- tryCatch(object$test_set(i), error = function(e) NULL)

    if (is.null(fold_train) || is.null(fold_test)) next

    # Check for overlap within fold
    fold_overlap <- intersect(fold_train, fold_test)
    if (length(fold_overlap) > 0) {
      risks <- c(risks, list(.new_risk(
        type = "cv_fold_overlap",
        severity = "hard_violation",
        description = sprintf(
          "mlr3 Resampling fold %d: %d indices appear in both train and test sets.",
          i, length(fold_overlap)
        ),
        affected_indices = as.integer(fold_overlap),
        source_object = sprintf("%s (fold %d)", object$id, i)
      )))
    }

    # Check if held-out test indices leak into CV training
    if (!is.null(test_idx)) {
      leaked_test <- intersect(fold_train, test_idx)
      if (length(leaked_test) > 0) {
        risks <- c(risks, list(.new_risk(
          type = "test_in_cv_train",
          severity = "hard_violation",
          description = sprintf(
            "mlr3 Resampling fold %d: %d held-out test indices in CV training set.",
            i, length(leaked_test)
          ),
          affected_indices = as.integer(leaked_test),
          source_object = sprintf("%s (fold %d)", object$id, i)
        )))
      }
    }
  }

  risks
}

#' Inspect mlr3 Task for leakage
#' @noRd
.inspect_mlr3_task <- function(object, train_idx, test_idx, ...) {
  risks <- list()

  # Check if task data includes test indices when it should be train-only
  task_nrow <- object$nrow
  n_total <- max(c(train_idx, test_idx))
  n_train <- length(train_idx)

  if (task_nrow == n_total && task_nrow != n_train) {
    risks <- c(risks, list(.new_risk(
      type = "task_contains_test_data",
      severity = "soft_inflation",
      description = sprintf(
        "mlr3 Task '%s' contains %d rows (full dataset). If used for CV, ensure test indices (%d rows) are properly held out.",
        object$id, task_nrow, length(test_idx)
      ),
      affected_indices = test_idx,
      source_object = object$id
    )))
  }

  risks
}

#' Inspect mlr3 ResampleResult for leakage
#' @noRd
.inspect_mlr3_resample_result <- function(object, train_idx, test_idx, data, ...) {
  risks <- list()

  # Check the resampling used
  resampling <- tryCatch(object$resampling, error = function(e) NULL)
  if (!is.null(resampling)) {
    resample_risks <- .inspect_mlr3_resampling(resampling, train_idx, test_idx, data, ...)
    risks <- c(risks, resample_risks)
  }

  # Check learners
  learners <- tryCatch(object$learners, error = function(e) NULL)
  if (!is.null(learners)) {
    for (learner in learners) {
      learner_risks <- .inspect_mlr3_learner(learner, train_idx, test_idx, data, ...)
      risks <- c(risks, learner_risks)
    }
  }

  risks
}
