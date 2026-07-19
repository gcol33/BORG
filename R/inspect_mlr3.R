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

    if (!is.null(n_train_used)) {
      n_total <- if (!is.null(data)) nrow(data) else max(c(train_idx, test_idx))

      risks <- c(risks, .check_train_scope(
        n_train_used, train_idx, test_idx, object$id,
        check_under = FALSE,
        type = function(n, expected) if (n == n_total) {
          "model_trained_on_full_data"
        } else {
          "model_training_size_mismatch"
        },
        severity_over = function(n, expected) if (n == n_total) "hard_violation" else "soft_inflation",
        affected_indices_over = function(n, expected) if (n == n_total) test_idx else integer(0),
        describe_over = function(label, n, expected) if (n == n_total) {
          sprintf(
            "mlr3 Learner (%s) trained on %d observations (full dataset). Expected %d (training only).",
            label, n, expected
          )
        } else {
          sprintf(
            "mlr3 Learner (%s) trained on %d observations. Expected %d (training set size).",
            label, n, expected
          )
        }
      ))
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
    risks <- c(risks, .check_cv_leak(
      list(fold_train), fold_test,
      source_prefix = sprintf("%s (fold %d)", object$id, i),
      type = "cv_fold_overlap",
      describe = function(prefix, j, n) sprintf(
        "mlr3 Resampling fold %d: %d indices appear in both train and test sets.", i, n
      ),
      source_object = function(prefix, j) prefix
    ))

    # Check if held-out test indices leak into CV training
    if (!is.null(test_idx)) {
      risks <- c(risks, .check_cv_leak(
        list(fold_train), test_idx,
        source_prefix = sprintf("%s (fold %d)", object$id, i),
        type = "test_in_cv_train",
        describe = function(prefix, j, n) sprintf(
          "mlr3 Resampling fold %d: %d held-out test indices in CV training set.", i, n
        ),
        source_object = function(prefix, j) prefix
      ))
    }
  }

  risks
}

#' Inspect mlr3 Task for leakage
#' @noRd
.inspect_mlr3_task <- function(object, train_idx, test_idx, ...) {
  risks <- list()

  # Check if task data includes test indices when it should be train-only
  n_total <- max(c(train_idx, test_idx))

  risks <- c(risks, .check_train_scope(
    object$nrow, train_idx, test_idx, object$id,
    type = "task_contains_test_data",
    severity_over = "soft_inflation",
    check_under = FALSE,
    only_if = function(n, expected) n == n_total,
    describe_over = function(label, n, expected) sprintf(
      "mlr3 Task '%s' contains %d rows (full dataset). If used for CV, ensure test indices (%d rows) are properly held out.",
      label, n, length(test_idx)
    )
  ))

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
