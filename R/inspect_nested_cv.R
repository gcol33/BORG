# Nested CV leak detection
# Detects when hyperparameter tuning inner resamples use outer test data

#' Inspect a caret train object for nested CV leakage
#'
#' Checks if the inner CV resampling (used for hyperparameter tuning)
#' contains data from the outer test fold.
#' @noRd
.inspect_nested_cv_caret <- function(object, train_idx, test_idx, data = NULL, ...) {
  risks <- list()

  if (is.null(train_idx) || is.null(test_idx)) return(risks)
  if (!inherits(object, "train")) return(risks)

  # caret::train$control$index holds inner CV indices
 ctrl <- object$control
  if (is.null(ctrl) || is.null(ctrl$index)) return(risks)

  # Check every inner fold's training indices against outer test
  n_leaking_folds <- 0L
  total_leaked <- integer(0)

  for (i in seq_along(ctrl$index)) {
    inner_train <- ctrl$index[[i]]
    leaked <- intersect(inner_train, test_idx)
    if (length(leaked) > 0) {
      n_leaking_folds <- n_leaking_folds + 1L
      total_leaked <- union(total_leaked, leaked)
    }
  }

  if (n_leaking_folds > 0) {
    risks <- c(risks, list(.new_risk(
      type = "nested_cv_leak",
      severity = "hard_violation",
      description = sprintf(
        "Hyperparameter tuning uses outer test data: %d of %d inner CV folds contain %d test indices. Tune on train-only data.",
        n_leaking_folds, length(ctrl$index), length(total_leaked)
      ),
      affected_indices = total_leaked,
      source_object = "train$control$index"
    )))
  }

  # Also check: was the full caret::train() called on data including test?
  if (!is.null(object$trainingData)) {
    n_used <- nrow(object$trainingData)
    n_train <- length(train_idx)
    if (n_used > n_train * 1.05) {  # 5% tolerance for rounding
      risks <- c(risks, list(.new_risk(
        type = "nested_cv_leak",
        severity = "hard_violation",
        description = sprintf(
          "caret::train() was called on %d rows but outer training set has %d rows. Tuning used non-training data.",
          n_used, n_train
        ),
        affected_indices = test_idx,
        source_object = "train$trainingData"
      )))
    }
  }

  risks
}


#' Inspect tidymodels tune results for nested CV leakage
#'
#' Checks if tune_grid() / tune_bayes() resampling includes outer test data.
#' @noRd
.inspect_nested_cv_tune <- function(object, train_idx, test_idx, data = NULL, ...) {
  risks <- list()

  if (is.null(train_idx) || is.null(test_idx)) return(risks)

  # tune_results objects have a $splits column or are tibbles with rsample splits
  # They inherit from tune_results class
  if (!inherits(object, "tune_results")) return(risks)

  # tune_results contain the resampling splits used for tuning
  # Check if .metrics or splits are available
  splits <- NULL
  if ("splits" %in% names(object)) {
    splits <- object$splits
  } else if (".metrics" %in% names(object)) {
    # tune_results from tune::tune_grid store the rset used
    # The rset is typically attached as an attribute
    rset_used <- attr(object, "rset")
    if (!is.null(rset_used) && "splits" %in% names(rset_used)) {
      splits <- rset_used$splits
    }
  }

  if (is.null(splits) || length(splits) == 0) {
    # Try to get n from the underlying data
    # If we can determine the tuning was done on more data than train, flag it
    if (!is.null(data)) {
      n_data <- nrow(data)
      n_train <- length(train_idx)
      if (n_data > n_train * 1.05) {
        risks <- c(risks, list(.new_risk(
          type = "nested_cv_leak",
          severity = "soft_inflation",
          description = sprintf(
            "Tuning results may use data beyond training set (%d total rows vs %d training). Verify resampling was done on train-only data.",
            n_data, n_train
          ),
          affected_indices = test_idx,
          source_object = "tune_results"
        )))
      }
    }
    return(risks)
  }

  # Check each tuning split for outer test data leakage
  n_leaking <- 0L
  total_leaked <- integer(0)

  for (i in seq_along(splits)) {
    split <- splits[[i]]
    # rsample splits have $in_id for analysis indices
    analysis_idx <- split$in_id
    if (is.null(analysis_idx)) next

    leaked <- intersect(analysis_idx, test_idx)
    if (length(leaked) > 0) {
      n_leaking <- n_leaking + 1L
      total_leaked <- union(total_leaked, leaked)
    }
  }

  if (n_leaking > 0) {
    risks <- c(risks, list(.new_risk(
      type = "nested_cv_leak",
      severity = "hard_violation",
      description = sprintf(
        "Tuning resamples contain outer test data: %d of %d folds leak %d test indices into hyperparameter selection.",
        n_leaking, length(splits), length(total_leaked)
      ),
      affected_indices = total_leaked,
      source_object = "tune_results$splits"
    )))
  }

  risks
}


#' Main dispatcher for nested CV leak detection
#'
#' Called from borg_inspect() and borg_validate() to check for
#' hyperparameter tuning leakage.
#' @noRd
.inspect_nested_cv <- function(object, train_idx, test_idx, data = NULL, ...) {
  if (inherits(object, "train")) {
    return(.inspect_nested_cv_caret(object, train_idx, test_idx, data, ...))
  }
  if (inherits(object, "tune_results")) {
    return(.inspect_nested_cv_tune(object, train_idx, test_idx, data, ...))
  }
  list()
}
