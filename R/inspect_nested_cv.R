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


#' Detect inner CV strategy mismatch (Internal)
#'
#' Flags when outer CV is blocked but inner hyperparameter tuning uses random CV.
#' @noRd
.inspect_strategy_mismatch <- function(inner_method, data = NULL,
                                         coords = NULL, time = NULL,
                                         groups = NULL) {
  risks <- list()

  # Determine if inner method is random

  random_methods <- c("cv", "repeatedcv", "boot", "LOOCV",
                       "vfold_cv", "bootstraps", "mc_cv")
  is_random <- tolower(inner_method) %in% tolower(random_methods)

  if (!is_random) return(risks)

  # Check if data has spatial/temporal/group structure
  has_structure <- !is.null(coords) || !is.null(time) || !is.null(groups)
  if (!has_structure || is.null(data)) return(risks)

  # Run a quick diagnosis
  diag <- tryCatch(
    borg_diagnose(data, coords = coords, time = time,
                   groups = groups, verbose = FALSE),
    error = function(e) NULL
  )

  if (is.null(diag) || diag@severity == "none") return(risks)

  risks <- c(risks, list(.new_risk(
    type = "nested_cv_strategy_mismatch",
    severity = "soft_inflation",
    description = sprintf(
      "Inner CV uses '%s' (random) but data has %s dependency (severity: %s). Inner hyperparameter estimates are optimistically biased.",
      inner_method, diag@dependency_type, diag@severity
    ),
    affected_indices = integer(0),
    source_object = "inner_resampling"
  )))

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


#' Check Nested CV for Leakage
#'
#' Validates a nested cross-validation setup for data leakage between
#' outer and inner CV loops, and detects strategy mismatches where the
#' inner CV uses random resampling despite data dependencies.
#'
#' @param inner_resamples The inner resampling object. One of:
#'   \itemize{
#'     \item A \code{caret::train} object
#'     \item A \code{tune_results} object from tidymodels
#'     \item A character string naming the inner CV method (e.g. "cv", "vfold_cv")
#'   }
#' @param outer_train_idx Integer vector. Indices of the outer training set.
#' @param outer_test_idx Integer vector. Indices of the outer test set.
#' @param data Data frame used for modeling.
#' @param coords Character vector of coordinate column names (for spatial check).
#' @param time Character. Time column name (for temporal check).
#' @param groups Character. Group column name (for clustered check).
#'
#' @return A \code{\link{BorgRisk}} object with any detected risks.
#'
#' @examples
#' # Check if inner random CV is appropriate given grouped data
#' d <- data.frame(
#'   site = rep(1:20, each = 10),
#'   x = rnorm(200),
#'   y = rep(rnorm(20), each = 10) + rnorm(200, sd = 0.5)
#' )
#' result <- borg_check_nested_cv(
#'   inner_resamples = "cv",
#'   outer_train_idx = 1:160,
#'   outer_test_idx = 161:200,
#'   data = d,
#'   groups = "site"
#' )
#'
#' @export
borg_check_nested_cv <- function(inner_resamples,
                                   outer_train_idx,
                                   outer_test_idx,
                                   data,
                                   coords = NULL,
                                   time = NULL,
                                   groups = NULL) {

  risks <- list()

  # Check for index leakage (outer test in inner folds)
  if (inherits(inner_resamples, "train") || inherits(inner_resamples, "tune_results")) {
    risks <- c(risks, .inspect_nested_cv(
      inner_resamples, outer_train_idx, outer_test_idx, data
    ))
  }

  # Check for strategy mismatch
  inner_method <- if (is.character(inner_resamples)) {
    inner_resamples
  } else if (inherits(inner_resamples, "train")) {
    inner_resamples$control$method %||% "unknown"
  } else if (inherits(inner_resamples, "rset")) {
    class(inner_resamples)[1]
  } else {
    "unknown"
  }

  risks <- c(risks, .inspect_strategy_mismatch(
    inner_method, data = data,
    coords = coords, time = time, groups = groups
  ))

  # Build BorgRisk result
  n_hard <- sum(vapply(risks, function(r) r$severity == "hard_violation", logical(1)))
  n_soft <- sum(vapply(risks, function(r) r$severity == "soft_inflation", logical(1)))

  methods::new("BorgRisk",
    risks = risks,
    n_hard = n_hard,
    n_soft = n_soft,
    is_valid = n_hard == 0L,
    train_indices = as.integer(outer_train_idx),
    test_indices = as.integer(outer_test_idx),
    timestamp = Sys.time(),
    call = match.call()
  )
}
