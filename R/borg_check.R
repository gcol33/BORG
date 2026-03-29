# ===========================================================================
# borg_check() — Pipe-friendly one-verb entry point
# ===========================================================================

#' Quick Leakage Check (Pipe-Friendly)
#'
#' Single-verb entry point that runs the full BORG pipeline:
#' diagnose dependencies, generate valid CV, validate the split,
#' and return a tidy summary. Designed for use in pipelines.
#'
#' @param data A data frame.
#' @param model A fitted model object (optional). If provided, predictions
#'   are evaluated under blocked CV.
#' @param target Character. Response variable name.
#' @param coords Character vector of length 2. Coordinate column names
#'   (optional, for spatial data).
#' @param time Character. Time column name (optional, for temporal data).
#' @param groups Character. Grouping column name (optional).
#' @param train_idx Integer vector. Training indices (optional, for
#'   validating an existing split).
#' @param test_idx Integer vector. Test indices (optional).
#' @param v Integer. Number of CV folds. Default: 5.
#' @param verbose Logical. Default: FALSE.
#'
#' @return A data frame with class \code{"borg_check"} containing one
#'   row per detected risk (or zero rows if clean), with columns:
#'   \describe{
#'     \item{risk_type}{Character. Type of risk detected.}
#'     \item{severity}{Character. \code{"hard_inflation"},
#'       \code{"soft_inflation"}, or \code{"info"}.}
#'     \item{description}{Character. Plain-language description.}
#'     \item{n_affected}{Integer. Number of observations affected.}
#'     \item{source}{Character. Object/step that triggered the risk.}
#'   }
#'   Also has attributes: \code{diagnosis}, \code{cv}, \code{risks}
#'   (the full objects for further inspection).
#'
#' @details
#' \code{borg_check()} is intentionally simple: one call, one result.
#' For finer control, use \code{borg_diagnose()}, \code{borg_cv()},
#' and \code{borg_validate()} individually.
#'
#' @examples
#' set.seed(42)
#' d <- data.frame(x = runif(100), y = runif(100), z = rnorm(100))
#'
#' # Pipe-friendly
#' result <- borg_check(d, target = "z", coords = c("x", "y"))
#' result
#' nrow(result)  # 0 if clean
#'
#' # Check an existing split
#' result <- borg_check(d, target = "z", coords = c("x", "y"),
#'                      train_idx = 1:70, test_idx = 71:100)
#'
#' @seealso \code{\link{borg}}, \code{\link{borg_diagnose}},
#'   \code{\link{borg_explain_risk}}
#'
#' @export
borg_check <- function(data,
                       model = NULL,
                       target = NULL,
                       coords = NULL,
                       time = NULL,
                       groups = NULL,
                       train_idx = NULL,
                       test_idx = NULL,
                       v = 5L,
                       verbose = FALSE) {

  if (!is.data.frame(data)) {
    stop("data must be a data.frame")
  }

  # Step 1: Diagnose
  diag_args <- list(data = data)
  if (!is.null(coords)) diag_args$coords <- coords
  if (!is.null(time)) diag_args$time <- time
  if (!is.null(groups)) diag_args$groups <- groups
  if (!is.null(target)) diag_args$target <- target
  diagnosis <- do.call(borg_diagnose, diag_args)

  # Step 2: Generate CV (if no existing split)
  cv_result <- NULL
  if (is.null(train_idx) && is.null(test_idx)) {
    cv_args <- list(data = data, diagnosis = diagnosis, v = v, verbose = verbose)
    if (!is.null(coords)) cv_args$coords <- coords
    if (!is.null(time)) cv_args$time <- time
    if (!is.null(groups)) cv_args$groups <- groups
    if (!is.null(target)) cv_args$target <- target
    cv_result <- do.call(borg_cv, cv_args)
  }

  # Step 3: Validate
  risk_list <- list()
  if (!is.null(train_idx) && !is.null(test_idx)) {
    val_args <- list(data = data, train_idx = train_idx, test_idx = test_idx)
    if (!is.null(coords)) val_args$coords <- coords
    if (!is.null(target)) val_args$target <- target
    val_result <- tryCatch(
      do.call(borg_validate, val_args),
      error = function(e) NULL
    )
    if (!is.null(val_result) && inherits(val_result, "BorgRisk")) {
      risk_list <- val_result@risks
    }
  }

  # Step 4: Also inspect model if provided
  if (!is.null(model)) {
    inspect_result <- tryCatch(
      borg_inspect(model, train_idx = train_idx, test_idx = test_idx, data = data),
      error = function(e) NULL
    )
    if (!is.null(inspect_result) && inherits(inspect_result, "BorgRisk")) {
      risk_list <- c(risk_list, inspect_result@risks)
    }
  }

  # Build tidy output
  if (length(risk_list) == 0) {
    result <- data.frame(
      risk_type = character(0),
      severity = character(0),
      description = character(0),
      n_affected = integer(0),
      source = character(0),
      stringsAsFactors = FALSE
    )
  } else {
    result <- data.frame(
      risk_type = vapply(risk_list, function(r) r$type %||% "unknown", character(1)),
      severity = vapply(risk_list, function(r) r$severity %||% "unknown", character(1)),
      description = vapply(risk_list, function(r) r$description %||% "", character(1)),
      n_affected = vapply(risk_list, function(r) length(r$affected_indices %||% integer(0)), integer(1)),
      source = vapply(risk_list, function(r) r$source_object %||% "", character(1)),
      stringsAsFactors = FALSE
    )
  }

  class(result) <- c("borg_check", "data.frame")
  attr(result, "diagnosis") <- diagnosis
  attr(result, "cv") <- cv_result
  attr(result, "risks") <- risk_list

  result
}


#' @export
print.borg_check <- function(x, ...) {
  diag <- attr(x, "diagnosis")
  cat("BORG Quick Check\n")
  if (!is.null(diag)) {
    cat(sprintf("  Dependency: %s | Severity: %s\n",
                diag@dependency_type, diag@severity))
    if (!is.null(diag@recommended_cv)) {
      cat(sprintf("  Recommended CV: %s\n", diag@recommended_cv))
    }
  }

  if (nrow(x) == 0) {
    cat("  No risks detected.\n")
  } else {
    n_hard <- sum(x$severity == "hard_inflation")
    n_soft <- sum(x$severity == "soft_inflation")
    cat(sprintf("  Risks: %d hard, %d soft, %d total\n",
                n_hard, n_soft, nrow(x)))
    cat("\n")
    print.data.frame(x, row.names = FALSE)
  }
  invisible(x)
}
