# ===========================================================================
# borg_workflow() — Full pipeline: diagnose → cv → fit → validate
# ===========================================================================

#' Create a BORG Validation Workflow
#'
#' Wraps the full model evaluation pipeline into a single trackable object:
#' diagnose data dependencies, generate appropriate CV folds, fit a model
#' per fold, and validate each split for leakage.
#'
#' @param data Data frame with predictor and target columns.
#' @param formula Model formula (e.g. \code{y ~ x1 + x2}).
#' @param coords Character vector of coordinate column names.
#' @param time Character. Time column name.
#' @param groups Character. Group column name.
#' @param v Integer. Number of folds. Default: 5.
#' @param fit_fun Function to fit a model. Default: \code{lm}. Must accept
#'   \code{formula} and \code{data} and return an object with \code{predict()}.
#' @param metric Character. Performance metric. Default: \code{"rmse"}.
#' @param buffer Numeric. Optional spatial buffer.
#' @param parallel Logical. If \code{TRUE} and \pkg{future.apply} is installed,
#'   fold fitting and evaluation run in parallel. Default: \code{FALSE}.
#' @param verbose Logical. Default: FALSE.
#' @param ... Additional arguments passed to \code{\link{borg_cv}()}.
#'
#' @return A \code{borg_workflow} object (list) containing:
#'   \describe{
#'     \item{diagnosis}{BorgDiagnosis object}
#'     \item{cv}{borg_cv object with folds}
#'     \item{models}{List of fitted models (one per fold)}
#'     \item{predictions}{List of prediction vectors (one per fold)}
#'     \item{performance}{borg_fold_perf data frame}
#'     \item{risks}{List of BorgRisk objects (one per fold)}
#'     \item{data}{Original data}
#'     \item{formula}{Model formula}
#'     \item{params}{Workflow parameters}
#'   }
#'
#' @examples
#' set.seed(42)
#' d <- data.frame(
#'   x = runif(100, 0, 100), y = runif(100, 0, 100),
#'   z = rnorm(100)
#' )
#' wf <- borg_workflow(d, z ~ x + y, coords = c("x", "y"))
#' wf
#'
#' @export
borg_workflow <- function(data, formula,
                           coords = NULL, time = NULL, groups = NULL,
                           v = 5, fit_fun = stats::lm,
                           metric = c("rmse", "mae", "rsq"),
                           buffer = NULL, parallel = FALSE,
                           verbose = FALSE, ...) {

  metric <- match.arg(metric)
  target <- all.vars(formula)[1]

  # Stage 1: Diagnose
  if (verbose) message("Stage 1/4: Diagnosing dependencies...")
  diagnosis <- borg_diagnose(data, coords = coords, time = time,
                               groups = groups, target = target,
                               verbose = verbose)

  # Stage 2: Generate CV folds
  if (verbose) message("Stage 2/4: Generating CV folds...")
  cv <- borg_cv(data, diagnosis = diagnosis, v = v,
                 coords = coords, time = time, groups = groups,
                 target = target, buffer = buffer,
                 verbose = verbose, ...)

  # Stage 3: Fit models per fold
  if (verbose) message("Stage 3/4: Fitting models...")
  folds <- cv$folds

  map_fn <- if (parallel && requireNamespace("future.apply", quietly = TRUE)) {
    future.apply::future_lapply
  } else {
    if (parallel) message("Install 'future.apply' for parallel execution; using sequential")
    lapply
  }

  fold_results <- map_fn(seq_along(folds), function(i) {
    eval_fold(
      data, folds[[i]], formula = formula, fit_fun = fit_fun,
      metric = metric, target = target,
      warn_on_fit_error = TRUE, fold_label = sprintf("Fold %d", i)
    )
  })

  models <- lapply(fold_results, `[[`, "model")
  predictions <- lapply(fold_results, `[[`, "preds")

  # Stage 4: Validate each fold
  if (verbose) message("Stage 4/4: Validating splits...")
  risks <- lapply(seq_along(folds), function(i) {
    tryCatch(
      borg_inspect(data, train_idx = folds[[i]]$train,
                    test_idx = folds[[i]]$test),
      error = function(e) NULL
    )
  })

  # Compute performance from the fold fits above; avoids fitting every fold
  # a second time through borg_fold_performance().
  performance <- .build_fold_perf(fold_results, data, folds, metric, coords = coords)

  result <- list(
    diagnosis = diagnosis,
    cv = cv,
    models = models,
    predictions = predictions,
    performance = performance,
    risks = risks,
    data = data,
    formula = formula,
    params = list(
      v = v, metric = metric, strategy = cv$strategy,
      buffer = buffer, target = target
    )
  )
  class(result) <- c("borg_workflow", "list")

  result
}


#' @export
print.borg_workflow <- function(x, ...) {
  cat("BORG Workflow\n")
  cat("=============\n\n")
  cat(sprintf("Strategy:  %s\n", x$cv$strategy))
  cat(sprintf("Folds:     %d\n", length(x$cv$folds)))
  cat(sprintf("Formula:   %s\n", deparse(x$formula)))
  cat(sprintf("Metric:    %s\n", x$params$metric))

  # Diagnosis summary
  diag <- x$diagnosis
  cat(sprintf("Dependency: %s (severity: %s)\n",
              diag@dependency_type, diag@severity))

  # Performance summary
  vals <- x$performance$value
  cat(sprintf("\nPerformance (%s):\n", toupper(x$params$metric)))
  cat(sprintf("  Mean: %.4f  SD: %.4f  Range: [%.4f, %.4f]\n",
              mean(vals, na.rm = TRUE), stats::sd(vals, na.rm = TRUE),
              min(vals, na.rm = TRUE), max(vals, na.rm = TRUE)))

  # Risk summary
  n_valid <- sum(vapply(x$risks, function(r) {
    if (is.null(r)) return(TRUE)
    r@is_valid
  }, logical(1)))
  cat(sprintf("\nValidation: %d/%d folds valid\n", n_valid, length(x$risks)))

  invisible(x)
}


#' @export
summary.borg_workflow <- function(object, ...) {
  print(object, ...)
  cat("\n--- Per-Fold Details ---\n")
  print(object$performance)
  invisible(object)
}
