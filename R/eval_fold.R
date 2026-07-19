# ===========================================================================
# eval_fold(): Shared fit/predict/evaluate primitive for a single CV fold
# ===========================================================================

#' Default prediction function shared by the CV evaluation helpers
#' @noRd
.default_cv_predict <- function(model, newdata) stats::predict(model, newdata = newdata)


#' Fit, Predict, and Score a Single CV Fold (Internal)
#'
#' Core primitive behind \code{borg_compare_cv}, \code{borg_compare_models},
#' \code{borg_fold_performance}, and \code{borg_workflow}: subsets a fold's
#' train/test rows, fits a model (or reuses a pre-fitted one), generates
#' predictions, and scores them with \code{.classify_metric}. Fit and
#' predict failures are caught and reported as a missing model/predictions
#' rather than raising, so callers can aggregate across many folds without a
#' per-fold \code{tryCatch}.
#'
#' @param data Full data frame.
#' @param fold List with \code{$train} and \code{$test} integer index vectors.
#' @param formula Model formula. Required when \code{model} is not supplied,
#'   and used to derive \code{target} when \code{target} is NULL.
#' @param fit_fun Function(formula, data) -> fitted model. Ignored when
#'   \code{model} is supplied.
#' @param model A pre-fitted model to evaluate directly, skipping
#'   \code{fit_fun}. Its response variable is inferred via
#'   \code{stats::formula(model)} when \code{target} is not supplied.
#' @param predict_fn Function(model, newdata) -> predictions. Default calls
#'   \code{stats::predict(model, newdata = newdata)}.
#' @param metric Character metric name understood by \code{.classify_metric},
#'   or NULL to skip scoring (model/predictions are still returned).
#' @param target Character response column name. Derived from \code{formula}
#'   or \code{model} when NULL.
#' @param warn_on_fit_error Logical. Emit a warning naming \code{fold_label}
#'   when \code{fit_fun} errors. Default FALSE.
#' @param fold_label Character label used in the fit-error warning.
#'
#' @return A list with \code{model}, \code{preds}, \code{actual},
#'   \code{target}, \code{n_train}, \code{n_test}, and \code{value} (the
#'   scored metric, or \code{NA_real_} on failure or when \code{metric} is
#'   NULL).
#' @noRd
eval_fold <- function(data, fold, formula = NULL, fit_fun = NULL, model = NULL,
                      predict_fn = .default_cv_predict,
                      metric = NULL, target = NULL,
                      warn_on_fit_error = FALSE, fold_label = "Fold") {
  train_data <- data[fold$train, , drop = FALSE]
  test_data <- data[fold$test, , drop = FALSE]

  if (is.null(target)) {
    target <- if (!is.null(formula)) {
      all.vars(formula)[1]
    } else if (!is.null(model)) {
      tryCatch(all.vars(stats::formula(model))[1], error = function(e) NA_character_)
    } else {
      NA_character_
    }
  }

  if (is.null(model) && !is.null(fit_fun)) {
    model <- tryCatch(
      fit_fun(formula, data = train_data),
      error = function(e) {
        if (warn_on_fit_error) {
          warning(sprintf("%s: model fitting failed: %s", fold_label, e$message))
        }
        NULL
      }
    )
  }

  preds <- NULL
  actual <- NULL
  value <- NA_real_

  if (!is.null(model) && !is.na(target)) {
    preds <- tryCatch(predict_fn(model, test_data), error = function(e) NULL)
    actual <- test_data[[target]]

    if (!is.null(preds) && !is.null(metric)) {
      value <- .classify_metric(actual, preds, metric)
    }
  }

  list(
    model = model,
    preds = preds,
    actual = actual,
    target = target,
    n_train = length(fold$train),
    n_test = length(fold$test),
    value = value
  )
}


#' Assemble a borg_fold_perf Data Frame from eval_fold() Results (Internal)
#'
#' Shared by \code{borg_fold_performance} and \code{borg_workflow} so
#' per-fold performance is computed once (via \code{eval_fold}) and
#' formatted consistently, instead of \code{borg_workflow} fitting every
#' fold a second time through \code{borg_fold_performance}.
#'
#' @param fold_results List of \code{eval_fold()} result lists, one per fold.
#' @param data Full data frame (used for spatial centroids).
#' @param fold_list List of fold train/test index lists, same order and
#'   length as \code{fold_results}.
#' @param metric Character metric name (recorded in the output, not
#'   recomputed).
#' @param coords Character vector of length 2 giving coordinate column
#'   names, or NULL to omit centroids.
#' @noRd
.build_fold_perf <- function(fold_results, data, fold_list, metric, coords = NULL) {
  rows <- lapply(seq_along(fold_results), function(i) {
    res <- fold_results[[i]]
    row <- data.frame(
      fold = i,
      metric = metric,
      value = res$value,
      n_train = res$n_train,
      n_test = res$n_test,
      stringsAsFactors = FALSE
    )

    if (!is.null(coords) && length(coords) >= 2) {
      test_idx <- fold_list[[i]]$test
      row$centroid_x <- mean(data[[coords[1]]][test_idx], na.rm = TRUE)
      row$centroid_y <- mean(data[[coords[2]]][test_idx], na.rm = TRUE)
    }

    row
  })

  result <- do.call(rbind, rows)
  class(result) <- c("borg_fold_perf", "data.frame")
  attr(result, "coords") <- coords
  result
}
