# ===========================================================================
# borg_compare_models() — Multi-model spatial CV comparison
# ===========================================================================

#' Compare Multiple Models with Spatial CV
#'
#' Evaluates multiple models (or formulas) using the same blocked CV
#' folds, producing a side-by-side comparison table.
#'
#' @param data Data frame.
#' @param folds A \code{borg_cv} object or fold list.
#' @param models Named list of either:
#'   \itemize{
#'     \item Formulas: \code{list(lm = y ~ x, full = y ~ x + z)}
#'     \item Fitted models: \code{list(lm = lm_fit, rf = rf_fit)}
#'   }
#' @param metric Character. Default: \code{"rmse"}.
#' @param fit_fun Function. Used when \code{models} contains formulas.
#'   Default: \code{lm}.
#'
#' @return A data frame with class \code{"borg_model_comparison"} containing
#'   model name, mean metric, SD, and rank. Has \code{autoplot()} method.
#'
#' @examples
#' set.seed(42)
#' d <- data.frame(x = runif(100), y = runif(100),
#'                  a = rnorm(100), b = rnorm(100))
#' d$z <- d$a * 2 + rnorm(100, sd = 0.5)
#' cv <- borg_cv(d, coords = c("x", "y"), target = "z")
#' comp <- borg_compare_models(d, cv,
#'   models = list(simple = z ~ a, full = z ~ a + b))
#' comp
#'
#' @export
borg_compare_models <- function(data, folds,
                                  models,
                                  metric = c("rmse", "mae", "rsq",
                                             "auc", "tss", "kappa"),
                                  fit_fun = stats::lm) {
  metric <- match.arg(metric)
  fold_list <- if (inherits(folds, "borg_cv")) folds$folds else folds

  if (!is.list(models) || is.null(names(models))) {
    stop("models must be a named list")
  }

  results <- lapply(names(models), function(nm) {
    m <- models[[nm]]

    fold_vals <- vapply(fold_list, function(fold) {
      train_data <- data[fold$train, , drop = FALSE]
      test_data <- data[fold$test, , drop = FALSE]

      if (inherits(m, "formula")) {
        fitted <- tryCatch(fit_fun(m, data = train_data), error = function(e) NULL)
        if (is.null(fitted)) return(NA_real_)
        preds <- tryCatch(stats::predict(fitted, newdata = test_data),
                           error = function(e) rep(NA_real_, nrow(test_data)))
        target_var <- all.vars(m)[1]
      } else {
        # Pre-fitted model — predict directly
        preds <- tryCatch(stats::predict(m, newdata = test_data),
                           error = function(e) rep(NA_real_, nrow(test_data)))
        # Infer target from model
        target_var <- tryCatch(all.vars(stats::formula(m))[1],
                                error = function(e) NULL)
        if (is.null(target_var)) return(NA_real_)
      }

      actual <- test_data[[target_var]]
      .classify_metric(actual, preds, metric)
    }, numeric(1))

    data.frame(
      model = nm,
      metric = metric,
      mean = mean(fold_vals, na.rm = TRUE),
      sd = stats::sd(fold_vals, na.rm = TRUE),
      min = min(fold_vals, na.rm = TRUE),
      max = max(fold_vals, na.rm = TRUE),
      stringsAsFactors = FALSE
    )
  })

  result <- do.call(rbind, results)
  minimize <- metric %in% c("rmse", "mae")
  result <- result[order(if (minimize) result$mean else -result$mean), ]
  result$rank <- seq_len(nrow(result))
  rownames(result) <- NULL

  class(result) <- c("borg_model_comparison", "data.frame")
  attr(result, "metric") <- metric
  result
}


#' @export
print.borg_model_comparison <- function(x, ...) {
  cat("BORG Model Comparison\n")
  cat(sprintf("  Metric: %s\n\n", toupper(attr(x, "metric"))))
  print.data.frame(x, row.names = FALSE)
  invisible(x)
}


#' @exportS3Method ggplot2::autoplot
autoplot.borg_model_comparison <- function(object, ...) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) stop("ggplot2 required")

  met <- toupper(attr(object, "metric"))
  object$model <- factor(object$model, levels = rev(object$model))

  ggplot2::ggplot(object, ggplot2::aes(x = .data$model, y = .data$mean)) +
    ggplot2::geom_col(fill = "#2C3E50", alpha = 0.8, width = 0.6) +
    ggplot2::geom_errorbar(
      ggplot2::aes(ymin = .data$mean - .data$sd, ymax = .data$mean + .data$sd),
      width = 0.2, color = "gray40"
    ) +
    ggplot2::geom_text(
      ggplot2::aes(label = sprintf("%.4f", .data$mean)),
      hjust = -0.2, size = 3.5
    ) +
    ggplot2::coord_flip(clip = "off") +
    ggplot2::labs(
      title = "Model Comparison (Spatial CV)",
      subtitle = sprintf("Metric: %s | Error bars: +/- 1 SD", met),
      x = NULL, y = met
    ) +
    borg_theme() +
    ggplot2::theme(
      panel.grid.major.y = ggplot2::element_blank(),
      plot.margin = ggplot2::margin(10, 40, 10, 10)
    )
}
