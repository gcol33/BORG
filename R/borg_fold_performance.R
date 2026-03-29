# ===========================================================================
# borg_fold_performance() — Per-fold spatial performance diagnostics
# ===========================================================================

#' Evaluate Per-Fold Model Performance
#'
#' Fits a model on each fold's training set and evaluates on its test set,
#' returning per-fold metrics with spatial centroids for geographic
#' performance mapping.
#'
#' @param data Data frame with predictor and target columns.
#' @param folds A \code{borg_cv} object, or a list of fold lists
#'   (each with \code{$train} and \code{$test} index vectors).
#' @param formula A model formula (e.g. \code{y ~ x1 + x2}).
#' @param coords Character vector of length 2. Coordinate column names
#'   for computing fold centroids. Optional.
#' @param metric Character. Performance metric: \code{"rmse"} (default),
#'   \code{"mae"}, or \code{"rsq"}.
#' @param fit_fun Function to fit a model. Default: \code{lm}. Must accept
#'   \code{formula} and \code{data} arguments and return an object with a
#'   \code{predict()} method.
#' @param parallel Logical. If \code{TRUE} and the \pkg{future.apply} package
#'   is installed, fold evaluation runs in parallel. Default: \code{FALSE}.
#'
#' @return A data frame with columns:
#'   \describe{
#'     \item{fold}{Fold index}
#'     \item{metric}{Metric name}
#'     \item{value}{Metric value}
#'     \item{n_train}{Training set size}
#'     \item{n_test}{Test set size}
#'     \item{centroid_x, centroid_y}{Spatial centroid of test set (if coords provided)}
#'   }
#'   Has class \code{"borg_fold_perf"} with an \code{autoplot()} method.
#'
#' @examples
#' set.seed(42)
#' d <- data.frame(
#'   x = runif(200, 0, 100), y = runif(200, 0, 100),
#'   z = rnorm(200)
#' )
#' cv <- borg_cv(d, coords = c("x", "y"), target = "z")
#' perf <- borg_fold_performance(d, cv, z ~ x + y, coords = c("x", "y"))
#' perf
#'
#' @export
borg_fold_performance <- function(data, folds, formula,
                                    coords = NULL,
                                    metric = c("rmse", "mae", "rsq",
                                               "auc", "tss", "kappa",
                                               "sensitivity", "specificity", "accuracy"),
                                    fit_fun = stats::lm,
                                    parallel = FALSE) {
  metric <- match.arg(metric)

  # Extract fold list
  fold_list <- if (inherits(folds, "borg_cv")) folds$folds else folds

  # Choose lapply variant
  map_fn <- if (parallel && requireNamespace("future.apply", quietly = TRUE)) {
    future.apply::future_lapply
  } else {
    if (parallel) message("Install 'future.apply' for parallel execution; using sequential")
    lapply
  }

  results <- map_fn(seq_along(fold_list), function(i) {
    fold <- fold_list[[i]]
    train_data <- data[fold$train, , drop = FALSE]
    test_data <- data[fold$test, , drop = FALSE]

    # Fit model
    model <- tryCatch(
      fit_fun(formula, data = train_data),
      error = function(e) NULL
    )

    if (is.null(model)) {
      val <- NA_real_
    } else {
      preds <- tryCatch(
        stats::predict(model, newdata = test_data),
        error = function(e) rep(NA_real_, nrow(test_data))
      )

      response_var <- all.vars(formula)[1]
      actual <- test_data[[response_var]]

      val <- .classify_metric(actual, preds, metric)
    }

    row <- data.frame(
      fold = i,
      metric = metric,
      value = val,
      n_train = length(fold$train),
      n_test = length(fold$test),
      stringsAsFactors = FALSE
    )

    # Add spatial centroids
    if (!is.null(coords) && length(coords) >= 2) {
      row$centroid_x <- mean(data[[coords[1]]][fold$test], na.rm = TRUE)
      row$centroid_y <- mean(data[[coords[2]]][fold$test], na.rm = TRUE)
    }

    row
  })

  result <- do.call(rbind, results)
  class(result) <- c("borg_fold_perf", "data.frame")
  attr(result, "coords") <- coords
  result
}


#' Autoplot Method for borg_fold_perf Objects
#'
#' @param object A \code{borg_fold_perf} object.
#' @param ... Additional arguments (currently unused).
#'
#' @return A \code{ggplot} object. If spatial centroids are available,
#'   shows a map of per-fold performance. Otherwise, a bar chart.
#'
#' @exportS3Method ggplot2::autoplot
autoplot.borg_fold_perf <- function(object, ...) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package 'ggplot2' required for autoplot()")
  }

  metric_label <- toupper(object$metric[1])
  has_spatial <- "centroid_x" %in% names(object) && !all(is.na(object$centroid_x))

  if (has_spatial) {
    # Spatial performance map
    ggplot2::ggplot(object, ggplot2::aes(
      x = .data$centroid_x, y = .data$centroid_y,
      color = .data$value, size = .data$n_test
    )) +
      ggplot2::geom_point(alpha = 0.8) +
      ggplot2::geom_text(
        ggplot2::aes(label = sprintf("F%d", .data$fold)),
        size = 2.5, color = "gray20", vjust = -1.5
      ) +
      ggplot2::scale_color_viridis_c(option = "plasma") +
      ggplot2::scale_size_continuous(range = c(4, 10), guide = "none") +
      ggplot2::coord_equal() +
      ggplot2::labs(
        title = "Per-Fold Performance Map",
        subtitle = sprintf("Mean %s = %.3f (SD = %.3f)",
                            metric_label,
                            mean(object$value, na.rm = TRUE),
                            stats::sd(object$value, na.rm = TRUE)),
        x = attr(object, "coords")[1],
        y = attr(object, "coords")[2],
        color = metric_label
      ) +
      borg_theme()
  } else {
    # Bar chart fallback
    ggplot2::ggplot(object, ggplot2::aes(
      x = factor(.data$fold), y = .data$value
    )) +
      ggplot2::geom_col(fill = "#2C3E50", alpha = 0.8, width = 0.6) +
      ggplot2::geom_hline(
        yintercept = mean(object$value, na.rm = TRUE),
        linetype = "dashed", color = "#C0392B"
      ) +
      ggplot2::geom_text(
        ggplot2::aes(label = sprintf("%.3f", .data$value)),
        vjust = -0.3, size = 3
      ) +
      ggplot2::labs(
        title = "Per-Fold Performance",
        subtitle = sprintf("Mean %s = %.3f | Dashed = mean",
                            metric_label, mean(object$value, na.rm = TRUE)),
        x = "Fold", y = metric_label
      ) +
      borg_theme()
  }
}
