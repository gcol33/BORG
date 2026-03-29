# ===========================================================================
# borg_prediction_map() — Spatial prediction uncertainty
# ===========================================================================

#' Spatial Prediction Uncertainty Map
#'
#' Collects predictions from all CV folds at each test location, computes
#' per-location mean and standard deviation, and provides a spatial
#' uncertainty map.
#'
#' @param data Data frame with predictor and coordinate columns.
#' @param folds A \code{borg_cv} object or fold list.
#' @param formula Model formula.
#' @param coords Character vector of length 2. Coordinate column names.
#' @param fit_fun Function. Default: \code{lm}.
#'
#' @return A data frame with class \code{"borg_pred_map"} containing:
#'   \describe{
#'     \item{x, y}{Coordinates}
#'     \item{pred_mean}{Mean prediction across folds where this obs was in test set}
#'     \item{pred_sd}{SD of predictions (uncertainty)}
#'     \item{actual}{Observed target value}
#'     \item{residual_mean}{Mean residual}
#'     \item{n_folds_tested}{Number of folds where this obs appeared in test set}
#'   }
#'
#' @examples
#' set.seed(42)
#' d <- data.frame(x = runif(100), y = runif(100), z = rnorm(100))
#' cv <- borg_cv(d, coords = c("x", "y"), target = "z")
#' pm <- borg_prediction_map(d, cv, z ~ x + y, coords = c("x", "y"))
#' head(pm)
#'
#' @export
borg_prediction_map <- function(data, folds, formula, coords,
                                  fit_fun = stats::lm) {
  fold_list <- if (inherits(folds, "borg_cv")) folds$folds else folds
  target_var <- all.vars(formula)[1]
  n <- nrow(data)

  # Collect predictions for each observation across folds
  pred_matrix <- matrix(NA_real_, nrow = n, ncol = length(fold_list))

  for (i in seq_along(fold_list)) {
    fold <- fold_list[[i]]
    train_data <- data[fold$train, , drop = FALSE]
    test_data <- data[fold$test, , drop = FALSE]

    model <- tryCatch(fit_fun(formula, data = train_data), error = function(e) NULL)
    if (!is.null(model)) {
      preds <- tryCatch(
        stats::predict(model, newdata = test_data),
        error = function(e) rep(NA_real_, nrow(test_data))
      )
      pred_matrix[fold$test, i] <- preds
    }
  }

  coord_info <- extract_coords(data, coords)

  result <- data.frame(
    x = coord_info$x,
    y = coord_info$y,
    pred_mean = rowMeans(pred_matrix, na.rm = TRUE),
    pred_sd = apply(pred_matrix, 1, stats::sd, na.rm = TRUE),
    actual = data[[target_var]],
    n_folds_tested = rowSums(!is.na(pred_matrix)),
    stringsAsFactors = FALSE
  )
  result$residual_mean <- result$actual - result$pred_mean

  # NaN for obs never in test set
  never_tested <- result$n_folds_tested == 0
  result$pred_mean[never_tested] <- NA_real_
  result$pred_sd[never_tested] <- NA_real_
  result$residual_mean[never_tested] <- NA_real_

  class(result) <- c("borg_pred_map", "data.frame")
  attr(result, "coord_names") <- coords
  result
}


#' @exportS3Method ggplot2::autoplot
autoplot.borg_pred_map <- function(object, type = c("uncertainty", "residual"), ...) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package 'ggplot2' required")
  }

  type <- match.arg(type)
  df <- object[!is.na(object$pred_mean), ]
  coord_names <- attr(object, "coord_names") %||% c("x", "y")

  if (type == "uncertainty") {
    ggplot2::ggplot(df, ggplot2::aes(x = .data$x, y = .data$y,
                                       color = .data$pred_sd)) +
      ggplot2::geom_point(size = 1.5, alpha = 0.8) +
      ggplot2::scale_color_viridis_c(option = "inferno", direction = -1) +
      ggplot2::coord_equal() +
      ggplot2::labs(
        title = "Prediction Uncertainty Map",
        subtitle = sprintf("SD of predictions across CV folds (n = %d obs)",
                            nrow(df)),
        x = coord_names[1], y = coord_names[2],
        color = "Pred SD"
      ) +
      borg_theme()
  } else {
    max_abs <- max(abs(df$residual_mean), na.rm = TRUE)
    ggplot2::ggplot(df, ggplot2::aes(x = .data$x, y = .data$y,
                                       color = .data$residual_mean)) +
      ggplot2::geom_point(size = 1.5, alpha = 0.8) +
      ggplot2::scale_color_gradient2(
        low = "#2166AC", mid = "gray95", high = "#B2182B",
        midpoint = 0, limits = c(-max_abs, max_abs)
      ) +
      ggplot2::coord_equal() +
      ggplot2::labs(
        title = "Residual Map",
        subtitle = "Mean residual (actual - predicted) across CV folds",
        x = coord_names[1], y = coord_names[2],
        color = "Residual"
      ) +
      borg_theme()
  }
}
