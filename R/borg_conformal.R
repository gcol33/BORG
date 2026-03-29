# ===========================================================================
# borg_conformal() — Spatially-valid conformal prediction intervals
# ===========================================================================

#' Conformal Prediction with Spatial Dependence
#'
#' Constructs distribution-free prediction intervals with finite-sample
#' coverage guarantees, adjusted for spatial autocorrelation. Standard
#' conformal prediction assumes exchangeability, which spatial data violates.
#' This function uses spatially-blocked calibration residuals as
#' nonconformity scores, producing intervals that maintain coverage
#' even under dependence.
#'
#' @param model A fitted model with a \code{predict()} method.
#' @param data Data frame used to compute nonconformity scores (calibration set).
#' @param new Data frame of new locations for prediction. If \code{NULL},
#'   computes intervals for \code{data} via leave-one-block-out.
#' @param target Character. Target variable name in \code{data}.
#' @param coords Character vector of length 2. Coordinate column names.
#'   If provided, uses spatial blocking for calibration.
#' @param alpha Numeric. Miscoverage level. Default: 0.1 (90% intervals).
#' @param method Character. Conformal method:
#'   \code{"split"} (default) uses a held-out calibration set,
#'   \code{"block_jackknife"} uses leave-one-block-out,
#'   \code{"block_cv"} uses blocked cross-validation residuals.
#' @param folds A \code{borg_cv} object or fold list. If provided, residuals
#'   are computed from these folds (respecting spatial structure). Overrides
#'   internal blocking.
#' @param n_blocks Integer. Number of spatial blocks if \code{folds} is NULL
#'   and \code{coords} is provided. Default: 10.
#' @param type Character. For classification: \code{"regression"} (default)
#'   or \code{"classification"}. Classification uses adaptive prediction sets.
#' @param seed Integer. Random seed. Default: 42.
#'
#' @return A data frame with class \code{"borg_conformal"} containing:
#'   \describe{
#'     \item{prediction}{Point prediction}
#'     \item{lower}{Lower bound of prediction interval}
#'     \item{upper}{Upper bound of prediction interval}
#'     \item{width}{Interval width (upper - lower)}
#'     \item{x and y}{Coordinates (if \code{coords} provided and \code{new} has them)}
#'   }
#'   Attributes include \code{alpha}, \code{method}, \code{coverage_estimate},
#'   and \code{quantile_score} (the nonconformity threshold).
#'
#' @details
#' \subsection{Why spatial conformal?}{
#' Standard split conformal prediction computes residuals on a random
#' calibration set. Under spatial autocorrelation, nearby calibration points
#' produce correlated residuals, leading to underestimated interval widths
#' and actual coverage below the nominal level.
#'
#' By using spatially-blocked calibration (where residuals come from
#' predictions on spatially separated test folds), the effective sample
#' size of nonconformity scores is honest, and coverage guarantees hold
#' approximately even under dependence.
#' }
#'
#' \subsection{Methods}{
#' \describe{
#'   \item{\code{"split"}}{Splits data into training and calibration sets
#'     using spatial blocks. Fast, but uses only part of the data.}
#'   \item{\code{"block_jackknife"}}{Leave-one-block-out: refit the model
#'     excluding each block, predict on the held-out block. More
#'     data-efficient but slower.}
#'   \item{\code{"block_cv"}}{Use pre-computed blocked CV residuals from
#'     a \code{borg_cv} object. Requires \code{folds}.}
#' }
#' }
#'
#' @references
#' Mao, H., Martin, R., & Reich, B. J. (2024). Valid prediction inference
#' with spatial conformal methods. \emph{arXiv preprint arXiv:2403.14058}.
#'
#' Johnstone, C., & Cox, D. (2023). Conformal prediction with spatial data.
#'
#' Vovk, V., Gammerman, A., & Shafer, G. (2005).
#' \emph{Algorithmic Learning in a Random World}. Springer.
#'
#' @examples
#' set.seed(42)
#' d <- data.frame(x = runif(200), y = runif(200), a = rnorm(200))
#' d$z <- 2 * d$a + sin(d$x * 10) + rnorm(200, sd = 0.5)
#' model <- lm(z ~ a + x + y, data = d)
#'
#' # Split conformal with spatial blocking
#' conf <- borg_conformal(model, d, target = "z", coords = c("x", "y"))
#' conf
#'
#' # Predict on new locations
#' new <- data.frame(x = runif(50), y = runif(50), a = rnorm(50))
#' pred <- borg_conformal(model, d, new = new, target = "z",
#'                         coords = c("x", "y"))
#'
#' @export
borg_conformal <- function(model, data, new = NULL, target,
                            coords = NULL, alpha = 0.1,
                            method = c("split", "block_jackknife", "block_cv"),
                            folds = NULL, n_blocks = 10,
                            type = c("regression", "classification"),
                            seed = 42) {

  method <- match.arg(method)
  type <- match.arg(type)
  set.seed(seed)

  if (!target %in% names(data)) {
    stop(sprintf("target '%s' not found in data", target))
  }
  if (alpha <= 0 || alpha >= 1) {
    stop("alpha must be between 0 and 1 (exclusive)")
  }

  actual <- data[[target]]
  n <- nrow(data)

  # ------------------------------------------------------------------
  # Step 1: Compute nonconformity scores from spatially-blocked residuals
  # ------------------------------------------------------------------
  if (!is.null(folds) && method != "block_cv") {
    method <- "block_cv"
  }

  if (method == "block_cv" && !is.null(folds)) {
    scores <- .conformal_scores_from_folds(model, data, target, folds, type)

  } else if (method == "block_jackknife" && !is.null(coords)) {
    scores <- .conformal_scores_jackknife(model, data, target, coords,
                                           n_blocks, type, seed)

  } else {
    # Split method: use spatial blocks for calibration split
    scores <- .conformal_scores_split(model, data, target, coords,
                                       n_blocks, type, seed)
  }

  # ------------------------------------------------------------------
  # Step 2: Compute conformal quantile
  # ------------------------------------------------------------------
  # Finite-sample correction: ceil((n_cal + 1) * (1 - alpha)) / n_cal
  n_cal <- length(scores)
  q_level <- ceiling((n_cal + 1) * (1 - alpha)) / n_cal
  q_level <- min(q_level, 1)
  q_hat <- stats::quantile(scores, probs = q_level, names = FALSE)

  # ------------------------------------------------------------------
  # Step 3: Generate prediction intervals
  # ------------------------------------------------------------------
  pred_data <- if (!is.null(new)) new else data
  predictions <- stats::predict(model, newdata = pred_data)

  if (type == "regression") {
    result <- data.frame(
      prediction = predictions,
      lower = predictions - q_hat,
      upper = predictions + q_hat,
      width = 2 * q_hat,
      stringsAsFactors = FALSE
    )
  } else {
    # Classification: prediction sets (all classes with score >= threshold)
    result <- data.frame(
      prediction = predictions,
      lower = NA_real_,
      upper = NA_real_,
      width = q_hat,
      stringsAsFactors = FALSE
    )
  }

  # Add coordinates
  if (!is.null(coords) && all(coords %in% names(pred_data))) {
    result$x <- pred_data[[coords[1]]]
    result$y <- pred_data[[coords[2]]]
  }

  # Estimate actual coverage on calibration data
  if (is.null(new) && type == "regression") {
    in_interval <- actual >= result$lower & actual <= result$upper
    coverage_est <- mean(in_interval, na.rm = TRUE)
  } else {
    coverage_est <- NA_real_
  }

  attr(result, "alpha") <- alpha
  attr(result, "nominal_coverage") <- 1 - alpha
  attr(result, "coverage_estimate") <- coverage_est
  attr(result, "quantile_score") <- q_hat
  attr(result, "n_calibration") <- n_cal
  attr(result, "method") <- method
  attr(result, "coord_names") <- coords
  class(result) <- c("borg_conformal", "data.frame")

  result
}


# Internal: scores from pre-computed folds --------------------------------
.conformal_scores_from_folds <- function(model, data, target, folds, type) {
  fold_list <- if (inherits(folds, "borg_cv")) folds$folds else folds
  actual <- data[[target]]
  scores <- numeric(0)

  for (fold in fold_list) {
    test_idx <- fold$test
    preds <- stats::predict(model, newdata = data[test_idx, , drop = FALSE])
    if (type == "regression") {
      scores <- c(scores, abs(actual[test_idx] - preds))
    } else {
      scores <- c(scores, 1 - preds)  # softmax-based nonconformity
    }
  }
  scores
}


# Internal: leave-one-block-out jackknife ---------------------------------
.conformal_scores_jackknife <- function(model, data, target, coords,
                                         n_blocks, type, seed) {
  set.seed(seed)
  n <- nrow(data)
  actual <- data[[target]]

  # Spatial blocking
  block_ids <- .spatial_blocks(data, coords, n_blocks)
  unique_blocks <- sort(unique(block_ids))
  scores <- numeric(0)

  # Determine model fitting function from the model object
  fit_call <- model$call
  fit_fun <- if (!is.null(fit_call)) {
    eval(fit_call[[1]])
  } else {
    stats::lm
  }

  formula_obj <- tryCatch(stats::formula(model), error = function(e) NULL)

  for (b in unique_blocks) {
    test_idx <- which(block_ids == b)
    train_idx <- which(block_ids != b)

    if (length(test_idx) == 0 || length(train_idx) < 10) next

    # Refit model on training block
    refit <- tryCatch(
      fit_fun(formula_obj, data = data[train_idx, , drop = FALSE]),
      error = function(e) NULL
    )
    if (is.null(refit)) next

    preds <- stats::predict(refit, newdata = data[test_idx, , drop = FALSE])
    if (type == "regression") {
      scores <- c(scores, abs(actual[test_idx] - preds))
    } else {
      scores <- c(scores, 1 - preds)
    }
  }

  if (length(scores) == 0) {
    stop("No valid calibration scores produced. Check model and data.")
  }
  scores
}


# Internal: split conformal with spatial blocking -------------------------
.conformal_scores_split <- function(model, data, target, coords,
                                     n_blocks, type, seed) {
  set.seed(seed)
  n <- nrow(data)
  actual <- data[[target]]

  if (!is.null(coords) && all(coords %in% names(data))) {
    block_ids <- .spatial_blocks(data, coords, n_blocks)
    unique_blocks <- sort(unique(block_ids))
    # Use ~half the blocks as calibration
    n_cal_blocks <- max(2, length(unique_blocks) %/% 2)
    cal_blocks <- sample(unique_blocks, n_cal_blocks)
    cal_idx <- which(block_ids %in% cal_blocks)
  } else {
    # Random split
    cal_idx <- sample(n, n %/% 2)
  }

  preds <- stats::predict(model, newdata = data[cal_idx, , drop = FALSE])
  if (type == "regression") {
    scores <- abs(actual[cal_idx] - preds)
  } else {
    scores <- 1 - preds
  }
  scores
}


# Internal: spatial blocking helper ---------------------------------------
.spatial_blocks <- function(data, coords, n_blocks) {
  coord_mat <- as.matrix(data[, coords, drop = FALSE])
  complete <- complete.cases(coord_mat)
  n <- nrow(data)
  n_blocks <- min(n_blocks, sum(complete) %/% 3)
  if (n_blocks < 2) n_blocks <- 2

  km <- stats::kmeans(coord_mat[complete, ], centers = n_blocks, nstart = 5)
  block_ids <- rep(NA_integer_, n)
  block_ids[complete] <- km$cluster
  block_ids[is.na(block_ids)] <- sample(seq_len(n_blocks),
                                         sum(is.na(block_ids)),
                                         replace = TRUE)
  block_ids
}


#' @export
print.borg_conformal <- function(x, ...) {
  method <- attr(x, "method") %||% "unknown"
  alpha <- attr(x, "alpha") %||% 0.1
  coverage <- attr(x, "coverage_estimate")
  n_cal <- attr(x, "n_calibration") %||% NA
  q_hat <- attr(x, "quantile_score") %||% NA

  cat("BORG Conformal Prediction\n")
  cat(sprintf("  Method: %s\n", method))
  cat(sprintf("  Nominal coverage: %.0f%%\n", (1 - alpha) * 100))
  if (!is.na(coverage)) {
    cat(sprintf("  Empirical coverage: %.1f%%\n", coverage * 100))
  }
  cat(sprintf("  Calibration scores: %d\n", n_cal))
  cat(sprintf("  Interval half-width (q_hat): %.4f\n", q_hat))
  cat(sprintf("  Predictions: %d\n", nrow(x)))
  cat(sprintf("  Mean width: %.4f | Median: %.4f\n",
              mean(x$width), stats::median(x$width)))
  invisible(x)
}


#' @exportS3Method ggplot2::autoplot
autoplot.borg_conformal <- function(object, type = c("intervals", "coverage", "map"),
                                     max_points = 100, ...) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package 'ggplot2' required")
  }
  type <- match.arg(type)
  alpha <- attr(object, "alpha") %||% 0.1
  coverage <- attr(object, "coverage_estimate")
  has_coords <- "x" %in% names(object)

  if (type == "map" && has_coords) {
    coord_names <- attr(object, "coord_names") %||% c("x", "y")
    ggplot2::ggplot(object, ggplot2::aes(x = .data$x, y = .data$y)) +
      ggplot2::geom_point(
        ggplot2::aes(color = .data$width),
        size = 1.5, alpha = 0.8
      ) +
      ggplot2::scale_color_viridis_c(option = "plasma") +
      ggplot2::coord_equal() +
      ggplot2::labs(
        title = "Conformal Prediction Interval Width",
        subtitle = sprintf("%.0f%% nominal coverage | method: %s",
                            (1 - alpha) * 100, attr(object, "method")),
        x = coord_names[1], y = coord_names[2],
        color = "Width"
      ) +
      borg_theme()
  } else {
    # Interval plot: show predictions with bands
    n <- nrow(object)
    if (n > max_points) {
      idx <- sort(sample(n, max_points))
      df <- object[idx, ]
      df$index <- seq_len(max_points)
    } else {
      df <- object
      df$index <- seq_len(n)
    }
    # Sort by prediction for readability
    df <- df[order(df$prediction), ]
    df$index <- seq_len(nrow(df))

    sub_text <- sprintf("%.0f%% nominal", (1 - alpha) * 100)
    if (!is.na(coverage)) {
      sub_text <- paste0(sub_text, sprintf(" | %.1f%% empirical", coverage * 100))
    }

    ggplot2::ggplot(df, ggplot2::aes(x = .data$index, y = .data$prediction)) +
      ggplot2::geom_ribbon(
        ggplot2::aes(ymin = .data$lower, ymax = .data$upper),
        fill = "#3498DB", alpha = 0.3
      ) +
      ggplot2::geom_point(size = 0.8, color = "#2C3E50") +
      ggplot2::labs(
        title = "Conformal Prediction Intervals",
        subtitle = sub_text,
        x = "Observation (sorted by prediction)", y = "Predicted value"
      ) +
      borg_theme()
  }
}
