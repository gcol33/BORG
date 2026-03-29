# ===========================================================================
# borg_importance() — Spatially-aware permutation importance
# ===========================================================================

#' Block-Permutation Variable Importance
#'
#' Computes permutation importance that respects spatial structure.
#' Instead of permuting individual rows (which breaks spatial
#' autocorrelation and inflates importance), permutes values
#' within spatial blocks.
#'
#' @param model A fitted model with a \code{predict()} method.
#' @param data Data frame with predictor columns.
#' @param target Character. Target variable name.
#' @param coords Character vector of length 2. Coordinate column names.
#'   If \code{NULL}, falls back to standard row permutation.
#' @param predictors Character vector. Variables to assess. If \code{NULL},
#'   uses all columns except target and coords.
#' @param n_blocks Integer. Number of spatial blocks for permutation.
#'   Default: 10.
#' @param n_rep Integer. Number of permutation repeats per variable.
#'   Default: 10.
#' @param metric Character. \code{"rmse"} (default) or \code{"mae"}.
#' @param seed Integer. Random seed. Default: 42.
#'
#' @return A data frame with class \code{"borg_importance"} containing:
#'   \describe{
#'     \item{variable}{Predictor name}
#'     \item{importance}{Mean increase in error after permutation}
#'     \item{importance_sd}{SD across repeats}
#'     \item{rank}{Importance rank (1 = most important)}
#'   }
#'   Has an \code{autoplot()} method.
#'
#' @examples
#' set.seed(42)
#' d <- data.frame(x = runif(100), y = runif(100),
#'                  a = rnorm(100), b = rnorm(100))
#' d$z <- d$a * 2 + rnorm(100, sd = 0.5)
#' model <- lm(z ~ a + b, data = d)
#' imp <- borg_importance(model, d, target = "z", coords = c("x", "y"))
#' imp
#'
#' @export
borg_importance <- function(model, data, target, coords = NULL,
                              predictors = NULL, n_blocks = 10,
                              n_rep = 10, metric = c("rmse", "mae"),
                              seed = 42) {
  metric <- match.arg(metric)
  set.seed(seed)

  n <- nrow(data)
  actual <- data[[target]]

  # Determine predictors
  exclude <- c(target, coords)
  if (is.null(predictors)) {
    predictors <- setdiff(names(data), exclude)
    # Keep only numeric predictors
    predictors <- predictors[vapply(data[predictors], is.numeric, logical(1))]
  }

  # Baseline performance
  baseline_pred <- stats::predict(model, newdata = data)
  baseline_error <- .compute_metric(actual, baseline_pred, metric)

  # Create spatial blocks for permutation
  if (!is.null(coords) && all(coords %in% names(data))) {
    x <- data[[coords[1]]]
    y <- data[[coords[2]]]
    # K-means blocking
    coord_mat <- cbind(x, y)
    complete <- complete.cases(coord_mat)
    n_blocks_actual <- min(n_blocks, sum(complete) %/% 3)
    if (n_blocks_actual < 2) n_blocks_actual <- 2

    km <- stats::kmeans(coord_mat[complete, ], centers = n_blocks_actual, nstart = 5)
    block_ids <- rep(NA_integer_, n)
    block_ids[complete] <- km$cluster
    block_ids[is.na(block_ids)] <- sample(seq_len(n_blocks_actual),
                                           sum(is.na(block_ids)), replace = TRUE)
  } else {
    # Fallback: random blocks
    block_ids <- sample(rep(seq_len(n_blocks), length.out = n), n)
  }

  unique_blocks <- sort(unique(block_ids))

  # Permutation importance per variable
  results <- lapply(predictors, function(var) {
    perm_errors <- vapply(seq_len(n_rep), function(r) {
      data_perm <- data
      # Permute the variable values across blocks (shuffle block assignment)
      block_perm <- sample(unique_blocks)
      mapping <- setNames(block_perm, unique_blocks)

      # For each observation, replace its value with the value from
      # the observation in the same position in the remapped block
      new_vals <- data_perm[[var]]
      for (b in unique_blocks) {
        source_block <- mapping[as.character(b)]
        source_idx <- which(block_ids == source_block)
        target_idx <- which(block_ids == b)
        # Match sizes by sampling with replacement if needed
        if (length(source_idx) >= length(target_idx)) {
          new_vals[target_idx] <- data[[var]][source_idx[seq_along(target_idx)]]
        } else {
          new_vals[target_idx] <- data[[var]][sample(source_idx, length(target_idx),
                                                      replace = TRUE)]
        }
      }
      data_perm[[var]] <- new_vals

      perm_pred <- stats::predict(model, newdata = data_perm)
      .compute_metric(actual, perm_pred, metric)
    }, numeric(1))

    data.frame(
      variable = var,
      importance = mean(perm_errors) - baseline_error,
      importance_sd = stats::sd(perm_errors),
      stringsAsFactors = FALSE
    )
  })

  result <- do.call(rbind, results)
  result <- result[order(-result$importance), ]
  result$rank <- seq_len(nrow(result))
  rownames(result) <- NULL

  class(result) <- c("borg_importance", "data.frame")
  attr(result, "baseline_error") <- baseline_error
  attr(result, "metric") <- metric
  attr(result, "method") <- if (!is.null(coords)) "block_permutation" else "row_permutation"

  result
}


# .compute_metric is defined in borg_compare.R (supports rmse, mae, rsq, accuracy, auc)


#' @export
print.borg_importance <- function(x, ...) {
  method <- attr(x, "method") %||% "unknown"
  metric <- attr(x, "metric") %||% "unknown"
  cat(sprintf("BORG Variable Importance (%s, %s)\n\n", method, metric))
  print.data.frame(x, row.names = FALSE)
  invisible(x)
}


#' @exportS3Method ggplot2::autoplot
autoplot.borg_importance <- function(object, max_vars = 20, ...) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package 'ggplot2' required")
  }

  df <- head(object, max_vars)
  df$variable <- factor(df$variable, levels = rev(df$variable))
  metric_label <- toupper(attr(object, "metric") %||% "error")
  method <- attr(object, "method") %||% "permutation"

  ggplot2::ggplot(df, ggplot2::aes(x = .data$variable, y = .data$importance)) +
    ggplot2::geom_segment(
      ggplot2::aes(xend = .data$variable, y = 0, yend = .data$importance),
      color = "#2C3E50", linewidth = 0.8
    ) +
    ggplot2::geom_point(color = "#2C3E50", size = 3) +
    ggplot2::geom_errorbar(
      ggplot2::aes(ymin = .data$importance - .data$importance_sd,
                    ymax = .data$importance + .data$importance_sd),
      width = 0.2, color = "gray50", linewidth = 0.4
    ) +
    ggplot2::coord_flip() +
    ggplot2::labs(
      title = sprintf("Variable Importance (%s)", method),
      subtitle = sprintf("Increase in %s after permutation", metric_label),
      x = NULL, y = sprintf("\u0394 %s", metric_label)
    ) +
    borg_theme() +
    ggplot2::theme(panel.grid.major.y = ggplot2::element_blank())
}
