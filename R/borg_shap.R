# ===========================================================================
# borg_shap() — Spatially-aware SHAP value approximation
# ===========================================================================

#' Spatial SHAP Values
#'
#' Approximates SHAP values using block-conditional expectations instead
#' of naive row permutation. Standard SHAP marginalizes over all
#' observations, which creates impossible feature combinations when data
#' is spatially structured. Spatial SHAP marginalizes within spatial
#' blocks, preserving realistic covariate relationships.
#'
#' @param model A fitted model with a \code{predict()} method.
#' @param data Data frame with predictors.
#' @param target Character. Target variable name (excluded from SHAP).
#' @param coords Character vector of length 2. Coordinate column names.
#'   If provided, uses spatial blocking for marginal expectations.
#' @param predictors Character vector. Variables to compute SHAP for.
#'   If \code{NULL}, uses all numeric columns except target and coords.
#' @param n_blocks Integer. Number of spatial blocks for marginal
#'   expectations. Default: 10.
#' @param n_samples Integer. Number of background samples per block for
#'   marginal expectations. Default: 50.
#' @param explain_idx Integer vector. Row indices to explain. If \code{NULL},
#'   explains all rows (can be slow).
#' @param seed Integer. Random seed. Default: 42.
#'
#' @return A list with class \code{"borg_shap"} containing:
#'   \describe{
#'     \item{shap_values}{Matrix (n_explain x n_predictors) of SHAP values}
#'     \item{baseline}{Mean prediction (expected value)}
#'     \item{feature_importance}{Named vector of mean |SHAP| per feature}
#'     \item{predictors}{Feature names}
#'     \item{method}{"spatial_block" or "standard"}
#'   }
#'   Has \code{print()} and \code{autoplot()} methods.
#'
#' @details
#' \subsection{Algorithm}{
#' For each observation \eqn{x_i} and feature \eqn{j}:
#' \enumerate{
#'   \item Identify the spatial block containing \eqn{x_i}.
#'   \item Sample background points from \emph{other} blocks.
#'   \item Compute marginal contribution: replace feature \eqn{j} with
#'     values from background points, keeping all other features fixed.
#'   \item Average the change in prediction.
#' }
#' This approximates the Shapley value while respecting spatial structure.
#' }
#'
#' \subsection{Why not standard SHAP?}{
#' Standard kernel SHAP or marginal SHAP samples replacement values
#' uniformly from the dataset. For spatial data, this creates combinations
#' that never occur in reality (e.g., tropical temperature with arctic
#' precipitation), biasing the SHAP values.
#' }
#'
#' @examples
#' set.seed(42)
#' d <- data.frame(x = runif(100), y = runif(100),
#'                  a = rnorm(100), b = rnorm(100))
#' d$z <- 3 * d$a - d$b + rnorm(100, sd = 0.5)
#' model <- lm(z ~ a + b, data = d)
#' shap <- borg_shap(model, d, target = "z", coords = c("x", "y"),
#'                    explain_idx = 1:20)
#' shap
#'
#' @export
borg_shap <- function(model, data, target, coords = NULL,
                        predictors = NULL, n_blocks = 10,
                        n_samples = 50, explain_idx = NULL,
                        seed = 42) {

  set.seed(seed)

  exclude <- c(target, coords)
  if (is.null(predictors)) {
    predictors <- setdiff(names(data), exclude)
    predictors <- predictors[vapply(data[predictors], is.numeric, logical(1))]
  }

  if (length(predictors) == 0) {
    stop("No numeric predictor columns found")
  }

  n <- nrow(data)
  if (is.null(explain_idx)) explain_idx <- seq_len(n)
  n_explain <- length(explain_idx)

  # Spatial blocking
  if (!is.null(coords) && all(coords %in% names(data))) {
    block_ids <- .spatial_blocks(data, coords, n_blocks)
    method <- "spatial_block"
  } else {
    block_ids <- sample(rep(seq_len(n_blocks), length.out = n))
    method <- "standard"
  }

  # Baseline prediction
  all_preds <- stats::predict(model, newdata = data)
  baseline <- mean(all_preds, na.rm = TRUE)

  # SHAP approximation via block-conditional marginals
  shap_mat <- matrix(0, nrow = n_explain, ncol = length(predictors))
  colnames(shap_mat) <- predictors

  for (idx_pos in seq_len(n_explain)) {
    i <- explain_idx[idx_pos]
    obs_block <- block_ids[i]

    # Background: sample from other blocks
    other_idx <- which(block_ids != obs_block)
    if (length(other_idx) == 0) other_idx <- seq_len(n)  # fallback
    bg_idx <- sample(other_idx, min(n_samples, length(other_idx)))

    # Prediction for the original observation
    pred_orig <- all_preds[i]

    for (j in seq_along(predictors)) {
      var <- predictors[j]

      # Replace feature j with background values
      data_modified <- data[rep(i, length(bg_idx)), , drop = FALSE]
      data_modified[[var]] <- data[[var]][bg_idx]

      # Marginal prediction without feature j's true value
      preds_without <- stats::predict(model, newdata = data_modified)

      # SHAP = original prediction - mean prediction with feature replaced
      shap_mat[idx_pos, j] <- pred_orig - mean(preds_without, na.rm = TRUE)
    }
  }

  # Feature importance: mean |SHAP|
  feature_importance <- colMeans(abs(shap_mat))
  feature_importance <- sort(feature_importance, decreasing = TRUE)

  result <- list(
    shap_values = shap_mat,
    baseline = baseline,
    feature_importance = feature_importance,
    predictors = predictors,
    explain_idx = explain_idx,
    method = method,
    n_blocks = n_blocks,
    n_samples = n_samples
  )

  class(result) <- c("borg_shap", "list")
  result
}


#' @export
print.borg_shap <- function(x, ...) {
  cat("BORG Spatial SHAP Values\n")
  cat("========================\n\n")
  cat(sprintf("  Method: %s (%d blocks, %d bg samples)\n",
              x$method, x$n_blocks, x$n_samples))
  cat(sprintf("  Explained: %d observations\n", nrow(x$shap_values)))
  cat(sprintf("  Baseline (E[f(x)]): %.4f\n\n", x$baseline))

  cat("Feature importance (mean |SHAP|):\n")
  imp <- x$feature_importance
  for (var in names(imp)) {
    bar <- paste(rep("#", max(1, round(imp[var] / max(imp) * 25))), collapse = "")
    cat(sprintf("  %-20s %.4f  %s\n", var, imp[var], bar))
  }
  invisible(x)
}


#' @exportS3Method ggplot2::autoplot
autoplot.borg_shap <- function(object, type = c("importance", "beeswarm"),
                                 max_vars = 15, ...) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package 'ggplot2' required")
  }
  type <- match.arg(type)

  if (type == "beeswarm") {
    # SHAP beeswarm plot
    shap <- object$shap_values
    vars <- names(sort(object$feature_importance, decreasing = TRUE))
    vars <- head(vars, max_vars)

    # Long format
    rows <- list()
    for (var in vars) {
      rows[[length(rows) + 1]] <- data.frame(
        variable = var,
        shap = shap[, var],
        stringsAsFactors = FALSE
      )
    }
    df <- do.call(rbind, rows)
    df$variable <- factor(df$variable, levels = rev(vars))

    ggplot2::ggplot(df, ggplot2::aes(x = .data$variable, y = .data$shap)) +
      ggplot2::geom_jitter(width = 0.2, height = 0, alpha = 0.4,
                            size = 0.8, color = "#2C3E50") +
      ggplot2::geom_hline(yintercept = 0, linetype = "dashed",
                           color = "gray50") +
      ggplot2::coord_flip() +
      ggplot2::labs(
        title = sprintf("Spatial SHAP Values (%s)", object$method),
        x = NULL, y = "SHAP value"
      ) +
      borg_theme() +
      ggplot2::theme(panel.grid.major.y = ggplot2::element_blank())

  } else {
    # Importance bar plot
    imp <- object$feature_importance
    vars <- head(names(imp), max_vars)
    df <- data.frame(
      variable = factor(vars, levels = rev(vars)),
      importance = imp[vars],
      stringsAsFactors = FALSE
    )

    ggplot2::ggplot(df, ggplot2::aes(x = .data$variable, y = .data$importance)) +
      ggplot2::geom_segment(
        ggplot2::aes(xend = .data$variable, y = 0, yend = .data$importance),
        linewidth = 0.8, color = "#2C3E50"
      ) +
      ggplot2::geom_point(color = "#2C3E50", size = 3) +
      ggplot2::coord_flip() +
      ggplot2::labs(
        title = sprintf("Spatial SHAP Feature Importance (%s)", object$method),
        x = NULL, y = "Mean |SHAP|"
      ) +
      borg_theme() +
      ggplot2::theme(panel.grid.major.y = ggplot2::element_blank())
  }
}
