# ===========================================================================
# borg_null_test() — Null model significance testing
# ===========================================================================

#' Null Model Significance Test
#'
#' Tests whether model performance is significantly better than random
#' expectation by permuting the response variable and re-evaluating
#' with the same CV scheme. Computes z-scores and p-values.
#'
#' @param data Data frame.
#' @param folds A \code{borg_cv} object or fold list.
#' @param formula Model formula.
#' @param metric Character. \code{"rmse"} (default), \code{"mae"}, or \code{"rsq"}.
#' @param fit_fun Function. Default: \code{lm}.
#' @param n_null Integer. Number of null permutations. Default: 99.
#' @param seed Integer. Random seed. Default: 42.
#' @param verbose Logical. Default: FALSE.
#'
#' @return A list with class \code{"borg_null_test"} containing:
#'   \describe{
#'     \item{empirical}{Empirical CV metric value}
#'     \item{null_distribution}{Numeric vector of null metric values}
#'     \item{z_score}{Z-score of empirical vs null}
#'     \item{p_value}{One-sided p-value}
#'     \item{significant}{Logical. Whether model is significantly better}
#'   }
#'   Has an \code{autoplot()} method showing null distribution with
#'   empirical value.
#'
#' @references
#' Kass, J. M., Muscarella, R., Galante, P. J., Bohl, C. L., Pinilla-Buitrago,
#' G. E., Boria, R. A., Soley-Guardia, M., & Anderson, R. P. (2021).
#' ENMeval 2.0: Redesigned for customizable and reproducible modeling of
#' species' niches and distributions.
#' \emph{Methods in Ecology and Evolution}, 12(9), 1602-1608.
#' \doi{10.1111/2041-210X.13628}
#'
#' @examples
#' set.seed(42)
#' d <- data.frame(x = runif(100), y = runif(100), z = rnorm(100))
#' d$z <- d$x * 3 + rnorm(100)
#' cv <- borg_cv(d, coords = c("x", "y"), target = "z")
#' nt <- borg_null_test(d, cv, z ~ x + y, n_null = 19)
#' nt
#'
#' @export
borg_null_test <- function(data, folds, formula,
                             metric = c("rmse", "mae", "rsq"),
                             fit_fun = stats::lm,
                             n_null = 99L, seed = 42L, verbose = FALSE) {
  metric <- match.arg(metric)
  minimize <- metric %in% c("rmse", "mae")
  fold_list <- if (inherits(folds, "borg_cv")) folds$folds else folds
  target_var <- all.vars(formula)[1]

  # Helper: compute mean CV metric
  eval_cv <- function(d) {
    vals <- vapply(fold_list, function(fold) {
      m <- tryCatch(fit_fun(formula, data = d[fold$train, , drop = FALSE]),
                     error = function(e) NULL)
      if (is.null(m)) return(NA_real_)
      p <- tryCatch(stats::predict(m, newdata = d[fold$test, , drop = FALSE]),
                     error = function(e) rep(NA_real_, length(fold$test)))
      a <- d[[target_var]][fold$test]
      switch(metric,
        "rmse" = sqrt(mean((a - p)^2, na.rm = TRUE)),
        "mae" = mean(abs(a - p), na.rm = TRUE),
        "rsq" = {
          ss <- sum((a - p)^2, na.rm = TRUE)
          st <- sum((a - mean(a, na.rm = TRUE))^2, na.rm = TRUE)
          if (st == 0) NA_real_ else 1 - ss / st
        }
      )
    }, numeric(1))
    mean(vals, na.rm = TRUE)
  }

  # Empirical performance
  empirical <- eval_cv(data)

  # Null distribution
  set.seed(seed)
  null_vals <- vapply(seq_len(n_null), function(i) {
    if (verbose && i %% 10 == 0) message(sprintf("Null %d/%d", i, n_null))
    d_null <- data
    d_null[[target_var]] <- sample(d_null[[target_var]])
    eval_cv(d_null)
  }, numeric(1))

  # Z-score and p-value
  null_mean <- mean(null_vals, na.rm = TRUE)
  null_sd <- stats::sd(null_vals, na.rm = TRUE)
  z_score <- if (null_sd > 0) (empirical - null_mean) / null_sd else NA_real_

  # One-sided p-value
  if (minimize) {
    p_value <- mean(null_vals <= empirical)  # proportion of nulls as good or better
  } else {
    p_value <- mean(null_vals >= empirical)
  }
  # Include the empirical value itself
  p_value <- (sum(if (minimize) null_vals <= empirical else null_vals >= empirical) + 1) / (n_null + 1)

  result <- list(
    empirical = empirical,
    null_distribution = null_vals,
    null_mean = null_mean,
    null_sd = null_sd,
    z_score = z_score,
    p_value = p_value,
    significant = p_value < 0.05,
    metric = metric,
    n_null = n_null
  )
  class(result) <- c("borg_null_test", "list")
  result
}


#' @export
print.borg_null_test <- function(x, ...) {
  cat("BORG Null Model Significance Test\n")
  cat(sprintf("  Empirical %s: %.4f\n", toupper(x$metric), x$empirical))
  cat(sprintf("  Null mean: %.4f (SD %.4f)\n", x$null_mean, x$null_sd))
  cat(sprintf("  Z-score: %.2f\n", x$z_score))
  cat(sprintf("  P-value: %.4f (%d permutations)\n", x$p_value, x$n_null))
  cat(sprintf("  Significant: %s\n", if (x$significant) "YES" else "NO"))
  invisible(x)
}


#' @exportS3Method ggplot2::autoplot
autoplot.borg_null_test <- function(object, ...) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) stop("ggplot2 required")

  df <- data.frame(value = object$null_distribution)
  metric_label <- toupper(object$metric)

  ggplot2::ggplot(df, ggplot2::aes(x = .data$value)) +
    ggplot2::geom_histogram(bins = 30, fill = "gray70", color = "white",
                             linewidth = 0.2) +
    ggplot2::geom_vline(xintercept = object$empirical, color = "#C0392B",
                         linewidth = 1, linetype = "solid") +
    ggplot2::annotate("text", x = object$empirical,
                       y = Inf, vjust = 2, hjust = -0.1,
                       label = sprintf("empirical = %.4f\np = %.4f",
                                        object$empirical, object$p_value),
                       size = 3.5, color = "#C0392B", fontface = "bold") +
    ggplot2::labs(
      title = "Null Model Test",
      subtitle = sprintf("%d permutations | z = %.2f | %s",
                          object$n_null, object$z_score,
                          if (object$significant) "SIGNIFICANT" else "not significant"),
      x = metric_label, y = "Count"
    ) +
    borg_theme()
}
