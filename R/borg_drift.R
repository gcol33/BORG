# ===========================================================================
# borg_drift() — Distribution shift detection
# ===========================================================================

#' Detect Distribution Shift Between Training and Deployment Data
#'
#' Goes beyond Area of Applicability (AOA) by quantifying \emph{how} the
#' distribution changed and \emph{which features} drifted. Combines
#' univariate tests (Kolmogorov-Smirnov per feature) with a multivariate
#' classifier two-sample test (train a model to distinguish train from
#' deployment; AUC > 0.5 indicates shift).
#'
#' @param train Data frame of training data.
#' @param new Data frame of deployment/prediction data.
#' @param predictors Character vector. Predictor column names. If \code{NULL},
#'   uses all shared numeric columns.
#' @param alpha Numeric. Significance level for per-feature KS tests.
#'   Default: 0.05.
#' @param n_perm Integer. Number of permutations for the classifier
#'   two-sample test p-value. Default: 100. Set to 0 to skip.
#' @param seed Integer. Random seed. Default: 42.
#'
#' @return A list with class \code{"borg_drift"} containing:
#'   \describe{
#'     \item{feature_drift}{Data frame with per-feature KS statistic,
#'       p-value, effect_size (Cohen's d), shifted (logical), and
#'       direction ("higher", "lower", "similar")}
#'     \item{n_shifted}{Number of features with significant drift}
#'     \item{classifier_auc}{AUC of train-vs-deployment classifier
#'       (0.5 = no shift, 1.0 = complete separation)}
#'     \item{classifier_pvalue}{Permutation p-value for AUC}
#'     \item{overall_severity}{Character: "none", "mild", "moderate", "severe"}
#'     \item{summary}{One-sentence summary}
#'   }
#'   Has \code{print()} and \code{autoplot()} methods.
#'
#' @details
#' \subsection{Univariate tests}{
#' For each feature, a two-sample Kolmogorov-Smirnov test detects
#' distributional differences. Effect size is measured by Cohen's d
#' (standardized mean difference). P-values are Bonferroni-corrected.
#' }
#'
#' \subsection{Multivariate classifier test}{
#' A logistic regression is trained to distinguish training from deployment
#' observations. If the data distributions are identical, the classifier
#' achieves AUC ~ 0.5. Higher AUC indicates multivariate shift that may
#' not be captured by univariate tests alone. Statistical significance is
#' assessed via permutation.
#' }
#'
#' @references
#' Ginsberg, T., Liang, Z., & Krishnan, R. G. (2023). A learning based
#' hypothesis test for harmful covariate shift. \emph{ICLR}.
#'
#' Lopez-Paz, D., & Oquab, M. (2017). Revisiting classifier two-sample
#' tests. \emph{ICLR}.
#'
#' @examples
#' set.seed(42)
#' train <- data.frame(a = rnorm(200), b = rnorm(200), c = rnorm(200))
#' # Deployment: feature 'a' has shifted
#' deploy <- data.frame(a = rnorm(100, mean = 1), b = rnorm(100),
#'                       c = rnorm(100))
#' drift <- borg_drift(train, deploy)
#' drift
#'
#' @export
borg_drift <- function(train, new, predictors = NULL, alpha = 0.05,
                        n_perm = 100, seed = 42) {

  set.seed(seed)

  if (is.null(predictors)) {
    shared <- intersect(names(train), names(new))
    predictors <- shared[vapply(train[shared], is.numeric, logical(1))]
  }

  if (length(predictors) == 0) {
    stop("No shared numeric predictor columns found")
  }

  n_train <- nrow(train)
  n_new <- nrow(new)

  # ------------------------------------------------------------------
  # Univariate KS tests per feature
  # ------------------------------------------------------------------
  bonf_alpha <- alpha / length(predictors)

  feature_results <- lapply(predictors, function(var) {
    x <- train[[var]]
    y <- new[[var]]
    x <- x[!is.na(x)]
    y <- y[!is.na(y)]

    ks <- stats::ks.test(x, y)

    # Cohen's d
    pooled_sd <- sqrt(((length(x) - 1) * stats::var(x) +
                        (length(y) - 1) * stats::var(y)) /
                       (length(x) + length(y) - 2))
    cohens_d <- if (pooled_sd > 0) (mean(y) - mean(x)) / pooled_sd else 0

    direction <- if (abs(cohens_d) < 0.2) "similar"
                 else if (cohens_d > 0) "higher"
                 else "lower"

    data.frame(
      variable = var,
      ks_statistic = ks$statistic,
      p_value = ks$p.value,
      cohens_d = cohens_d,
      abs_d = abs(cohens_d),
      shifted = ks$p.value < bonf_alpha,
      direction = direction,
      stringsAsFactors = FALSE,
      row.names = NULL
    )
  })

  feature_df <- do.call(rbind, feature_results)
  feature_df <- feature_df[order(-feature_df$abs_d), ]
  rownames(feature_df) <- NULL

  n_shifted <- sum(feature_df$shifted)

  # ------------------------------------------------------------------
  # Classifier two-sample test (logistic regression)
  # ------------------------------------------------------------------
  classifier_auc <- NA_real_
  classifier_pvalue <- NA_real_

  if (n_perm > 0) {
    train_mat <- as.matrix(train[, predictors, drop = FALSE])
    new_mat <- as.matrix(new[, predictors, drop = FALSE])

    combined <- rbind(train_mat, new_mat)
    labels <- c(rep(0, n_train), rep(1, n_new))

    # Handle NAs
    complete <- complete.cases(combined)
    combined <- combined[complete, , drop = FALSE]
    labels <- labels[complete]

    # Scale
    means <- colMeans(combined)
    sds <- apply(combined, 2, stats::sd)
    sds[sds == 0] <- 1
    combined_scaled <- scale(combined, center = means, scale = sds)

    # Fit logistic regression
    obs_auc <- .classifier_auc(combined_scaled, labels, seed)

    # Permutation test
    perm_aucs <- vapply(seq_len(n_perm), function(i) {
      perm_labels <- sample(labels)
      .classifier_auc(combined_scaled, perm_labels, seed + i)
    }, numeric(1))

    classifier_auc <- obs_auc
    classifier_pvalue <- (sum(perm_aucs >= obs_auc) + 1) / (n_perm + 1)
  }

  # ------------------------------------------------------------------
  # Overall severity
  # ------------------------------------------------------------------
  frac_shifted <- n_shifted / length(predictors)
  max_d <- max(feature_df$abs_d)

  severity <- if (frac_shifted == 0 && (is.na(classifier_auc) || classifier_auc < 0.6)) {
    "none"
  } else if (frac_shifted <= 0.2 && max_d < 0.5) {
    "mild"
  } else if (frac_shifted <= 0.5 || max_d < 1.0) {
    "moderate"
  } else {
    "severe"
  }

  summary_text <- sprintf(
    "%d/%d features shifted (max |d| = %.2f). Classifier AUC = %.3f (%s).",
    n_shifted, length(predictors), max_d,
    if (is.na(classifier_auc)) 0 else classifier_auc,
    severity
  )

  result <- list(
    feature_drift = feature_df,
    n_shifted = n_shifted,
    n_features = length(predictors),
    classifier_auc = classifier_auc,
    classifier_pvalue = classifier_pvalue,
    overall_severity = severity,
    summary = summary_text
  )

  class(result) <- c("borg_drift", "list")
  result
}


# Internal: compute AUC via logistic regression ---------------------------
.classifier_auc <- function(X, labels, seed) {
  set.seed(seed)
  n <- length(labels)

  # Simple 5-fold CV AUC
  folds <- sample(rep(seq_len(5), length.out = n))
  probs <- numeric(n)

  for (f in seq_len(5)) {
    test_idx <- which(folds == f)
    train_idx <- which(folds != f)

    df_train <- data.frame(y = labels[train_idx], X[train_idx, , drop = FALSE])
    df_test <- data.frame(X[test_idx, , drop = FALSE])

    fit <- tryCatch(
      stats::glm(y ~ ., data = df_train, family = stats::binomial()),
      error = function(e) NULL,
      warning = function(w) {
        suppressWarnings(stats::glm(y ~ ., data = df_train,
                                     family = stats::binomial()))
      }
    )

    if (is.null(fit)) {
      probs[test_idx] <- 0.5
    } else {
      p <- tryCatch(
        stats::predict(fit, newdata = df_test, type = "response"),
        error = function(e) rep(0.5, length(test_idx))
      )
      probs[test_idx] <- p
    }
  }

  # Compute AUC via Mann-Whitney U
  pos <- probs[labels == 1]
  neg <- probs[labels == 0]
  if (length(pos) == 0 || length(neg) == 0) return(0.5)

  u <- sum(vapply(pos, function(p) mean(p > neg) + 0.5 * mean(p == neg),
                   numeric(1)))
  u / length(pos)
}


#' @export
print.borg_drift <- function(x, ...) {
  cat("BORG Distribution Shift Detection\n")
  cat("==================================\n\n")
  cat(sprintf("  %s\n\n", x$summary))

  cat("Per-feature drift:\n")
  df <- x$feature_drift
  for (i in seq_len(nrow(df))) {
    r <- df[i, ]
    flag <- if (r$shifted) " ***" else ""
    cat(sprintf("  %-20s |d| = %.3f  KS = %.3f  p = %.4f  %s%s\n",
                r$variable, r$abs_d, r$ks_statistic, r$p_value,
                r$direction, flag))
  }

  if (!is.na(x$classifier_auc)) {
    cat(sprintf("\nClassifier two-sample test:\n  AUC = %.3f (p = %.4f)\n",
                x$classifier_auc, x$classifier_pvalue))
  }
  invisible(x)
}


#' @exportS3Method ggplot2::autoplot
autoplot.borg_drift <- function(object, ...) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package 'ggplot2' required")
  }

  df <- object$feature_drift
  df$variable <- factor(df$variable, levels = rev(df$variable))

  ggplot2::ggplot(df, ggplot2::aes(x = .data$variable, y = .data$abs_d)) +
    ggplot2::geom_segment(
      ggplot2::aes(xend = .data$variable, y = 0, yend = .data$abs_d),
      linewidth = 0.8, color = "#2C3E50"
    ) +
    ggplot2::geom_point(
      ggplot2::aes(color = .data$shifted, shape = .data$shifted),
      size = 3
    ) +
    ggplot2::geom_hline(yintercept = c(0.2, 0.5, 0.8),
                         linetype = "dashed", color = "gray60",
                         linewidth = 0.3) +
    ggplot2::annotate("text", x = 0.5, y = c(0.2, 0.5, 0.8),
                       label = c("small", "medium", "large"),
                       hjust = -0.1, vjust = -0.5, size = 2.5,
                       color = "gray50") +
    ggplot2::scale_color_manual(
      values = c("TRUE" = "#E74C3C", "FALSE" = "#27AE60"),
      labels = c("TRUE" = "Shifted", "FALSE" = "Stable"),
      guide = "none"
    ) +
    ggplot2::scale_shape_manual(
      values = c("TRUE" = 17, "FALSE" = 16),
      guide = "none"
    ) +
    ggplot2::coord_flip() +
    ggplot2::labs(
      title = "Feature-Level Distribution Shift",
      subtitle = sprintf("%d/%d shifted | Classifier AUC = %.3f | %s",
                          object$n_shifted, object$n_features,
                          if (is.na(object$classifier_auc)) 0 else object$classifier_auc,
                          object$overall_severity),
      x = NULL, y = "|Cohen's d|"
    ) +
    borg_theme() +
    ggplot2::theme(panel.grid.major.y = ggplot2::element_blank())
}
