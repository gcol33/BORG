# ===========================================================================
# borg_adversarial() — Adversarial validation for CV strategy selection
# ===========================================================================

#' Adversarial Validation
#'
#' @description
#' Trains a binary classifier to distinguish training data from prediction
#' locations. High classification accuracy (AUC > 0.7) indicates the
#' prediction domain differs substantially from training, and spatial/blocked
#' CV is essential. Low accuracy (AUC ~ 0.5) means random CV may suffice.
#'
#' @param train Data frame of training data.
#' @param prediction Data frame of prediction locations (same predictor columns).
#' @param predictors Character vector. If \code{NULL}, all shared numeric columns.
#' @param v Integer. Number of CV folds for adversarial classifier. Default: 5.
#'
#' @return A list with class \code{"borg_adversarial"} containing:
#'   \describe{
#'     \item{auc}{Cross-validated AUC of the adversarial classifier}
#'     \item{dissimilarity}{Dissimilarity score (0 to 100 percent)}
#'     \item{recommendation}{Suggested CV strategy based on dissimilarity}
#'     \item{importance}{Variable importance for distinguishing domains}
#'   }
#'
#' @details
#' Uses logistic regression as the adversarial classifier (no external
#' dependencies). The AUC is computed via cross-validation on the combined
#' train+prediction dataset with a binary label (0=train, 1=prediction).
#'
#' Interpretation:
#' \itemize{
#'   \item AUC < 0.6: Low dissimilarity. Random CV likely adequate.
#'   \item AUC 0.6-0.8: Moderate dissimilarity. Spatial CV recommended.
#'   \item AUC > 0.8: High dissimilarity. Spatial CV essential; check AOA.
#' }
#'
#' @examples
#' set.seed(42)
#' train <- data.frame(a = rnorm(100), b = rnorm(100))
#' pred <- data.frame(a = rnorm(50, mean = 2), b = rnorm(50))
#' av <- borg_adversarial(train, pred)
#' av
#'
#' @export
borg_adversarial <- function(train, prediction, predictors = NULL, v = 5L) {

  if (is.null(predictors)) {
    shared <- intersect(names(train), names(prediction))
    predictors <- shared[vapply(train[shared], is.numeric, logical(1))]
  }
  if (length(predictors) == 0) stop("No shared numeric predictor columns")

  # Combine datasets with label
  n_train <- nrow(train)
  n_pred <- nrow(prediction)

  combined <- rbind(
    train[, predictors, drop = FALSE],
    prediction[, predictors, drop = FALSE]
  )
  combined$.label <- c(rep(0L, n_train), rep(1L, n_pred))

  # Scale
  for (p in predictors) {
    combined[[p]] <- as.numeric(scale(combined[[p]]))
  }

  n <- nrow(combined)

  # Cross-validated AUC using logistic regression
  set.seed(42)
  fold_assign <- sample(rep(seq_len(v), length.out = n))

  all_probs <- numeric(n)
  all_labels <- combined$.label

  for (f in seq_len(v)) {
    train_f <- combined[fold_assign != f, , drop = FALSE]
    test_f <- combined[fold_assign == f, , drop = FALSE]

    formula <- stats::as.formula(
      paste(".label ~", paste(predictors, collapse = " + "))
    )
    model <- tryCatch(
      stats::glm(formula, data = train_f, family = stats::binomial()),
      error = function(e) NULL
    )

    if (!is.null(model)) {
      probs <- stats::predict(model, newdata = test_f, type = "response")
      all_probs[fold_assign == f] <- probs
    } else {
      all_probs[fold_assign == f] <- 0.5
    }
  }

  # Compute AUC (no external dependency)
  auc <- .compute_auc(all_labels, all_probs)

  # Variable importance (absolute logistic coefficients)
  full_model <- tryCatch(
    stats::glm(
      stats::as.formula(paste(".label ~", paste(predictors, collapse = " + "))),
      data = combined, family = stats::binomial()
    ),
    error = function(e) NULL
  )

  importance <- NULL
  if (!is.null(full_model)) {
    coefs <- stats::coef(full_model)[-1]  # drop intercept
    importance <- data.frame(
      variable = names(coefs),
      importance = abs(as.numeric(coefs)),
      stringsAsFactors = FALSE
    )
    importance <- importance[order(-importance$importance), ]
    importance$rank <- seq_len(nrow(importance))
  }

  # Recommendation
  dissimilarity <- (auc - 0.5) * 200  # scale 0-100%
  dissimilarity <- max(0, min(100, dissimilarity))

  recommendation <- if (dissimilarity < 20) {
    "Random CV likely adequate"
  } else if (dissimilarity < 60) {
    "Spatial/blocked CV recommended"
  } else {
    "Spatial CV essential; check Area of Applicability"
  }

  result <- list(
    auc = auc,
    dissimilarity = dissimilarity,
    recommendation = recommendation,
    importance = importance,
    n_train = n_train,
    n_prediction = n_pred
  )
  class(result) <- c("borg_adversarial", "list")
  result
}


#' @noRd
.compute_auc <- function(labels, probs) {
  # Mann-Whitney U statistic
  pos <- probs[labels == 1]
  neg <- probs[labels == 0]
  if (length(pos) == 0 || length(neg) == 0) return(0.5)

  n_pos <- length(pos)
  n_neg <- length(neg)

  # Count concordant pairs
  u <- sum(vapply(pos, function(p) sum(p > neg) + 0.5 * sum(p == neg), numeric(1)))
  u / (n_pos * n_neg)
}


#' @export
print.borg_adversarial <- function(x, ...) {
  cat("BORG Adversarial Validation\n")
  cat(sprintf("  AUC: %.3f\n", x$auc))
  cat(sprintf("  Dissimilarity: %.1f%%\n", x$dissimilarity))
  cat(sprintf("  Recommendation: %s\n", x$recommendation))
  if (!is.null(x$importance)) {
    cat("\n  Top distinguishing variables:\n")
    for (i in seq_len(min(5, nrow(x$importance)))) {
      cat(sprintf("    %s (%.3f)\n", x$importance$variable[i], x$importance$importance[i]))
    }
  }
  invisible(x)
}
