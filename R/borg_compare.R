# ===========================================================================
# borg_compare_cv(): Empirical comparison of random vs blocked CV
# ===========================================================================

#' Compare Random vs Blocked Cross-Validation
#'
#' Runs both random and blocked cross-validation on the same data and model,
#' providing empirical evidence of metric inflation from ignoring data dependencies.
#'
#' @param data A data frame containing predictors and response.
#' @param formula A formula specifying the model (e.g., \code{y ~ .}).
#' @param model_fn A function that fits a model. Should accept \code{formula} and
#'   \code{data} arguments and return a fitted model with a \code{predict} method.
#'   Default uses \code{lm}.
#' @param predict_fn A function to generate predictions. Should accept \code{model}

#'   and \code{newdata} arguments. Default uses \code{predict}.
#' @param metric A character string specifying the metric to compute. One of
#'   \code{"rmse"}, \code{"mae"}, \code{"rsq"}, \code{"auc"}, \code{"accuracy"}.
#'   Default: \code{"rmse"} for regression, \code{"accuracy"} for classification.
#' @param diagnosis A \code{BorgDiagnosis} object. If NULL, will be computed
#'   automatically using the provided structure hints.
#' @param coords Character vector of length 2 specifying coordinate column names.
#' @param time Character string specifying the time column name.
#' @param groups Character string specifying the grouping column name.
#' @param target Character string specifying the response variable name. If NULL,
#'   extracted from formula.
#' @param v Integer. Number of CV folds. Default: 5.
#' @param repeats Integer. Number of times to repeat CV. Default: 10 for stable
#'   estimates.
#' @param seed Integer. Random seed for reproducibility.
#' @param verbose Logical. Print progress messages. Default: TRUE.
#'
#' @return A \code{borg_comparison} object (S3 class) containing:
#' \describe{
#'   \item{random_cv}{Data frame of metrics from random CV (one row per repeat)}
#'   \item{blocked_cv}{Data frame of metrics from blocked CV (one row per repeat)}
#'   \item{summary}{Summary statistics comparing the two approaches}
#'   \item{inflation}{Estimated metric inflation from using random CV}
#'   \item{diagnosis}{The BorgDiagnosis object used}
#'   \item{p_value}{P-value from paired t-test comparing approaches}
#' }
#'
#' @details
#' This function provides the "smoking gun" evidence for reviewers. It runs
#' cross-validation twice on the same data:
#'
#' 1. **Random CV**: Standard k-fold CV ignoring data structure
#' 2. **Blocked CV**: Structure-aware CV based on BORG diagnosis
#'
#' The difference in metrics demonstrates empirically how much random CV
#' inflates performance estimates when data dependencies exist.
#'
#' For stable estimates, the comparison is repeated multiple times (default: 10)
#' and a paired t-test assesses whether the difference is statistically significant.
#'
#' @examples
#' # Spatial data example
#' set.seed(42)
#' n <- 200
#' spatial_data <- data.frame(
#'   x = runif(n, 0, 100),
#'   y = runif(n, 0, 100)
#' )
#' # Create spatially autocorrelated response
#' spatial_data$response <- spatial_data$x * 0.5 + rnorm(n, sd = 5)
#'
#' # Compare CV approaches
#' comparison <- borg_compare_cv(
#'   spatial_data,
#'   formula = response ~ x + y,
#'   coords = c("x", "y"),
#'   repeats = 5  # Use more repeats in practice
#' )
#'
#' print(comparison)
#' plot(comparison)
#'
#' @seealso
#' \code{\link{borg_diagnose}} for dependency detection,
#' \code{\link{borg_cv}} for generating blocked CV folds.
#'
#' @export
borg_compare_cv <- function(data,
                            formula,
                            model_fn = NULL,
                            predict_fn = NULL,
                            metric = NULL,
                            diagnosis = NULL,
                            coords = NULL,
                            time = NULL,
                            groups = NULL,
                            target = NULL,
                            v = 5,
                            repeats = 10,
                            seed = NULL,
                            verbose = TRUE) {


  # ===========================================================================
  # Input validation
  # ===========================================================================

  if (!is.data.frame(data)) {
    stop("'data' must be a data frame")
  }

  if (!inherits(formula, "formula")) {
    stop("'formula' must be a formula (e.g., y ~ .)")
  }

  # Extract target from formula if not provided
  if (is.null(target)) {
    target <- as.character(formula[[2]])
  }

  if (!target %in% names(data)) {
    stop(sprintf("Target variable '%s' not found in data", target))
  }

  # Check structure hints
  if (is.null(coords) && is.null(time) && is.null(groups)) {
    stop("At least one of 'coords', 'time', or 'groups' must be specified")
  }

  # Default model function
  if (is.null(model_fn)) {
    model_fn <- function(formula, data) stats::lm(formula, data = data)
  }


  # Default predict function
  if (is.null(predict_fn)) {
    predict_fn <- function(model, newdata) stats::predict(model, newdata = newdata)
  }

  # Determine metric based on response type
  response <- data[[target]]
  is_classification <- is.factor(response) || is.character(response) ||
    (is.numeric(response) && length(unique(response)) == 2)

  if (is.null(metric)) {
    metric <- if (is_classification) "accuracy" else "rmse"
  }

  metric <- match.arg(metric, c("rmse", "mae", "rsq", "auc", "accuracy"))

  # Set seed if provided
  if (!is.null(seed)) {
    set.seed(seed)
  }

  # ===========================================================================
  # Get or compute diagnosis
  # ===========================================================================

  if (is.null(diagnosis)) {
    if (verbose) message("Computing dependency diagnosis...")
    diagnosis <- borg_diagnose(
      data = data,
      coords = coords,
      time = time,
      groups = groups,
      target = target,
      verbose = FALSE
    )
  }

  if (!inherits(diagnosis, "BorgDiagnosis"))

    stop("'diagnosis' must be a BorgDiagnosis object")

  if (verbose) {
    message(sprintf("Dependency: %s (severity: %s)",
                    diagnosis@dependency_type, diagnosis@severity))
    message(sprintf("Blocked CV strategy: %s", diagnosis@recommended_cv))
  }

  # ===========================================================================
  # Run repeated CV comparisons
  # ===========================================================================

  random_results <- vector("list", repeats)
  blocked_results <- vector("list", repeats)

  for (rep in seq_len(repeats)) {
    if (verbose && rep %% 5 == 1) {
      message(sprintf("Repeat %d/%d...", rep, repeats))
    }

    # Generate random CV folds
    random_folds <- generate_random_folds(data, v = v)

    # Generate blocked CV folds
    blocked_folds <- borg_cv(
      data = data,
      diagnosis = diagnosis,
      v = v,
      coords = coords,
      time = time,
      groups = groups,
      target = target,
      output = "list",
      allow_random = FALSE,
      verbose = FALSE
    )$folds

    # Evaluate random CV
    random_metrics <- evaluate_cv_folds(
      data = data,
      formula = formula,
      folds = random_folds,
      model_fn = model_fn,
      predict_fn = predict_fn,
      metric = metric,
      target = target
    )
    random_results[[rep]] <- random_metrics

    # Evaluate blocked CV
    blocked_metrics <- evaluate_cv_folds(
      data = data,
      formula = formula,
      folds = blocked_folds,
      model_fn = model_fn,
      predict_fn = predict_fn,
      metric = metric,
      target = target
    )
    blocked_results[[rep]] <- blocked_metrics
  }

  # ===========================================================================
  # Aggregate results
  # ===========================================================================

  random_df <- data.frame(
    repeat_id = seq_len(repeats),
    cv_type = "random",
    metric_name = metric,
    metric_value = sapply(random_results, function(x) x$mean),
    metric_sd = sapply(random_results, function(x) x$sd)
  )

  blocked_df <- data.frame(
    repeat_id = seq_len(repeats),
    cv_type = "blocked",
    metric_name = metric,
    metric_value = sapply(blocked_results, function(x) x$mean),
    metric_sd = sapply(blocked_results, function(x) x$sd)
  )

  # Compute summary statistics
  random_mean <- mean(random_df$metric_value)
  blocked_mean <- mean(blocked_df$metric_value)
  random_sd <- sd(random_df$metric_value)
  blocked_sd <- sd(blocked_df$metric_value)

  # Paired t-test
  t_test <- stats::t.test(
    random_df$metric_value,
    blocked_df$metric_value,
    paired = TRUE
  )

  # Compute inflation
  # For error metrics (rmse, mae): inflation = (blocked - random) / blocked

  # For performance metrics (rsq, auc, accuracy): inflation = (random - blocked) / blocked
  if (metric %in% c("rmse", "mae")) {
    # Error metrics: random CV shows lower (better) error than reality
    inflation <- (blocked_mean - random_mean) / blocked_mean
    inflation_direction <- "deflated"
    better_is_lower <- TRUE
  } else {
    # Performance metrics: random CV shows higher (better) performance than reality
    inflation <- (random_mean - blocked_mean) / blocked_mean
    inflation_direction <- "inflated"
    better_is_lower <- FALSE
  }

  summary_df <- data.frame(
    cv_type = c("random", "blocked"),
    mean = c(random_mean, blocked_mean),
    sd = c(random_sd, blocked_sd),
    n = c(repeats, repeats)
  )

  # ===========================================================================
  # Build result object
  # ===========================================================================

  result <- list(
    random_cv = random_df,
    blocked_cv = blocked_df,
    summary = summary_df,
    inflation = list(
      estimate = inflation,
      direction = inflation_direction,
      metric = metric,
      random_mean = random_mean,
      blocked_mean = blocked_mean,
      difference = abs(random_mean - blocked_mean)
    ),
    diagnosis = diagnosis,
    p_value = t_test$p.value,
    t_statistic = t_test$statistic,
    conf_int = t_test$conf.int,
    formula = formula,
    v = v,
    repeats = repeats
  )

  class(result) <- c("borg_comparison", "list")

  if (verbose) {
    message("\n", format_comparison_summary(result))
  }

  result
}


# ===========================================================================
# Internal: Generate random CV folds
# ===========================================================================

generate_random_folds <- function(data, v = 5) {
  n <- nrow(data)
  fold_ids <- sample(rep(seq_len(v), length.out = n))

  folds <- lapply(seq_len(v), function(fold) {
    list(
      train = which(fold_ids != fold),
      test = which(fold_ids == fold)
    )
  })

  folds
}


# ===========================================================================
# Internal: Evaluate CV folds
# ===========================================================================

evaluate_cv_folds <- function(data, formula, folds, model_fn, predict_fn,
                              metric, target) {

  fold_metrics <- vapply(folds, function(fold) {
    train_data <- data[fold$train, , drop = FALSE]
    test_data <- data[fold$test, , drop = FALSE]

    # Fit model
    model <- tryCatch(
      model_fn(formula, train_data),
      error = function(e) NULL
    )

    if (is.null(model)) return(NA_real_)

    # Predict
    preds <- tryCatch(
      predict_fn(model, test_data),
      error = function(e) NULL
    )

    if (is.null(preds)) return(NA_real_)

    # Get actual values
    actual <- test_data[[target]]

    # Compute metric
    compute_metric(actual, preds, metric)

  }, numeric(1))

  list(
    mean = mean(fold_metrics, na.rm = TRUE),
    sd = sd(fold_metrics, na.rm = TRUE),
    values = fold_metrics
  )
}


# ===========================================================================
# Internal: Compute evaluation metrics
# ===========================================================================

compute_metric <- function(actual, predicted, metric) {
  switch(metric,
    rmse = sqrt(mean((actual - predicted)^2, na.rm = TRUE)),
    mae = mean(abs(actual - predicted), na.rm = TRUE),
    rsq = {
      ss_res <- sum((actual - predicted)^2, na.rm = TRUE)
      ss_tot <- sum((actual - mean(actual, na.rm = TRUE))^2, na.rm = TRUE)
      1 - ss_res / ss_tot
    },
    accuracy = {
      if (is.numeric(predicted)) {
        # Binary classification with numeric predictions
        pred_class <- ifelse(predicted > 0.5, 1, 0)
        mean(pred_class == actual, na.rm = TRUE)
      } else {
        mean(predicted == actual, na.rm = TRUE)
      }
    },
    auc = {
      # Simple AUC calculation for binary classification
      if (!requireNamespace("stats", quietly = TRUE)) {
        return(NA_real_)
      }
      # Wilcoxon-Mann-Whitney AUC
      if (length(unique(actual)) != 2) return(NA_real_)
      pos <- predicted[actual == max(actual)]
      neg <- predicted[actual == min(actual)]
      if (length(pos) == 0 || length(neg) == 0) return(NA_real_)
      mean(outer(pos, neg, ">")) + 0.5 * mean(outer(pos, neg, "=="))
    },
    NA_real_
  )
}


# ===========================================================================
# Internal: Format comparison summary
# ===========================================================================

format_comparison_summary <- function(x) {
  inf <- x$inflation
  sig <- if (x$p_value < 0.001) "***" else if (x$p_value < 0.01) "**" else
         if (x$p_value < 0.05) "*" else ""

  paste0(
    "CV Comparison Results\n",
    "=====================\n\n",
    sprintf("Metric: %s\n", inf$metric),
    sprintf("Random CV:  %.4f (SD: %.4f)\n", inf$random_mean, x$summary$sd[1]),
    sprintf("Blocked CV: %.4f (SD: %.4f)\n", inf$blocked_mean, x$summary$sd[2]),
    sprintf("\nInflation: %.1f%% %s%s\n",
            abs(inf$estimate) * 100, inf$direction, sig),
    sprintf("p-value: %.4g (paired t-test, n=%d repeats)\n", x$p_value, x$repeats),
    if (x$p_value < 0.05) {
      sprintf("\nRandom CV significantly %ss %s estimates.\n",
              if (inf$direction == "inflated") "inflate" else "deflate",
              inf$metric)
    } else {
      "\nDifference not statistically significant.\n"
    }
  )
}


# ===========================================================================
# S3 methods
# ===========================================================================

#' @export
print.borg_comparison <- function(x, ...) {
  cat(format_comparison_summary(x))
  invisible(x)
}


#' @export
summary.borg_comparison <- function(object, ...) {
  cat("BORG CV Comparison Summary\n")
  cat("==========================\n\n")

  cat("Dependency Diagnosis:\n")
  cat(sprintf("  Type: %s\n", object$diagnosis@dependency_type))
  cat(sprintf("  Severity: %s\n", object$diagnosis@severity))
  cat(sprintf("  Blocked strategy: %s\n\n", object$diagnosis@recommended_cv))

  cat("Cross-Validation Setup:\n")
  cat(sprintf("  Folds: %d\n", object$v))
  cat(sprintf("  Repeats: %d\n", object$repeats))
  cat(sprintf("  Metric: %s\n\n", object$inflation$metric))

  cat("Results:\n")
  print(object$summary, row.names = FALSE)

  cat(sprintf("\nInflation estimate: %.1f%% %s\n",
              abs(object$inflation$estimate) * 100,
              object$inflation$direction))
  cat(sprintf("Statistical test: t = %.2f, p = %.4g\n",
              object$t_statistic, object$p_value))

  invisible(object)
}


#' Plot CV Comparison Results
#'
#' Creates a visualization comparing random vs blocked CV performance.
#'
#' @param x A \code{borg_comparison} object from \code{borg_compare_cv}.
#' @param type Character. Plot type: \code{"boxplot"} (default), \code{"density"},
#'   or \code{"paired"}.
#' @param ... Additional arguments passed to plotting functions.
#'
#' @export
plot.borg_comparison <- function(x, type = c("boxplot", "density", "paired"), ...) {
  type <- match.arg(type)

  # Combine data
  combined <- rbind(x$random_cv, x$blocked_cv)

  # Set up plot
  old_par <- graphics::par(no.readonly = TRUE)
  on.exit(graphics::par(old_par))

  if (type == "boxplot") {
    graphics::par(mar = c(5, 4, 4, 2) + 0.1)

    graphics::boxplot(
      metric_value ~ cv_type,
      data = combined,
      col = c("#E8B4B8", "#A8D5BA"),
      names = c("Blocked CV", "Random CV"),
      ylab = toupper(x$inflation$metric),
      main = "Random vs Blocked Cross-Validation",
      ...
    )

    # Add significance indicator
    if (x$p_value < 0.05) {
      y_max <- max(combined$metric_value) * 1.05
      graphics::segments(1, y_max, 2, y_max)
      sig <- if (x$p_value < 0.001) "***" else if (x$p_value < 0.01) "**" else "*"
      graphics::text(1.5, y_max * 1.02, sig, cex = 1.5)
    }

    # Add inflation annotation
    graphics::mtext(
      sprintf("Inflation: %.1f%%", abs(x$inflation$estimate) * 100),
      side = 1, line = 3, cex = 0.9
    )

  } else if (type == "density") {
    graphics::par(mar = c(5, 4, 4, 2) + 0.1)

    d_random <- stats::density(x$random_cv$metric_value)
    d_blocked <- stats::density(x$blocked_cv$metric_value)

    xlim <- range(c(d_random$x, d_blocked$x))
    ylim <- range(c(d_random$y, d_blocked$y))

    graphics::plot(
      d_random,
      col = "#E74C3C",
      lwd = 2,
      xlim = xlim,
      ylim = ylim,
      main = "Distribution of CV Estimates",
      xlab = toupper(x$inflation$metric),
      ...
    )
    graphics::lines(d_blocked, col = "#27AE60", lwd = 2)
    graphics::legend(
      "topright",
      legend = c("Random CV", "Blocked CV"),
      col = c("#E74C3C", "#27AE60"),
      lwd = 2
    )

  } else if (type == "paired") {
    graphics::par(mar = c(5, 4, 4, 2) + 0.1)

    graphics::plot(
      x$random_cv$metric_value,
      x$blocked_cv$metric_value,
      pch = 19,
      col = "#3498DB",
      xlab = sprintf("Random CV %s", toupper(x$inflation$metric)),
      ylab = sprintf("Blocked CV %s", toupper(x$inflation$metric)),
      main = "Paired Comparison",
      ...
    )
    graphics::abline(0, 1, lty = 2, col = "gray50")
    graphics::legend(
      "topleft",
      legend = sprintf("n = %d repeats", x$repeats),
      bty = "n"
    )
  }

  invisible(x)
}
