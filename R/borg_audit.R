# ===========================================================================
# BORG Audit Functions
# ===========================================================================

#' Audit Predictions for Data Leakage
#'
#' Validates that predictions were generated correctly without data leakage.
#' Checks that predictions correspond to test data only and that the
#' prediction process did not use information from the test set.
#'
#' @param predictions Vector of predictions (numeric or factor).
#' @param train_idx Integer vector of training indices.
#' @param test_idx Integer vector of test indices.
#' @param actual Optional vector of actual values for comparison.
#' @param data Optional data frame containing the original data.
#' @param model Optional fitted model object for additional checks.
#'
#' @return A BorgRisk object with audit results.
#'
#' @examples
#' # Create data and split
#' set.seed(42)
#' data <- data.frame(y = rnorm(100), x = rnorm(100))
#' train_idx <- 1:70
#' test_idx <- 71:100
#'
#' # Fit model and predict
#' model <- lm(y ~ x, data = data[train_idx, ])
#' predictions <- predict(model, newdata = data[test_idx, ])
#'
#' # Audit predictions
#' result <- audit_predictions(predictions, train_idx, test_idx)
#'
#' @export
audit_predictions <- function(predictions, train_idx, test_idx,
                               actual = NULL, data = NULL, model = NULL) {

  risks <- list()

  # ===========================================================================
  # Basic validation
  # ===========================================================================

  if (is.null(predictions) || length(predictions) == 0) {
    stop("'predictions' must be a non-empty vector")
  }

  n_pred <- length(predictions)
  n_test <- length(test_idx)

  # Check 1: Prediction count matches test set
  if (n_pred != n_test) {
    risks <- c(risks, list(list(
      type = "prediction_count_mismatch",
      severity = "hard_violation",
      description = sprintf(
        "Number of predictions (%d) does not match test set size (%d)",
        n_pred, n_test
      ),
      affected_indices = test_idx,
      source_object = "predictions"
    )))
  }

  # ===========================================================================
  # Perfect prediction check (potential target leak)
  # ===========================================================================

  if (!is.null(actual) && length(actual) == n_pred) {
    if (is.numeric(predictions) && is.numeric(actual)) {
      # Check for suspiciously perfect correlation
      if (n_pred >= 3) {
        cor_val <- tryCatch(
          cor(predictions, actual, use = "pairwise.complete.obs"),
          error = function(e) NA
        )

        if (!is.na(cor_val) && abs(cor_val) > 0.999) {
          risks <- c(risks, list(list(
            type = "perfect_prediction",
            severity = "hard_violation",
            description = sprintf(
              "Predictions have near-perfect correlation with actual values (r = %.4f). Likely target leakage.",
              cor_val
            ),
            affected_indices = test_idx,
            source_object = "predictions"
          )))
        }
      }

      # Check for exact matches (suspicious for continuous data)
      exact_matches <- sum(abs(predictions - actual) < 1e-10, na.rm = TRUE)
      if (exact_matches == n_pred && n_pred > 5) {
        risks <- c(risks, list(list(
          type = "exact_predictions",
          severity = "hard_violation",
          description = sprintf(
            "All %d predictions exactly match actual values. Almost certainly target leakage.",
            n_pred
          ),
          affected_indices = test_idx,
          source_object = "predictions"
        )))
      }
    } else if (is.factor(predictions) || is.factor(actual)) {
      # Classification: check for perfect accuracy
      pred_char <- as.character(predictions)
      actual_char <- as.character(actual)

      accuracy <- mean(pred_char == actual_char, na.rm = TRUE)
      if (accuracy > 0.99 && n_pred > 20) {
        risks <- c(risks, list(list(
          type = "suspiciously_high_accuracy",
          severity = "soft_inflation",
          description = sprintf(
            "Classification accuracy is %.1f%% on %d test samples. Verify no target leakage.",
            100 * accuracy, n_pred
          ),
          affected_indices = test_idx,
          source_object = "predictions"
        )))
      }
    }
  }

  # ===========================================================================
  # Model-based checks
  # ===========================================================================

  if (!is.null(model)) {
    # Check if model was trained on correct data
    model_risks <- borg_inspect(model, train_idx, test_idx, data = data)

    if (model_risks@n_hard > 0) {
      risks <- c(risks, list(list(
        type = "model_leakage",
        severity = "hard_violation",
        description = sprintf(
          "Model used for predictions has %d hard violations",
          model_risks@n_hard
        ),
        affected_indices = test_idx,
        source_object = class(model)[1]
      )))
    }
  }

  # ===========================================================================
  # Data-based checks
  # ===========================================================================

  if (!is.null(data)) {
    # Check that we're not predicting on training data
    # This would indicate predictions were made on wrong subset
    n_total <- nrow(data)

    # If predictions match training size instead of test size
    if (n_pred == length(train_idx) && n_pred != n_test) {
      risks <- c(risks, list(list(
        type = "wrong_prediction_set",
        severity = "hard_violation",
        description = sprintf(
          "Prediction count (%d) matches training set size, not test set. Predictions may be on wrong data.",
          n_pred
        ),
        affected_indices = train_idx,
        source_object = "predictions"
      )))
    }

    # If predictions match full data size
    if (n_pred == n_total && n_pred != n_test) {
      risks <- c(risks, list(list(
        type = "full_data_prediction",
        severity = "hard_violation",
        description = sprintf(
          "Prediction count (%d) matches full dataset size. Predictions include training data.",
          n_pred
        ),
        affected_indices = train_idx,
        source_object = "predictions"
      )))
    }
  }

  # ===========================================================================
  # Build result
  # ===========================================================================

  n_hard <- sum(vapply(risks, function(r) r$severity == "hard_violation", logical(1)))
  n_soft <- sum(vapply(risks, function(r) r$severity == "soft_inflation", logical(1)))

  new("BorgRisk",
      risks = risks,
      n_hard = as.integer(n_hard),
      n_soft = as.integer(n_soft),
      is_valid = n_hard == 0L,
      train_indices = train_idx,
      test_indices = test_idx,
      timestamp = Sys.time(),
      call = match.call()
  )
}


#' Generate CV Leakage Report
#'
#' Generates a detailed report of cross-validation leakage issues.
#'
#' @param cv_object A cross-validation object (trainControl, vfold_cv, etc.).
#' @param train_idx Integer vector of training indices.
#' @param test_idx Integer vector of test indices.
#'
#' @return A list with detailed CV leakage information.
#'
#' @examples
#' # Using caret trainControl
#' if (requireNamespace("caret", quietly = TRUE)) {
#'   folds <- list(Fold1 = 1:10, Fold2 = 11:20, Fold3 = 21:25)
#'   ctrl <- caret::trainControl(method = "cv", index = folds)
#'   report <- cv_leakage_report(ctrl, train_idx = 1:25, test_idx = 26:32)
#'   print(report)
#' }
#'
#' @export
cv_leakage_report <- function(cv_object, train_idx, test_idx) {

  report <- list(
    summary = list(
      n_train = length(train_idx),
      n_test = length(test_idx),
      overlap = intersect(train_idx, test_idx)
    ),
    folds = list(),
    issues = list()
  )

  # Detect CV type and extract folds
  if (inherits(cv_object, "vfold_cv") || inherits(cv_object, "rset")) {
    # rsample object
    if ("splits" %in% names(cv_object)) {
      for (i in seq_len(nrow(cv_object))) {
        split <- cv_object$splits[[i]]
        fold_id <- if ("id" %in% names(cv_object)) cv_object$id[i] else paste0("Fold", i)

        # Get analysis (training) indices for this fold
        analysis_idx <- NULL
        if (!is.null(split$in_id)) {
          analysis_idx <- split$in_id
        }

        if (!is.null(analysis_idx)) {
          # Check for test indices in analysis set
          leaked_test <- intersect(analysis_idx, test_idx)
          leaked_train <- setdiff(analysis_idx, train_idx)

          fold_info <- list(
            fold_id = fold_id,
            n_analysis = length(analysis_idx),
            n_assessment = if (!is.null(split$out_id)) length(split$out_id) else NA,
            leaked_test_indices = leaked_test,
            n_leaked_test = length(leaked_test),
            non_train_indices = leaked_train,
            n_non_train = length(leaked_train)
          )

          report$folds[[fold_id]] <- fold_info

          if (length(leaked_test) > 0) {
            report$issues <- c(report$issues, list(list(
              fold = fold_id,
              type = "test_in_analysis",
              severity = "hard_violation",
              description = sprintf("%s: %d test indices in analysis set", fold_id, length(leaked_test)),
              indices = leaked_test
            )))
          }

          if (length(leaked_train) > 0) {
            report$issues <- c(report$issues, list(list(
              fold = fold_id,
              type = "non_train_in_analysis",
              severity = "hard_violation",
              description = sprintf("%s: %d non-training indices in analysis set", fold_id, length(leaked_train)),
              indices = leaked_train
            )))
          }
        }
      }
    }
  } else if (.is_trainControl(cv_object)) {
    # caret trainControl
    if (!is.null(cv_object$index)) {
      for (i in seq_along(cv_object$index)) {
        fold_idx <- cv_object$index[[i]]
        fold_id <- names(cv_object$index)[i]
        if (is.null(fold_id)) fold_id <- paste0("Fold", i)

        leaked_test <- intersect(fold_idx, test_idx)

        fold_info <- list(
          fold_id = fold_id,
          n_in_fold = length(fold_idx),
          leaked_test_indices = leaked_test,
          n_leaked_test = length(leaked_test)
        )

        report$folds[[fold_id]] <- fold_info

        if (length(leaked_test) > 0) {
          report$issues <- c(report$issues, list(list(
            fold = fold_id,
            type = "test_in_fold",
            severity = "hard_violation",
            description = sprintf("%s: %d test indices in CV fold", fold_id, length(leaked_test)),
            indices = leaked_test
          )))
        }
      }
    }
  }

  # Summary statistics
  report$summary$n_folds <- length(report$folds)
  report$summary$n_issues <- length(report$issues)
  report$summary$has_leakage <- report$summary$n_issues > 0

  class(report) <- c("borg_cv_report", "list")
  report
}


#' Print CV Leakage Report
#' @param x A borg_cv_report object.
#' @param ... Additional arguments (ignored).
#' @export
print.borg_cv_report <- function(x, ...) {
  cat("BORG CV Leakage Report\n")
  cat("======================\n\n")

  cat(sprintf("Training set: %d indices\n", x$summary$n_train))
  cat(sprintf("Test set: %d indices\n", x$summary$n_test))

  if (length(x$summary$overlap) > 0) {
    cat(sprintf("Index overlap: %d (train/test share indices!)\n",
                length(x$summary$overlap)))
  }

  cat(sprintf("CV folds analyzed: %d\n", x$summary$n_folds))
  cat("\n")

  if (x$summary$has_leakage) {
    cat("--- ISSUES DETECTED ---\n\n")
    for (issue in x$issues) {
      cat(sprintf("[%s] %s\n", issue$severity, issue$description))
      if (length(issue$indices) <= 10) {
        cat(sprintf("  Affected indices: %s\n", paste(issue$indices, collapse = ", ")))
      } else {
        cat(sprintf("  Affected indices: %s, ... (%d more)\n",
                    paste(head(issue$indices, 5), collapse = ", "),
                    length(issue$indices) - 5))
      }
      cat("\n")
    }
  } else {
    cat("No CV leakage detected.\n")
  }

  invisible(x)
}
