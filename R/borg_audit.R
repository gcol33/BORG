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


#' Audit Feature Importance Calculations
#'
#' Detects when feature importance (SHAP, permutation importance, etc.) is
#' computed using test data, which can lead to biased feature selection and
#' data leakage.
#'
#' @param importance A vector, matrix, or data frame of importance values.
#' @param data The data used to compute importance.
#' @param train_idx Integer vector of training indices.
#' @param test_idx Integer vector of test indices.
#' @param method Character indicating the importance method. One of
#'   "shap", "permutation", "gain", "impurity", or "auto" (default).
#' @param model Optional fitted model object for additional validation.
#'
#' @return A BorgRisk object with audit results.
#'
#' @details
#' Feature importance computed on test data is a form of data leakage because:
#' \itemize{
#'   \item SHAP values computed on test data reveal test set structure
#'   \item Permutation importance on test data uses test labels
#'   \item Feature selection based on test importance leads to overfit models
#' }
#'
#' This function checks if the data used for importance calculation includes
#' test indices and flags potential violations.
#'
#' @examples
#' set.seed(42)
#' data <- data.frame(y = rnorm(100), x1 = rnorm(100), x2 = rnorm(100))
#' train_idx <- 1:70
#' test_idx <- 71:100
#'
#' # Simulate importance values
#' importance <- c(x1 = 0.6, x2 = 0.4)
#'
#' # Good: importance computed on training data
#' result <- audit_importance(importance, data[train_idx, ], train_idx, test_idx)
#'
#' # Bad: importance computed on full data (includes test)
#' result_bad <- audit_importance(importance, data, train_idx, test_idx)
#'
#' @export
audit_importance <- function(importance, data, train_idx, test_idx,
                              method = "auto", model = NULL) {

  risks <- list()

  # ===========================================================================
  # Input validation
  # ===========================================================================

  if (is.null(importance)) {
    stop("'importance' must be provided")
  }

  if (is.null(data)) {
    stop("'data' must be provided to check for test data usage")
  }

  n_data <- if (is.data.frame(data) || is.matrix(data)) nrow(data) else length(data)
  n_train <- length(train_idx)
  n_test <- length(test_idx)
  n_total <- max(c(train_idx, test_idx))

  # ===========================================================================
  # Check 1: Data size indicates test data was used
  # ===========================================================================

  # If data size matches full dataset (train + test), likely includes test
  if (n_data == n_total || n_data == (n_train + n_test)) {
    risks <- c(risks, list(list(
      type = "importance_on_full_data",
      severity = "hard_violation",
      description = sprintf(
        "Feature importance computed on %d observations (full dataset). Should use only training data (%d observations).",
        n_data, n_train
      ),
      affected_indices = test_idx,
      source_object = "importance"
    )))
  }

  # If data size is larger than train but not full, something is off
  if (n_data > n_train && n_data < n_total) {
    risks <- c(risks, list(list(
      type = "importance_data_size_mismatch",
      severity = "soft_inflation",
      description = sprintf(
        "Feature importance data has %d observations. Expected %d (train) or %d (full). Verify correct data was used.",
        n_data, n_train, n_total
      ),
      affected_indices = integer(0),
      source_object = "importance"
    )))
  }

  # ===========================================================================
  # Check 2: Method-specific checks
  # ===========================================================================

  method <- tolower(method)

  if (method %in% c("shap", "auto")) {
    # SHAP values should be computed on training data only for model explanation
    # If computing on test data, it's using test information for feature selection
    if (n_data == n_test) {
      risks <- c(risks, list(list(
        type = "shap_on_test_data",
        severity = "hard_violation",
        description = sprintf(
          "SHAP values computed on %d observations (matches test set size). SHAP should be computed on training data for feature selection.",
          n_data
        ),
        affected_indices = test_idx,
        source_object = "importance"
      )))
    }
  }

  if (method %in% c("permutation", "auto")) {
    # Permutation importance requires labels - using test labels is leakage
    if (n_data == n_test) {
      risks <- c(risks, list(list(
        type = "permutation_on_test_data",
        severity = "hard_violation",
        description = "Permutation importance computed on test data uses test labels, causing information leakage.",
        affected_indices = test_idx,
        source_object = "importance"
      )))
    }
  }

  # ===========================================================================
  # Check 3: Model-based checks
  # ===========================================================================

  if (!is.null(model)) {
    # Validate model was trained correctly
    model_risk <- borg_inspect(model, train_idx, test_idx)
    if (model_risk@n_hard > 0) {
      risks <- c(risks, list(list(
        type = "importance_from_leaked_model",
        severity = "hard_violation",
        description = sprintf(
          "Model used for feature importance has %d hard violations. Importance values are unreliable.",
          model_risk@n_hard
        ),
        affected_indices = test_idx,
        source_object = class(model)[1]
      )))
    }
  }

  # ===========================================================================
  # Check 4: Importance value sanity checks
  # ===========================================================================

  # Extract numeric importance values
  imp_vals <- if (is.data.frame(importance)) {
    unlist(importance[sapply(importance, is.numeric)])
  } else if (is.matrix(importance)) {
    as.numeric(importance)
  } else {
    as.numeric(importance)
  }

  imp_vals <- imp_vals[!is.na(imp_vals)]

  if (length(imp_vals) > 0) {
    # Check for suspiciously uniform importance (might indicate random/meaningless)
    if (length(imp_vals) > 2) {
      imp_sd <- sd(imp_vals)
      imp_range <- diff(range(imp_vals))

      if (imp_range > 0 && imp_sd / imp_range < 0.05) {
        risks <- c(risks, list(list(
          type = "uniform_importance",
          severity = "soft_inflation",
          description = "Feature importance values are nearly uniform. May indicate incorrect calculation or uninformative features.",
          affected_indices = integer(0),
          source_object = "importance"
        )))
      }
    }

    # Check for negative importance (valid for some methods, suspicious for others)
    if (any(imp_vals < 0) && method %in% c("gain", "impurity")) {
      risks <- c(risks, list(list(
        type = "negative_importance",
        severity = "soft_inflation",
        description = "Negative importance values detected for gain/impurity method. This is unusual.",
        affected_indices = integer(0),
        source_object = "importance"
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
