#' Validate Complete Evaluation Workflow
#'
#' `borg_validate()` performs post-hoc validation of an entire evaluation
#' workflow, checking all components for information leakage.
#'
#' @param workflow A list containing the evaluation workflow components:
#'   \describe{
#'     \item{data}{The full dataset}
#'     \item{train_idx}{Integer vector of training indices}
#'     \item{test_idx}{Integer vector of test indices}
#'     \item{preprocess}{Optional preprocessing object(s)}
#'     \item{model}{The fitted model object}
#'     \item{predictions}{Model predictions on test data}
#'     \item{metrics}{Computed evaluation metrics}
#'   }
#' @param strict Logical. If TRUE, any hard violation causes an error.
#'   Default: FALSE (returns report only).
#'
#' @return A \code{\link{BorgRisk}} object containing a comprehensive
#'   assessment of the workflow.
#'
#' @details
#' `borg_validate()` inspects each component of an evaluation workflow:
#'
#' \enumerate{
#'   \item \strong{Split validation}: Checks train/test index isolation
#'   \item \strong{Preprocessing audit}: Traces preprocessing parameters to
#'     verify train-only origin
#'   \item \strong{Feature audit}: Checks for target leakage and proxy features
#'   \item \strong{Model audit}: Validates that model used only training data
#'   \item \strong{Threshold audit}: Checks if any thresholds were optimized
#'     on test data
#' }
#'
#' @seealso
#' \code{\link{borg_guard}} for proactive enforcement,
#' \code{\link{borg_inspect}} for single-object inspection.
#'
#' @examples
#' \dontrun{
#' # Validate an existing workflow
#' result <- borg_validate(list(
#'   data = my_data,
#'   train_idx = train_idx,
#'   test_idx = test_idx,
#'   preprocess = my_recipe,
#'   model = my_model,
#'   predictions = preds,
#'   metrics = list(rmse = 0.5, mae = 0.3)
#' ))
#'
#' # Check validity
#' if (!result@is_valid) {
#'   print(result)  # Shows detailed risk report
#' }
#' }
#'
#' @export
borg_validate <- function(workflow, strict = FALSE) {

 # ===========================================================================
 # Input validation
 # ===========================================================================

 if (!is.list(workflow)) {
   stop("'workflow' must be a list")
 }

 required_fields <- c("data", "train_idx", "test_idx")
 missing <- setdiff(required_fields, names(workflow))
 if (length(missing) > 0) {
   stop(sprintf("'workflow' missing required fields: %s",
                paste(missing, collapse = ", ")))
 }

 data <- workflow$data
 train_idx <- as.integer(workflow$train_idx)
 test_idx <- as.integer(workflow$test_idx)

 # ===========================================================================
 # Collect all risks
 # ===========================================================================

 all_risks <- list()

 # 1. Validate train/test split
 overlap <- intersect(train_idx, test_idx)
 if (length(overlap) > 0) {
   all_risks <- c(all_risks, list(list(
     type = "index_overlap",
     severity = "hard_violation",
     description = sprintf(
       "Train and test indices overlap (%d shared indices)",
       length(overlap)
     ),
     affected_indices = overlap,
     source_object = "workflow$train_idx/test_idx"
   )))
 }

 # 2. Inspect data for duplicate rows
 data_risks <- .inspect_data_frame(data, train_idx, test_idx)
 all_risks <- c(all_risks, data_risks)

 # 3. Inspect preprocessing (if provided)
 if (!is.null(workflow$preprocess)) {
   preprocess <- workflow$preprocess

   # Handle single object or list of objects
   # A single preprocessing object (like preProcess) has a specific class,
   # while a list of such objects just has class "list"
   is_single_object <- inherits(preprocess, "preProcess") ||
                       inherits(preprocess, "recipe") ||
                       inherits(preprocess, "prcomp") ||
                       (length(class(preprocess)) == 1 && class(preprocess) != "list")

   if (is_single_object) {
     preprocess <- list(preprocess)
   }

   for (i in seq_along(preprocess)) {
     pp <- preprocess[[i]]
     pp_risks <- borg_inspect(pp, train_idx, test_idx, data)@risks
     all_risks <- c(all_risks, pp_risks)
   }
 }

 # 4. Inspect model (if provided)
 if (!is.null(workflow$model)) {
   model_risks <- borg_inspect(workflow$model, train_idx, test_idx)@risks
   all_risks <- c(all_risks, model_risks)
 }

 # 5. Check for target leakage in features
 if (is.data.frame(data)) {
   target_leak_risks <- .check_target_leakage(data, train_idx, test_idx, workflow)
   all_risks <- c(all_risks, target_leak_risks)
 }

 # 6. Check for feature engineering leakage
 if (is.data.frame(data)) {
   fe_risks <- .check_feature_engineering(data, train_idx, test_idx, workflow)
   all_risks <- c(all_risks, fe_risks)
 }

 # 7. Check threshold selection issues
 if (!is.null(workflow$thresholds)) {
   thresh_risks <- .check_threshold_selection(workflow)
   all_risks <- c(all_risks, thresh_risks)
 }

 # 8. Check spatial autocorrelation issues
 if (!is.null(workflow$spatial_cols) || !is.null(workflow$coords)) {
   spatial_risks <- .check_spatial_autocorrelation(data, train_idx, test_idx, workflow)
   all_risks <- c(all_risks, spatial_risks)
 }

 # 9. Check HPO validation issues
 if (!is.null(workflow$hpo)) {
   hpo_risks <- .check_hpo_validation(workflow)
   all_risks <- c(all_risks, hpo_risks)
 }

 # ===========================================================================
 # Build result
 # ===========================================================================

 n_hard <- sum(vapply(all_risks, function(r) r$severity == "hard_violation", logical(1)))
 n_soft <- sum(vapply(all_risks, function(r) r$severity == "soft_inflation", logical(1)))

 result <- new("BorgRisk",
   risks = all_risks,
   n_hard = as.integer(n_hard),
   n_soft = as.integer(n_soft),
   is_valid = n_hard == 0L,
   train_indices = train_idx,
   test_indices = test_idx,
   timestamp = Sys.time(),
   call = match.call()
 )

 # Strict mode: error on hard violations
 if (strict && !result@is_valid) {
   print(result)
   stop("BORG VALIDATION FAILED: Hard violations detected (see report above)")
 }

 result
}


#' Check for target leakage in features
#' @noRd
.check_target_leakage <- function(data, train_idx, test_idx, workflow) {
  risks <- list()

  # If no target column specified, skip
  if (is.null(workflow$target_col)) {
    return(risks)
  }

  target_col <- workflow$target_col
  if (!target_col %in% names(data)) {
    return(risks)
  }

  target <- data[[target_col]]
  features <- data[, setdiff(names(data), target_col), drop = FALSE]

  # ===========================================================================
  # 1. Direct correlation check (existing)
  # ===========================================================================
  for (col in names(features)) {
    feature <- features[[col]]

    # Only check numeric features
    if (!is.numeric(feature) || !is.numeric(target)) {
      next
    }

    # Compute correlation on training data only
    train_cor <- tryCatch(
      cor(feature[train_idx], target[train_idx], use = "complete.obs"),
      error = function(e) NA
    )

    if (is.na(train_cor)) next

    # Flag extremely high correlations (likely target leak)
    if (abs(train_cor) > 0.99) {
      risks <- c(risks, list(list(
        type = "target_leakage_direct",
        severity = "hard_violation",
        description = sprintf(
          "Feature '%s' has correlation %.4f with target (likely derived from outcome)",
          col, train_cor
        ),
        affected_indices = integer(0),
        source_object = col
      )))
    } else if (abs(train_cor) > 0.95) {
      risks <- c(risks, list(list(
        type = "target_leakage_proxy",
        severity = "soft_inflation",
        description = sprintf(
          "Feature '%s' has suspiciously high correlation %.4f with target (review for proxy leakage)",
          col, train_cor
        ),
        affected_indices = integer(0),
        source_object = col
      )))
    }
  }

  # ===========================================================================
  # 2. Suspicious naming patterns (common leakage indicators)
  # ===========================================================================
  leaky_patterns <- c(
    "^outcome", "^target", "^label", "^y_",
    "_outcome$", "_target$", "_label$",
    "^future_", "_future$", "^next_", "_next$",
    "^result", "_result$",
    "_encoded$", "_mean_target$", "_target_mean$"
  )

  for (col in names(features)) {
    for (pattern in leaky_patterns) {
      if (grepl(pattern, col, ignore.case = TRUE)) {
        risks <- c(risks, list(list(
          type = "target_leakage_naming",
          severity = "soft_inflation",
          description = sprintf(
            "Feature '%s' has suspicious naming pattern suggesting potential target leakage",
            col
          ),
          affected_indices = integer(0),
          source_object = col
        )))
        break
      }
    }
  }

  # ===========================================================================
  # 3. Perfect categorical prediction (target encoding leak)
  # ===========================================================================
  for (col in names(features)) {
    feature <- features[[col]]

    # Check categorical/factor features
    if (is.factor(feature) || is.character(feature)) {
      train_feature <- as.character(feature[train_idx])
      train_target <- target[train_idx]

      # Check if categories perfectly predict target
      if (is.factor(train_target) || is.character(train_target)) {
        # For categorical target: check if each feature level maps to one target level
        cross_tab <- tryCatch(
          table(train_feature, train_target),
          error = function(e) NULL
        )

        if (!is.null(cross_tab) && nrow(cross_tab) > 1 && ncol(cross_tab) > 1) {
          # Check if each row has only one non-zero entry (perfect separation)
          rows_with_single_class <- sum(apply(cross_tab, 1, function(r) sum(r > 0) == 1))
          if (rows_with_single_class == nrow(cross_tab)) {
            risks <- c(risks, list(list(
              type = "target_leakage_categorical",
              severity = "hard_violation",
              description = sprintf(
                "Categorical feature '%s' perfectly separates target classes (likely target encoding or derived feature)",
                col
              ),
              affected_indices = integer(0),
              source_object = col
            )))
          }
        }
      } else if (is.numeric(train_target)) {
        # For numeric target: check if variance within categories is near zero
        within_var <- tryCatch({
          category_vars <- tapply(train_target, train_feature, var, na.rm = TRUE)
          mean(category_vars, na.rm = TRUE)
        }, error = function(e) NA)

        total_var <- var(train_target, na.rm = TRUE)

        if (!is.na(within_var) && !is.na(total_var) && total_var > 0) {
          # R-squared equivalent
          explained_ratio <- 1 - (within_var / total_var)
          if (explained_ratio > 0.99) {
            risks <- c(risks, list(list(
              type = "target_leakage_categorical",
              severity = "hard_violation",
              description = sprintf(
                "Categorical feature '%s' explains %.1f%% of target variance (likely derived from outcome)",
                col, explained_ratio * 100
              ),
              affected_indices = integer(0),
              source_object = col
            )))
          }
        }
      }
    }
  }

  # ===========================================================================
  # 4. Temporal ordering check (if timestamp column provided)
  # ===========================================================================
  if (!is.null(workflow$time_col)) {
    time_col <- workflow$time_col
    if (time_col %in% names(data)) {
      timestamps <- data[[time_col]]

      # Check each feature for future information
      for (col in names(features)) {
        feature <- features[[col]]

        # Skip non-numeric
        if (!is.numeric(feature)) next

        # Check if feature values correlate with future timestamps
        # A leaky feature might show values that correlate with future outcomes
        # This is a heuristic check - not definitive

        # If feature has different variance in test vs train proportionally to time
        train_times <- timestamps[train_idx]
        test_times <- timestamps[test_idx]

        if (inherits(train_times, c("Date", "POSIXct", "POSIXt"))) {
          # Check if test times are all after train times (proper temporal split)
          max_train_time <- max(train_times, na.rm = TRUE)
          min_test_time <- min(test_times, na.rm = TRUE)

          if (min_test_time < max_train_time) {
            # Only flag once, not per feature
            if (col == names(features)[1]) {
              risks <- c(risks, list(list(
                type = "temporal_leak",
                severity = "soft_inflation",
                description = sprintf(
                  "Temporal split is not strict: test data (%s) overlaps with training period (max train: %s)",
                  as.character(min_test_time), as.character(max_train_time)
                ),
                affected_indices = test_idx[test_times < max_train_time],
                source_object = time_col
              )))
            }
          }
        }
      }
    }
  }

  # ===========================================================================
  # 5. Group mean leakage (target encoding without proper CV)
  # ===========================================================================
  if (!is.null(workflow$group_col)) {
    group_col <- workflow$group_col
    if (group_col %in% names(data)) {
      groups <- data[[group_col]]

      # Check for features that look like group-level target means
      for (col in names(features)) {
        feature <- features[[col]]

        if (!is.numeric(feature) || !is.numeric(target)) next

        # Compute what the group means would be on training data
        train_groups <- groups[train_idx]
        train_targets <- target[train_idx]
        train_features <- feature[train_idx]

        group_means <- tapply(train_targets, train_groups, mean, na.rm = TRUE)

        # Check if feature values match group means suspiciously well
        expected_from_groups <- group_means[as.character(train_groups)]

        if (all(!is.na(expected_from_groups)) && all(!is.na(train_features))) {
          cor_with_group_mean <- tryCatch(
            cor(train_features, expected_from_groups, use = "complete.obs"),
            error = function(e) NA
          )

          if (!is.na(cor_with_group_mean) && abs(cor_with_group_mean) > 0.99) {
            risks <- c(risks, list(list(
              type = "target_leakage_group_mean",
              severity = "hard_violation",
              description = sprintf(
                "Feature '%s' appears to be group-level target mean (correlation %.4f with group means)",
                col, cor_with_group_mean
              ),
              affected_indices = integer(0),
              source_object = col
            )))
          }
        }
      }
    }
  }

  risks
}


#' Check for feature engineering leakage
#' @noRd
.check_feature_engineering <- function(data, train_idx, test_idx, workflow) {
  risks <- list()

  # ===========================================================================
  # 1. Check for global statistics that should be train-only
  # ===========================================================================
  for (col in names(data)) {
    feature <- data[[col]]
    if (!is.numeric(feature)) next

    train_vals <- feature[train_idx]
    test_vals <- feature[test_idx]
    all_vals <- feature

    # Check if feature appears to be standardized using global stats
    # A properly standardized feature on train only would have mean ~0 only on train
    train_mean <- mean(train_vals, na.rm = TRUE)
    train_sd <- sd(train_vals, na.rm = TRUE)
    all_mean <- mean(all_vals, na.rm = TRUE)
    all_sd <- sd(all_vals, na.rm = TRUE)

    # If the feature has mean ~0 and sd ~1 on ALL data but not on train,
    # it was standardized on full data
    if (!is.na(all_sd) && all_sd > 0 &&
        !is.na(train_sd) && train_sd > 0) {

      # Check if standardized on full data (all_mean ~0, all_sd ~1)
      is_globally_standardized <- abs(all_mean) < 0.01 && abs(all_sd - 1) < 0.01

      # Check if NOT standardized on train only (train_mean != 0 or train_sd != 1)
      train_not_standard <- abs(train_mean) > 0.05 || abs(train_sd - 1) > 0.05

      if (is_globally_standardized && train_not_standard) {
        risks <- c(risks, list(list(
          type = "feature_engineering_leak",
          severity = "hard_violation",
          description = sprintf(
            "Feature '%s' appears standardized on full data (global: mean=%.3f, sd=%.3f; train: mean=%.3f, sd=%.3f)",
            col, all_mean, all_sd, train_mean, train_sd
          ),
          affected_indices = test_idx,
          source_object = col
        )))
      }
    }
  }

  # ===========================================================================
  # 2. Check for rank/percentile features computed on full data
  # ===========================================================================
  for (col in names(data)) {
    feature <- data[[col]]
    if (!is.numeric(feature)) next

    # Percentile features are typically in [0, 1] or [0, 100]
    feature_range <- range(feature, na.rm = TRUE)

    is_percentile_like <- (feature_range[1] >= 0 && feature_range[2] <= 1) ||
                          (feature_range[1] >= 0 && feature_range[2] <= 100 &&
                           all(feature == floor(feature), na.rm = TRUE))

    if (is_percentile_like && grepl("rank|percentile|quantile|pct", col, ignore.case = TRUE)) {
      # Check if the ranking was done on full data
      # If train values don't span the full range but test does, that's suspicious
      train_range <- range(feature[train_idx], na.rm = TRUE)
      test_range <- range(feature[test_idx], na.rm = TRUE)

      # If test range extends beyond train range significantly, ranking used test data
      if (test_range[2] > train_range[2] * 1.1 || test_range[1] < train_range[1] * 0.9) {
        # This could be legitimate, so just flag as soft
        risks <- c(risks, list(list(
          type = "feature_engineering_rank",
          severity = "soft_inflation",
          description = sprintf(
            "Feature '%s' appears to be a rank/percentile that may use test data (train range: [%.2f, %.2f], test range: [%.2f, %.2f])",
            col, train_range[1], train_range[2], test_range[1], test_range[2]
          ),
          affected_indices = integer(0),
          source_object = col
        )))
      }
    }
  }

  # ===========================================================================
  # 3. Check for binned features where bin edges used test data
  # ===========================================================================
  for (col in names(data)) {
    feature <- data[[col]]
    if (!is.numeric(feature) && !is.factor(feature)) next

    # Detect binned features by looking for few unique values with specific patterns
    if (is.numeric(feature)) {
      n_unique <- length(unique(feature[!is.na(feature)]))
      n_total <- sum(!is.na(feature))

      # Features with very few unique values relative to size might be binned
      if (n_unique >= 2 && n_unique <= 20 && n_unique < n_total * 0.1) {
        # Check if bin assignments differ systematically between train/test
        # If binning used test data, the test distribution should be "too perfect"
        train_tab <- table(feature[train_idx])
        test_tab <- table(feature[test_idx])

        # If test has bins not in train, binning may have used test data
        test_only_bins <- setdiff(names(test_tab), names(train_tab))
        if (length(test_only_bins) > 0) {
          risks <- c(risks, list(list(
            type = "feature_engineering_binning",
            severity = "soft_inflation",
            description = sprintf(
              "Binned feature '%s' has %d bin(s) that appear only in test data",
              col, length(test_only_bins)
            ),
            affected_indices = integer(0),
            source_object = col
          )))
        }
      }
    }
  }

  # ===========================================================================
  # 4. Check for lag/lead features in time series (if time column provided)
  # ===========================================================================
  if (!is.null(workflow$time_col) && !is.null(workflow$target_col)) {
    time_col <- workflow$time_col
    target_col <- workflow$target_col

    if (time_col %in% names(data) && target_col %in% names(data)) {
      timestamps <- data[[time_col]]
      target <- data[[target_col]]

      # Check for features that look like lagged target values
      for (col in setdiff(names(data), c(time_col, target_col))) {
        feature <- data[[col]]
        if (!is.numeric(feature) || !is.numeric(target)) next

        # Check if this feature is a shifted version of target
        for (lag in 1:min(5, floor(length(target) / 10))) {
          if (length(target) <= lag) next

          # Compare feature to lagged target
          lagged_target <- c(rep(NA, lag), target[1:(length(target) - lag)])

          if (all(!is.na(feature)) && all(!is.na(lagged_target[-c(1:lag)]))) {
            cor_with_lag <- tryCatch(
              cor(feature[-(1:lag)], lagged_target[-(1:lag)], use = "complete.obs"),
              error = function(e) NA
            )

            if (!is.na(cor_with_lag) && abs(cor_with_lag) > 0.99) {
              risks <- c(risks, list(list(
                type = "feature_engineering_lag",
                severity = "soft_inflation",
                description = sprintf(
                  "Feature '%s' highly correlates (%.4f) with target lagged by %d - verify lag direction",
                  col, cor_with_lag, lag
                ),
                affected_indices = integer(0),
                source_object = col
              )))
              break
            }
          }
        }
      }
    }
  }

  risks
}


#' Check for threshold selection issues
#' @noRd
.check_threshold_selection <- function(workflow) {
  risks <- list()

  thresholds <- workflow$thresholds

  if (!is.list(thresholds)) {
    return(risks)
  }

  # Check if threshold was optimized on test data
  if (!is.null(thresholds$optimized_on)) {
    if (thresholds$optimized_on == "test") {
      risks <- c(risks, list(list(
        type = "threshold_leak",
        severity = "hard_violation",
        description = "Classification threshold was optimized on test data",
        affected_indices = workflow$test_idx,
        source_object = "workflow$thresholds"
      )))
    } else if (thresholds$optimized_on == "full") {
      risks <- c(risks, list(list(
        type = "threshold_leak",
        severity = "hard_violation",
        description = "Classification threshold was optimized on full data (includes test)",
        affected_indices = workflow$test_idx,
        source_object = "workflow$thresholds"
      )))
    }
  }

  # Check if threshold selection used information from predictions on test
  if (!is.null(thresholds$selection_method)) {
    if (thresholds$selection_method %in% c("youden", "f1_max", "roc_optimal")) {
      # These methods require predictions - check if they used test predictions
      if (isTRUE(thresholds$used_test_predictions)) {
        risks <- c(risks, list(list(
          type = "threshold_leak",
          severity = "hard_violation",
          description = sprintf(
            "Threshold selection method '%s' used test set predictions",
            thresholds$selection_method
          ),
          affected_indices = workflow$test_idx,
          source_object = "workflow$thresholds"
        )))
      }
    }
  }

  # Check for post-hoc threshold adjustment based on test performance
  if (!is.null(thresholds$adjusted_after_test) && isTRUE(thresholds$adjusted_after_test)) {
    risks <- c(risks, list(list(
      type = "threshold_leak",
      severity = "hard_violation",
      description = "Threshold was adjusted after seeing test set performance",
      affected_indices = workflow$test_idx,
      source_object = "workflow$thresholds"
    )))
  }

  risks
}


#' Check for spatial autocorrelation issues
#' @noRd
.check_spatial_autocorrelation <- function(data, train_idx, test_idx, workflow) {
  risks <- list()

  # Get coordinates
  coords <- NULL

  if (!is.null(workflow$coords)) {
    # Coordinates provided directly as matrix/data.frame
    coords <- as.matrix(workflow$coords)
  } else if (!is.null(workflow$spatial_cols)) {
    # Coordinate column names provided
    x_col <- workflow$spatial_cols[1]
    y_col <- if (length(workflow$spatial_cols) > 1) workflow$spatial_cols[2] else NULL

    if (x_col %in% names(data)) {
      if (!is.null(y_col) && y_col %in% names(data)) {
        coords <- as.matrix(data[, c(x_col, y_col)])
      } else {
        # 1D spatial case
        coords <- as.matrix(data[, x_col, drop = FALSE])
      }
    }
  }

  if (is.null(coords)) {
    return(risks)
  }

  # ===========================================================================
  # 1. Check for spatial proximity between train and test sets
  # ===========================================================================

  train_coords <- coords[train_idx, , drop = FALSE]
  test_coords <- coords[test_idx, , drop = FALSE]

  # Compute minimum distances from each test point to any train point
  min_distances <- numeric(nrow(test_coords))

  for (i in seq_len(nrow(test_coords))) {
    test_point <- test_coords[i, ]

    # Euclidean distances to all train points
    if (ncol(coords) == 1) {
      distances <- abs(train_coords[, 1] - test_point[1])
    } else {
      distances <- sqrt(rowSums((sweep(train_coords, 2, test_point))^2))
    }

    min_distances[i] <- min(distances, na.rm = TRUE)
  }

  # Check if any test points are very close to train points
  # Use median nearest neighbor distance in training set as reference
  if (nrow(train_coords) > 1) {
    train_nn_distances <- numeric(nrow(train_coords))

    for (i in seq_len(nrow(train_coords))) {
      if (ncol(coords) == 1) {
        distances <- abs(train_coords[-i, 1] - train_coords[i, 1])
      } else {
        distances <- sqrt(rowSums((sweep(train_coords[-i, , drop = FALSE], 2, train_coords[i, ]))^2))
      }
      train_nn_distances[i] <- min(distances, na.rm = TRUE)
    }

    median_nn <- median(train_nn_distances, na.rm = TRUE)

    # Count test points that are closer to train than typical train-train distance
    very_close <- min_distances < median_nn * 0.5
    n_very_close <- sum(very_close, na.rm = TRUE)

    if (n_very_close > length(test_idx) * 0.3) {
      # More than 30% of test points are very close to training points
      risks <- c(risks, list(list(
        type = "spatial_autocorrelation",
        severity = "soft_inflation",
        description = sprintf(
          "%.1f%% of test points are within half the median train nearest-neighbor distance (%.3g units). Consider spatial blocking.",
          100 * n_very_close / length(test_idx), median_nn / 2
        ),
        affected_indices = test_idx[very_close],
        source_object = "spatial coordinates"
      )))
    }

    # Check for test points that are essentially duplicates of train locations
    n_duplicates <- sum(min_distances < median_nn * 0.01, na.rm = TRUE)
    if (n_duplicates > 0) {
      risks <- c(risks, list(list(
        type = "spatial_duplicate",
        severity = "hard_violation",
        description = sprintf(
          "%d test point(s) have nearly identical location to training points (< 1%% of median NN distance)",
          n_duplicates
        ),
        affected_indices = test_idx[min_distances < median_nn * 0.01],
        source_object = "spatial coordinates"
      )))
    }
  }

  # ===========================================================================
  # 2. Check if spatial blocking was used properly
  # ===========================================================================

  if (!is.null(workflow$spatial_block_col)) {
    block_col <- workflow$spatial_block_col

    if (block_col %in% names(data)) {
      blocks <- data[[block_col]]
      train_blocks <- unique(blocks[train_idx])
      test_blocks <- unique(blocks[test_idx])

      # Check for block overlap
      shared_blocks <- intersect(train_blocks, test_blocks)

      if (length(shared_blocks) > 0) {
        risks <- c(risks, list(list(
          type = "spatial_block_leak",
          severity = "hard_violation",
          description = sprintf(
            "Spatial blocks shared between train and test: %s",
            paste(head(shared_blocks, 5), collapse = ", ")
          ),
          affected_indices = which(blocks %in% shared_blocks),
          source_object = block_col
        )))
      }
    }
  }

  # ===========================================================================
  # 3. Check for buffer zone violations
  # ===========================================================================

  if (!is.null(workflow$buffer_distance)) {
    buffer <- workflow$buffer_distance

    # Check if any test points are within buffer distance of train
    n_in_buffer <- sum(min_distances < buffer, na.rm = TRUE)

    if (n_in_buffer > 0) {
      risks <- c(risks, list(list(
        type = "spatial_buffer_violation",
        severity = "soft_inflation",
        description = sprintf(
          "%d test point(s) are within the specified buffer distance (%.3g) of training points",
          n_in_buffer, buffer
        ),
        affected_indices = test_idx[min_distances < buffer],
        source_object = "spatial coordinates"
      )))
    }
  }

  risks
}


#' Check for hyperparameter optimization validation issues
#' @noRd
.check_hpo_validation <- function(workflow) {
  risks <- list()

  hpo <- workflow$hpo

  if (!is.list(hpo)) {
    return(risks)
  }

  # ===========================================================================
  # 1. Check if HPO used test data
  # ===========================================================================

  if (!is.null(hpo$used_test_data) && isTRUE(hpo$used_test_data)) {
    risks <- c(risks, list(list(
      type = "hpo_test_leak",
      severity = "hard_violation",
      description = "Hyperparameter optimization used test data for tuning",
      affected_indices = workflow$test_idx,
      source_object = "workflow$hpo"
    )))
  }

  # ===========================================================================
  # 2. Check if final model selection used test performance
  # ===========================================================================

  if (!is.null(hpo$model_selected_on)) {
    if (hpo$model_selected_on == "test") {
      risks <- c(risks, list(list(
        type = "hpo_selection_leak",
        severity = "hard_violation",
        description = "Final model was selected based on test set performance",
        affected_indices = workflow$test_idx,
        source_object = "workflow$hpo"
      )))
    } else if (hpo$model_selected_on == "full") {
      risks <- c(risks, list(list(
        type = "hpo_selection_leak",
        severity = "hard_violation",
        description = "Final model was selected based on full data performance",
        affected_indices = workflow$test_idx,
        source_object = "workflow$hpo"
      )))
    }
  }

  # ===========================================================================
  # 3. Check for proper nested CV structure
  # ===========================================================================

  if (!is.null(hpo$cv_type)) {
    if (hpo$cv_type == "simple" && !isTRUE(hpo$nested)) {
      # Simple CV without nesting can cause optimistic bias
      risks <- c(risks, list(list(
        type = "hpo_cv_bias",
        severity = "soft_inflation",
        description = "HPO uses simple (non-nested) CV which may cause optimistic bias in performance estimates",
        affected_indices = integer(0),
        source_object = "workflow$hpo"
      )))
    }
  }

  # ===========================================================================
  # 4. Check for excessive number of configurations tested
  # ===========================================================================

  if (!is.null(hpo$n_configurations)) {
    n_train <- length(workflow$train_idx)
    n_configs <- hpo$n_configurations

    # Rule of thumb: more configurations than training samples is suspicious
    if (n_configs > n_train) {
      risks <- c(risks, list(list(
        type = "hpo_overfit_risk",
        severity = "soft_inflation",
        description = sprintf(
          "HPO tested %d configurations with only %d training samples - high risk of overfitting to validation set",
          n_configs, n_train
        ),
        affected_indices = integer(0),
        source_object = "workflow$hpo"
      )))
    }
  }

  # ===========================================================================
  # 5. Check for data snooping in feature selection during HPO
  # ===========================================================================

  if (!is.null(hpo$feature_selection)) {
    if (isTRUE(hpo$feature_selection$used_full_data)) {
      risks <- c(risks, list(list(
        type = "hpo_feature_leak",
        severity = "hard_violation",
        description = "Feature selection during HPO used full data instead of CV folds",
        affected_indices = workflow$test_idx,
        source_object = "workflow$hpo"
      )))
    }
  }

  # ===========================================================================
  # 6. Check for early stopping based on test data
  # ===========================================================================

  if (!is.null(hpo$early_stopping)) {
    if (isTRUE(hpo$early_stopping$used_test_data)) {
      risks <- c(risks, list(list(
        type = "hpo_early_stopping_leak",
        severity = "hard_violation",
        description = "Early stopping during HPO was based on test set performance",
        affected_indices = workflow$test_idx,
        source_object = "workflow$hpo"
      )))
    }
  }

  risks
}
