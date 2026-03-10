# Target leakage and feature engineering leakage checks for borg_validate()

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
  # 1. Direct correlation check
  # ===========================================================================
  if (is.numeric(target)) {
    numeric_features <- features[vapply(features, is.numeric, logical(1))]
    if (length(numeric_features) > 0) {
      cor_risks <- .detect_correlation_leakage(
        numeric_features, target, train_idx, target_col_name = target_col
      )
      risks <- c(risks, cor_risks)
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
        risks <- c(risks, list(.new_risk(
          type = "target_leakage_naming",
          severity = "soft_inflation",
          description = sprintf(
            "Feature '%s' has suspicious naming pattern suggesting potential target leakage",
            col
          ),
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
            risks <- c(risks, list(.new_risk(
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
            risks <- c(risks, list(.new_risk(
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
              risks <- c(risks, list(.new_risk(
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
            risks <- c(risks, list(.new_risk(
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
        risks <- c(risks, list(.new_risk(
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
        risks <- c(risks, list(.new_risk(
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
          risks <- c(risks, list(.new_risk(
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
              risks <- c(risks, list(.new_risk(
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


#' Check for rolling/window feature engineering leakage
#'
#' Detects lag features, rolling means, cumulative sums, and other
#' time-derived features that may have been computed using future data.
#' @noRd
.check_rolling_features <- function(data, train_idx, test_idx, workflow) {
  risks <- list()

  # Only relevant if time column is specified
  time_col <- workflow$time_col
  if (is.null(time_col) || !time_col %in% names(data)) return(risks)

  timestamps <- data[[time_col]]
  time_numeric <- as.numeric(timestamps)
  ord <- order(time_numeric)

  train_sorted <- sort(train_idx)
  test_sorted <- sort(test_idx)

  target_col <- workflow$target_col
  exclude_cols <- c(time_col, target_col)
  feature_cols <- setdiff(names(data), exclude_cols)
  feature_cols <- feature_cols[vapply(data[feature_cols], is.numeric, logical(1))]

  for (col in feature_cols) {
    feature <- data[[col]]

    # Heuristic 1: Name-based detection
    is_rolling_name <- grepl(
      "roll|rolling|window|moving|lag|lead|cumsum|cummax|cummin|cummean|ewm|ema|sma|diff_",
      col, ignore.case = TRUE
    )

    # Heuristic 2: Leading NA pattern (lag/rolling features have NAs at start)
    na_positions <- which(is.na(feature[ord]))
    has_leading_nas <- length(na_positions) > 0 &&
      all(na_positions == seq_along(na_positions))
    n_leading_nas <- if (has_leading_nas) length(na_positions) else 0

    if (!is_rolling_name && n_leading_nas == 0) next

    # Heuristic 3: Boundary smoothness check
    # A rolling feature computed on full data will be smooth across the
    # train/test boundary. One computed on train-only will show a
    # discontinuity (jump or NA) at the boundary.
    if (length(test_sorted) >= 3 && length(train_sorted) >= 3) {
      boundary_test <- test_sorted[1:min(3, length(test_sorted))]
      boundary_train <- train_sorted[(max(1, length(train_sorted) - 2)):length(train_sorted)]

      test_vals <- feature[boundary_test]
      train_vals <- feature[boundary_train]

      if (all(!is.na(test_vals)) && all(!is.na(train_vals)) && is_rolling_name) {
        all_boundary <- c(train_vals, test_vals)
        boundary_smooth <- sd(diff(all_boundary), na.rm = TRUE)
        overall_roughness <- sd(diff(feature[ord]), na.rm = TRUE)

        if (!is.na(boundary_smooth) && !is.na(overall_roughness) &&
            overall_roughness > 0) {
          smoothness_ratio <- boundary_smooth / overall_roughness
          if (smoothness_ratio < 0.3) {
            risks <- c(risks, list(.new_risk(
              type = "rolling_feature_leak",
              severity = "soft_inflation",
              description = sprintf(
                "Feature '%s' appears to be a rolling computation with no discontinuity at the train/test boundary (smoothness ratio: %.2f). Verify it was computed on training data only.",
                col, smoothness_ratio
              ),
              affected_indices = boundary_test,
              source_object = col
            )))
            next
          }
        }
      }
    }

    # Heuristic 4: Named rolling/lag feature with leading NAs
    if (is_rolling_name && n_leading_nas > 0) {
      first_test_in_order <- which(ord %in% test_sorted)[1]
      if (!is.na(first_test_in_order) && first_test_in_order > n_leading_nas) {
        risks <- c(risks, list(.new_risk(
          type = "rolling_feature_leak",
          severity = "soft_inflation",
          description = sprintf(
            "Feature '%s' has %d leading NAs (suggesting window size %d). Values at test indices may include future data in their window. Verify it was computed on training data only.",
            col, n_leading_nas, n_leading_nas
          ),
          affected_indices = test_sorted[1:min(n_leading_nas, length(test_sorted))],
          source_object = col
        )))
      }
    }
  }

  risks
}
