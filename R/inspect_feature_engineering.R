# Feature engineering leakage detection
# Detects rolling window, spatial lag, and neighbor features that use test/future data

#' Inspect Data for Feature Engineering Leakage (Internal)
#'
#' Checks column names and values for signs that engineered features
#' were computed using data that should be unseen at prediction time.
#'
#' @noRd
.inspect_feature_engineering <- function(data, train_idx, test_idx,
                                          time_col = NULL, coords = NULL) {
  risks <- list()
  if (is.null(data) || !is.data.frame(data)) return(risks)

  col_names <- names(data)

  # --- 1. Detect rolling/lag/lead features by column name patterns ---
  temporal_patterns <- c(
    "^lag_", "_lag[0-9]", "_lag$",
    "^lead_", "_lead[0-9]", "_lead$",
    "^rolling_", "_rolling", "^roll_",
    "_ma[0-9]", "_ma$",           # moving average
    "_ewm", "_ema",               # exponential moving average
    "_diff$", "_pct_change",
    "_cumsum", "_cummax", "_cummin"
  )

  spatial_patterns <- c(
    "^spatial_lag", "_spatial_lag",
    "^neighbor_", "_neighbor",
    "^nn_", "_nn[0-9]",           # nearest neighbor features
    "_buffer_", "_focal_",
    "_idw$", "_kriged"            # interpolated features
  )

  temporal_suspects <- character(0)
  for (pat in temporal_patterns) {
    temporal_suspects <- union(temporal_suspects,
                                grep(pat, col_names, ignore.case = TRUE, value = TRUE))
  }

  spatial_suspects <- character(0)
  for (pat in spatial_patterns) {
    spatial_suspects <- union(spatial_suspects,
                               grep(pat, col_names, ignore.case = TRUE, value = TRUE))
  }

  # --- 2. Validate temporal features against train/test split ---
  if (length(temporal_suspects) > 0 && !is.null(time_col) && time_col %in% col_names) {
    # Check if temporal features in test set contain values that could only
    # come from computing on the full dataset (train+test)
    for (feat in temporal_suspects) {
      if (!is.numeric(data[[feat]])) next

      train_vals <- data[[feat]][train_idx]
      test_vals <- data[[feat]][test_idx]

      # If test feature values have no NAs where we'd expect NAs from
      # properly windowed computation, flag as suspicious
      # (A properly computed lag on train-only data would have NAs at the boundary)
      if (all(!is.na(test_vals)) && any(is.na(train_vals))) {
        # Test has no NAs but train does — feature was likely computed on full data
        risks <- c(risks, list(.new_risk(
          type = "feature_engineering_leak",
          severity = "hard_violation",
          description = sprintf(
            "Temporal feature '%s' appears computed on full data (test has no NAs, train does). Recompute on training data only per fold.",
            feat
          ),
          affected_indices = test_idx,
          source_object = feat
        )))
        break  # One example is enough
      }
    }

    # Even if we can't prove leakage from values, warn about the pattern
    if (length(risks) == 0 && length(temporal_suspects) > 0) {
      risks <- c(risks, list(.new_risk(
        type = "feature_engineering_leak",
        severity = "soft_inflation",
        description = sprintf(
          "Temporal features detected: [%s]. Verify these were computed per-fold on training data only.",
          paste(head(temporal_suspects, 5), collapse = ", ")
        ),
        affected_indices = integer(0),
        source_object = paste(temporal_suspects, collapse = ", ")
      )))
    }
  } else if (length(temporal_suspects) > 0) {
    risks <- c(risks, list(.new_risk(
      type = "feature_engineering_leak",
      severity = "soft_inflation",
      description = sprintf(
        "Temporal features detected: [%s]. Verify these were computed per-fold on training data only.",
        paste(head(temporal_suspects, 5), collapse = ", ")
      ),
      affected_indices = integer(0),
      source_object = paste(temporal_suspects, collapse = ", ")
    )))
  }

  # --- 3. Spatial feature warnings ---
  if (length(spatial_suspects) > 0) {
    risks <- c(risks, list(.new_risk(
      type = "feature_engineering_leak",
      severity = "soft_inflation",
      description = sprintf(
        "Spatial features detected: [%s]. These may use test-set neighbor values. Recompute per-fold on training data only.",
        paste(head(spatial_suspects, 5), collapse = ", ")
      ),
      affected_indices = integer(0),
      source_object = paste(spatial_suspects, collapse = ", ")
    )))
  }

  risks
}
