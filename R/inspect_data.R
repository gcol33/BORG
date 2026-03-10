# Data frame inspector: duplicate rows, target leakage, spatial checks

#' @noRd
.inspect_data_frame <- function(object, train_idx, test_idx,
                                target_col = NULL, spatial_cols = NULL, ...) {
  risks <- list()

  # Check for duplicate rows between train and test
  if (!is.null(train_idx) && !is.null(test_idx)) {
    # Check bounds
    max_idx <- max(c(train_idx, test_idx))
    if (max_idx > nrow(object)) {
      risks <- c(risks, list(.new_risk(
        type = "invalid_indices",
        severity = "hard_violation",
        description = sprintf(
          "Indices exceed data dimensions (max index %d > nrow %d)",
          max_idx, nrow(object)
        ),
        source_object = "data.frame"
      )))
      return(risks)
    }

    # Check for duplicate rows using C++ backend if available
    # Get numeric columns for comparison
    numeric_cols <- vapply(object, is.numeric, logical(1))

    if (any(numeric_cols)) {
      # Use C++ backend for numeric data
      numeric_data <- as.matrix(object[, numeric_cols, drop = FALSE])

      dup_result <- tryCatch(
        checkDuplicateRows(numeric_data, train_idx, test_idx),
        error = function(e) NULL
      )

      if (!is.null(dup_result) && dup_result$has_duplicates) {
        risks <- c(risks, list(.new_risk(
          type = "duplicate_rows",
          severity = "hard_violation",
          description = sprintf(
            "Test set contains %d rows identical to training rows (memorization risk)",
            dup_result$n_duplicates
          ),
          affected_indices = dup_result$duplicate_indices,
          source_object = "data.frame"
        )))
      }
    } else {
      # Fallback to R-based comparison for non-numeric data
      train_data <- object[train_idx, , drop = FALSE]
      test_data <- object[test_idx, , drop = FALSE]

      train_hashes <- apply(train_data, 1, function(r) paste(r, collapse = "|"))
      test_hashes <- apply(test_data, 1, function(r) paste(r, collapse = "|"))

      duplicates <- which(test_hashes %in% train_hashes)
      if (length(duplicates) > 0) {
        risks <- c(risks, list(.new_risk(
          type = "duplicate_rows",
          severity = "hard_violation",
          description = sprintf(
            "Test set contains %d rows identical to training rows (memorization risk)",
            length(duplicates)
          ),
          affected_indices = test_idx[duplicates],
          source_object = "data.frame"
        )))
      }
    }
  }

  # =========================================================================
  # Target leakage detection
  # =========================================================================

  if (!is.null(target_col) && target_col %in% names(object)) {
    target <- object[[target_col]]

    if (is.numeric(target)) {
      feature_cols <- setdiff(names(object), target_col)
      numeric_features <- feature_cols[vapply(object[feature_cols], is.numeric, logical(1))]

      if (length(numeric_features) > 0) {
        use_idx <- train_idx %||% seq_len(nrow(object))
        features_df <- object[, numeric_features, drop = FALSE]

        cor_risks <- .detect_correlation_leakage(
          features_df, target, use_idx, target_col_name = target_col
        )
        risks <- c(risks, cor_risks)
      }
    }
  }

  # =========================================================================
  # Spatial separation check
  # =========================================================================

  if (!is.null(spatial_cols) && length(spatial_cols) >= 2 &&
      !is.null(train_idx) && !is.null(test_idx)) {

    # Check that spatial columns exist and are numeric
    spatial_cols_valid <- spatial_cols[spatial_cols %in% names(object)]
    spatial_cols_valid <- spatial_cols_valid[
      vapply(object[spatial_cols_valid], is.numeric, logical(1))
    ]

    if (length(spatial_cols_valid) >= 2) {
      # Use first two columns as coordinates
      coord_cols <- spatial_cols_valid[1:2]

      train_coords <- as.matrix(object[train_idx, coord_cols, drop = FALSE])
      test_coords <- as.matrix(object[test_idx, coord_cols, drop = FALSE])

      # Compute minimum distance from each test point to any train point
      # This is O(n*m) but acceptable for moderate dataset sizes
      min_distances <- vapply(seq_len(nrow(test_coords)), function(i) {
        test_point <- test_coords[i, ]
        dists <- sqrt(rowSums((train_coords - matrix(test_point, nrow = nrow(train_coords),
                                                      ncol = 2, byrow = TRUE))^2))
        min(dists, na.rm = TRUE)
      }, numeric(1))

      # Estimate spatial scale from training data spread
      train_spread <- sqrt(sum(apply(train_coords, 2, var, na.rm = TRUE)))

      # Flag if test points are very close to train points (< 1% of spread)
      close_threshold <- train_spread * 0.01
      very_close <- which(min_distances < close_threshold)

      if (length(very_close) > 0) {
        risks <- c(risks, list(.new_risk(
          type = "spatial_proximity",
          severity = "soft_inflation",
          description = sprintf(
            "%d test points are very close to training points (< 1%% of spatial spread). Spatial autocorrelation may inflate performance.",
            length(very_close)
          ),
          affected_indices = test_idx[very_close],
          source_object = "data.frame"
        )))
      }

      # Also check if train and test regions overlap substantially
      # Use convex hull overlap as a proxy
      train_hull <- tryCatch(chull(train_coords), error = function(e) NULL)
      test_hull <- tryCatch(chull(test_coords), error = function(e) NULL)

      if (!is.null(train_hull) && !is.null(test_hull) &&
          length(train_hull) >= 3 && nrow(test_coords) >= 1) {
        # Check how many test points fall inside training hull
        # Simple point-in-polygon check (needs >= 3 hull vertices)
        hull_polygon <- train_coords[train_hull, , drop = FALSE]
        n_test_in_train <- sum(vapply(seq_len(nrow(test_coords)), function(i) {
          tryCatch(
            .point_in_polygon(test_coords[i, ], hull_polygon),
            error = function(e) FALSE
          )
        }, logical(1)))

        overlap_pct <- n_test_in_train / nrow(test_coords) * 100

        if (overlap_pct > 50) {
          risks <- c(risks, list(.new_risk(
            type = "spatial_overlap",
            severity = "soft_inflation",
            description = sprintf(
              "%.0f%% of test points fall within the training region convex hull. Consider spatial blocking.",
              overlap_pct
            ),
            affected_indices = test_idx,
            source_object = "data.frame"
          )))
        }
      }
    }
  }

  risks
}
