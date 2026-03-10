# Spatial autocorrelation checks for borg_validate()

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
      risks <- c(risks, list(.new_risk(
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
      risks <- c(risks, list(.new_risk(
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
        risks <- c(risks, list(.new_risk(
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
      risks <- c(risks, list(.new_risk(
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
