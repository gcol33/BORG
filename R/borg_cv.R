# BORG CV Generator
# Generates valid cross-validation schemes based on data dependency structure

#' Generate Valid Cross-Validation Scheme
#'
#' Creates cross-validation folds that respect data dependency structure.
#' When spatial, temporal, or clustered dependencies are detected, random
#' CV is disabled and appropriate blocking strategies are enforced.
#'
#' @param data A data frame to create CV folds for.
#' @param diagnosis A \code{\link{BorgDiagnosis}} object from \code{\link{borg_diagnose}}.
#'   If NULL, diagnosis is performed automatically.
#' @param v Integer. Number of folds. Default: 5.
#' @param coords Character vector of length 2 specifying coordinate column names.
#'   Required for spatial blocking if diagnosis is NULL.
#' @param time Character string specifying the time column name.
#'   Required for temporal blocking if diagnosis is NULL.
#' @param groups Character string specifying the grouping column name.
#'   Required for group CV if diagnosis is NULL.
#' @param target Character string specifying the response variable column name.
#' @param block_size Numeric. For spatial blocking, the minimum block size.
#'   If NULL, automatically determined from diagnosis. Should be larger than
#'   the autocorrelation range.
#' @param embargo Integer. For temporal blocking, minimum gap between train and test.
#'   If NULL, automatically determined from diagnosis.
#' @param output Character. Output format: "list" (default), "rsample", "caret", "mlr3".
#' @param allow_random Logical. If TRUE, allows random CV even when dependencies detected.
#'   Default: FALSE. Setting to TRUE requires explicit acknowledgment.
#' @param verbose Logical. If TRUE, print diagnostic messages. Default: FALSE.
#'
#' @return Depending on \code{output}:
#' \describe{
#'   \item{"list"}{A list with elements: \code{folds} (list of train/test index vectors),
#'     \code{diagnosis} (the BorgDiagnosis used), \code{strategy} (CV strategy name),
#'     \code{params} (parameters used).}
#'   \item{"rsample"}{An \code{rsample} \code{rset} object compatible with tidymodels.}
#'   \item{"caret"}{A \code{trainControl} object for caret.}
#'   \item{"mlr3"}{An \code{mlr3} \code{Resampling} object.}
#' }
#'
#' @details
#' \subsection{The Enforcement Principle}{
#' Unlike traditional CV helpers, \code{borg_cv} enforces valid evaluation:
#' \itemize{
#'   \item If spatial autocorrelation is detected, \strong{random CV is disabled}
#'   \item If temporal autocorrelation is detected, \strong{random CV is disabled}
#'   \item If clustered structure is detected, \strong{random CV is disabled}
#'   \item To use random CV on dependent data, you must set \code{allow_random = TRUE}
#'         and provide justification (this is logged).
#' }
#' }
#'
#' \subsection{Spatial Blocking}{
#' When spatial dependencies are detected, data are partitioned into spatial blocks
#' using k-means clustering on coordinates. Block size is set to exceed the
#' estimated autocorrelation range. This ensures train and test sets are
#' spatially separated.
#' }
#'
#' \subsection{Temporal Blocking}{
#' When temporal dependencies are detected, data are split chronologically with
#' an embargo period between train and test sets. This prevents information from
#' future observations leaking into training.
#' }
#'
#' \subsection{Group CV}{
#' When clustered structure is detected, entire groups (clusters) are held out
#' together. No group appears in both train and test within a fold.
#' }
#'
#' @examples
#' # Spatial data with autocorrelation
#' set.seed(42)
#' spatial_data <- data.frame(
#'   x = runif(200, 0, 100),
#'   y = runif(200, 0, 100),
#'   response = rnorm(200)
#' )
#'
#' # Diagnose and create CV
#' cv <- borg_cv(spatial_data, coords = c("x", "y"), target = "response")
#' str(cv$folds)  # List of train/test indices
#'
#' # Clustered data
#' clustered_data <- data.frame(
#'   site = rep(1:20, each = 10),
#'   value = rep(rnorm(20, sd = 2), each = 10) + rnorm(200, sd = 0.5)
#' )
#'
#' cv <- borg_cv(clustered_data, groups = "site", target = "value")
#' cv$strategy  # "group_fold"
#'
#' # Get rsample-compatible output for tidymodels
#' \dontrun{
#' cv_rsample <- borg_cv(spatial_data, coords = c("x", "y"), output = "rsample")
#' }
#'
#' @seealso \code{\link{borg_diagnose}}, \code{\link{BorgDiagnosis}}
#'
#' @export
borg_cv <- function(data,
                    diagnosis = NULL,
                    v = 5,
                    coords = NULL,
                    time = NULL,
                    groups = NULL,
                    target = NULL,
                    block_size = NULL,
                    embargo = NULL,
                    output = c("list", "rsample", "caret", "mlr3"),
                    allow_random = FALSE,
                    verbose = FALSE) {

  output <- match.arg(output)

  # Input validation
  if (!is.data.frame(data)) {
    stop("'data' must be a data frame")
  }
  n <- nrow(data)
  if (n < v * 2) {
    stop(sprintf("Insufficient data: need at least %d rows for %d folds", v * 2, v))
  }

  # Run diagnosis if not provided
  if (is.null(diagnosis)) {
    if (verbose) message("Running borg_diagnose()...")
    diagnosis <- borg_diagnose(data,
                               coords = coords,
                               time = time,
                               groups = groups,
                               target = target,
                               verbose = verbose)
  }

  # Check if random CV is appropriate
  if (diagnosis@severity != "none" && !allow_random) {
    if (verbose) {
      message(sprintf("Dependency detected (%s). Using %s instead of random CV.",
                      diagnosis@dependency_type, diagnosis@recommended_cv))
    }
  } else if (diagnosis@severity != "none" && allow_random) {
    warning(sprintf(
      paste0("BORG WARNING: Random CV requested despite %s dependency.\n",
             "  Estimated metric inflation: ~%.0f%%\n",
             "  This may produce invalid performance estimates."),
      diagnosis@dependency_type,
      diagnosis@inflation_estimate$auc_inflation * 100
    ))
  }

  # Select CV strategy
  strategy <- if (allow_random && diagnosis@severity != "none") {
    "random_override"
  } else {
    diagnosis@recommended_cv
  }

  # Generate folds based on strategy
  folds <- switch(strategy,
    "random" = generate_random_folds(data, v),
    "random_override" = generate_random_folds(data, v),
    "spatial_block" = generate_spatial_block_folds(data, v, diagnosis, coords, block_size, verbose),
    "temporal_block" = generate_temporal_block_folds(data, v, diagnosis, time, embargo, verbose),
    "group_fold" = generate_group_folds(data, v, diagnosis, groups, verbose),
    "spatial_temporal" = generate_spatial_temporal_folds(data, v, diagnosis, coords, time, verbose),
    "spatial_group" = generate_spatial_group_folds(data, v, diagnosis, coords, groups, verbose),
    "temporal_group" = generate_temporal_group_folds(data, v, diagnosis, time, groups, verbose),
    stop(sprintf("Unknown CV strategy: %s", strategy))
  )

  # Build result
  params <- list(
    v = v,
    n = n,
    strategy = strategy,
    allow_random = allow_random
  )

  if (!is.null(block_size)) params$block_size <- block_size
  if (!is.null(embargo)) params$embargo <- embargo

  result <- list(
    folds = folds,
    diagnosis = diagnosis,
    strategy = strategy,
    params = params
  )
  class(result) <- c("borg_cv", "list")

  # Convert output format if requested
  if (output == "list") {
    return(result)
  } else if (output == "rsample") {
    return(convert_to_rsample(result, data))
  } else if (output == "caret") {
    return(convert_to_caret(result))
  } else if (output == "mlr3") {
    return(convert_to_mlr3(result, data))
  }

  result
}


#' @export
print.borg_cv <- function(x, ...) {
  cat("BORG Cross-Validation\n")
  cat("=====================\n\n")
  cat(sprintf("Strategy:    %s\n", x$strategy))
  cat(sprintf("Folds:       %d\n", length(x$folds)))
  cat(sprintf("Total obs:   %d\n", x$params$n))

  # Show fold sizes
  train_sizes <- vapply(x$folds, function(f) length(f$train), integer(1))
  test_sizes <- vapply(x$folds, function(f) length(f$test), integer(1))
  cat(sprintf("Train sizes: %d - %d (mean: %.0f)\n",
              min(train_sizes), max(train_sizes), mean(train_sizes)))
  cat(sprintf("Test sizes:  %d - %d (mean: %.0f)\n",
              min(test_sizes), max(test_sizes), mean(test_sizes)))

  # Warning if random override
  if (x$strategy == "random_override") {
    cat("\nWARNING: Random CV used despite detected dependencies.\n")
    cat(sprintf("         Estimated inflation: ~%.0f%%\n",
                x$diagnosis@inflation_estimate$auc_inflation * 100))
  }

  invisible(x)
}


# ============================================================================
# Fold generation functions
# ============================================================================

#' Generate Random Folds (Internal)
#' @noRd
generate_random_folds <- function(data, v) {
  n <- nrow(data)
  idx <- sample(n)
  fold_ids <- rep(1:v, length.out = n)

  lapply(1:v, function(i) {
    test_idx <- idx[fold_ids == i]
    train_idx <- idx[fold_ids != i]
    list(train = train_idx, test = test_idx)
  })
}


#' Generate Spatial Block Folds (Internal)
#' @noRd
generate_spatial_block_folds <- function(data, v, diagnosis, coords, block_size, verbose) {
  # Get coordinates
  if (is.null(coords)) {
    coords <- diagnosis@spatial$coords_used
  }
  if (is.null(coords)) {
    stop("Spatial blocking requires coordinates. Provide 'coords' argument.")
  }

  x_coord <- data[[coords[1]]]
  y_coord <- data[[coords[2]]]
  n <- nrow(data)

  # Determine block size
  if (is.null(block_size)) {
    if (!is.na(diagnosis@spatial$range_estimate)) {
      block_size <- diagnosis@spatial$range_estimate * 1.5  # 1.5x range for safety
    } else {
      # Fallback: divide extent by sqrt(v)
      x_range <- diff(range(x_coord, na.rm = TRUE))
      y_range <- diff(range(y_coord, na.rm = TRUE))
      block_size <- max(x_range, y_range) / sqrt(v)
    }
  }

  if (verbose) message(sprintf("Using spatial block size: %.2f", block_size))

  # Use k-means to create spatial clusters
  # Number of clusters should be >= v
  n_clusters <- max(v, ceiling(n / 20))  # At least 20 points per cluster
  n_clusters <- min(n_clusters, n %/% 3)  # But not too many

  coord_mat <- cbind(x_coord, y_coord)
  complete <- complete.cases(coord_mat)
  coord_mat_complete <- coord_mat[complete, , drop = FALSE]

  set.seed(42)  # Reproducibility
  km <- stats::kmeans(coord_mat_complete, centers = n_clusters, nstart = 10)

  # Assign clusters back (NA for incomplete)
  clusters <- rep(NA_integer_, n)
  clusters[complete] <- km$cluster

  # Assign clusters to folds (balanced by size)
  cluster_table <- table(clusters)
  cluster_ids <- as.integer(names(cluster_table))
  cluster_order <- order(cluster_table, decreasing = TRUE)

  fold_assignment <- rep(NA_integer_, length(cluster_ids))
  fold_sizes <- rep(0L, v)

  for (cid in cluster_ids[cluster_order]) {
    # Assign to fold with smallest current size
    target_fold <- which.min(fold_sizes)
    fold_assignment[cid] <- target_fold
    fold_sizes[target_fold] <- fold_sizes[target_fold] + cluster_table[as.character(cid)]
  }

  # Create folds
  obs_folds <- fold_assignment[clusters]
  obs_folds[is.na(obs_folds)] <- sample(1:v, sum(is.na(obs_folds)), replace = TRUE)

  lapply(1:v, function(i) {
    test_idx <- which(obs_folds == i)
    train_idx <- which(obs_folds != i)
    list(train = train_idx, test = test_idx)
  })
}


#' Generate Temporal Block Folds (Internal)
#' @noRd
generate_temporal_block_folds <- function(data, v, diagnosis, time_col, embargo, verbose) {
  # Get time column
  if (is.null(time_col)) {
    time_col <- diagnosis@temporal$time_col
  }
  if (is.null(time_col)) {
    stop("Temporal blocking requires time column. Provide 'time' argument.")
  }

  time_vals <- data[[time_col]]
  n <- nrow(data)

  # Convert to numeric
  if (inherits(time_vals, "Date") || inherits(time_vals, "POSIXt")) {
    time_numeric <- as.numeric(time_vals)
  } else {
    time_numeric <- as.numeric(time_vals)
  }

  # Determine embargo
  if (is.null(embargo)) {
    if (!is.na(diagnosis@temporal$embargo_minimum)) {
      embargo <- diagnosis@temporal$embargo_minimum
    } else {
      # Fallback: 5% of time range
      embargo <- diff(range(time_numeric, na.rm = TRUE)) * 0.05
    }
  }

  if (verbose) message(sprintf("Using temporal embargo: %.2f", embargo))

  # Sort by time
  ord <- order(time_numeric)

  # Create temporal folds with embargo
  # Use forward chaining approach
  fold_boundaries <- round(seq(0, n, length.out = v + 1))

  folds <- lapply(1:v, function(i) {
    # For fold i, test on block i, train on blocks before
    test_start <- fold_boundaries[i] + 1
    test_end <- fold_boundaries[i + 1]
    test_idx <- ord[test_start:test_end]

    # Training: all data before test block (minus embargo)
    if (test_start > 1) {
      test_min_time <- min(time_numeric[test_idx], na.rm = TRUE)
      train_mask <- time_numeric[ord[1:(test_start - 1)]] < (test_min_time - embargo)
      train_idx <- ord[1:(test_start - 1)][train_mask]
    } else {
      train_idx <- integer(0)
    }

    # If no training data, use later data (expanding window)
    if (length(train_idx) < 10) {
      # Use later blocks as training instead
      if (test_end < n) {
        test_max_time <- max(time_numeric[test_idx], na.rm = TRUE)
        after_mask <- time_numeric[ord[(test_end + 1):n]] > (test_max_time + embargo)
        train_idx <- c(train_idx, ord[(test_end + 1):n][after_mask])
      }
    }

    list(train = train_idx, test = test_idx)
  })

  # Filter out folds with insufficient training data
  valid_folds <- Filter(function(f) length(f$train) >= 10, folds)

  if (length(valid_folds) < 2) {
    warning("Temporal blocking with embargo produced < 2 valid folds. ",
            "Consider reducing embargo or using a different strategy.")
    # Fallback to simple temporal split without embargo
    folds <- lapply(1:v, function(i) {
      test_start <- fold_boundaries[i] + 1
      test_end <- fold_boundaries[i + 1]
      test_idx <- ord[test_start:test_end]
      train_idx <- setdiff(ord, test_idx)
      list(train = train_idx, test = test_idx)
    })
  } else {
    folds <- valid_folds
  }

  folds
}


#' Generate Group Folds (Internal)
#' @noRd
generate_group_folds <- function(data, v, diagnosis, group_col, verbose) {
  # Get group column
  if (is.null(group_col)) {
    group_col <- diagnosis@clustered$group_col
  }
  if (is.null(group_col)) {
    stop("Group CV requires group column. Provide 'groups' argument.")
  }

  groups <- data[[group_col]]
  unique_groups <- unique(groups)
  n_groups <- length(unique_groups)

  if (n_groups < v) {
    warning(sprintf("Only %d groups available, reducing to %d folds", n_groups, n_groups))
    v <- n_groups
  }

  if (verbose) message(sprintf("Creating %d-fold group CV with %d groups", v, n_groups))

  # Assign groups to folds (balanced by total observations)
  group_sizes <- table(groups)
  group_order <- order(group_sizes, decreasing = TRUE)

  fold_assignment <- rep(NA_integer_, n_groups)
  names(fold_assignment) <- unique_groups
  fold_sizes <- rep(0L, v)

  for (g in unique_groups[group_order]) {
    target_fold <- which.min(fold_sizes)
    fold_assignment[g] <- target_fold
    fold_sizes[target_fold] <- fold_sizes[target_fold] + group_sizes[g]
  }

  # Create folds
  lapply(1:v, function(i) {
    test_groups <- names(fold_assignment)[fold_assignment == i]
    test_idx <- which(groups %in% test_groups)
    train_idx <- which(!groups %in% test_groups)
    list(train = train_idx, test = test_idx)
  })
}


#' Generate Spatial-Temporal Folds (Internal)
#' @noRd
generate_spatial_temporal_folds <- function(data, v, diagnosis, coords, time_col, verbose) {
  # Hybrid approach: temporal blocks with spatial buffer
  if (is.null(time_col)) time_col <- diagnosis@temporal$time_col
  if (is.null(coords)) coords <- diagnosis@spatial$coords_used

  # Primary split by time
  temporal_folds <- generate_temporal_block_folds(data, v, diagnosis, time_col, NULL, verbose)

  # For each fold, apply additional spatial buffer
  if (!is.null(coords)) {
    x_coord <- data[[coords[1]]]
    y_coord <- data[[coords[2]]]

    buffer_dist <- if (!is.na(diagnosis@spatial$range_estimate)) {
      diagnosis@spatial$range_estimate
    } else {
      # 10% of spatial extent
      max(diff(range(x_coord, na.rm = TRUE)), diff(range(y_coord, na.rm = TRUE))) * 0.1
    }

    temporal_folds <- lapply(temporal_folds, function(fold) {
      test_x <- x_coord[fold$test]
      test_y <- y_coord[fold$test]

      # Remove training points too close to test points
      train_keep <- vapply(fold$train, function(i) {
        min_dist <- min(sqrt((x_coord[i] - test_x)^2 + (y_coord[i] - test_y)^2))
        min_dist > buffer_dist
      }, logical(1))

      list(train = fold$train[train_keep], test = fold$test)
    })
  }

  temporal_folds
}


#' Generate Spatial-Group Folds (Internal)
#' @noRd
generate_spatial_group_folds <- function(data, v, diagnosis, coords, group_col, verbose) {
  # Use groups as primary structure, ensure spatial separation
  group_folds <- generate_group_folds(data, v, diagnosis, group_col, verbose)

  # Apply spatial buffer
  if (!is.null(coords) || !is.null(diagnosis@spatial$coords_used)) {
    if (is.null(coords)) coords <- diagnosis@spatial$coords_used

    x_coord <- data[[coords[1]]]
    y_coord <- data[[coords[2]]]

    buffer_dist <- if (!is.na(diagnosis@spatial$range_estimate)) {
      diagnosis@spatial$range_estimate
    } else {
      max(diff(range(x_coord, na.rm = TRUE)), diff(range(y_coord, na.rm = TRUE))) * 0.1
    }

    group_folds <- lapply(group_folds, function(fold) {
      test_x <- x_coord[fold$test]
      test_y <- y_coord[fold$test]

      train_keep <- vapply(fold$train, function(i) {
        min_dist <- min(sqrt((x_coord[i] - test_x)^2 + (y_coord[i] - test_y)^2))
        min_dist > buffer_dist
      }, logical(1))

      list(train = fold$train[train_keep], test = fold$test)
    })
  }

  group_folds
}


#' Generate Temporal-Group Folds (Internal)
#' @noRd
generate_temporal_group_folds <- function(data, v, diagnosis, time_col, group_col, verbose) {
  # Group-out with temporal ordering
  if (is.null(time_col)) time_col <- diagnosis@temporal$time_col
  if (is.null(group_col)) group_col <- diagnosis@clustered$group_col

  groups <- data[[group_col]]
  time_vals <- data[[time_col]]

  # Order groups by their mean time
  group_times <- tapply(as.numeric(time_vals), groups, mean, na.rm = TRUE)
  group_order <- names(sort(group_times))

  unique_groups <- group_order
  n_groups <- length(unique_groups)

  if (n_groups < v) {
    v <- n_groups
  }

  # Assign groups to folds in temporal order
  fold_assignment <- rep(1:v, length.out = n_groups)
  names(fold_assignment) <- unique_groups

  lapply(1:v, function(i) {
    test_groups <- names(fold_assignment)[fold_assignment == i]
    test_idx <- which(groups %in% test_groups)
    train_idx <- which(!groups %in% test_groups)
    list(train = train_idx, test = test_idx)
  })
}


# ============================================================================
# Output format converters
# ============================================================================

#' Convert to rsample Format (Internal)
#' @noRd
convert_to_rsample <- function(borg_cv_obj, data) {
  if (!requireNamespace("rsample", quietly = TRUE)) {
    stop("Package 'rsample' required for output='rsample'. Install with: install.packages('rsample')")
  }

  # Create manual_rset
  splits <- lapply(borg_cv_obj$folds, function(fold) {
    rsample::make_splits(
      x = list(analysis = fold$train, assessment = fold$test),
      data = data
    )
  })

  ids <- paste0("Fold", seq_along(splits))

  rsample::manual_rset(splits, ids)
}


#' Convert to caret Format (Internal)
#' @noRd
convert_to_caret <- function(borg_cv_obj) {
  if (!requireNamespace("caret", quietly = TRUE)) {
    stop("Package 'caret' required for output='caret'. Install with: install.packages('caret')")
  }

  # Create index and indexOut lists
  index <- lapply(borg_cv_obj$folds, function(f) f$train)
  indexOut <- lapply(borg_cv_obj$folds, function(f) f$test)

  names(index) <- paste0("Fold", seq_along(index))
  names(indexOut) <- paste0("Fold", seq_along(indexOut))

  caret::trainControl(
    method = "cv",
    index = index,
    indexOut = indexOut,
    savePredictions = "final"
  )
}


#' Convert to mlr3 Format (Internal)
#' @noRd
convert_to_mlr3 <- function(borg_cv_obj, data) {
  if (!requireNamespace("mlr3", quietly = TRUE)) {
    stop("Package 'mlr3' required for output='mlr3'. Install with: install.packages('mlr3')")
  }

  # Create custom resampling
  train_sets <- lapply(borg_cv_obj$folds, function(f) f$train)
  test_sets <- lapply(borg_cv_obj$folds, function(f) f$test)

  mlr3::rsmp("custom")$instantiate(
    mlr3::TaskClassif$new(id = "temp", backend = data, target = names(data)[1]),
    train_sets = train_sets,
    test_sets = test_sets
  )
}
