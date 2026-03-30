# ===========================================================================
# borg_disc_cv() — Leave-disc-out cross-validation
# ===========================================================================

#' Leave-Disc-Out Cross-Validation
#'
#' Spatial cross-validation with circular exclusion buffers around test
#' points. For each fold, all training points within a specified radius
#' of any test point are removed, creating a spatial gap that eliminates
#' autocorrelation leakage. More principled than rectangular or k-means
#' blocking for point data.
#'
#' @param data A data frame with coordinate and predictor columns.
#' @param coords Character vector of length 2. Coordinate column names.
#' @param target Character. Target variable name (for diagnostics).
#' @param radius Numeric. Exclusion buffer radius in coordinate units.
#'   If \code{NULL}, automatically set to the autocorrelation range
#'   estimated by \code{\link{borg_diagnose}()}.
#' @param v Integer. Number of folds. Default: 5.
#' @param min_train Numeric. Minimum fraction of data that must remain
#'   in training after exclusion. Folds that violate this are dropped.
#'   Default: 0.5.
#' @param seed Integer. Random seed. Default: 42.
#' @param output Character. Output format: \code{"list"} (default),
#'   \code{"rsample"}.
#' @param verbose Logical. Print diagnostic messages. Default: FALSE.
#'
#' @return A list with class \code{"borg_disc_cv"} containing:
#'   \describe{
#'     \item{folds}{List of train/test index vectors}
#'     \item{radius}{Exclusion radius used}
#'     \item{n_excluded}{Number of training points excluded per fold
#'       (in the buffer zone)}
#'     \item{effective_training}{Fraction of data available for training
#'       per fold (after exclusion)}
#'     \item{strategy}{"leave_disc_out"}
#'     \item{params}{List of parameters used}
#'   }
#'   Compatible with other BORG functions that accept fold lists.
#'
#' @details
#' \subsection{How it works}{
#' \enumerate{
#'   \item Partition data into \code{v} spatial folds using k-means on
#'     coordinates (same as \code{borg_cv}).
#'   \item For each fold, identify the test points.
#'   \item Remove from training all points within \code{radius} of any
#'     test point. These excluded points are neither train nor test —
#'     they form a buffer zone.
#'   \item If the remaining training set is too small
#'     (\code{< min_train * n}), the fold is dropped with a warning.
#' }
#' }
#'
#' \subsection{Choosing the radius}{
#' The radius should match the autocorrelation range of the spatial
#' process. \code{borg_diagnose()} estimates this via variogram
#' analysis. Setting \code{radius = NULL} uses this estimate
#' automatically.
#' }
#'
#' @examples
#' set.seed(42)
#' d <- data.frame(x = runif(200, 0, 100), y = runif(200, 0, 100))
#' d$z <- sin(d$x / 10) + rnorm(200, sd = 0.5)
#' cv <- borg_disc_cv(d, coords = c("x", "y"), target = "z", radius = 15)
#' cv
#'
#' @export
borg_disc_cv <- function(data, coords, target = NULL, radius = NULL,
                           v = 5, min_train = 0.5, seed = 42,
                           output = c("list", "rsample"),
                           verbose = FALSE) {

  output <- match.arg(output)
  set.seed(seed)

  if (length(coords) != 2 || !all(coords %in% names(data))) {
    stop("coords must be a character vector of length 2 naming columns in data")
  }

  n <- nrow(data)
  s1 <- data[[coords[1]]]
  s2 <- data[[coords[2]]]
  coord_mat <- cbind(s1, s2)

  # Auto-detect radius from autocorrelation range
  if (is.null(radius)) {
    if (!is.null(target) && target %in% names(data)) {
      diag <- borg_diagnose(data, coords = coords, target = target)
      range_est <- diag@spatial$range_estimate
      if (!is.null(range_est) && !is.na(range_est) && range_est > 0) {
        radius <- range_est
        if (verbose) message(sprintf("Auto-detected radius: %.2f", radius))
      }
    }
    if (is.null(radius)) {
      # Fallback: 10% of domain extent
      extent <- sqrt((max(s1) - min(s1))^2 + (max(s2) - min(s2))^2)
      radius <- extent * 0.10
      if (verbose) message(sprintf("Fallback radius (10%% extent): %.2f", radius))
    }
  }

  # Step 1: Create base folds via k-means spatial blocking
  n_blocks <- min(v, n %/% 5)
  if (n_blocks < 2) n_blocks <- 2

  km <- stats::kmeans(coord_mat, centers = n_blocks, nstart = 10)
  base_folds <- km$cluster

  unique_folds <- sort(unique(base_folds))

  # Step 2: For each fold, apply circular buffer exclusion
  folds <- list()
  n_excluded <- integer(0)
  effective_training <- numeric(0)
  dropped <- integer(0)

 for (f in unique_folds) {
    test_idx <- which(base_folds == f)
    test_coords <- coord_mat[test_idx, , drop = FALSE]

    # Compute distance from each non-test point to nearest test point
    other_idx <- which(base_folds != f)

    if (length(other_idx) == 0) {
      dropped <- c(dropped, f)
      next
    }

    # Distance matrix: other_points x test_points
    min_dists <- apply(coord_mat[other_idx, , drop = FALSE], 1, function(pt) {
      dists <- sqrt((test_coords[, 1] - pt[1])^2 + (test_coords[, 2] - pt[2])^2)
      min(dists)
    })

    # Exclude points within radius
    in_buffer <- min_dists <= radius
    train_idx <- other_idx[!in_buffer]
    buffer_idx <- other_idx[in_buffer]

    # Check minimum training size
    if (length(train_idx) < min_train * n) {
      if (verbose) {
        message(sprintf("Fold %d: only %.1f%% training after exclusion (< %.0f%%). Dropped.",
                        f, 100 * length(train_idx) / n, min_train * 100))
      }
      dropped <- c(dropped, f)
      next
    }

    folds[[length(folds) + 1]] <- list(
      train = train_idx,
      test = test_idx,
      excluded = buffer_idx
    )
    n_excluded <- c(n_excluded, length(buffer_idx))
    effective_training <- c(effective_training, length(train_idx) / n)
  }

  if (length(folds) == 0) {
    stop("All folds dropped. Reduce radius or min_train.")
  }

  if (length(dropped) > 0 && verbose) {
    message(sprintf("%d fold(s) dropped due to insufficient training data.",
                    length(dropped)))
  }

  result <- list(
    folds = folds,
    n = n,
    radius = radius,
    n_excluded = n_excluded,
    effective_training = effective_training,
    n_folds = length(folds),
    n_dropped = length(dropped),
    strategy = "leave_disc_out",
    params = list(v = v, radius = radius, min_train = min_train, seed = seed)
  )

  class(result) <- c("borg_disc_cv", "borg_cv", "list")

  if (output == "rsample") {
    return(.disc_cv_to_rsample(result, data))
  }

  result
}


# Internal: convert to rsample -------------------------------------------
.disc_cv_to_rsample <- function(cv_obj, data) {
  if (!requireNamespace("rsample", quietly = TRUE)) {
    stop("Package 'rsample' required for output = 'rsample'")
  }
  # Return as list with note — rsample custom_rsample not always available
  warning("Returning list format. Convert manually if needed.")
  cv_obj
}


#' @export
print.borg_disc_cv <- function(x, ...) {
  cat("BORG Leave-Disc-Out Cross-Validation\n")
  cat("=====================================\n\n")
  cat(sprintf("  Folds: %d (of %d requested)\n", x$n_folds,
              x$n_folds + x$n_dropped))
  cat(sprintf("  Exclusion radius: %.2f\n", x$radius))
  cat(sprintf("  Mean points excluded per fold: %.0f (%.1f%%)\n",
              mean(x$n_excluded),
              mean(x$n_excluded) / length(x$folds[[1]]$train) * 100))
  cat(sprintf("  Effective training fraction: %.1f%% - %.1f%%\n",
              min(x$effective_training) * 100,
              max(x$effective_training) * 100))
  if (x$n_dropped > 0) {
    cat(sprintf("  Folds dropped (too little training): %d\n", x$n_dropped))
  }
  invisible(x)
}


#' @exportS3Method ggplot2::autoplot
autoplot.borg_disc_cv <- function(object, data = NULL, coords = NULL,
                                    fold = 1, ...) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package 'ggplot2' required")
  }

  if (is.null(data) || is.null(coords)) {
    # Fallback: bar chart of exclusion counts
    df <- data.frame(
      fold = seq_along(object$n_excluded),
      excluded = object$n_excluded,
      training_frac = object$effective_training
    )
    return(
      ggplot2::ggplot(df, ggplot2::aes(x = factor(.data$fold),
                                        y = .data$training_frac)) +
        ggplot2::geom_col(fill = "#2C3E50", alpha = 0.7, width = 0.6) +
        ggplot2::geom_text(ggplot2::aes(label = sprintf("-%d", .data$excluded)),
                            vjust = -0.5, size = 3, color = "#E74C3C") +
        ggplot2::labs(
          title = "Leave-Disc-Out: Training Data Retained",
          subtitle = sprintf("Radius = %.2f | Red = points excluded",
                              object$radius),
          x = "Fold", y = "Training fraction"
        ) +
        borg_theme()
    )
  }

  # Spatial map of a specific fold
  fold <- min(fold, length(object$folds))
  f <- object$folds[[fold]]

  n <- nrow(data)
  role <- rep("excluded", n)
  role[f$train] <- "train"
  role[f$test] <- "test"
  if (!is.null(f$excluded)) role[f$excluded] <- "buffer"

  df <- data.frame(
    x = data[[coords[1]]],
    y = data[[coords[2]]],
    role = factor(role, levels = c("train", "buffer", "test", "excluded"))
  )

  ggplot2::ggplot(df, ggplot2::aes(x = .data$x, y = .data$y,
                                    color = .data$role)) +
    ggplot2::geom_point(size = 1.5, alpha = 0.7) +
    ggplot2::scale_color_manual(
      values = c(train = "#1B4F72", buffer = "#E67E22",
                 test = "#C0392B", excluded = "#D5D8DC")
    ) +
    ggplot2::coord_equal() +
    ggplot2::labs(
      title = sprintf("Leave-Disc-Out: Fold %d", fold),
      subtitle = sprintf("Radius = %.2f | %d train, %d buffer, %d test",
                          object$radius, length(f$train),
                          length(f$excluded), length(f$test)),
      x = coords[1], y = coords[2], color = "Role"
    ) +
    borg_theme()
}
