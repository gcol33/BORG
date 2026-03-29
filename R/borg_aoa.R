# ===========================================================================
# borg_di() — Dissimilarity Index
# borg_aoa() — Area of Applicability (Meyer & Pebesma 2021)
# ===========================================================================

#' Dissimilarity Index
#'
#' Computes the weighted Euclidean distance in feature space from each
#' point to its nearest training observation. Points with high DI are
#' dissimilar to the training data and predictions may be unreliable.
#'
#' @param train Data frame of training predictors (or full data with
#'   \code{train_idx}).
#' @param new Data frame of new/prediction locations with the same
#'   predictor columns. If \code{NULL}, computes DI for training data
#'   (leave-one-out).
#' @param train_idx Integer vector. If provided, \code{train} is the full
#'   dataset and \code{train_idx} selects training rows.
#' @param predictors Character vector. Predictor column names. If \code{NULL},
#'   uses all shared numeric columns.
#' @param weights Numeric vector. Variable importance weights (length =
#'   number of predictors). If \code{NULL}, all variables weighted equally.
#'   Typically from \code{\link{borg_importance}()}.
#'
#' @return A numeric vector of DI values (one per row of \code{new}, or
#'   per training row if \code{new} is NULL). Has class
#'   \code{"borg_di"} and attribute \code{"threshold"} (mean + sd of
#'   training DI, used as default AOA cutoff).
#'
#' @references
#' Meyer, H., & Pebesma, E. (2021). Predicting into unknown space?
#' Estimating the area of applicability of spatial prediction models.
#' \emph{Methods in Ecology and Evolution}, 12(9), 1620-1633.
#' \doi{10.1111/2041-210X.13650}
#'
#' @examples
#' set.seed(42)
#' train <- data.frame(a = rnorm(80), b = rnorm(80))
#' new <- data.frame(a = rnorm(20, mean = 3), b = rnorm(20))
#' di <- borg_di(train, new)
#' summary(di)
#'
#' @export
borg_di <- function(train, new = NULL, train_idx = NULL,
                      predictors = NULL, weights = NULL) {

  # Extract training data
  if (!is.null(train_idx)) {
    train_data <- train[train_idx, , drop = FALSE]
    if (is.null(new)) new <- train[-train_idx, , drop = FALSE]
  } else {
    train_data <- train
  }

  # Determine predictors
  if (is.null(predictors)) {
    if (!is.null(new)) {
      shared <- intersect(names(train_data), names(new))
    } else {
      shared <- names(train_data)
    }
    predictors <- shared[vapply(train_data[shared], is.numeric, logical(1))]
  }

  if (length(predictors) == 0) {
    stop("No numeric predictor columns found")
  }

  # Scale training data
  train_mat <- as.matrix(train_data[, predictors, drop = FALSE])
  train_means <- colMeans(train_mat, na.rm = TRUE)
  train_sds <- apply(train_mat, 2, stats::sd, na.rm = TRUE)
  train_sds[train_sds == 0] <- 1  # avoid division by zero

  train_scaled <- scale(train_mat, center = train_means, scale = train_sds)

  # Apply importance weights
  if (!is.null(weights)) {
    if (length(weights) != length(predictors)) {
      stop(sprintf("weights length (%d) must match predictors (%d)",
                   length(weights), length(predictors)))
    }
    w <- weights / sum(weights) * length(weights)  # normalize
    train_scaled <- sweep(train_scaled, 2, sqrt(w), "*")
  } else {
    w <- rep(1, length(predictors))
  }

  # Compute DI for new data or LOO for training
  if (!is.null(new)) {
    new_mat <- as.matrix(new[, predictors, drop = FALSE])
    new_scaled <- scale(new_mat, center = train_means, scale = train_sds)
    if (!is.null(weights)) {
      new_scaled <- sweep(new_scaled, 2, sqrt(w), "*")
    }

    # Nearest training point distance for each new point
    di_values <- apply(new_scaled, 1, function(row) {
      dists <- sqrt(colSums((t(train_scaled) - row)^2))
      min(dists, na.rm = TRUE)
    })
  } else {
    # LOO: distance to nearest other training point
    di_values <- apply(train_scaled, 1, function(row) {
      dists <- sqrt(colSums((t(train_scaled) - row)^2))
      dists[dists == 0] <- Inf  # exclude self
      min(dists, na.rm = TRUE)
    })
  }

  # Training DI distribution (LOO)
  train_di <- apply(train_scaled, 1, function(row) {
    dists <- sqrt(colSums((t(train_scaled) - row)^2))
    dists[dists == 0] <- Inf
    min(dists, na.rm = TRUE)
  })

  threshold <- mean(train_di) + stats::sd(train_di)

  class(di_values) <- c("borg_di", "numeric")
  attr(di_values, "threshold") <- threshold
  attr(di_values, "train_di_mean") <- mean(train_di)
  attr(di_values, "train_di_sd") <- stats::sd(train_di)
  attr(di_values, "predictors") <- predictors

  di_values
}


#' Area of Applicability
#'
#' Determines where a spatial prediction model can be trusted, following
#' Meyer & Pebesma (2021). Combines the dissimilarity index (DI) with
#' a threshold derived from cross-validated DI values to produce a
#' binary applicability mask.
#'
#' @param train Data frame of training data.
#' @param new Data frame of prediction locations (same predictor columns).
#' @param predictors Character vector. Predictor column names.
#' @param coords Character vector of length 2. Coordinate columns in
#'   \code{new} for spatial mapping. Optional.
#' @param weights Numeric vector. Variable importance weights.
#' @param folds Optional \code{borg_cv} object or fold list. If provided,
#'   the threshold is derived from cross-validated DI (more robust).
#' @param threshold Numeric. Manual DI threshold override. If \code{NULL},
#'   computed from training data (mean + sd of LOO-DI).
#'
#' @return A data frame with class \code{"borg_aoa"} containing:
#'   \describe{
#'     \item{di}{Dissimilarity index for each prediction point}
#'     \item{aoa}{Logical. TRUE if inside AOA (DI <= threshold)}
#'     \item{x, y}{Coordinates (if \code{coords} provided)}
#'   }
#'   Has \code{autoplot()} method showing the AOA map.
#'
#' @references
#' Meyer, H., & Pebesma, E. (2021). Predicting into unknown space?
#' Estimating the area of applicability of spatial prediction models.
#' \emph{Methods in Ecology and Evolution}, 12(9), 1620-1633.
#' \doi{10.1111/2041-210X.13650}
#'
#' @examples
#' set.seed(42)
#' train <- data.frame(x = runif(80, 0, 50), y = runif(80, 0, 50),
#'                      a = rnorm(80), b = rnorm(80))
#' pred <- data.frame(x = runif(200, 0, 100), y = runif(200, 0, 100),
#'                     a = rnorm(200), b = rnorm(200))
#' aoa <- borg_aoa(train, pred, predictors = c("a", "b"), coords = c("x", "y"))
#' table(aoa$aoa)
#'
#' @export
borg_aoa <- function(train, new, predictors = NULL, coords = NULL,
                       weights = NULL, folds = NULL, threshold = NULL) {

  # Compute DI
  di_vals <- borg_di(train, new, predictors = predictors, weights = weights)

  # Determine threshold
  if (is.null(threshold)) {
    if (!is.null(folds)) {
      # Cross-validated threshold: compute DI on test sets using train-only scaling
      fold_list <- if (inherits(folds, "borg_cv")) folds$folds else folds
      cv_di <- numeric(0)

      if (is.null(predictors)) {
        shared <- intersect(names(train), names(new))
        predictors <- shared[vapply(train[shared], is.numeric, logical(1))]
      }

      for (fold in fold_list) {
        fold_di <- borg_di(train[fold$train, , drop = FALSE],
                            train[fold$test, , drop = FALSE],
                            predictors = predictors, weights = weights)
        cv_di <- c(cv_di, as.numeric(fold_di))
      }
      threshold <- stats::quantile(cv_di, 0.95)
    } else {
      threshold <- attr(di_vals, "threshold")
    }
  }

  # Compute Local Point Density (LPD)
  # Number of training points within the AOA threshold in feature space
  if (is.null(predictors)) {
    shared <- intersect(names(train), names(new))
    predictors <- shared[vapply(train[shared], is.numeric, logical(1))]
  }
  train_mat <- as.matrix(train[, predictors, drop = FALSE])
  train_means <- colMeans(train_mat, na.rm = TRUE)
  train_sds <- apply(train_mat, 2, stats::sd, na.rm = TRUE)
  train_sds[train_sds == 0] <- 1
  train_scaled <- scale(train_mat, center = train_means, scale = train_sds)

  new_mat <- as.matrix(new[, predictors, drop = FALSE])
  new_scaled <- scale(new_mat, center = train_means, scale = train_sds)

  lpd <- apply(new_scaled, 1, function(row) {
    dists <- sqrt(colSums((t(train_scaled) - row)^2))
    sum(dists <= threshold)
  })

  # Build result
  result <- data.frame(
    di = as.numeric(di_vals),
    lpd = lpd,
    aoa = as.numeric(di_vals) <= threshold,
    stringsAsFactors = FALSE
  )

  if (!is.null(coords) && all(coords %in% names(new))) {
    result$x <- new[[coords[1]]]
    result$y <- new[[coords[2]]]
  }

  attr(result, "threshold") <- threshold
  attr(result, "predictors") <- attr(di_vals, "predictors")
  attr(result, "coord_names") <- coords
  class(result) <- c("borg_aoa", "data.frame")

  result
}


#' @export
print.borg_aoa <- function(x, ...) {
  n <- nrow(x)
  n_inside <- sum(x$aoa)
  cat("BORG Area of Applicability\n")
  cat(sprintf("  %d / %d points inside AOA (%.1f%%)\n",
              n_inside, n, 100 * n_inside / n))
  cat(sprintf("  DI threshold: %.3f\n", attr(x, "threshold")))
  cat(sprintf("  DI range: [%.3f, %.3f]\n", min(x$di), max(x$di)))
  invisible(x)
}


#' @exportS3Method ggplot2::autoplot
autoplot.borg_aoa <- function(object, ...) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package 'ggplot2' required")
  }

  has_coords <- "x" %in% names(object)
  coord_names <- attr(object, "coord_names") %||% c("x", "y")
  threshold <- attr(object, "threshold")

  if (has_coords) {
    ggplot2::ggplot(object, ggplot2::aes(x = .data$x, y = .data$y)) +
      ggplot2::geom_point(
        ggplot2::aes(color = .data$di, shape = .data$aoa),
        size = 1.5, alpha = 0.8
      ) +
      ggplot2::scale_color_viridis_c(option = "magma", direction = -1) +
      ggplot2::scale_shape_manual(
        values = c("TRUE" = 16, "FALSE" = 4),
        labels = c("TRUE" = "Inside AOA", "FALSE" = "Outside AOA")
      ) +
      ggplot2::coord_equal() +
      ggplot2::labs(
        title = "Area of Applicability",
        subtitle = sprintf("%.1f%% inside AOA | DI threshold = %.3f",
                            100 * mean(object$aoa), threshold),
        x = coord_names[1], y = coord_names[2],
        color = "DI", shape = NULL
      ) +
      borg_theme()
  } else {
    ggplot2::ggplot(object, ggplot2::aes(x = .data$di)) +
      ggplot2::geom_histogram(
        ggplot2::aes(fill = .data$aoa),
        bins = 30, alpha = 0.7, color = "white", linewidth = 0.2
      ) +
      ggplot2::geom_vline(xintercept = threshold, linetype = "dashed",
                           color = "#C0392B", linewidth = 0.8) +
      ggplot2::annotate("text", x = threshold, y = Inf,
                         label = sprintf("threshold = %.3f", threshold),
                         hjust = -0.1, vjust = 2, size = 3, color = "#C0392B") +
      ggplot2::scale_fill_manual(
        values = c("TRUE" = "#27AE60", "FALSE" = "#E74C3C"),
        labels = c("TRUE" = "Inside AOA", "FALSE" = "Outside")
      ) +
      ggplot2::labs(
        title = "Dissimilarity Index Distribution",
        subtitle = sprintf("%.1f%% inside AOA", 100 * mean(object$aoa)),
        x = "Dissimilarity Index", y = "Count", fill = NULL
      ) +
      borg_theme()
  }
}
