# ===========================================================================
# borg_geodist() — Geographic and feature-space distance distributions
# ===========================================================================

#' Distance Distribution Diagnostics
#'
#' Computes nearest-neighbor distance distributions in geographic space
#' and/or feature space between training data, CV test sets, and optional
#' prediction locations. Diagnostic for whether CV folds are representative
#' of the prediction task.
#'
#' If the CV test-to-train distance distribution differs strongly from the
#' prediction-to-train distribution, the CV does not mimic the real
#' prediction scenario and performance estimates may be misleading.
#'
#' @param data Data frame of training/modelling data.
#' @param folds A \code{borg_cv} object or fold list.
#' @param prediction_points Optional data frame of prediction locations.
#' @param coords Character vector of length 2. Coordinate columns for
#'   geographic distance. If \code{NULL}, only feature-space distances
#'   are computed.
#' @param predictors Character vector. Feature columns for feature-space
#'   distance. If \code{NULL}, uses all shared numeric columns excluding
#'   coords.
#' @param type Character. \code{"geo"} (geographic only), \code{"feature"}
#'   (feature space only), or \code{"both"} (default).
#'
#' @return A list with class \code{"borg_geodist"} containing:
#'   \describe{
#'     \item{cv_distances}{Data frame of NN distances: test-to-train per fold}
#'     \item{prediction_distances}{NN distances: prediction-to-train (if provided)}
#'     \item{sample_distances}{NN distances: within training data (reference)}
#'     \item{ks_statistic}{KS test statistic comparing CV vs prediction distances}
#'   }
#'   Has an \code{autoplot()} method showing overlaid density curves.
#'
#' @references
#' Meyer, H., & Pebesma, E. (2022). Machine learning-based global maps of
#' ecological variables and the challenge of assessing them.
#' \emph{Nature Communications}, 13, 2208.
#' \doi{10.1038/s41467-022-29838-9}
#'
#' @examples
#' set.seed(42)
#' d <- data.frame(x = runif(100, 0, 50), y = runif(100, 0, 50),
#'                  a = rnorm(100), z = rnorm(100))
#' cv <- borg_cv(d, coords = c("x", "y"), target = "z")
#' pred <- data.frame(x = runif(50, 0, 100), y = runif(50, 0, 100),
#'                     a = rnorm(50))
#' gd <- borg_geodist(d, cv, prediction_points = pred, coords = c("x", "y"))
#' gd
#'
#' @export
borg_geodist <- function(data, folds, prediction_points = NULL,
                           coords = NULL, predictors = NULL,
                           type = c("both", "geo", "feature")) {
  type <- match.arg(type)
  fold_list <- if (inherits(folds, "borg_cv")) folds$folds else folds

  exclude <- coords
  if (is.null(predictors)) {
    cols <- names(data)
    if (!is.null(prediction_points)) cols <- intersect(cols, names(prediction_points))
    predictors <- setdiff(cols, exclude)
    predictors <- predictors[vapply(data[predictors], is.numeric, logical(1))]
  }

  results <- list()

  # --- Geographic distances ---
  if (type %in% c("both", "geo") && !is.null(coords)) {
    x_all <- data[[coords[1]]]
    y_all <- data[[coords[2]]]

    # CV test-to-train distances (per fold)
    cv_geo <- numeric(0)
    for (fold in fold_list) {
      for (ti in fold$test) {
        dists <- sqrt((x_all[ti] - x_all[fold$train])^2 +
                       (y_all[ti] - y_all[fold$train])^2)
        cv_geo <- c(cv_geo, min(dists))
      }
    }

    # Within-training NN distances (reference)
    sample_geo <- .nn_distances_xy(x_all, y_all, max_n = 1000)

    # Prediction-to-train distances
    pred_geo <- NULL
    if (!is.null(prediction_points) && all(coords %in% names(prediction_points))) {
      xp <- prediction_points[[coords[1]]]
      yp <- prediction_points[[coords[2]]]
      pred_geo <- vapply(seq_along(xp), function(i) {
        min(sqrt((xp[i] - x_all)^2 + (yp[i] - y_all)^2))
      }, numeric(1))
    }

    results$geo <- list(
      cv = cv_geo, prediction = pred_geo, sample = sample_geo
    )
  }

  # --- Feature-space distances ---
  if (type %in% c("both", "feature") && length(predictors) > 0) {
    train_mat <- scale(as.matrix(data[, predictors, drop = FALSE]))
    center <- attr(train_mat, "scaled:center")
    sc <- attr(train_mat, "scaled:scale")

    cv_feat <- numeric(0)
    for (fold in fold_list) {
      train_sub <- train_mat[fold$train, , drop = FALSE]
      for (ti in fold$test) {
        dists <- sqrt(colSums((t(train_sub) - train_mat[ti, ])^2))
        cv_feat <- c(cv_feat, min(dists))
      }
    }

    sample_feat <- .nn_distances_mat(train_mat, max_n = 1000)

    pred_feat <- NULL
    if (!is.null(prediction_points) && all(predictors %in% names(prediction_points))) {
      pred_mat <- scale(as.matrix(prediction_points[, predictors, drop = FALSE]),
                         center = center, scale = sc)
      pred_feat <- apply(pred_mat, 1, function(row) {
        min(sqrt(colSums((t(train_mat) - row)^2)))
      })
    }

    results$feature <- list(
      cv = cv_feat, prediction = pred_feat, sample = sample_feat
    )
  }

  # KS statistics
  ks_geo <- ks_feat <- NA_real_
  if (!is.null(results$geo$prediction)) {
    ks_geo <- suppressWarnings(
      stats::ks.test(results$geo$cv, results$geo$prediction)$statistic
    )
  }
  if (!is.null(results$feature$prediction)) {
    ks_feat <- suppressWarnings(
      stats::ks.test(results$feature$cv, results$feature$prediction)$statistic
    )
  }

  result <- c(results, list(
    ks_geo = ks_geo, ks_feature = ks_feat, type = type
  ))
  class(result) <- c("borg_geodist", "list")
  result
}


#' @noRd
.nn_distances_xy <- function(x, y, max_n = 1000) {
  n <- length(x)
  if (n > max_n) {
    idx <- sample(n, max_n)
    x <- x[idx]; y <- y[idx]; n <- max_n
  }
  vapply(seq_len(n), function(i) {
    dists <- sqrt((x[i] - x[-i])^2 + (y[i] - y[-i])^2)
    min(dists)
  }, numeric(1))
}

#' @noRd
.nn_distances_mat <- function(mat, max_n = 1000) {
  n <- nrow(mat)
  if (n > max_n) {
    idx <- sample(n, max_n)
    mat <- mat[idx, , drop = FALSE]; n <- max_n
  }
  vapply(seq_len(n), function(i) {
    dists <- sqrt(colSums((t(mat[-i, , drop = FALSE]) - mat[i, ])^2))
    min(dists)
  }, numeric(1))
}


#' @export
print.borg_geodist <- function(x, ...) {
  cat("BORG Distance Distribution Diagnostic\n")
  if (!is.null(x$geo)) {
    cat(sprintf("  Geographic: CV median=%.2f", stats::median(x$geo$cv)))
    if (!is.null(x$geo$prediction))
      cat(sprintf(", Prediction median=%.2f, KS=%.3f", stats::median(x$geo$prediction), x$ks_geo))
    cat("\n")
  }
  if (!is.null(x$feature)) {
    cat(sprintf("  Feature:    CV median=%.2f", stats::median(x$feature$cv)))
    if (!is.null(x$feature$prediction))
      cat(sprintf(", Prediction median=%.2f, KS=%.3f", stats::median(x$feature$prediction), x$ks_feature))
    cat("\n")
  }
  invisible(x)
}


#' @exportS3Method ggplot2::autoplot
autoplot.borg_geodist <- function(object, space = c("geo", "feature"), ...) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) stop("ggplot2 required")
  space <- match.arg(space)

  dists <- object[[space]]
  if (is.null(dists)) stop(sprintf("No %s distances computed", space))

  # Build long data frame
  rows <- list(
    data.frame(distance = dists$sample, group = "Within training", stringsAsFactors = FALSE),
    data.frame(distance = dists$cv, group = "CV test-to-train", stringsAsFactors = FALSE)
  )
  if (!is.null(dists$prediction)) {
    rows <- c(rows, list(
      data.frame(distance = dists$prediction, group = "Prediction-to-train",
                  stringsAsFactors = FALSE)
    ))
  }
  df <- do.call(rbind, rows)
  df$group <- factor(df$group, levels = c("Within training", "CV test-to-train",
                                            "Prediction-to-train"))

  colors <- c("Within training" = "gray60",
               "CV test-to-train" = "#2C3E50",
               "Prediction-to-train" = "#C0392B")

  space_label <- if (space == "geo") "Geographic" else "Feature-space"
  ks_val <- if (space == "geo") object$ks_geo else object$ks_feature
  ks_text <- if (!is.na(ks_val)) sprintf(" | KS = %.3f", ks_val) else ""

  ggplot2::ggplot(df, ggplot2::aes(x = .data$distance, color = .data$group,
                                     fill = .data$group)) +
    ggplot2::geom_density(alpha = 0.2, linewidth = 0.8) +
    ggplot2::scale_color_manual(values = colors) +
    ggplot2::scale_fill_manual(values = colors) +
    ggplot2::labs(
      title = sprintf("%s NN Distance Distribution", space_label),
      subtitle = sprintf("CV should match prediction distribution%s", ks_text),
      x = "Nearest-neighbor distance", y = "Density",
      color = NULL, fill = NULL
    ) +
    borg_theme()
}
