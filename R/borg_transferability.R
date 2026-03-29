# ===========================================================================
# borg_transferability() — Geographic transferability assessment
# ===========================================================================

#' Assess Geographic Transferability
#'
#' Evaluates how well a model transfers across geographic regions by
#' splitting data into spatial zones, training on each zone, and testing
#' on all others. Quantifies performance decay with geographic distance.
#'
#' @param data Data frame with coordinates and predictors.
#' @param formula Model formula.
#' @param coords Character vector of length 2. Coordinate columns.
#' @param n_regions Integer. Number of geographic regions to create.
#'   Default: 4.
#' @param metric Character. Default: \code{"rmse"}.
#' @param fit_fun Function. Default: \code{lm}.
#'
#' @return A list with class \code{"borg_transferability"} containing:
#'   \describe{
#'     \item{matrix}{Performance matrix (n_regions x n_regions): entry
#'       (i, j) = metric when training on region i, testing on region j}
#'     \item{distance_matrix}{Geographic distance between region centroids}
#'     \item{decay}{Data frame: distance, metric_value for plotting decay}
#'     \item{mean_transfer}{Mean cross-region performance}
#'     \item{mean_within}{Mean within-region performance}
#'   }
#'
#' @examples
#' set.seed(42)
#' d <- data.frame(x = runif(200, 0, 100), y = runif(200, 0, 100),
#'                  a = rnorm(200))
#' d$z <- d$a * 2 + sin(d$x / 20) + rnorm(200, sd = 0.5)
#' tf <- borg_transferability(d, z ~ a, coords = c("x", "y"), n_regions = 4)
#' tf
#'
#' @export
borg_transferability <- function(data, formula, coords,
                                   n_regions = 4L,
                                   metric = c("rmse", "mae", "rsq"),
                                   fit_fun = stats::lm) {
  metric <- match.arg(metric)
  target_var <- all.vars(formula)[1]
  n <- nrow(data)

  x <- data[[coords[1]]]
  y <- data[[coords[2]]]

  # Create spatial regions via k-means on coordinates
  coord_mat <- cbind(x, y)
  set.seed(42)
  km <- stats::kmeans(coord_mat, centers = n_regions, nstart = 10)
  regions <- km$cluster
  centroids <- km$centers

  # Distance between region centroids
  dist_mat <- as.matrix(stats::dist(centroids))

  # Performance matrix: train on region i, test on region j
  perf_mat <- matrix(NA_real_, n_regions, n_regions)
  dimnames(perf_mat) <- list(
    train = paste0("R", seq_len(n_regions)),
    test = paste0("R", seq_len(n_regions))
  )

  for (i in seq_len(n_regions)) {
    train_idx <- which(regions == i)
    train_data <- data[train_idx, , drop = FALSE]

    model <- tryCatch(fit_fun(formula, data = train_data), error = function(e) NULL)
    if (is.null(model)) next

    for (j in seq_len(n_regions)) {
      test_idx <- which(regions == j)
      test_data <- data[test_idx, , drop = FALSE]

      preds <- tryCatch(stats::predict(model, newdata = test_data),
                         error = function(e) rep(NA_real_, nrow(test_data)))
      actual <- test_data[[target_var]]

      perf_mat[i, j] <- .classify_metric(actual, preds, metric)
    }
  }

  # Build distance-performance decay data
  decay <- data.frame(
    train_region = rep(seq_len(n_regions), each = n_regions),
    test_region = rep(seq_len(n_regions), n_regions),
    distance = as.numeric(dist_mat),
    metric_value = as.numeric(perf_mat),
    stringsAsFactors = FALSE
  )
  decay <- decay[!is.na(decay$metric_value), ]
  decay$is_within <- decay$train_region == decay$test_region

  # Summary stats
  within_vals <- diag(perf_mat)
  cross_vals <- perf_mat[row(perf_mat) != col(perf_mat)]

  result <- list(
    matrix = perf_mat,
    distance_matrix = dist_mat,
    decay = decay,
    mean_within = mean(within_vals, na.rm = TRUE),
    mean_transfer = mean(cross_vals, na.rm = TRUE),
    metric = metric,
    n_regions = n_regions,
    centroids = centroids
  )
  class(result) <- c("borg_transferability", "list")
  result
}


#' @export
print.borg_transferability <- function(x, ...) {
  cat("BORG Geographic Transferability\n")
  cat(sprintf("  %d regions\n", x$n_regions))
  cat(sprintf("  Within-region %s: %.4f\n", toupper(x$metric), x$mean_within))
  cat(sprintf("  Cross-region %s:  %.4f\n", toupper(x$metric), x$mean_transfer))
  cat(sprintf("  Transfer ratio: %.2f\n",
              if (x$mean_within != 0) x$mean_transfer / x$mean_within else NA))
  cat("\nPerformance matrix (rows=train, cols=test):\n")
  print(round(x$matrix, 4))
  invisible(x)
}


#' @exportS3Method ggplot2::autoplot
autoplot.borg_transferability <- function(object, type = c("decay", "matrix"), ...) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) stop("ggplot2 required")
  type <- match.arg(type)
  met <- toupper(object$metric)

  if (type == "decay") {
    df <- object$decay

    ggplot2::ggplot(df, ggplot2::aes(x = .data$distance, y = .data$metric_value)) +
      ggplot2::geom_point(
        ggplot2::aes(shape = .data$is_within),
        size = 3, alpha = 0.7, color = "#2C3E50"
      ) +
      ggplot2::geom_smooth(method = "lm", se = TRUE, color = "#C0392B",
                            linewidth = 0.8, alpha = 0.2) +
      ggplot2::scale_shape_manual(
        values = c("TRUE" = 16, "FALSE" = 1),
        labels = c("TRUE" = "Within-region", "FALSE" = "Cross-region")
      ) +
      ggplot2::labs(
        title = "Transferability Decay",
        subtitle = sprintf("Within: %.4f | Cross: %.4f",
                            object$mean_within, object$mean_transfer),
        x = "Distance between region centroids",
        y = met, shape = NULL
      ) +
      borg_theme()
  } else {
    # Heatmap
    df <- object$decay
    df$train_label <- paste0("R", df$train_region)
    df$test_label <- paste0("R", df$test_region)

    ggplot2::ggplot(df, ggplot2::aes(x = .data$test_label, y = .data$train_label,
                                       fill = .data$metric_value)) +
      ggplot2::geom_tile(color = "white", linewidth = 0.5) +
      ggplot2::geom_text(
        ggplot2::aes(label = sprintf("%.3f", .data$metric_value)),
        size = 3.5
      ) +
      ggplot2::scale_fill_viridis_c(option = "viridis") +
      ggplot2::labs(
        title = "Transfer Performance Matrix",
        subtitle = "Rows = training region, Columns = test region",
        x = "Test region", y = "Train region", fill = met
      ) +
      borg_theme() +
      ggplot2::theme(panel.grid = ggplot2::element_blank())
  }
}
