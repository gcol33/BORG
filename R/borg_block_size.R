# ===========================================================================
# borg_block_size() — Data-driven block size optimization
# ===========================================================================

#' Optimize Spatial Block Size
#'
#' Tests multiple block sizes and selects the one that minimizes residual
#' spatial autocorrelation between CV folds. For each candidate size,
#' generates spatial block folds and computes the mean Moran's I of
#' residuals within test sets.
#'
#' @param data Data frame with coordinate and target columns.
#' @param coords Character vector of length 2. Coordinate column names.
#' @param target Character. Target variable name.
#' @param v Integer. Number of folds. Default: 5.
#' @param n_sizes Integer. Number of candidate block sizes to test.
#'   Default: 10.
#' @param range Numeric vector of length 2. Min and max block sizes to test.
#'   If \code{NULL}, automatically determined from the variogram range
#'   estimate (0.5x to 3x range).
#' @param formula Optional model formula for computing residual
#'   autocorrelation. If \code{NULL}, uses the raw target values.
#' @param verbose Logical. Default: FALSE.
#'
#' @return A list with class \code{"borg_block_opt"} containing:
#'   \describe{
#'     \item{optimal}{Optimal block size}
#'     \item{results}{Data frame with columns: block_size, mean_morans_i,
#'       mean_test_size, n_empty_folds}
#'     \item{range_estimate}{Variogram-based range estimate}
#'   }
#'   Has an \code{autoplot()} method showing the optimization curve.
#'
#' @examples
#' set.seed(42)
#' d <- data.frame(
#'   x = runif(200, 0, 100), y = runif(200, 0, 100),
#'   z = rnorm(200)
#' )
#' opt <- borg_block_size(d, coords = c("x", "y"), target = "z")
#' opt$optimal
#'
#' @export
borg_block_size <- function(data, coords, target, v = 5, n_sizes = 10,
                              range = NULL, formula = NULL, verbose = FALSE) {

  coord_info <- extract_coords(data, coords)
  x <- coord_info$x
  y <- coord_info$y
  n <- nrow(data)
  y_vals <- data[[target]]

  # Get variogram range estimate for setting search bounds
  complete <- complete.cases(x, y, y_vals)
  x_sub <- x[complete]
  y_sub <- y[complete]
  y_sub_val <- y_vals[complete]

  n_sub <- min(length(x_sub), 2000)
  if (n_sub < length(x_sub)) {
    idx <- sample(length(x_sub), n_sub)
    x_sub <- x_sub[idx]; y_sub <- y_sub[idx]; y_sub_val <- y_sub_val[idx]
  }

  dist_mat <- compute_distance_matrix(x_sub, y_sub)
  range_est <- estimate_spatial_range(y_sub_val, dist_mat)

  # Define search range
  if (is.null(range)) {
    x_extent <- diff(range(x, na.rm = TRUE))
    y_extent <- diff(range(y, na.rm = TRUE))
    max_extent <- max(x_extent, y_extent)
    range <- c(
      max(range_est * 0.5, max_extent / (v * 4)),
      min(range_est * 3, max_extent / 2)
    )
  }

  sizes <- seq(range[1], range[2], length.out = n_sizes)

  if (verbose) {
    message(sprintf("Testing %d block sizes from %.1f to %.1f (range est: %.1f)",
                     n_sizes, range[1], range[2], range_est))
  }

  # Evaluate each block size
  results <- lapply(sizes, function(bs) {
    folds <- tryCatch(
      generate_spatial_block_folds(data, v, new("BorgDiagnosis",
        dependency_type = "spatial", severity = "moderate",
        recommended_cv = "spatial_block",
        spatial = list(detected = TRUE, morans_i = NA_real_, morans_p = NA_real_,
                       range_estimate = bs, effective_n = as.numeric(n),
                       coords_used = coords),
        temporal = list(detected = FALSE), clustered = list(detected = FALSE),
        inflation_estimate = list(auc_inflation = 0, rmse_deflation = 0,
                                   confidence = "low", basis = "spatial"),
        n_obs = as.integer(n), timestamp = Sys.time(), call = match.call()
      ), coords, bs, verbose = FALSE),
      error = function(e) NULL
    )

    if (is.null(folds)) {
      return(data.frame(block_size = bs, mean_morans_i = NA_real_,
                         mean_test_size = NA_real_, n_empty_folds = NA_integer_,
                         stringsAsFactors = FALSE))
    }

    # Compute between-fold Moran's I on test residuals
    morans_vals <- vapply(folds, function(fold) {
      if (length(fold$test) < 10) return(NA_real_)

      test_y <- y_vals[fold$test]
      if (!is.null(formula)) {
        train_data <- data[fold$train, , drop = FALSE]
        test_data <- data[fold$test, , drop = FALSE]
        model <- tryCatch(stats::lm(formula, data = train_data), error = function(e) NULL)
        if (!is.null(model)) {
          preds <- tryCatch(stats::predict(model, newdata = test_data),
                             error = function(e) NULL)
          if (!is.null(preds)) test_y <- test_y - preds
        }
      }

      test_x_c <- x[fold$test]
      test_y_c <- y[fold$test]
      n_test <- length(test_y)
      if (n_test > 500) {
        idx <- sample(n_test, 500)
        test_y <- test_y[idx]; test_x_c <- test_x_c[idx]; test_y_c <- test_y_c[idx]
      }

      d <- compute_distance_matrix(test_x_c, test_y_c)
      tryCatch(compute_morans_i(test_y, d)$I, error = function(e) NA_real_)
    }, numeric(1))

    n_empty <- sum(vapply(folds, function(f) length(f$test) == 0, logical(1)))
    test_sizes <- vapply(folds, function(f) length(f$test), integer(1))

    data.frame(
      block_size = bs,
      mean_morans_i = mean(abs(morans_vals), na.rm = TRUE),
      mean_test_size = mean(test_sizes),
      n_empty_folds = n_empty,
      stringsAsFactors = FALSE
    )
  })

  results_df <- do.call(rbind, results)

  # Find optimal: minimum Moran's I with no empty folds
  valid <- results_df$n_empty_folds == 0 & !is.na(results_df$mean_morans_i)
  if (any(valid)) {
    optimal <- results_df$block_size[valid][which.min(results_df$mean_morans_i[valid])]
  } else {
    optimal <- range_est
    if (verbose) message("No valid block sizes found; using variogram range estimate")
  }

  if (verbose) message(sprintf("Optimal block size: %.1f", optimal))

  result <- list(
    optimal = optimal,
    results = results_df,
    range_estimate = range_est
  )
  class(result) <- c("borg_block_opt", "list")
  result
}


#' @exportS3Method ggplot2::autoplot
autoplot.borg_block_opt <- function(object, ...) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package 'ggplot2' required")
  }

  df <- object$results
  df <- df[!is.na(df$mean_morans_i), ]

  ggplot2::ggplot(df, ggplot2::aes(x = .data$block_size, y = .data$mean_morans_i)) +
    ggplot2::geom_line(color = "#2C3E50", linewidth = 0.8) +
    ggplot2::geom_point(
      ggplot2::aes(size = .data$mean_test_size),
      color = "#2C3E50", alpha = 0.7
    ) +
    # Optimal marker
    ggplot2::geom_vline(
      xintercept = object$optimal,
      linetype = "solid", color = "#27AE60", linewidth = 0.8
    ) +
    ggplot2::annotate(
      "text", x = object$optimal, y = max(df$mean_morans_i) * 0.95,
      label = sprintf("optimal = %.1f", object$optimal),
      hjust = -0.1, size = 3.5, color = "#27AE60", fontface = "bold"
    ) +
    # Variogram range reference
    ggplot2::geom_vline(
      xintercept = object$range_estimate,
      linetype = "dashed", color = "#C0392B", linewidth = 0.5
    ) +
    ggplot2::annotate(
      "text", x = object$range_estimate, y = max(df$mean_morans_i) * 0.85,
      label = sprintf("range = %.1f", object$range_estimate),
      hjust = -0.1, size = 3, color = "#C0392B"
    ) +
    ggplot2::scale_size_continuous(range = c(2, 5), guide = "none") +
    ggplot2::labs(
      title = "Block Size Optimization",
      subtitle = "Lower |Moran's I| = less between-fold autocorrelation",
      x = "Block size", y = "Mean |Moran's I| across folds"
    ) +
    borg_theme()
}
