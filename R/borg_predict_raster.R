# ===========================================================================
# borg_predict_raster() — Spatial prediction with AOA masking
# ===========================================================================

#' Predict onto a SpatRaster with AOA Masking
#'
#' Generates spatial predictions from a fitted model onto a
#' \code{terra::SpatRaster}, computes the dissimilarity index and area
#' of applicability, and returns a multi-layer raster with prediction,
#' DI, and AOA mask layers.
#'
#' @param model A fitted model with a \code{predict()} method.
#' @param raster A \code{terra::SpatRaster} with predictor layers matching
#'   the model's training variables.
#' @param train_data Data frame of training data used to fit the model.
#' @param predictors Character vector. Predictor column names. If
#'   \code{NULL}, uses raster layer names.
#' @param weights Numeric vector. Variable importance weights for DI.
#' @param threshold Numeric. Manual AOA threshold. If \code{NULL},
#'   computed from training data.
#' @param type Character. Prediction type passed to \code{predict()}.
#'   Default: \code{"response"}.
#'
#' @return A \code{terra::SpatRaster} with three layers:
#'   \describe{
#'     \item{prediction}{Model predictions}
#'     \item{di}{Dissimilarity index}
#'     \item{aoa}{Area of applicability (1 = inside, 0 = outside)}
#'   }
#'
#' @details Requires the \pkg{terra} package.
#'
#' @examples
#' \donttest{
#' if (requireNamespace("terra", quietly = TRUE)) {
#'   set.seed(42)
#'   r <- terra::rast(nrows = 20, ncols = 20, xmin = 0, xmax = 100,
#'                    ymin = 0, ymax = 100, nlyrs = 2)
#'   terra::values(r) <- cbind(rnorm(400), rnorm(400))
#'   names(r) <- c("bio1", "bio2")
#'   train <- data.frame(bio1 = rnorm(50), bio2 = rnorm(50))
#'   train$y <- train$bio1 * 2 + rnorm(50, sd = 0.5)
#'   model <- lm(y ~ bio1 + bio2, data = train)
#'   result <- borg_predict_raster(model, r, train, predictors = c("bio1", "bio2"))
#'   names(result)
#' }
#' }
#'
#' @export
borg_predict_raster <- function(model, raster, train_data,
                                  predictors = NULL, weights = NULL,
                                  threshold = NULL, type = "response") {

  if (!requireNamespace("terra", quietly = TRUE)) {
    stop("Package 'terra' required for borg_predict_raster()")
  }

  if (!inherits(raster, "SpatRaster")) {
    stop("raster must be a terra::SpatRaster")
  }

  if (is.null(predictors)) {
    predictors <- names(raster)
  }

  # Check all predictors exist in raster
  missing <- setdiff(predictors, names(raster))
  if (length(missing) > 0) {
    stop(sprintf("Raster missing layers: %s", paste(missing, collapse = ", ")))
  }

  # Extract raster values as data frame
  raster_df <- as.data.frame(raster[[predictors]])
  complete_mask <- complete.cases(raster_df)

  # Predict
  pred_vals <- rep(NA_real_, nrow(raster_df))
  if (any(complete_mask)) {
    pred_vals[complete_mask] <- tryCatch(
      stats::predict(model, newdata = raster_df[complete_mask, , drop = FALSE],
                      type = type),
      error = function(e) {
        # Try without type argument
        stats::predict(model, newdata = raster_df[complete_mask, , drop = FALSE])
      }
    )
  }

  # Compute DI
  di_vals <- rep(NA_real_, nrow(raster_df))
  if (any(complete_mask)) {
    di_result <- borg_di(train_data, raster_df[complete_mask, , drop = FALSE],
                          predictors = predictors, weights = weights)
    di_vals[complete_mask] <- as.numeric(di_result)

    if (is.null(threshold)) {
      threshold <- attr(di_result, "threshold")
    }
  }

  # AOA mask
  aoa_vals <- rep(NA_real_, nrow(raster_df))
  aoa_vals[complete_mask] <- as.integer(di_vals[complete_mask] <= threshold)

  # Build output raster
  pred_rast <- terra::rast(raster[[1]])
  terra::values(pred_rast) <- pred_vals
  names(pred_rast) <- "prediction"

  di_rast <- terra::rast(raster[[1]])
  terra::values(di_rast) <- di_vals
  names(di_rast) <- "di"

  aoa_rast <- terra::rast(raster[[1]])
  terra::values(aoa_rast) <- aoa_vals
  names(aoa_rast) <- "aoa"

  result <- c(pred_rast, di_rast, aoa_rast)

  attr(result, "threshold") <- threshold
  result
}
