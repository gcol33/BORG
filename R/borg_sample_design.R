# ===========================================================================
# borg_sample_design() — Where to collect new data
# ===========================================================================

#' Suggest Sampling Locations to Improve AOA
#'
#' Identifies locations in the prediction domain where new training data
#' would most reduce the dissimilarity index, thereby expanding the
#' area of applicability.
#'
#' @param train Data frame of existing training data.
#' @param prediction Data frame of prediction locations (with coordinates).
#' @param predictors Character vector. Predictor columns for DI computation.
#' @param coords Character vector of length 2. Coordinate columns in
#'   \code{prediction}.
#' @param n Integer. Number of suggested sampling locations. Default: 10.
#' @param weights Numeric vector. Variable importance weights for DI.
#'
#' @return A data frame with class \code{"borg_sample_design"} containing
#'   the top-n prediction locations ranked by DI (highest first), with
#'   columns: x, y, di, rank.
#'
#' @examples
#' set.seed(42)
#' train <- data.frame(x = runif(50, 0, 50), y = runif(50, 0, 50),
#'                      a = rnorm(50))
#' pred <- data.frame(x = runif(200, 0, 100), y = runif(200, 0, 100),
#'                     a = rnorm(200))
#' design <- borg_sample_design(train, pred, predictors = "a",
#'                                coords = c("x", "y"), n = 5)
#' design
#'
#' @export
borg_sample_design <- function(train, prediction, predictors = NULL,
                                  coords = NULL, n = 10L,
                                  weights = NULL) {

  di_vals <- as.numeric(borg_di(train, prediction,
                                  predictors = predictors, weights = weights))

  # Rank by DI (highest = most novel = highest sampling priority)
  ranking <- order(di_vals, decreasing = TRUE)
  top_n <- head(ranking, n)

  result <- data.frame(
    rank = seq_len(length(top_n)),
    di = di_vals[top_n],
    stringsAsFactors = FALSE
  )

  if (!is.null(coords) && all(coords %in% names(prediction))) {
    result$x <- prediction[[coords[1]]][top_n]
    result$y <- prediction[[coords[2]]][top_n]
  }

  # Add predictor values for context
  if (!is.null(predictors)) {
    for (p in predictors) {
      if (p %in% names(prediction)) {
        result[[p]] <- prediction[[p]][top_n]
      }
    }
  }

  class(result) <- c("borg_sample_design", "data.frame")
  attr(result, "coord_names") <- coords
  attr(result, "threshold") <- attr(borg_di(train, prediction,
                                              predictors = predictors,
                                              weights = weights), "threshold")
  result
}


#' @export
print.borg_sample_design <- function(x, ...) {
  cat("BORG Sampling Design\n")
  cat(sprintf("  %d suggested locations (ranked by DI)\n\n", nrow(x)))
  print.data.frame(x, row.names = FALSE)
  invisible(x)
}


#' @exportS3Method ggplot2::autoplot
autoplot.borg_sample_design <- function(object, ...) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) stop("ggplot2 required")

  if (!("x" %in% names(object))) {
    stop("Coordinates required for spatial plot")
  }

  coord_names <- attr(object, "coord_names") %||% c("x", "y")

  ggplot2::ggplot(object, ggplot2::aes(x = .data$x, y = .data$y)) +
    ggplot2::geom_point(
      ggplot2::aes(color = .data$di, size = .data$di),
      alpha = 0.8
    ) +
    ggplot2::geom_text(
      ggplot2::aes(label = .data$rank),
      size = 3, vjust = -1.2, fontface = "bold"
    ) +
    ggplot2::scale_color_viridis_c(option = "magma", direction = -1) +
    ggplot2::scale_size_continuous(range = c(3, 8), guide = "none") +
    ggplot2::coord_equal() +
    ggplot2::labs(
      title = "Suggested Sampling Locations",
      subtitle = "Ranked by dissimilarity to training data (highest DI first)",
      x = coord_names[1], y = coord_names[2], color = "DI"
    ) +
    borg_theme()
}
