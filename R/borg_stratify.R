# ===========================================================================
# Geographic stratification — ensure folds have representative coverage
# ===========================================================================

#' Check Geographic Representativeness of CV Folds
#'
#' @description
#' Evaluates whether each fold's test set covers a representative portion
#' of the study area. Flags folds that are geographically isolated or
#' biased toward one region.
#'
#' @param folds A \code{borg_cv} object or list of fold lists.
#' @param data Data frame with coordinate columns.
#' @param coords Character vector of length 2. Coordinate column names.
#' @param threshold Numeric. Minimum proportion of geographic extent that
#'   each fold should cover (0-1). Default: 0.2 (each fold covers at least
#'   20 percent of the x and y extent).
#'
#' @return A data frame with class \code{"borg_geo_strat"} containing
#'   per-fold coverage metrics:
#'   \describe{
#'     \item{fold}{Fold index}
#'     \item{x_coverage}{Proportion of x-extent covered by test set}
#'     \item{y_coverage}{Proportion of y-extent covered by test set}
#'     \item{area_ratio}{Convex hull area ratio (test / total)}
#'     \item{centroid_x, centroid_y}{Test set centroid}
#'     \item{balanced}{Whether fold meets the threshold}
#'   }
#'
#' @examples
#' set.seed(42)
#' d <- data.frame(x = runif(200, 0, 100), y = runif(200, 0, 100), z = rnorm(200))
#' cv <- borg_cv(d, coords = c("x", "y"), target = "z")
#' strat <- borg_check_coverage(cv, d, coords = c("x", "y"))
#' strat
#'
#' @export
borg_check_coverage <- function(folds, data, coords, threshold = 0.2) {

  fold_list <- if (inherits(folds, "borg_cv")) folds$folds else folds

  coord_info <- extract_coords(data, coords)
  x_all <- coord_info$x
  y_all <- coord_info$y

  x_extent <- diff(range(x_all, na.rm = TRUE))
  y_extent <- diff(range(y_all, na.rm = TRUE))

  # Total convex hull area (simple Shoelace formula)
  total_area <- .hull_area(x_all, y_all)

  results <- lapply(seq_along(fold_list), function(i) {
    test_idx <- fold_list[[i]]$test
    tx <- x_all[test_idx]
    ty <- y_all[test_idx]

    x_cov <- if (x_extent > 0) diff(range(tx, na.rm = TRUE)) / x_extent else 1
    y_cov <- if (y_extent > 0) diff(range(ty, na.rm = TRUE)) / y_extent else 1

    fold_area <- .hull_area(tx, ty)
    area_ratio <- if (total_area > 0) fold_area / total_area else 0

    data.frame(
      fold = i,
      x_coverage = x_cov,
      y_coverage = y_cov,
      area_ratio = area_ratio,
      centroid_x = mean(tx, na.rm = TRUE),
      centroid_y = mean(ty, na.rm = TRUE),
      balanced = (x_cov >= threshold) & (y_cov >= threshold),
      stringsAsFactors = FALSE
    )
  })

  result <- do.call(rbind, results)
  class(result) <- c("borg_geo_strat", "data.frame")

  n_unbalanced <- sum(!result$balanced)
  if (n_unbalanced > 0) {
    warning(sprintf(
      "%d of %d folds have poor geographic coverage (below %.0f%% threshold). Consider increasing v or using checkerboard blocking.",
      n_unbalanced, nrow(result), threshold * 100
    ))
  }

  result
}


#' Convex hull area via Shoelace formula (Internal)
#' @noRd
.hull_area <- function(x, y) {
  if (length(x) < 3) return(0)
  idx <- grDevices::chull(x, y)
  idx <- c(idx, idx[1])
  hx <- x[idx]
  hy <- y[idx]
  n <- length(hx)
  abs(sum(hx[-n] * hy[-1] - hx[-1] * hy[-n])) / 2
}
