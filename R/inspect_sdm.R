# ===========================================================================
# SDM framework integration (ENMeval, biomod2)
# ===========================================================================


#' Convert BORG Folds to ENMeval Partition Format
#'
#' Converts BORG CV folds to the partition format expected by ENMeval's
#' \code{ENMevaluate()} function (a vector of fold assignments).
#'
#' @param borg_cv A \code{borg_cv} object.
#'
#' @return A named list with:
#'   \describe{
#'     \item{occs.grp}{Integer vector of fold assignments for occurrence points}
#'     \item{bg.grp}{Integer vector for background points (all assigned to fold 0)}
#'   }
#'
#' @examples
#' set.seed(42)
#' d <- data.frame(x = runif(100), y = runif(100), z = rnorm(100))
#' cv <- borg_cv(d, coords = c("x", "y"), target = "z")
#' parts <- borg_to_enmeval(cv)
#' table(parts$occs.grp)
#'
#' @export
borg_to_enmeval <- function(borg_cv) {
  if (!inherits(borg_cv, "borg_cv")) {
    stop("borg_cv must be a borg_cv object")
  }

  folds <- borg_cv$folds
  n <- borg_cv$params$n

  # Build fold assignment vector
  fold_vec <- rep(NA_integer_, n)
  for (i in seq_along(folds)) {
    fold_vec[folds[[i]]$test] <- i
  }
  # Points not in any test set get assigned to fold 1
  fold_vec[is.na(fold_vec)] <- 1L

  list(
    occs.grp = fold_vec,
    bg.grp = rep(0L, n)  # placeholder for background
  )
}


#' Convert BORG Folds to biomod2 Format
#'
#' Converts BORG CV folds to the data split table format expected by
#' biomod2's \code{BIOMOD_Modeling()} function.
#'
#' @param borg_cv A \code{borg_cv} object.
#'
#' @return A matrix where each column is a CV run and each row is an
#'   observation. Values are \code{TRUE} (calibration) or \code{FALSE}
#'   (validation), matching biomod2's \code{DataSplitTable} format.
#'
#' @examples
#' set.seed(42)
#' d <- data.frame(x = runif(100), y = runif(100), z = rnorm(100))
#' cv <- borg_cv(d, coords = c("x", "y"), target = "z")
#' split_table <- borg_to_biomod2(cv)
#' dim(split_table)
#'
#' @export
borg_to_biomod2 <- function(borg_cv) {
  if (!inherits(borg_cv, "borg_cv")) {
    stop("borg_cv must be a borg_cv object")
  }

  folds <- borg_cv$folds
  n <- borg_cv$params$n

  # Build DataSplitTable matrix (TRUE = calibration, FALSE = validation)
  split_table <- matrix(TRUE, nrow = n, ncol = length(folds))
  colnames(split_table) <- paste0("RUN", seq_along(folds))

  for (i in seq_along(folds)) {
    split_table[folds[[i]]$test, i] <- FALSE
  }

  split_table
}
