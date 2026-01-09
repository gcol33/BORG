#' BORG: Validate Model Evaluation
#'
#' The main entry point for BORG. Automatically detects the appropriate
#' validation method based on the input and returns a risk assessment.
#'
#' @param x An object to validate. Can be:
#'   \itemize{
#'     \item A \code{data.frame} - validates train/test split
#'     \item A preprocessing object (\code{preProcess}, \code{recipe}, \code{prcomp})
#'     \item A model object (\code{lm}, \code{glm}, \code{ranger}, \code{train})
#'     \item A CV object (\code{trainControl}, \code{vfold_cv}, \code{rset})
#'     \item A workflow or parsnip model
#'   }
#' @param train_idx Integer vector of training indices.
#' @param test_idx Integer vector of test indices.
#' @param data Optional data frame for context when inspecting models.
#' @param temporal_col Optional name of a timestamp column. If provided,
#'   validates that test observations do not precede training observations.
#' @param group_col Optional name of a grouping column (e.g., patient ID).
#'   If provided, validates that no group appears in both train and test.
#' @param spatial_cols Optional character vector of coordinate column names
#'   (e.g., \code{c("longitude", "latitude")}). If provided, checks spatial
#'   separation between train and test.
#' @param ... Additional arguments passed to underlying functions.
#'
#' @return A \code{\link{BorgRisk}} object containing the risk assessment.
#'
#' @details
#' \code{borg()} is the recommended entry point for most users. It automatically
#' dispatches to the appropriate validation function:
#'
#' \itemize{
#'   \item Data frames -> \code{\link{borg_inspect}()} with full validation
#'   \item Preprocessing/model objects -> \code{\link{borg_inspect}()}
#'   \item Workflow lists -> \code{\link{borg_validate}()}
#' }
#'
#' When validating data frames, you can optionally specify \code{temporal_col},
#' \code{group_col}, or \code{spatial_cols} to enable additional checks for
#' temporal ordering, group isolation, or spatial separation.
#'
#' @examples
#' # Validate a train/test split
#' data <- data.frame(x = 1:100, y = rnorm(100))
#' result <- borg(data, train_idx = 1:70, test_idx = 71:100)
#' result
#'
#' # Check a preprocessing object
#' pca <- prcomp(data[1:70, ], center = TRUE, scale. = TRUE)
#' borg(pca, train_idx = 1:70, test_idx = 71:100, data = data)
#'
#' # Check a model
#' model <- lm(y ~ x, data = data[1:70, ])
#' borg(model, train_idx = 1:70, test_idx = 71:100, data = data)
#'
#' # Validate with group isolation
#' data$patient <- rep(1:10, each = 10)
#' borg(data, train_idx = 1:50, test_idx = 51:100, group_col = "patient")
#'
#' # Validate temporal ordering
#' data$date <- seq.Date(as.Date("2020-01-01"), by = "day", length.out = 100)
#' borg(data, train_idx = 1:70, test_idx = 71:100, temporal_col = "date")
#'
#' @seealso
#' \code{\link{borg_inspect}} for detailed object inspection,
#' \code{\link{borg_validate}} for workflow validation.
#'
#' @export
borg <- function(x, train_idx = NULL, test_idx = NULL, data = NULL,
                 temporal_col = NULL, group_col = NULL, spatial_cols = NULL,
                 ...) {

  # Dispatch based on input type

  if (inherits(x, "BorgRisk")) {
    # Already a BorgRisk - just return it
    return(x)
  }

  # Check if x is a workflow list (for borg_validate)
  if (is.list(x) && !is.data.frame(x) &&
      all(c("data", "train_idx", "test_idx") %in% names(x))) {
    return(borg_validate(x, ...))
  }

  # For everything else, use borg_inspect
  if (is.data.frame(x)) {
    if (is.null(train_idx) || is.null(test_idx)) {
      stop("'train_idx' and 'test_idx' are required for data frame validation")
    }

    # Validate indices
    n <- nrow(x)
    train_idx <- as.integer(train_idx)
    test_idx <- as.integer(test_idx)

    if (any(train_idx < 1) || any(train_idx > n)) {
      stop(sprintf("'train_idx' contains out-of-bounds values (data has %d rows)", n))
    }
    if (any(test_idx < 1) || any(test_idx > n)) {
      stop(sprintf("'test_idx' contains out-of-bounds values (data has %d rows)", n))
    }

    # Check for index overlap (hard violation)
    overlap <- intersect(train_idx, test_idx)
    if (length(overlap) > 0) {
      stop(sprintf(
        "BORG HARD VIOLATION: train_idx and test_idx overlap (%d shared indices: %s%s)",
        length(overlap),
        paste(head(overlap, 5), collapse = ", "),
        if (length(overlap) > 5) "..." else ""
      ))
    }

    # Check group overlap if group_col specified
    if (!is.null(group_col)) {
      if (!group_col %in% names(x)) {
        stop(sprintf("'group_col' = '%s' not found in data", group_col))
      }
      train_groups <- unique(x[[group_col]][train_idx])
      test_groups <- unique(x[[group_col]][test_idx])
      group_overlap <- intersect(train_groups, test_groups)

      if (length(group_overlap) > 0) {
        stop(sprintf(
          "BORG HARD VIOLATION: Groups appear in both train and test (%d overlapping: %s%s)",
          length(group_overlap),
          paste(head(group_overlap, 5), collapse = ", "),
          if (length(group_overlap) > 5) "..." else ""
        ))
      }
    }

    # Check temporal ordering if temporal_col specified
    if (!is.null(temporal_col)) {
      if (!temporal_col %in% names(x)) {
        stop(sprintf("'temporal_col' = '%s' not found in data", temporal_col))
      }
      train_times <- x[[temporal_col]][train_idx]
      test_times <- x[[temporal_col]][test_idx]

      max_train_time <- max(train_times, na.rm = TRUE)
      min_test_time <- min(test_times, na.rm = TRUE)

      if (min_test_time < max_train_time) {
        n_violations <- sum(test_times < max_train_time, na.rm = TRUE)
        stop(sprintf(
          "BORG HARD VIOLATION: Temporal ordering violated. %d test observations predate training data.",
          n_violations
        ))
      }
    }

    # Validate spatial columns exist if specified
    if (!is.null(spatial_cols)) {
      missing_cols <- setdiff(spatial_cols, names(x))
      if (length(missing_cols) > 0) {
        stop(sprintf("'spatial_cols' not found in data: %s",
                     paste(missing_cols, collapse = ", ")))
      }
    }
  }

  borg_inspect(x, train_idx, test_idx, data = data, ...)
}
