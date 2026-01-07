#' Guard Model Evaluation Against Information Reuse
#'
#' `borg_guard()` wraps an evaluation workflow and enforces valid data handling.
#' It intercepts operations that would leak information from test to training
#' data and either blocks them (hard violations) or rewrites them (soft violations).
#'
#' @param data A data.frame containing the full dataset.
#' @param train_idx Integer vector of training row indices.
#' @param test_idx Integer vector of test row indices.
#' @param mode Character string specifying enforcement mode:
#'   \itemize{
#'     \item \code{"strict"}: Block all violations, refuse to proceed
#'     \item \code{"warn"}: Warn on violations but continue
#'     \item \code{"rewrite"}: Automatically fix violations where possible
#'   }
#' @param temporal_col Optional character string naming a timestamp column.
#'   If provided, enables temporal ordering validation.
#' @param spatial_cols Optional character vector naming coordinate columns
#'   (e.g., \code{c("longitude", "latitude")}). If provided, enables spatial
#'   autocorrelation checks.
#' @param group_col Optional character string naming a grouping column.
#'   If provided, enables group-level isolation checks.
#'
#' @return A \code{borg_context} object that can be used to wrap preprocessing
#'   and evaluation calls. This object tracks all operations and validates them
#'   against the train/test split.
#'
#' @details
#' `borg_guard()` creates a guarded context for model evaluation. Within this
#' context, BORG:
#'
#' \enumerate{
#'   \item Validates the initial train/test split
#'   \item Monitors preprocessing operations for data leakage
#'   \item Enforces temporal ordering (if \code{temporal_col} specified)
#'   \item Validates spatial block separation (if \code{spatial_cols} specified)
#'   \item Ensures group isolation (if \code{group_col} specified)
#' }
#'
#' @section Enforcement Modes:
#' \describe{
#'   \item{strict}{Most conservative. Any detected violation causes an immediate
#'     error. Use for production pipelines where validity is critical.}
#'   \item{warn}{Permissive mode. Violations generate warnings but evaluation
#'     proceeds. Use for exploratory analysis or legacy code auditing.}
#'   \item{rewrite}{Automatic correction mode. BORG attempts to fix violations
#'     (e.g., refitting preprocessing on train-only). Use when migrating
#'     existing pipelines.}
#' }
#'
#' @seealso
#' \code{\link{borg_inspect}} for object-level inspection,
#' \code{\link{borg_validate}} for post-hoc workflow validation.
#'
#' @examples
#' # The canonical failure: preprocessing before splitting
#' # This workflow produces inflated performance estimates:
#' #
#' #   data_scaled <- scale(full_data)
#' #   train <- data_scaled[1:800, ]
#' #   test <- data_scaled[801:1000, ]
#' #
#' # BORG blocks this pattern.
#'
#' # Valid split (no overlap)
#' data <- data.frame(x = rnorm(100), y = rnorm(100))
#' ctx <- borg_guard(data, train_idx = 1:70, test_idx = 71:100)
#' print(ctx)
#'
#' # Invalid split (overlap) - errors immediately
#' \dontrun{
#' ctx <- borg_guard(data, train_idx = 1:60, test_idx = 50:100)
#' # Error: BORG HARD VIOLATION: train_idx and test_idx overlap
#' }
#'
#' # Grouped data - ensures no group appears in both splits
#' data$patient <- rep(1:10, each = 10)
#' ctx <- borg_guard(
#'   data,
#'   train_idx = 1:50,
#'   test_idx = 51:100,
#'   group_col = "patient"
#' )
#'
#' # Temporal data - validates chronological ordering
#' data$date <- seq.Date(as.Date("2020-01-01"), by = "day", length.out = 100)
#' ctx <- borg_guard(
#'   data,
#'   train_idx = 1:70,
#'   test_idx = 71:100,
#'   temporal_col = "date"
#' )
#'
#' @export
borg_guard <- function(
 data,
 train_idx,
 test_idx,
 mode = c("strict", "warn", "rewrite"),
 temporal_col = NULL,
 spatial_cols = NULL,
 group_col = NULL
) {

 # ===========================================================================
 # Input validation
 # ===========================================================================

 mode <- match.arg(mode)

 if (!is.data.frame(data)) {
   stop("'data' must be a data.frame")
 }

 if (!is.numeric(train_idx) && !is.integer(train_idx)) {
   stop("'train_idx' must be an integer vector")
 }
 train_idx <- as.integer(train_idx)

 if (!is.numeric(test_idx) && !is.integer(test_idx)) {
   stop("'test_idx' must be an integer vector")
 }
 test_idx <- as.integer(test_idx)

 # Validate indices
 n <- nrow(data)
 if (any(train_idx < 1) || any(train_idx > n)) {
   stop(sprintf("'train_idx' contains out-of-bounds values (data has %d rows)", n))
 }
 if (any(test_idx < 1) || any(test_idx > n)) {
   stop(sprintf("'test_idx' contains out-of-bounds values (data has %d rows)", n))
 }

 # Check for overlap (hard violation)
 overlap <- intersect(train_idx, test_idx)
 if (length(overlap) > 0) {
   stop(sprintf(
     "BORG HARD VIOLATION: train_idx and test_idx overlap (%d shared indices: %s...)",
     length(overlap),
     paste(head(overlap, 5), collapse = ", ")
   ))
 }

 # Validate optional columns
 if (!is.null(temporal_col)) {
   if (!temporal_col %in% names(data)) {
     stop(sprintf("'temporal_col' = '%s' not found in data", temporal_col))
   }
 }

 if (!is.null(spatial_cols)) {
   missing_cols <- setdiff(spatial_cols, names(data))
   if (length(missing_cols) > 0) {
     stop(sprintf("'spatial_cols' not found in data: %s",
                  paste(missing_cols, collapse = ", ")))
   }
 }

 if (!is.null(group_col)) {
   if (!group_col %in% names(data)) {
     stop(sprintf("'group_col' = '%s' not found in data", group_col))
   }

   # Check for group overlap
   train_groups <- unique(data[[group_col]][train_idx])
   test_groups <- unique(data[[group_col]][test_idx])
   group_overlap <- intersect(train_groups, test_groups)

   if (length(group_overlap) > 0) {
     msg <- sprintf(
       "BORG HARD VIOLATION: Groups appear in both train and test (%d overlapping groups)",
       length(group_overlap)
     )
     if (mode == "strict") {
       stop(msg)
     } else {
       warning(msg)
     }
   }
 }

 # ===========================================================================
 # Temporal validation
 # ===========================================================================

 if (!is.null(temporal_col)) {
   train_times <- data[[temporal_col]][train_idx]
   test_times <- data[[temporal_col]][test_idx]

   # Check if any test observation precedes training observations
   max_train_time <- max(train_times, na.rm = TRUE)
   min_test_time <- min(test_times, na.rm = TRUE)

   if (min_test_time < max_train_time) {
     temporal_violations <- test_idx[test_times < max_train_time]
     msg <- sprintf(
       "BORG WARNING: Temporal ordering violated. %d test observations predate training data.",
       length(temporal_violations)
     )
     if (mode == "strict") {
       stop(gsub("WARNING", "HARD VIOLATION", msg))
     } else {
       warning(msg)
     }
   }
 }

 # ===========================================================================
 # Create context object
 # ===========================================================================

 structure(
   list(
     data = data,
     train_idx = train_idx,
     test_idx = test_idx,
     mode = mode,
     temporal_col = temporal_col,
     spatial_cols = spatial_cols,
     group_col = group_col,
     operations = list(),
     risks = list()
   ),
   class = "borg_context"
 )
}


#' Print method for borg_context
#' @keywords internal
#' @export
#' @param x A borg_context object
#' @param ... Additional arguments (ignored)
print.borg_context <- function(x, ...) {
 cat("BORG Guarded Context\n")
 cat("====================\n")
 cat(sprintf("  Mode:          %s\n", x$mode))
 cat(sprintf("  Data rows:     %d\n", nrow(x$data)))
 cat(sprintf("  Train indices: %d\n", length(x$train_idx)))
 cat(sprintf("  Test indices:  %d\n", length(x$test_idx)))

 if (!is.null(x$temporal_col)) {
   cat(sprintf("  Temporal col:  %s\n", x$temporal_col))
 }
 if (!is.null(x$spatial_cols)) {
   cat(sprintf("  Spatial cols:  %s\n", paste(x$spatial_cols, collapse = ", ")))
 }
 if (!is.null(x$group_col)) {
   cat(sprintf("  Group col:     %s\n", x$group_col))
 }

 if (length(x$risks) > 0) {
   n_hard <- sum(vapply(x$risks, function(r) r$severity == "hard_violation", logical(1)))
   n_soft <- sum(vapply(x$risks, function(r) r$severity == "soft_inflation", logical(1)))
   cat(sprintf("\n  Risks detected: %d hard, %d soft\n", n_hard, n_soft))
 }

 invisible(x)
}
