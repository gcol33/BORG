#' BORG: Guard Your Model Evaluation
#'
#' The main entry point for BORG. Diagnoses data dependencies, generates
#' valid cross-validation schemes, and validates evaluation workflows.
#'
#' @param data A data frame to diagnose and create CV folds for.
#' @param coords Character vector of length 2 specifying coordinate column names
#'   (e.g., \code{c("lon", "lat")}). Triggers spatial autocorrelation detection.
#' @param time Character string specifying the time column name. Triggers
#'   temporal autocorrelation detection.
#' @param groups Character string specifying the grouping column name
#'   (e.g., "site_id", "patient_id"). Triggers clustered structure detection.
#' @param target Character string specifying the response variable column name.
#'   Used for more accurate autocorrelation diagnostics.
#' @param v Integer. Number of CV folds. Default: 5.
#' @param train_idx Integer vector of training indices. If provided along with
#'   \code{test_idx}, validates an existing split instead of generating one.
#' @param test_idx Integer vector of test indices. Required if \code{train_idx}
#'   is provided.
#' @param output Character. CV output format: "list" (default), "rsample",
#'   "caret", "mlr3". Ignored when validating an existing split.
#' @param ... Additional arguments passed to underlying functions.
#'
#' @return Depends on usage mode:
#'
#' \strong{Diagnosis mode} (no train_idx/test_idx): A list with class "borg_result"
#' containing:
#' \describe{
#'   \item{diagnosis}{A \code{\link{BorgDiagnosis}} object with dependency analysis}
#'   \item{cv}{A \code{borg_cv} object with valid cross-validation folds}
#'   \item{folds}{Shortcut to \code{cv$folds} for convenience}
#' }
#'
#' \strong{Validation mode} (with train_idx/test_idx): A \code{\link{BorgRisk}}
#' object containing the risk assessment of the provided split.
#'
#' @details
#' \code{borg()} operates in two modes:
#'
#' \subsection{Diagnosis Mode (Recommended)}{
#' When called with structure hints (\code{coords}, \code{time}, \code{groups})
#' but without \code{train_idx}/\code{test_idx}, BORG:
#' \enumerate{
#'   \item Diagnoses data dependencies (spatial, temporal, clustered)
#'   \item Estimates how much random CV would inflate metrics
#'   \item Generates appropriate CV folds that respect the dependency structure
#'   \item Returns everything needed to proceed with valid evaluation
#' }
#'
#' This is the recommended workflow. Let BORG tell you how to split your data.
#' }
#'
#' \subsection{Validation Mode}{
#' When called with \code{train_idx} and \code{test_idx}, BORG validates the
#' existing split:
#' \itemize{
#'   \item Checks for index overlap
#'   \item Validates group isolation (if \code{groups} specified)
#'   \item Validates temporal ordering (if \code{time} specified)
#'   \item Checks spatial separation (if \code{coords} specified)
#'   \item Detects preprocessing leakage, target leakage, etc.
#' }
#'
#' Use this mode to verify splits you've created yourself.
#' }
#'
#' @examples
#' # ===== DIAGNOSIS MODE (recommended) =====
#'
#' # Spatial data: let BORG create valid folds
#' set.seed(42)
#' spatial_data <- data.frame(
#'   x = runif(200, 0, 100),
#'   y = runif(200, 0, 100),
#'   response = rnorm(200)
#' )
#'
#' result <- borg(spatial_data, coords = c("x", "y"), target = "response")
#' result$diagnosis
#' result$folds[[1]]  # First fold's train/test indices
#'
#' # Clustered data
#' clustered_data <- data.frame(
#'   site = rep(1:20, each = 10),
#'   value = rep(rnorm(20), each = 10) + rnorm(200, sd = 0.5)
#' )
#'
#' result <- borg(clustered_data, groups = "site", target = "value")
#' result$diagnosis@recommended_cv  # "group_fold"
#'
#' # Temporal data
#' temporal_data <- data.frame(
#'   date = seq(as.Date("2020-01-01"), by = "day", length.out = 200),
#'   value = cumsum(rnorm(200))
#' )
#'
#' result <- borg(temporal_data, time = "date", target = "value")
#'
#' \dontrun{
#' # Get rsample-compatible output for tidymodels (requires rsample package)
#' result <- borg(spatial_data, coords = c("x", "y"), output = "rsample")
#' }
#'
#' # ===== VALIDATION MODE =====
#'
#' # Validate an existing split
#' data <- data.frame(x = 1:100, y = rnorm(100))
#' borg(data, train_idx = 1:70, test_idx = 71:100)
#'
#' # Validate with group constraint
#' data$patient <- rep(1:10, each = 10)
#' borg(data, train_idx = 1:50, test_idx = 51:100, groups = "patient")
#'
#' @seealso
#' \code{\link{borg_diagnose}} for diagnosis only,
#' \code{\link{borg_cv}} for CV generation only,
#' \code{\link{borg_inspect}} for detailed object inspection.
#'
#' @export
borg <- function(data,
                 coords = NULL,
                 time = NULL,
                 groups = NULL,
                 target = NULL,
                 v = 5,
                 train_idx = NULL,
                 test_idx = NULL,
                 output = c("list", "rsample", "caret", "mlr3"),
                 ...) {

  output <- match.arg(output)

  # =========================================================================
  # Handle non-data.frame inputs (backward compatibility)
  # For object inspection, use borg_inspect() directly
  # =========================================================================

  if (!is.data.frame(data)) {
    # Legacy: validate preprocessing/model objects
    if (inherits(data, "BorgRisk")) {
      return(data)
    }

    # Workflow list for borg_validate
    if (is.list(data) && all(c("data", "train_idx", "test_idx") %in% names(data))) {
      return(borg_validate(data, ...))
    }

    # For non-data.frame objects, redirect to borg_inspect
    # Users should use: borg_inspect(object, train_idx, test_idx, data = df)
    if (is.null(train_idx) || is.null(test_idx)) {
      stop("For inspecting preprocessing/model objects, use borg_inspect() directly:\n",
           "  borg_inspect(object, train_idx, test_idx, data = your_data)")
    }
    return(borg_inspect(data, train_idx, test_idx, ...))
  }

  # =========================================================================
  # VALIDATION MODE: User provided train_idx and test_idx
  # =========================================================================

  if (!is.null(train_idx) && !is.null(test_idx)) {
    return(validate_existing_split(
      data = data,
      train_idx = train_idx,
      test_idx = test_idx,
      coords = coords,
      time = time,
      groups = groups,
      target = target,
      ...
    ))
  }

  # Error if only one of train_idx/test_idx provided
  if (!is.null(train_idx) || !is.null(test_idx)) {
    stop("Both 'train_idx' and 'test_idx' must be provided together, or neither")
  }

  # =========================================================================
  # DIAGNOSIS MODE: Diagnose dependencies and generate valid CV
  # =========================================================================

  # Must have at least one structure hint
  if (is.null(coords) && is.null(time) && is.null(groups)) {
    # No structure specified - still run diagnosis but warn
    message("No structure specified (coords/time/groups). ",
            "Running diagnosis on data characteristics only.")
  }

  # Diagnose
  diagnosis <- borg_diagnose(
    data = data,
    coords = coords,
    time = time,
    groups = groups,
    target = target,
    verbose = FALSE
  )

  # Generate CV
  cv <- borg_cv(
    data = data,
    diagnosis = diagnosis,
    v = v,
    coords = coords,
    time = time,
    groups = groups,
    target = target,
    output = output,
    verbose = FALSE
  )

  # For rsample/caret/mlr3 output, just return the CV object
  if (output != "list") {
    return(cv)
  }

  # Build result
  result <- list(
    diagnosis = diagnosis,
    cv = cv,
    folds = cv$folds
  )
  class(result) <- c("borg_result", "list")

  result
}


#' @export
print.borg_result <- function(x, ...) {
  cat("BORG Result\n")
  cat("===========\n\n")

  # Diagnosis summary
  diag <- x$diagnosis
  cat(sprintf("Dependency:  %s (%s severity)\n",
              toupper(diag@dependency_type), diag@severity))
  cat(sprintf("Strategy:    %s\n", x$cv$strategy))
  cat(sprintf("Folds:       %d\n", length(x$folds)))

  # Inflation warning
  if (!is.na(diag@inflation_estimate$auc_inflation) &&
      diag@inflation_estimate$auc_inflation > 0.05) {
    cat(sprintf("\nRandom CV would inflate metrics by ~%.0f%%\n",
                diag@inflation_estimate$auc_inflation * 100))
  }

  # Quick fold summary
  train_sizes <- vapply(x$folds, function(f) length(f$train), integer(1))
  test_sizes <- vapply(x$folds, function(f) length(f$test), integer(1))
  cat(sprintf("\nFold sizes:  train %d-%d, test %d-%d\n",
              min(train_sizes), max(train_sizes),
              min(test_sizes), max(test_sizes)))

  cat("\nAccess components:\n")
  cat("  $diagnosis  - BorgDiagnosis object\n")
  cat("  $folds      - List of train/test index vectors\n")
  cat("  $cv         - Full borg_cv object\n")

  invisible(x)
}


# ===========================================================================
# Internal: Validate existing split
# ===========================================================================

#' Validate an Existing Split (Internal)
#' @noRd
validate_existing_split <- function(data, train_idx, test_idx,
                                    coords, time, groups, target, ...) {

  n <- nrow(data)
  train_idx <- as.integer(train_idx)
  test_idx <- as.integer(test_idx)

  # Bounds check
  if (any(train_idx < 1) || any(train_idx > n)) {
    stop(sprintf("'train_idx' contains out-of-bounds values (data has %d rows)", n))
  }
  if (any(test_idx < 1) || any(test_idx > n)) {
    stop(sprintf("'test_idx' contains out-of-bounds values (data has %d rows)", n))
  }

  # Index overlap
  overlap <- intersect(train_idx, test_idx)
  if (length(overlap) > 0) {
    stop(sprintf(
      "BORG HARD VIOLATION: train_idx and test_idx overlap (%d shared indices: %s%s)",
      length(overlap),
      paste(head(overlap, 5), collapse = ", "),
      if (length(overlap) > 5) "..." else ""
    ))
  }

  # Group overlap
  if (!is.null(groups)) {
    if (!groups %in% names(data)) {
      stop(sprintf("'groups' column '%s' not found in data", groups))
    }
    train_groups <- unique(data[[groups]][train_idx])
    test_groups <- unique(data[[groups]][test_idx])
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

  # Temporal ordering
  if (!is.null(time)) {
    if (!time %in% names(data)) {
      stop(sprintf("'time' column '%s' not found in data", time))
    }
    train_times <- data[[time]][train_idx]
    test_times <- data[[time]][test_idx]

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

  # Spatial columns validation
  if (!is.null(coords)) {
    missing_cols <- setdiff(coords, names(data))
    if (length(missing_cols) > 0) {
      stop(sprintf("'coords' columns not found in data: %s",
                   paste(missing_cols, collapse = ", ")))
    }
  }

  # Target column validation
  if (!is.null(target)) {
    if (!target %in% names(data)) {
      stop(sprintf("'target' column '%s' not found in data", target))
    }
  }

  # Run full inspection
  borg_inspect(data, train_idx, test_idx,
               target_col = target, spatial_cols = coords, ...)
}
