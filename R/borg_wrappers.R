# ===========================================================================
# BORG Framework Wrappers: Guarded versions of common CV functions
# ===========================================================================

#' BORG-Guarded Cross-Validation Functions
#'
#' @description
#' These functions wrap common cross-validation functions from popular ML
#' frameworks, adding automatic BORG validation. They block random CV when
#' data dependencies are detected.
#'
#' @details
#' BORG provides guarded versions of:
#' \itemize{
#'   \item \code{borg_vfold_cv()}: Wraps \code{rsample::vfold_cv()}
#'   \item \code{borg_group_vfold_cv()}: Wraps \code{rsample::group_vfold_cv()}
#'   \item \code{borg_initial_split()}: Wraps \code{rsample::initial_split()}
#' }
#'
#' When dependencies are detected, these functions either:
#' \enumerate{
#'   \item Block the operation and suggest \code{borg_cv()} instead
#'   \item Automatically switch to an appropriate blocked CV strategy
#' }
#'
#' @name borg-wrappers
NULL


#' BORG-Guarded vfold_cv
#'
#' A guarded version of \code{rsample::vfold_cv()} that checks for data
#' dependencies before creating folds. If spatial, temporal, or clustered
#' dependencies are detected, random CV is blocked.
#'
#' @param data A data frame.
#' @param v Integer. Number of folds. Default: 10.
#' @param repeats Integer. Number of repeats. Default: 1.
#' @param strata Character. Column name for stratification.
#' @param coords Character vector of length 2. Coordinate columns for spatial check.
#' @param time Character. Time column for temporal check.
#' @param groups Character. Group column for clustered check.
#' @param target Character. Target variable for dependency detection.
#' @param allow_override Logical. If TRUE, allow random CV with explicit confirmation.
#'   Default: FALSE.
#' @param auto_block Logical. If TRUE, automatically switch to blocked CV when
#'   dependencies detected. If FALSE, throw error. Default: FALSE.
#' @param ... Additional arguments passed to \code{rsample::vfold_cv()}.
#'
#' @return If no dependencies detected or \code{allow_override = TRUE}, returns
#'   an \code{rset} object from rsample. If dependencies detected and
#'   \code{auto_block = TRUE}, returns BORG-generated blocked CV folds.
#'
#' @examples
#' \dontrun{
#' library(rsample)
#'
#' # Safe: no dependencies
#' data <- data.frame(x = rnorm(100), y = rnorm(100))
#' folds <- borg_vfold_cv(data, v = 5)
#'
#' # Blocked: spatial dependencies detected
#' spatial_data <- data.frame(
#'   lon = runif(100, -10, 10),
#'   lat = runif(100, -10, 10),
#'   response = rnorm(100)
#' )
#' # This will error:
#' # borg_vfold_cv(spatial_data, coords = c("lon", "lat"))
#'
#' # Use auto_block to automatically switch to spatial CV:
#' folds <- borg_vfold_cv(spatial_data, coords = c("lon", "lat"),
#'                        target = "response", auto_block = TRUE)
#' }
#'
#' @seealso \code{\link{borg_cv}} for direct blocked CV generation.
#'
#' @export
borg_vfold_cv <- function(data,
                          v = 10,
                          repeats = 1,
                          strata = NULL,
                          coords = NULL,
                          time = NULL,
                          groups = NULL,
                          target = NULL,
                          allow_override = FALSE,
                          auto_block = FALSE,
                          ...) {

  # Check if rsample is available

  if (!requireNamespace("rsample", quietly = TRUE)) {
    stop("Package 'rsample' required. Install with: install.packages('rsample')")
  }

  # If no structure hints, pass through to rsample

  if (is.null(coords) && is.null(time) && is.null(groups)) {
    if (getOption("borg.verbose", FALSE)) {
      message("BORG: No structure hints provided, using standard vfold_cv()")
    }
    return(rsample::vfold_cv(data, v = v, repeats = repeats, strata = strata, ...))
  }

  # Diagnose dependencies
  diagnosis <- borg_diagnose(
    data = data,
    coords = coords,
    time = time,
    groups = groups,
    target = target,
    verbose = FALSE
  )

  # Check if dependencies warrant blocking
  if (diagnosis@dependency_type != "none" && diagnosis@severity != "none") {

    if (allow_override) {
      # User explicitly overriding - warn but allow
      warning(sprintf(
        "BORG WARNING: %s dependency detected (severity: %s). Random CV may inflate metrics by ~%.0f%%.",
        diagnosis@dependency_type,
        diagnosis@severity,
        diagnosis@inflation_estimate$auc_inflation * 100
      ))
      return(rsample::vfold_cv(data, v = v, repeats = repeats, strata = strata, ...))
    }

    if (auto_block) {
      # Automatically switch to blocked CV
      if (getOption("borg.verbose", FALSE)) {
        message(sprintf(
          "BORG: Switching to %s (detected %s dependency)",
          diagnosis@recommended_cv,
          diagnosis@dependency_type
        ))
      }

      cv <- borg_cv(
        data = data,
        diagnosis = diagnosis,
        v = v,
        coords = coords,
        time = time,
        groups = groups,
        target = target,
        output = "rsample",
        verbose = FALSE
      )
      return(cv)
    }

    # Block the operation

    stop(sprintf(
      paste0(
        "BORG BLOCKED: %s dependency detected (severity: %s).\n",
        "Random CV would inflate metrics by ~%.0f%%.\n\n",
        "Options:\n",
        "  1. Use borg_cv(data, ...) to generate valid %s folds\
",
        "  2. Set auto_block = TRUE to automatically use blocked CV\n",
        "  3. Set allow_override = TRUE to proceed anyway (not recommended)"
      ),
      diagnosis@dependency_type,
      diagnosis@severity,
      diagnosis@inflation_estimate$auc_inflation * 100,
      diagnosis@recommended_cv
    ))
  }

  # No significant dependencies - proceed with standard vfold_cv
  rsample::vfold_cv(data, v = v, repeats = repeats, strata = strata, ...)
}


#' BORG-Guarded group_vfold_cv
#'
#' A guarded version of \code{rsample::group_vfold_cv()} that validates
#' group-based CV is appropriate for the data structure.
#'
#' @param data A data frame.
#' @param group Character. Column name for grouping.
#' @param v Integer. Number of folds. Default: number of groups.
#' @param balance Character. How to balance folds: "groups" or "observations".
#' @param coords Character vector. Coordinate columns for spatial check.
#' @param time Character. Time column for temporal check.
#' @param target Character. Target variable for dependency detection.
#' @param ... Additional arguments passed to \code{rsample::group_vfold_cv()}.
#'
#' @return An \code{rset} object from rsample.
#'
#' @examples
#' \dontrun{
#' # Clustered data - group CV is appropriate
#' data <- data.frame(
#'   site = rep(1:20, each = 5),
#'   x = rnorm(100),
#'   y = rnorm(100)
#' )
#' folds <- borg_group_vfold_cv(data, group = "site", v = 5)
#' }
#'
#' @export
borg_group_vfold_cv <- function(data,
                                group,
                                v = NULL,
                                balance = c("groups", "observations"),
                                coords = NULL,
                                time = NULL,
                                target = NULL,
                                ...) {

  if (!requireNamespace("rsample", quietly = TRUE)) {
    stop("Package 'rsample' required. Install with: install.packages('rsample')")
  }

  balance <- match.arg(balance)

  # Group CV is generally safe for clustered data

  # But warn if there are additional dependencies not handled by grouping

  if (!is.null(coords) || !is.null(time)) {
    diagnosis <- borg_diagnose(
      data = data,
      coords = coords,
      time = time,
      groups = group,
      target = target,
      verbose = FALSE
    )

    # Warn if spatial/temporal dependencies exist beyond clustering
    if (diagnosis@dependency_type == "mixed" ||
        (diagnosis@dependency_type == "spatial" && !is.null(coords)) ||
        (diagnosis@dependency_type == "temporal" && !is.null(time))) {
      warning(sprintf(
        "BORG: Group CV may not fully address %s dependencies. Consider borg_cv() for mixed strategies.",
        diagnosis@dependency_type
      ))
    }
  }


  # Proceed with group_vfold_cv
  # Use non-standard evaluation without rlang
  call <- match.call(expand.dots = FALSE)
  call[[1]] <- quote(rsample::group_vfold_cv)
  call$coords <- NULL
  call$time <- NULL
  call$target <- NULL
  call$data <- quote(data)
  call$group <- as.name(group)
  call$v <- v
  call$balance <- balance
  eval(call, parent.frame())
}


#' BORG-Guarded initial_split
#'
#' A guarded version of \code{rsample::initial_split()} that checks for
#' temporal ordering when time structure is specified.
#'
#' @param data A data frame.
#' @param prop Numeric. Proportion of data for training. Default: 0.75.
#' @param strata Character. Column name for stratification.
#' @param time Character. Time column - if provided, ensures chronological split.
#' @param coords Character vector. Coordinate columns for spatial check.
#' @param groups Character. Group column for clustered check.
#' @param target Character. Target variable.
#' @param ... Additional arguments passed to \code{rsample::initial_split()}.
#'
#' @return An \code{rsplit} object.
#'
#' @details
#' When \code{time} is specified, this function ensures the split respects
#' temporal ordering (training data comes before test data). For spatial data,
#' it warns if random splitting may cause issues.
#'
#' @examples
#' \dontrun{
#' # Temporal data - ensures chronological split
#' ts_data <- data.frame(
#'   date = seq(as.Date("2020-01-01"), by = "day", length.out = 100),
#'   value = cumsum(rnorm(100))
#' )
#' split <- borg_initial_split(ts_data, prop = 0.8, time = "date")
#' }
#'
#' @export
borg_initial_split <- function(data,
                               prop = 3/4,
                               strata = NULL,
                               time = NULL,
                               coords = NULL,
                               groups = NULL,
                               target = NULL,
                               ...) {

  if (!requireNamespace("rsample", quietly = TRUE)) {
    stop("Package 'rsample' required. Install with: install.packages('rsample')")
  }

  # If time is specified, use temporal splitting

  if (!is.null(time)) {
    if (!time %in% names(data)) {
      stop(sprintf("Time column '%s' not found in data", time))
    }

    # Sort by time and split chronologically
    time_order <- order(data[[time]])
    n <- nrow(data)
    n_train <- floor(n * prop)

    # Create indices based on temporal order
    train_idx <- time_order[1:n_train]
    test_idx <- time_order[(n_train + 1):n]

    # Create rsplit object manually
    split <- rsample::make_splits(
      x = list(analysis = train_idx, assessment = test_idx),
      data = data
    )

    if (getOption("borg.verbose", FALSE)) {
      message("BORG: Created chronological split based on '", time, "' column")
    }

    return(split)
  }

  # Check for spatial dependencies
  if (!is.null(coords)) {
    diagnosis <- borg_diagnose(
      data = data,
      coords = coords,
      groups = groups,
      target = target,
      verbose = FALSE
    )

    if (diagnosis@dependency_type == "spatial" && diagnosis@severity != "none") {
      warning(sprintf(
        paste0(
          "BORG WARNING: Spatial dependency detected. Random initial_split() may cause leakage.\n",
          "Consider using spatial blocking via borg_cv() instead."
        )
      ))
    }
  }

  # Standard split
 rsample::initial_split(data, prop = prop, strata = strata, ...)
}


# ===========================================================================
# Caret wrappers
# ===========================================================================

#' BORG-Guarded trainControl
#'
#' A guarded version of \code{caret::trainControl()} that validates CV settings
#' against data dependencies.
#'
#' @param data A data frame. Required for dependency checking.
#' @param method Character. Resampling method.
#' @param number Integer. Number of folds or iterations.
#' @param coords Character vector. Coordinate columns for spatial check.
#' @param time Character. Time column for temporal check.
#' @param groups Character. Group column for clustered check.
#' @param target Character. Target variable.
#' @param allow_override Logical. Allow random CV despite dependencies.
#' @param ... Additional arguments passed to \code{caret::trainControl()}.
#'
#' @return A \code{trainControl} object, potentially modified for blocked CV.
#'
#' @examples
#' \dontrun{
#' library(caret)
#'
#' # This will warn/error if dependencies detected
#' ctrl <- borg_trainControl(
#'   data = spatial_data,
#'   method = "cv",
#'   number = 5,
#'   coords = c("lon", "lat")
#' )
#' }
#'
#' @export
borg_trainControl <- function(data,
                              method = "cv",
                              number = 10,
                              coords = NULL,
                              time = NULL,
                              groups = NULL,
                              target = NULL,
                              allow_override = FALSE,
                              ...) {

  if (!requireNamespace("caret", quietly = TRUE)) {
    stop("Package 'caret' required. Install with: install.packages('caret')")
  }

  # If no structure hints, return standard trainControl
  if (is.null(coords) && is.null(time) && is.null(groups)) {
    return(caret::trainControl(method = method, number = number, ...))
  }

  # Check for dependencies
  diagnosis <- borg_diagnose(
    data = data,
    coords = coords,
    time = time,
    groups = groups,
    target = target,
    verbose = FALSE
  )

  # If random CV methods with dependencies
  random_methods <- c("cv", "repeatedcv", "boot", "boot632", "optimism_boot", "LOOCV")
  if (method %in% random_methods &&
      diagnosis@dependency_type != "none" &&
      diagnosis@severity != "none") {

    if (!allow_override) {
      stop(sprintf(
        paste0(
          "BORG BLOCKED: %s dependency detected with method='%s'.\n",
          "Random resampling would inflate metrics by ~%.0f%%.\n\n",
          "Use borg_cv(data, ..., output = 'caret') to generate valid folds,\n",
          "then pass them via trainControl(method = 'cv', index = folds)"
        ),
        diagnosis@dependency_type,
        method,
        diagnosis@inflation_estimate$auc_inflation * 100
      ))
    }

    warning(sprintf(
      "BORG WARNING: %s dependency detected. method='%s' may inflate metrics.",
      diagnosis@dependency_type, method
    ))
  }

  caret::trainControl(method = method, number = number, ...)
}


# ===========================================================================
# Hook registration for automatic interception
# ===========================================================================

#' Register BORG Hooks
#'
#' Registers BORG validation hooks that automatically check data dependencies
#' when using common ML framework functions. This is an experimental feature.
#'
#' @param frameworks Character vector. Which frameworks to hook into.
#'   Options: "rsample", "caret", "mlr3". Default: all available.
#' @param action Character. What to do when dependencies detected:
#'   "error" (block), "warn" (warn but proceed), "message" (info only).
#'
#' @return Invisible NULL. Called for side effect.
#'
#' @details
#' This function uses R's trace mechanism to add BORG checks to framework
#' functions. The hooks are session-specific and do not persist.
#'
#' To remove hooks, use \code{borg_unregister_hooks()}.
#'
#' @examples
#' \dontrun{
#' # Register hooks for rsample
#' borg_register_hooks("rsample")
#'
#' # Now vfold_cv() will check for dependencies
#' # (requires borg.check_data to be set)
#' options(borg.check_data = my_spatial_data)
#' options(borg.check_coords = c("lon", "lat"))
#'
#' rsample::vfold_cv(my_spatial_data)  # Will warn/error
#'
#' # Remove hooks
#' borg_unregister_hooks()
#' }
#'
#' @export
borg_register_hooks <- function(frameworks = c("rsample", "caret", "mlr3"),
                                action = c("error", "warn", "message")) {

  action <- match.arg(action)
  frameworks <- match.arg(frameworks, several.ok = TRUE)

  registered <- character(0)

  # Note: This is experimental and may not work in all environments
  # The safer approach is to use borg_vfold_cv() etc. directly

  if ("rsample" %in% frameworks && requireNamespace("rsample", quietly = TRUE)) {
    # Store original function
    .borg_env$original_vfold_cv <- rsample::vfold_cv

    # Create hook function
    hook_fn <- function() {
      # Check if BORG checking is enabled and data is set
      if (isTRUE(getOption("borg.auto_check")) &&
          !is.null(getOption("borg.check_data"))) {

        data <- getOption("borg.check_data")
        coords <- getOption("borg.check_coords")
        time <- getOption("borg.check_time")
        groups <- getOption("borg.check_groups")

        if (!is.null(coords) || !is.null(time) || !is.null(groups)) {
          diagnosis <- borg_diagnose(data, coords = coords, time = time,
                                     groups = groups, verbose = FALSE)

          if (diagnosis@dependency_type != "none" && diagnosis@severity != "none") {
            msg <- sprintf(
              "BORG: %s dependency detected. Consider using borg_cv() instead.",
              diagnosis@dependency_type
            )

            if (action == "error") {
              stop(msg)
            } else if (action == "warn") {
              warning(msg)
            } else {
              message(msg)
            }
          }
        }
      }
    }

    # Add trace
    suppressMessages(
      trace(rsample::vfold_cv, hook_fn, print = FALSE, where = asNamespace("rsample"))
    )
    registered <- c(registered, "rsample::vfold_cv")
  }

  if (length(registered) > 0) {
    message("BORG hooks registered for: ", paste(registered, collapse = ", "))
    message("Use borg_unregister_hooks() to remove.")
  } else {
    message("No hooks registered. Required packages may not be installed.")
  }

  invisible(NULL)
}


#' Unregister BORG Hooks
#'
#' Removes BORG validation hooks from framework functions.
#'
#' @return Invisible NULL.
#'
#' @export
borg_unregister_hooks <- function() {
  # Remove traces
  if (requireNamespace("rsample", quietly = TRUE)) {
    suppressMessages(try(untrace(rsample::vfold_cv, where = asNamespace("rsample")), silent = TRUE))
  }

  # Clear stored originals
  if (exists(".borg_env")) {
    rm(list = ls(.borg_env), envir = .borg_env)
  }

  message("BORG hooks removed.")
  invisible(NULL)
}


# Environment for storing hook state
.borg_env <- new.env(parent = emptyenv())
