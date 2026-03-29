# Pipeline-level end-to-end validation
# Validates every step of a modeling pipeline for information leakage

#' Validate an Entire Modeling Pipeline
#'
#' Walks a tidymodels \code{workflow()} or \code{caret::train()} object and
#' validates every step — preprocessing, feature selection, tuning, and
#' model fitting — for information leakage.
#'
#' @param pipeline A modeling pipeline object. Supported types:
#'   \itemize{
#'     \item A tidymodels \code{workflow} object (fitted or unfitted)
#'     \item A \code{caret::train} object
#'     \item A list with named components (recipe, model, tune_results, etc.)
#'   }
#' @param train_idx Integer vector of training row indices.
#' @param test_idx Integer vector of test row indices.
#' @param data Optional data frame. Required for parameter-level checks.
#' @param ... Additional arguments passed to inspectors.
#'
#' @return An object of class \code{"borg_pipeline"} containing:
#' \describe{
#'   \item{stages}{Named list of per-stage BorgRisk results}
#'   \item{overall}{Aggregated BorgRisk for the full pipeline}
#'   \item{n_stages}{Number of stages inspected}
#'   \item{leaking_stages}{Character vector of stage names with hard violations}
#' }
#'
#' @details
#' \code{borg_pipeline()} decomposes a pipeline into stages and inspects each:
#' \enumerate{
#'   \item \strong{Preprocessing}: Recipe steps, preProcess, PCA, scaling
#'   \item \strong{Feature selection}: Variable importance, filtering
#'   \item \strong{Hyperparameter tuning}: Inner CV resamples
#'   \item \strong{Model fitting}: Training data scope, row counts
#'   \item \strong{Post-processing}: Threshold optimization, calibration
#' }
#'
#' Each stage gets its own BorgRisk assessment. The overall result aggregates
#' all risks across stages.
#'
#' @examples
#' \donttest{
#' if (requireNamespace("caret", quietly = TRUE)) {
#'   ctrl <- caret::trainControl(method = "cv", number = 5)
#'   model <- caret::train(mpg ~ ., data = mtcars[1:25, ], method = "lm",
#'                         trControl = ctrl, preProcess = c("center", "scale"))
#'   result <- borg_pipeline(model, train_idx = 1:25, test_idx = 26:32,
#'                           data = mtcars)
#'   print(result)
#' }
#' }
#'
#' @seealso \code{\link{borg_validate}}, \code{\link{borg_inspect}}
#'
#' @export
borg_pipeline <- function(pipeline, train_idx, test_idx, data = NULL, ...) {

  if (is.null(train_idx) || is.null(test_idx)) {
    stop("'train_idx' and 'test_idx' are required for pipeline validation")
  }

  train_idx <- as.integer(train_idx)
  test_idx <- as.integer(test_idx)

  stages <- list()

  # =========================================================================
  # Dispatch based on pipeline type
  # =========================================================================

  if (inherits(pipeline, "workflow")) {
    stages <- .decompose_tidymodels_workflow(pipeline, train_idx, test_idx, data, ...)
  } else if (inherits(pipeline, "train")) {
    stages <- .decompose_caret_train(pipeline, train_idx, test_idx, data, ...)
  } else if (is.list(pipeline)) {
    stages <- .decompose_list_pipeline(pipeline, train_idx, test_idx, data, ...)
  } else {
    stop(sprintf("Unsupported pipeline type: %s. Expected workflow, train, or list.",
                 paste(class(pipeline), collapse = "/")))
  }

  # =========================================================================
  # Aggregate results
  # =========================================================================

  all_risks <- list()
  leaking_stages <- character(0)

  for (stage_name in names(stages)) {
    stage_result <- stages[[stage_name]]
    if (inherits(stage_result, "BorgRisk")) {
      all_risks <- c(all_risks, stage_result@risks)
      if (!stage_result@is_valid) {
        leaking_stages <- c(leaking_stages, stage_name)
      }
    }
  }

  overall <- .make_borg_risk(all_risks, train_idx, test_idx, call = match.call())

  result <- list(
    stages = stages,
    overall = overall,
    n_stages = length(stages),
    leaking_stages = leaking_stages
  )
  class(result) <- c("borg_pipeline", "list")
  result
}


#' Decompose a tidymodels workflow into inspectable stages
#' @noRd
.decompose_tidymodels_workflow <- function(pipeline, train_idx, test_idx, data, ...) {
  stages <- list()

  # Stage 1: Preprocessor (recipe)
  preprocessor <- tryCatch(pipeline$pre$actions$recipe$recipe, error = function(e) NULL)
  if (!is.null(preprocessor)) {
    stages$preprocessing <- borg_inspect(preprocessor, train_idx, test_idx, data, ...)
  }

  # Stage 2: Model spec
  model_spec <- tryCatch(pipeline$fit$fit, error = function(e) NULL)
  if (!is.null(model_spec)) {
    stages$model <- borg_inspect(model_spec, train_idx, test_idx, data, ...)
  }

  # Stage 3: Post-processing (if any)
  post <- tryCatch(pipeline$post, error = function(e) NULL)
  if (!is.null(post) && length(post$actions) > 0) {
    # Post-processing steps exist - flag for manual review
    stages$postprocessing <- .make_borg_risk(
      list(.new_risk(
        type = "pipeline_postprocessing",
        severity = "soft_inflation",
        description = "Workflow has post-processing steps. Verify they don't use test data.",
        source_object = "workflow$post"
      )),
      train_idx, test_idx, call = quote(borg_pipeline())
    )
  }

  # If no stages extracted, try inspecting as a whole
  if (length(stages) == 0) {
    stages$workflow <- borg_inspect(pipeline, train_idx, test_idx, data, ...)
  }

  stages
}


#' Decompose a caret train object into inspectable stages
#' @noRd
.decompose_caret_train <- function(pipeline, train_idx, test_idx, data, ...) {
  stages <- list()

  # Stage 1: Preprocessing (preProcess embedded in train)
  if (!is.null(pipeline$preProcess)) {
    stages$preprocessing <- borg_inspect(pipeline$preProcess, train_idx, test_idx, data, ...)
  }

  # Stage 2: Resampling / CV structure
  if (!is.null(pipeline$control)) {
    stages$resampling <- borg_inspect(pipeline$control, train_idx, test_idx, ...)
  }

  # Stage 3: Nested CV (hyperparameter tuning)
  nested_risks <- .inspect_nested_cv_caret(pipeline, train_idx, test_idx, data, ...)
  if (length(nested_risks) > 0) {
    stages$tuning <- .make_borg_risk(nested_risks, train_idx, test_idx,
                                     call = quote(borg_pipeline()))
  }

  # Stage 4: Model training scope
  model_risks <- list()
  if (!is.null(pipeline$trainingData)) {
    n_used <- nrow(pipeline$trainingData)
    n_expected <- length(train_idx)
    if (n_used > n_expected * 1.05) {
      model_risks <- c(model_risks, list(.new_risk(
        type = "data_scope",
        severity = "hard_violation",
        description = sprintf(
          "Model trained on %d rows but training set has %d. Non-training data included.",
          n_used, n_expected
        ),
        affected_indices = test_idx,
        source_object = "train$trainingData"
      )))
    }
  }
  if (length(model_risks) > 0) {
    stages$model <- .make_borg_risk(model_risks, train_idx, test_idx,
                                    call = quote(borg_pipeline()))
  }

  stages
}


#' Decompose a list-based pipeline into inspectable stages
#' @noRd
.decompose_list_pipeline <- function(pipeline, train_idx, test_idx, data, ...) {
  stages <- list()

  # Use data from pipeline if not provided
  if (is.null(data) && !is.null(pipeline$data)) {
    data <- pipeline$data
  }

  # Check each known component
  if (!is.null(pipeline$preprocess)) {
    pp <- pipeline$preprocess
    if (is.list(pp) && !inherits(pp, "preProcess") && !inherits(pp, "recipe") &&
        !inherits(pp, "prcomp")) {
      # List of preprocessing objects
      for (i in seq_along(pp)) {
        stage_name <- paste0("preprocessing_", i)
        stages[[stage_name]] <- borg_inspect(pp[[i]], train_idx, test_idx, data, ...)
      }
    } else {
      stages$preprocessing <- borg_inspect(pp, train_idx, test_idx, data, ...)
    }
  }

  if (!is.null(pipeline$recipe)) {
    stages$recipe <- borg_inspect(pipeline$recipe, train_idx, test_idx, data, ...)
  }

  if (!is.null(pipeline$model)) {
    stages$model <- borg_inspect(pipeline$model, train_idx, test_idx, data, ...)
  }

  if (!is.null(pipeline$tune_results)) {
    nested_risks <- .inspect_nested_cv(pipeline$tune_results, train_idx, test_idx, data, ...)
    if (length(nested_risks) > 0) {
      stages$tuning <- .make_borg_risk(nested_risks, train_idx, test_idx,
                                       call = quote(borg_pipeline()))
    }
  }

  if (!is.null(pipeline$thresholds)) {
    thresh_risks <- .check_threshold_selection(pipeline)
    if (length(thresh_risks) > 0) {
      stages$thresholds <- .make_borg_risk(thresh_risks, train_idx, test_idx,
                                           call = quote(borg_pipeline()))
    }
  }

  stages
}


#' @export
print.borg_pipeline <- function(x, ...) {
  cat("BORG Pipeline Validation\n")
  cat("========================\n\n")

  cat(sprintf("Stages inspected: %d\n", x$n_stages))
  cat(sprintf("Overall status:   %s\n",
              if (x$overall@is_valid) "VALID" else "INVALID"))
  cat(sprintf("Hard violations:  %d\n", x$overall@n_hard))
  cat(sprintf("Soft warnings:    %d\n", x$overall@n_soft))

  if (length(x$leaking_stages) > 0) {
    cat(sprintf("\nLeaking stages:   %s\n",
                paste(x$leaking_stages, collapse = ", ")))
  }

  cat("\n--- Per-Stage Results ---\n")
  for (stage_name in names(x$stages)) {
    stage <- x$stages[[stage_name]]
    if (inherits(stage, "BorgRisk")) {
      status <- if (stage@is_valid) "OK" else "LEAK"
      n_issues <- stage@n_hard + stage@n_soft
      cat(sprintf("  %-20s [%s] %d issue(s)\n", stage_name, status, n_issues))
    }
  }

  invisible(x)
}
