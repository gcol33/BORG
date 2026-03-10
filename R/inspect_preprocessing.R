# Preprocessing object inspectors: preProcess, recipe, prcomp

#' @noRd
.inspect_preProcess <- function(object, train_idx, test_idx, data, ...) {
  risks <- list()

  if (is.null(data) || is.null(train_idx) || is.null(test_idx)) {
    return(risks)
  }

  methods_used <- object$method
  if (is.null(methods_used)) {
    return(risks)
  }

  method_names <- names(methods_used)

  numeric_cols <- names(object$mean)
  if (is.null(numeric_cols) || length(numeric_cols) == 0) {
    numeric_cols <- names(object$std)
  }
  if (is.null(numeric_cols) || length(numeric_cols) == 0) {
    return(risks)
  }

  train_data <- data[train_idx, numeric_cols, drop = FALSE]

  # Check centering (mean)
  if ("center" %in% method_names && !is.null(object$mean)) {
    risk <- .check_param_leak(
      stored = object$mean[numeric_cols],
      recomputed = colMeans(train_data, na.rm = TRUE),
      description = "preProcess centering parameters were computed on data beyond training set (mean difference: %.6g)",
      source_object = "preProcess",
      test_idx = test_idx
    )
    if (!is.null(risk)) risks <- c(risks, list(risk))
  }

  # Check scaling (sd)
  if ("scale" %in% method_names && !is.null(object$std)) {
    risk <- .check_param_leak(
      stored = object$std[numeric_cols],
      recomputed = apply(train_data, 2, sd, na.rm = TRUE),
      description = "preProcess scaling parameters were computed on data beyond training set (sd difference: %.6g)",
      source_object = "preProcess",
      test_idx = test_idx
    )
    if (!is.null(risk)) risks <- c(risks, list(risk))
  }

  risks
}

#' @noRd
.inspect_recipe <- function(object, train_idx, test_idx, data, ...) {
  risks <- list()

  # Check if recipe has been prepped
  is_prepped <- length(object$steps) > 0 &&
    any(vapply(object$steps, function(s) isTRUE(s$trained), logical(1)))

  if (!is_prepped) {
    return(risks)
  }

  # Check 1: Compare prep() row count to expected train-only count
  tr_info <- object$tr_info
  if (!is.null(tr_info) && !is.null(tr_info$nrows)) {
    prep_nrows <- tr_info$nrows
    expected_nrows <- length(train_idx)

    if (!is.null(train_idx) && prep_nrows != expected_nrows) {
      risks <- c(risks, list(.new_risk(
        type = "preprocessing_leak",
        severity = "hard_violation",
        description = sprintf(
          "Recipe was prepped on %d rows, but train set has %d rows. Preprocessing may include test data.",
          prep_nrows, expected_nrows
        ),
        affected_indices = test_idx,
        source_object = "recipe"
      )))
    }
  }

  # Check 2: Inspect step-specific parameters if data is provided
  if (!is.null(data) && !is.null(train_idx) && !is.null(test_idx)) {
    for (i in seq_along(object$steps)) {
      step <- object$steps[[i]]
      step_class <- class(step)[1]

      # step_center stores means
      if (step_class == "step_center" && !is.null(step$means)) {
        risk <- .check_step_means(step$means, data, train_idx, test_idx,
                                  "step_center", i)
        if (!is.null(risk)) risks <- c(risks, list(risk))
      }

      # step_scale stores sds
      if (step_class == "step_scale" && !is.null(step$sds)) {
        risk <- .check_step_sds(step$sds, data, train_idx, test_idx,
                                "step_scale", i)
        if (!is.null(risk)) risks <- c(risks, list(risk))
      }

      # step_normalize combines center and scale
      if (step_class == "step_normalize") {
        if (!is.null(step$means)) {
          risk <- .check_step_means(step$means, data, train_idx, test_idx,
                                    "step_normalize", i)
          if (!is.null(risk)) risks <- c(risks, list(risk))
        }
        if (!is.null(step$sds)) {
          risk <- .check_step_sds(step$sds, data, train_idx, test_idx,
                                  "step_normalize", i)
          if (!is.null(risk)) risks <- c(risks, list(risk))
        }
      }

      # step_pca stores rotation matrix
      if (step_class == "step_pca" && !is.null(step$res)) {
        pca_risks <- .inspect_prcomp(step$res, train_idx, test_idx, data, ...)
        for (r in pca_risks) {
          r$source_object <- sprintf("recipe$steps[[%d]] (step_pca)", i)
          risks <- c(risks, list(r))
        }
      }
    }
  }

  risks
}

#' Check recipe step means against train-only means
#' @noRd
.check_step_means <- function(stored_means, data, train_idx, test_idx,
                              step_name, step_index) {
  cols <- intersect(names(stored_means), names(data))
  if (length(cols) == 0) return(NULL)

  train_data <- data[train_idx, cols, drop = FALSE]
  .check_param_leak(
    stored = stored_means[cols],
    recomputed = colMeans(train_data, na.rm = TRUE),
    description = sprintf(
      "%s centering parameters were computed on data beyond training set (mean difference: %%.6g)",
      step_name
    ),
    source_object = sprintf("recipe$steps[[%d]] (%s)", step_index, step_name),
    test_idx = test_idx
  )
}

#' Check recipe step sds against train-only sds
#' @noRd
.check_step_sds <- function(stored_sds, data, train_idx, test_idx,
                            step_name, step_index) {
  cols <- intersect(names(stored_sds), names(data))
  if (length(cols) == 0) return(NULL)

  train_data <- data[train_idx, cols, drop = FALSE]
  .check_param_leak(
    stored = stored_sds[cols],
    recomputed = apply(train_data, 2, sd, na.rm = TRUE),
    description = sprintf(
      "%s scaling parameters were computed on data beyond training set (sd difference: %%.6g)",
      step_name
    ),
    source_object = sprintf("recipe$steps[[%d]] (%s)", step_index, step_name),
    test_idx = test_idx
  )
}

#' @noRd
.inspect_prcomp <- function(object, train_idx, test_idx, data, ...) {
  risks <- list()

  if (is.null(data) || is.null(train_idx) || is.null(test_idx)) {
    return(risks)
  }

  pca_cols <- rownames(object$rotation)
  if (is.null(pca_cols)) {
    return(risks)
  }

  pca_cols <- intersect(pca_cols, names(data))
  if (length(pca_cols) == 0) {
    return(risks)
  }

  train_data <- data[train_idx, pca_cols, drop = FALSE]

  # Check centering
  if (!is.null(object$center) && !isFALSE(object$center) &&
      is.numeric(object$center)) {
    risk <- .check_param_leak(
      stored = object$center[pca_cols],
      recomputed = colMeans(train_data, na.rm = TRUE),
      description = "PCA centering was computed on data beyond training set (mean difference: %.6g)",
      source_object = "prcomp",
      test_idx = test_idx
    )
    if (!is.null(risk)) risks <- c(risks, list(risk))
  }

  # Check scaling
  if (!is.null(object$scale) && !isFALSE(object$scale) &&
      is.numeric(object$scale)) {
    risk <- .check_param_leak(
      stored = object$scale[pca_cols],
      recomputed = apply(train_data, 2, sd, na.rm = TRUE),
      description = "PCA scaling was computed on data beyond training set (sd difference: %.6g)",
      source_object = "prcomp",
      test_idx = test_idx
    )
    if (!is.null(risk)) risks <- c(risks, list(risk))
  }

  # Check rotation matrix (loadings)
  if (!is.null(object$rotation)) {
    train_centered <- scale(train_data,
                            center = !isFALSE(object$center),
                            scale = !isFALSE(object$scale))

    train_svd <- tryCatch(
      svd(train_centered, nu = 0, nv = ncol(train_centered)),
      error = function(e) NULL
    )

    if (!is.null(train_svd)) {
      stored_rotation <- object$rotation[pca_cols, , drop = FALSE]
      n_components <- min(ncol(stored_rotation), ncol(train_svd$v))

      if (n_components > 0) {
        n_check <- min(3, n_components)
        stored_abs <- abs(stored_rotation[, 1:n_check, drop = FALSE])
        train_abs <- abs(train_svd$v[, 1:n_check, drop = FALSE])
        max_diff <- max(abs(stored_abs - train_abs), na.rm = TRUE)

        if (max_diff > 0.01) {
          risks <- c(risks, list(.new_risk(
            type = "preprocessing_leak",
            severity = "hard_violation",
            description = sprintf(
              "PCA loadings were computed on data beyond training set (max loading difference: %.4f)",
              max_diff
            ),
            affected_indices = test_idx,
            source_object = "prcomp"
          )))
        }
      }
    }
  }

  risks
}
