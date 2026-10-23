# Preprocessing object inspectors: preProcess, recipe, prcomp, base scale()

#' Does an object carry the attributes written by base scale()?
#' @noRd
.has_scale_attrs <- function(x) {
  is.numeric(x) &&
    (!is.null(attr(x, "scaled:center")) || !is.null(attr(x, "scaled:scale")))
}

#' Parameters and unscaled values of base scale() output
#'
#' Accepts a matrix returned by \code{scale()} or a data frame whose columns
#' were replaced by \code{scale()} output (matrix columns carrying the
#' attributes). The original values are recovered as \code{z * scale + center},
#' so the train-only statistics can be recomputed without the raw data.
#'
#' @return \code{NULL} if no scale() attributes are present, otherwise a list
#'   with \code{raw} (unscaled numeric matrix), \code{center} and \code{scale}
#'   (numeric, \code{NA} where that column was not centred / not scaled), and
#'   \code{names} (column names).
#' @noRd
.scale_attributes <- function(object) {
  if (is.data.frame(object)) {
    cols <- Filter(function(nm) .has_scale_attrs(object[[nm]]), names(object))
    if (length(cols) == 0) return(NULL)
    parts <- lapply(cols, function(nm) {
      part <- .scale_attributes(object[[nm]])
      part$names <- if (length(part$names) == 1) {
        nm
      } else {
        paste(nm, part$names, sep = ".")
      }
      part
    })
    return(list(
      raw = do.call(cbind, lapply(parts, `[[`, "raw")),
      center = unlist(lapply(parts, `[[`, "center")),
      scale = unlist(lapply(parts, `[[`, "scale")),
      names = unlist(lapply(parts, `[[`, "names"))
    ))
  }

  if (!.has_scale_attrs(object)) return(NULL)

  z <- as.matrix(object)
  p <- ncol(z)
  ctr <- attr(object, "scaled:center")
  scl <- attr(object, "scaled:scale")
  center <- if (is.null(ctr)) rep(NA_real_, p) else as.numeric(ctr)
  scale_ <- if (is.null(scl)) rep(NA_real_, p) else as.numeric(scl)

  raw <- sweep(z, 2, ifelse(is.na(scale_), 1, scale_), `*`)
  raw <- sweep(raw, 2, ifelse(is.na(center), 0, center), `+`)

  nms <- colnames(z) %||% names(ctr) %||% names(scl) %||% paste0("V", seq_len(p))
  colnames(raw) <- nms
  attr(raw, "scaled:center") <- NULL
  attr(raw, "scaled:scale") <- NULL

  list(raw = raw, center = center, scale = scale_, names = nms)
}

#' @noRd
.inspect_scale <- function(object, train_idx, test_idx, data, ...) {
  risks <- list()

  if (is.null(train_idx) || is.null(test_idx)) {
    return(risks)
  }

  sa <- .scale_attributes(object)
  if (is.null(sa)) {
    return(risks)
  }

  # Training rows: taken from the unscaled object when it spans the split,
  # the whole object when it holds exactly the training rows, otherwise the
  # matching columns of `data`.
  n <- nrow(sa$raw)
  data_cols <- if (is.null(data)) NULL else colnames(data)
  train_raw <- if (n >= max(train_idx, test_idx)) {
    sa$raw[train_idx, , drop = FALSE]
  } else if (n == length(train_idx)) {
    sa$raw
  } else if (all(sa$names %in% data_cols)) {
    as.matrix(data[train_idx, sa$names, drop = FALSE])
  } else {
    return(risks)
  }

  centred <- !is.na(sa$center)
  scaled <- !is.na(sa$scale)

  # scale() divides by the root mean square sqrt(sum(x^2) / (n - 1)) of the
  # (centred) column, which equals the sd when the column is centred.
  train_center <- colMeans(train_raw, na.rm = TRUE)
  dev <- sweep(train_raw, 2, ifelse(centred, train_center, 0))
  train_scale <- sqrt(colSums(dev^2, na.rm = TRUE) /
                        (colSums(!is.na(dev)) - 1))

  if (any(centred)) {
    risk <- .check_param_leak(
      stored = sa$center[centred],
      recomputed = train_center[centred],
      description = "scale() centering parameters were computed on data beyond training set (mean difference: %.6g)",
      source_object = "scale()",
      test_idx = test_idx
    )
    if (!is.null(risk)) risks <- c(risks, list(risk))
  }

  if (any(scaled)) {
    risk <- .check_param_leak(
      stored = sa$scale[scaled],
      recomputed = train_scale[scaled],
      description = "scale() scaling parameters were computed on data beyond training set (sd difference: %.6g)",
      source_object = "scale()",
      test_idx = test_idx
    )
    if (!is.null(risk)) risks <- c(risks, list(risk))
  }

  risks
}

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
        risk <- .check_step_params(step$means, data, train_idx, test_idx,
                                   "step_center", i, stat = "mean")
        if (!is.null(risk)) risks <- c(risks, list(risk))
      }

      # step_scale stores sds
      if (step_class == "step_scale" && !is.null(step$sds)) {
        risk <- .check_step_params(step$sds, data, train_idx, test_idx,
                                   "step_scale", i, stat = "sd")
        if (!is.null(risk)) risks <- c(risks, list(risk))
      }

      # step_normalize combines center and scale
      if (step_class == "step_normalize") {
        if (!is.null(step$means)) {
          risk <- .check_step_params(step$means, data, train_idx, test_idx,
                                     "step_normalize", i, stat = "mean")
          if (!is.null(risk)) risks <- c(risks, list(risk))
        }
        if (!is.null(step$sds)) {
          risk <- .check_step_params(step$sds, data, train_idx, test_idx,
                                     "step_normalize", i, stat = "sd")
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

#' Check recipe step parameters against train-only values
#' @param stored_params Named numeric vector of stored parameters.
#' @param data Full dataset.
#' @param train_idx Integer vector of training row indices.
#' @param test_idx Integer vector of test row indices.
#' @param step_name Character label for the recipe step.
#' @param step_index Integer position of the step in the recipe.
#' @param stat One of \code{"mean"} or \code{"sd"}.
#' @noRd
.check_step_params <- function(stored_params, data, train_idx, test_idx,
                               step_name, step_index, stat = c("mean", "sd")) {
  stat <- match.arg(stat)
  cols <- intersect(names(stored_params), names(data))
  if (length(cols) == 0) return(NULL)

  train_data <- data[train_idx, cols, drop = FALSE]
  recomputed <- if (stat == "mean") {
    colMeans(train_data, na.rm = TRUE)
  } else {
    apply(train_data, 2, sd, na.rm = TRUE)
  }
  label <- if (stat == "mean") "centering" else "scaling"

  .check_param_leak(
    stored = stored_params[cols],
    recomputed = recomputed,
    description = sprintf(
      "%s %s parameters were computed on data beyond training set (%s difference: %%.6g)",
      step_name, label, stat
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
