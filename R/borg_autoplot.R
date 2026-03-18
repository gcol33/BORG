# ===========================================================================
# BORG ggplot2 Visualization — autoplot() methods
# ===========================================================================

# Suppress R CMD check NSE notes for ggplot2 aes() variables
utils::globalVariables(c(".data"))

# Palette and theme --------------------------------------------------------

borg_palette <- c(
  train    = "#2E86AB",
  test     = "#E94F37",
  overlap  = "#F9A03F",
  valid    = "#2E7D32",
  excluded = "gray85"
)

#' Minimal ggplot2 theme for BORG plots (Internal)
#' @noRd
borg_theme <- function() {
  ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      panel.grid.minor = ggplot2::element_blank(),
      strip.text = ggplot2::element_text(face = "bold"),
      legend.position = "bottom"
    )
}

#' Check ggplot2 availability (Internal)
#' @noRd
check_ggplot2 <- function() {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package 'ggplot2' required for autoplot(). ",
         "Install with: install.packages('ggplot2')")
  }
}


# autoplot.BorgRisk --------------------------------------------------------

#' Autoplot Method for BorgRisk Objects
#'
#' Creates a ggplot2 visualization of detected risks from a BORG validation.
#'
#' @param object A \code{\link{BorgRisk}} object from \code{borg_inspect()} or
#'   \code{borg()} in validation mode.
#' @param max_risks Integer. Maximum number of risks to display. Default: 10.
#' @param ... Additional arguments (currently unused).
#'
#' @return A \code{ggplot} object.
#'
#' @details Requires the \pkg{ggplot2} package.
#'
#' @examples
#' \donttest{
#' if (requireNamespace("ggplot2", quietly = TRUE)) {
#'   data <- data.frame(x = 1:100, y = 101:200)
#'   result <- borg_inspect(data, train_idx = 1:60, test_idx = 51:100)
#'   ggplot2::autoplot(result)
#' }
#' }
#'
#' @exportS3Method ggplot2::autoplot
autoplot.BorgRisk <- function(object, max_risks = 10, ...) {
  check_ggplot2()

  risks <- object@risks

  if (length(risks) == 0) {
    df <- data.frame(x = 0.5, y = 0.5, label = "No risks detected")
    p <- ggplot2::ggplot(df, ggplot2::aes(x = .data$x, y = .data$y)) +
      ggplot2::geom_text(ggplot2::aes(label = .data$label),
                         size = 8, color = borg_palette["valid"]) +
      ggplot2::theme_void() +
      ggplot2::ggtitle("BORG Risk Assessment: VALID")
    return(p)
  }

  if (length(risks) > max_risks) risks <- risks[seq_len(max_risks)]

  df <- data.frame(
    type = vapply(risks, function(r) r$type, character(1)),
    severity = vapply(risks, function(r) r$severity, character(1)),
    description = vapply(risks, function(r) {
      d <- r$description
      if (nchar(d) > 60) paste0(substr(d, 1, 57), "...") else d
    }, character(1)),
    stringsAsFactors = FALSE
  )
  df$type <- factor(df$type, levels = rev(df$type))

  ggplot2::ggplot(df, ggplot2::aes(
    x = .data$type, y = 1, fill = .data$severity
  )) +
    ggplot2::geom_col(width = 0.7, show.legend = TRUE) +
    ggplot2::geom_text(ggplot2::aes(label = .data$description),
                       hjust = 0, nudge_y = 0.02, size = 3) +
    ggplot2::scale_fill_manual(
      values = c(hard_violation = "#C62828", soft_inflation = "#F9A825"),
      labels = c(hard_violation = "Hard Violation", soft_inflation = "Soft Inflation")
    ) +
    ggplot2::coord_flip() +
    ggplot2::labs(title = "BORG Risk Assessment", x = NULL, y = NULL, fill = NULL) +
    borg_theme() +
    ggplot2::theme(
      axis.text.x = ggplot2::element_blank(),
      panel.grid.major.x = ggplot2::element_blank()
    )
}


# autoplot.borg_result -----------------------------------------------------

#' Autoplot Method for borg_result Objects
#'
#' Creates ggplot2 visualizations of BORG diagnosis + CV results.
#'
#' @param object A \code{borg_result} object from \code{borg()}.
#' @param type Character. Plot type: \code{"split"} (default), \code{"spatial"},
#'   \code{"temporal"}, or \code{"groups"}.
#' @param fold Integer. Which fold to plot. Default: 1.
#' @param data Optional data frame (or sf/SpatVector) for spatial plots.
#'   Required for \code{type = "spatial"} to obtain coordinates.
#' @param coords Character vector of coordinate column names. Required for
#'   \code{type = "spatial"} when \code{data} is a plain data.frame.
#' @param ... Additional arguments (currently unused).
#'
#' @return A \code{ggplot} object.
#'
#' @details Requires the \pkg{ggplot2} package. For spatial plots, the
#'   \pkg{sf} package is recommended for proper map projections.
#'
#' @examples
#' \donttest{
#' if (requireNamespace("ggplot2", quietly = TRUE)) {
#'   set.seed(42)
#'   d <- data.frame(x = runif(100), y = runif(100), z = rnorm(100))
#'   result <- borg(d, coords = c("x", "y"), target = "z")
#'   ggplot2::autoplot(result)
#'   ggplot2::autoplot(result, type = "spatial", data = d, coords = c("x", "y"))
#' }
#' }
#'
#' @exportS3Method ggplot2::autoplot
autoplot.borg_result <- function(object,
                                  type = c("split", "spatial", "temporal", "groups"),
                                  fold = 1,
                                  data = NULL,
                                  coords = NULL,
                                  ...) {
  check_ggplot2()
  type <- match.arg(type)

  if (is.null(object$folds) || length(object$folds) == 0) {
    stop("No folds available in borg_result object")
  }
  if (fold > length(object$folds)) {
    stop(sprintf("Fold %d not available (only %d folds)", fold, length(object$folds)))
  }

  train_idx <- object$folds[[fold]]$train
  test_idx <- object$folds[[fold]]$test

  switch(type,
    "split" = autoplot_split(train_idx, test_idx, fold),
    "spatial" = autoplot_spatial_single(train_idx, test_idx, data, coords, fold),
    "temporal" = stop("Temporal autoplot requires a 'time' vector. Use plot() for now."),
    "groups" = stop("Groups autoplot requires a 'groups' vector. Use plot() for now.")
  )
}


# autoplot.borg_cv ---------------------------------------------------------

#' Autoplot Method for borg_cv Objects
#'
#' Visualizes cross-validation fold structure.
#'
#' @param object A \code{borg_cv} object from \code{borg_cv()}.
#' @param type Character. Plot type: \code{"folds"} (default), \code{"spatial"},
#'   or \code{"sizes"}.
#' @param data Optional data frame (or sf/SpatVector) for spatial plots.
#'   Required for \code{type = "spatial"}.
#' @param coords Character vector of coordinate column names. Required for
#'   \code{type = "spatial"} when \code{data} is a plain data.frame.
#' @param ... Additional arguments (currently unused).
#'
#' @return A \code{ggplot} object.
#'
#' @details Requires the \pkg{ggplot2} package.
#'
#' @examples
#' \donttest{
#' if (requireNamespace("ggplot2", quietly = TRUE)) {
#'   set.seed(42)
#'   d <- data.frame(x = runif(100), y = runif(100), z = rnorm(100))
#'   cv <- borg_cv(d, coords = c("x", "y"), target = "z")
#'   ggplot2::autoplot(cv, type = "sizes")
#'   ggplot2::autoplot(cv, type = "spatial", data = d, coords = c("x", "y"))
#' }
#' }
#'
#' @exportS3Method ggplot2::autoplot
autoplot.borg_cv <- function(object,
                              type = c("folds", "spatial", "sizes"),
                              data = NULL,
                              coords = NULL,
                              ...) {
  check_ggplot2()
  type <- match.arg(type)

  folds <- object$folds

  switch(type,
    "folds" = autoplot_folds_tile(folds),
    "spatial" = autoplot_spatial_faceted(folds, data, coords),
    "sizes" = autoplot_fold_sizes(folds)
  )
}


# autoplot.borg_comparison -------------------------------------------------

#' Autoplot Method for borg_comparison Objects
#'
#' Visualizes random vs blocked CV comparison results.
#'
#' @param object A \code{borg_comparison} object from \code{borg_compare_cv()}.
#' @param type Character. Plot type: \code{"boxplot"} (default), \code{"density"},
#'   or \code{"paired"}.
#' @param ... Additional arguments (currently unused).
#'
#' @return A \code{ggplot} object.
#'
#' @details Requires the \pkg{ggplot2} package.
#'
#' @examples
#' \donttest{
#' if (requireNamespace("ggplot2", quietly = TRUE)) {
#'   set.seed(42)
#'   d <- data.frame(
#'     site = rep(1:20, each = 10),
#'     x = rnorm(200),
#'     y = rep(rnorm(20, sd = 2), each = 10) + rnorm(200, sd = 0.5)
#'   )
#'   comp <- borg_compare_cv(d, formula = y ~ x, groups = "site", repeats = 3)
#'   ggplot2::autoplot(comp)
#' }
#' }
#'
#' @exportS3Method ggplot2::autoplot
autoplot.borg_comparison <- function(object,
                                      type = c("boxplot", "density", "paired"),
                                      ...) {
  check_ggplot2()
  type <- match.arg(type)

  combined <- rbind(object$random_cv, object$blocked_cv)
  metric_label <- toupper(object$inflation$metric)

  switch(type,
    "boxplot" = {
      p <- ggplot2::ggplot(combined,
                           ggplot2::aes(x = .data$cv_type, y = .data$metric_value,
                                        fill = .data$cv_type)) +
        ggplot2::geom_boxplot(width = 0.5, show.legend = FALSE) +
        ggplot2::scale_fill_manual(values = c(blocked = "#A8D5BA", random = "#E8B4B8")) +
        ggplot2::labs(
          title = "Random vs Blocked Cross-Validation",
          subtitle = sprintf("Inflation: %.1f%% | p = %.4f",
                             abs(object$inflation$estimate) * 100, object$p_value),
          x = NULL, y = metric_label
        ) +
        borg_theme()
      p
    },
    "density" = {
      ggplot2::ggplot(combined,
                      ggplot2::aes(x = .data$metric_value, fill = .data$cv_type,
                                   color = .data$cv_type)) +
        ggplot2::geom_density(alpha = 0.3, linewidth = 1) +
        ggplot2::scale_fill_manual(values = c(blocked = "#27AE60", random = "#E74C3C")) +
        ggplot2::scale_color_manual(values = c(blocked = "#27AE60", random = "#E74C3C")) +
        ggplot2::labs(
          title = "Distribution of CV Estimates",
          x = metric_label, y = "Density", fill = "CV Type", color = "CV Type"
        ) +
        borg_theme()
    },
    "paired" = {
      ggplot2::ggplot(
        data.frame(
          random = object$random_cv$metric_value,
          blocked = object$blocked_cv$metric_value
        ),
        ggplot2::aes(x = .data$random, y = .data$blocked)
      ) +
        ggplot2::geom_abline(slope = 1, intercept = 0, linetype = "dashed",
                             color = "gray50") +
        ggplot2::geom_point(color = "#3498DB", size = 3, alpha = 0.7) +
        ggplot2::labs(
          title = "Paired Comparison",
          x = sprintf("Random CV %s", metric_label),
          y = sprintf("Blocked CV %s", metric_label)
        ) +
        borg_theme()
    }
  )
}


# autoplot.BorgDiagnosis ---------------------------------------------------

#' Autoplot Method for BorgDiagnosis Objects
#'
#' Creates a summary panel of detected dependency diagnostics.
#'
#' @param object A \code{\link{BorgDiagnosis}} object.
#' @param ... Additional arguments (currently unused).
#'
#' @return A \code{ggplot} object.
#'
#' @details Requires the \pkg{ggplot2} package.
#'
#' @examples
#' \donttest{
#' if (requireNamespace("ggplot2", quietly = TRUE)) {
#'   set.seed(42)
#'   d <- data.frame(
#'     site = rep(1:20, each = 10),
#'     value = rep(rnorm(20, sd = 2), each = 10) + rnorm(200, sd = 0.5)
#'   )
#'   diag <- borg_diagnose(d, groups = "site", target = "value")
#'   ggplot2::autoplot(diag)
#' }
#' }
#'
#' @exportS3Method ggplot2::autoplot
autoplot.BorgDiagnosis <- function(object, ...) {
  check_ggplot2()

  # Build diagnostics data frame
  metrics <- data.frame(
    metric = character(0),
    value = numeric(0),
    detected = logical(0),
    stringsAsFactors = FALSE
  )

  if (!is.null(object@spatial$morans_i) && !is.na(object@spatial$morans_i)) {
    metrics <- rbind(metrics, data.frame(
      metric = "Moran's I", value = object@spatial$morans_i,
      detected = object@spatial$detected, stringsAsFactors = FALSE
    ))
  }
  if (!is.null(object@temporal$acf_lag1) && !is.na(object@temporal$acf_lag1)) {
    metrics <- rbind(metrics, data.frame(
      metric = "ACF lag-1", value = object@temporal$acf_lag1,
      detected = object@temporal$detected, stringsAsFactors = FALSE
    ))
  }
  if (!is.null(object@clustered$icc) && !is.na(object@clustered$icc)) {
    metrics <- rbind(metrics, data.frame(
      metric = "ICC", value = object@clustered$icc,
      detected = object@clustered$detected, stringsAsFactors = FALSE
    ))
  }

  if (nrow(metrics) == 0) {
    df <- data.frame(x = 0.5, y = 0.5, label = "No dependencies detected")
    return(
      ggplot2::ggplot(df, ggplot2::aes(x = .data$x, y = .data$y)) +
        ggplot2::geom_text(ggplot2::aes(label = .data$label),
                           size = 6, color = borg_palette["valid"]) +
        ggplot2::theme_void() +
        ggplot2::ggtitle("BORG Diagnosis: No Dependencies")
    )
  }

  metrics$metric <- factor(metrics$metric, levels = rev(metrics$metric))

  ggplot2::ggplot(metrics, ggplot2::aes(
    x = .data$metric, y = .data$value,
    fill = .data$detected
  )) +
    ggplot2::geom_col(width = 0.6) +
    ggplot2::geom_text(ggplot2::aes(label = sprintf("%.3f", .data$value)),
                       hjust = -0.2, size = 3.5) +
    ggplot2::scale_fill_manual(
      values = c("TRUE" = "#E94F37", "FALSE" = "#A8D5BA"),
      labels = c("TRUE" = "Detected", "FALSE" = "Not significant")
    ) +
    ggplot2::coord_flip(clip = "off") +
    ggplot2::labs(
      title = sprintf("BORG Diagnosis: %s (%s)",
                       toupper(object@dependency_type), object@severity),
      subtitle = sprintf("Recommended CV: %s", object@recommended_cv),
      x = NULL, y = "Value", fill = NULL
    ) +
    borg_theme()
}


# Internal helpers ---------------------------------------------------------

#' Split tile plot for a single fold (Internal)
#' @noRd
autoplot_split <- function(train_idx, test_idx, fold = 1) {
  n <- max(c(train_idx, test_idx))
  role <- rep("excluded", n)
  role[train_idx] <- "train"
  role[test_idx] <- "test"

  overlap <- intersect(train_idx, test_idx)
  role[overlap] <- "overlap"

  df <- data.frame(
    idx = seq_len(n),
    role = factor(role, levels = c("train", "test", "overlap", "excluded"))
  )

  ggplot2::ggplot(df, ggplot2::aes(x = .data$idx, y = 1, fill = .data$role)) +
    ggplot2::geom_tile(height = 0.8) +
    ggplot2::scale_fill_manual(values = borg_palette) +
    ggplot2::labs(title = sprintf("Fold %d Split", fold),
                  x = "Observation", y = NULL, fill = NULL) +
    borg_theme() +
    ggplot2::theme(
      axis.text.y = ggplot2::element_blank(),
      panel.grid = ggplot2::element_blank()
    )
}


#' Spatial scatter for a single fold (Internal)
#' @noRd
autoplot_spatial_single <- function(train_idx, test_idx, data, coords, fold = 1) {
  if (is.null(data)) {
    stop("'data' is required for spatial autoplot")
  }

  use_sf <- inherits(data, "sf") && requireNamespace("sf", quietly = TRUE)

  if (use_sf) {
    coord_info <- extract_coords(data)
    df <- sf::st_drop_geometry(data)
  } else if (inherits(data, "SpatVector") && requireNamespace("terra", quietly = TRUE)) {
    coord_info <- extract_coords(data)
    df <- extract_data_frame(data)
    use_sf <- FALSE
  } else {
    if (is.null(coords)) stop("'coords' required for data.frame spatial plots")
    coord_info <- extract_coords(data, coords)
    df <- data
  }

  n <- nrow(df)
  role <- rep("excluded", n)
  role[train_idx] <- "train"
  role[test_idx] <- "test"
  overlap <- intersect(train_idx, test_idx)
  role[overlap] <- "overlap"

  plot_df <- data.frame(
    x = coord_info$x,
    y = coord_info$y,
    role = factor(role, levels = c("train", "test", "overlap", "excluded"))
  )

  # Use geom_sf if data was sf and we can reconstruct geometry
  if (use_sf && requireNamespace("sf", quietly = TRUE)) {
    plot_sf <- sf::st_as_sf(plot_df, coords = c("x", "y"),
                            crs = sf::st_crs(data))
    p <- ggplot2::ggplot(plot_sf) +
      ggplot2::geom_sf(ggplot2::aes(color = .data$role), size = 1.5, alpha = 0.7) +
      ggplot2::scale_color_manual(values = borg_palette) +
      ggplot2::labs(title = sprintf("Fold %d Spatial Split", fold),
                    color = NULL) +
      borg_theme()
  } else {
    p <- ggplot2::ggplot(plot_df, ggplot2::aes(
      x = .data$x, y = .data$y, color = .data$role
    )) +
      ggplot2::geom_point(size = 1.5, alpha = 0.7) +
      ggplot2::scale_color_manual(values = borg_palette) +
      ggplot2::coord_equal() +
      ggplot2::labs(title = sprintf("Fold %d Spatial Split", fold),
                    x = coord_info$coord_names[1],
                    y = coord_info$coord_names[2],
                    color = NULL) +
      borg_theme()
  }

  p
}


#' Faceted spatial map across all folds (Internal)
#' @noRd
autoplot_spatial_faceted <- function(folds, data, coords) {
  if (is.null(data)) {
    stop("'data' is required for spatial autoplot. Pass the original data.")
  }

  use_sf <- inherits(data, "sf") && requireNamespace("sf", quietly = TRUE)

  if (use_sf) {
    coord_info <- extract_coords(data)
  } else if (inherits(data, "SpatVector") && requireNamespace("terra", quietly = TRUE)) {
    coord_info <- extract_coords(data)
  } else {
    if (is.null(coords)) stop("'coords' required for data.frame spatial plots")
    coord_info <- extract_coords(data, coords)
  }

  n <- length(coord_info$x)
  n_folds <- length(folds)

  # Build long-format data
  rows <- vector("list", n_folds)
  for (i in seq_len(n_folds)) {
    role <- rep("excluded", n)
    role[folds[[i]]$train] <- "train"
    role[folds[[i]]$test] <- "test"
    rows[[i]] <- data.frame(
      x = coord_info$x,
      y = coord_info$y,
      role = role,
      fold = sprintf("Fold %d", i),
      stringsAsFactors = FALSE
    )
  }
  plot_df <- do.call(rbind, rows)
  plot_df$role <- factor(plot_df$role, levels = c("train", "test", "excluded"))

  if (use_sf && requireNamespace("sf", quietly = TRUE)) {
    plot_sf <- sf::st_as_sf(plot_df, coords = c("x", "y"),
                            crs = sf::st_crs(data))
    p <- ggplot2::ggplot(plot_sf) +
      ggplot2::geom_sf(ggplot2::aes(color = .data$role), size = 0.8, alpha = 0.7) +
      ggplot2::scale_color_manual(values = borg_palette) +
      ggplot2::facet_wrap(~ fold) +
      ggplot2::labs(title = "Spatial CV Folds", color = NULL) +
      borg_theme()
  } else {
    p <- ggplot2::ggplot(plot_df, ggplot2::aes(
      x = .data$x, y = .data$y, color = .data$role
    )) +
      ggplot2::geom_point(size = 0.8, alpha = 0.7) +
      ggplot2::scale_color_manual(values = borg_palette) +
      ggplot2::facet_wrap(~ fold) +
      ggplot2::coord_equal() +
      ggplot2::labs(title = "Spatial CV Folds",
                    x = coord_info$coord_names[1],
                    y = coord_info$coord_names[2],
                    color = NULL) +
      borg_theme()
  }

  p
}


#' Fold tile matrix (Internal)
#' @noRd
autoplot_folds_tile <- function(folds) {
  n_folds <- length(folds)
  n <- max(vapply(folds, function(f) max(c(f$train, f$test)), integer(1)))

  rows <- vector("list", n_folds)
  for (i in seq_len(n_folds)) {
    role <- rep("excluded", n)
    role[folds[[i]]$train] <- "train"
    role[folds[[i]]$test] <- "test"
    rows[[i]] <- data.frame(
      obs = seq_len(n),
      fold = i,
      role = role,
      stringsAsFactors = FALSE
    )
  }
  plot_df <- do.call(rbind, rows)
  plot_df$role <- factor(plot_df$role, levels = c("train", "test", "excluded"))

  ggplot2::ggplot(plot_df, ggplot2::aes(
    x = .data$obs, y = factor(.data$fold), fill = .data$role
  )) +
    ggplot2::geom_tile() +
    ggplot2::scale_fill_manual(values = borg_palette) +
    ggplot2::labs(title = "Cross-Validation Fold Structure",
                  x = "Observation", y = "Fold", fill = NULL) +
    borg_theme() +
    ggplot2::theme(panel.grid = ggplot2::element_blank())
}


#' Fold sizes bar chart (Internal)
#' @noRd
autoplot_fold_sizes <- function(folds) {
  n_folds <- length(folds)

  df <- data.frame(
    fold = rep(seq_len(n_folds), 2),
    set = rep(c("train", "test"), each = n_folds),
    size = c(
      vapply(folds, function(f) length(f$train), integer(1)),
      vapply(folds, function(f) length(f$test), integer(1))
    )
  )
  df$set <- factor(df$set, levels = c("train", "test"))

  ggplot2::ggplot(df, ggplot2::aes(
    x = factor(.data$fold), y = .data$size, fill = .data$set
  )) +
    ggplot2::geom_col(position = "dodge", width = 0.7) +
    ggplot2::scale_fill_manual(values = borg_palette[c("train", "test")]) +
    ggplot2::labs(title = "Fold Sizes",
                  x = "Fold", y = "Number of observations", fill = NULL) +
    borg_theme()
}
