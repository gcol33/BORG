# ===========================================================================
# BORG ggplot2 Visualization — autoplot() methods
# ===========================================================================

# Suppress R CMD check NSE notes for ggplot2 aes() variables
utils::globalVariables(c(".data", "geometry"))

# Palette and theme --------------------------------------------------------

borg_palette <- c(
  train    = "#1B4F72",
  test     = "#C0392B",
  overlap  = "#E67E22",
  valid    = "#1E8449",
  excluded = "#D5D8DC"
)

# Secondary palette for CV comparison
borg_cv_palette <- c(
  random  = "#E74C3C",
  blocked = "#2ECC71"
)

#' ggplot2 theme for BORG plots (Internal)
#' @noRd
borg_theme <- function(base_size = 11) {
  ggplot2::theme_minimal(base_size = base_size) +
    ggplot2::theme(
      # Typography
      plot.title = ggplot2::element_text(
        size = ggplot2::rel(1.2), face = "bold", margin = ggplot2::margin(b = 4)
      ),
      plot.subtitle = ggplot2::element_text(
        size = ggplot2::rel(0.85), color = "gray40", margin = ggplot2::margin(b = 8)
      ),
      plot.caption = ggplot2::element_text(
        size = ggplot2::rel(0.7), color = "gray50", hjust = 1
      ),

      # Panel
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major = ggplot2::element_line(color = "gray92", linewidth = 0.3),
      panel.background = ggplot2::element_rect(fill = "gray99", color = NA),

      # Strip (facets)
      strip.text = ggplot2::element_text(
        face = "bold", size = ggplot2::rel(0.9), margin = ggplot2::margin(b = 4, t = 4)
      ),
      strip.background = ggplot2::element_rect(fill = "gray95", color = NA),

      # Legend
      legend.position = "bottom",
      legend.background = ggplot2::element_rect(fill = NA, color = NA),
      legend.key.size = ggplot2::unit(0.8, "lines"),
      legend.text = ggplot2::element_text(size = ggplot2::rel(0.8)),

      # Margins
      plot.margin = ggplot2::margin(10, 10, 10, 10)
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
#' @param show_fixes Logical. If TRUE (default), annotate each risk with a
#'   suggested fix from \code{borg_assimilate()} or manual action. If FALSE,
#'   show the risk description instead.
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
autoplot.BorgRisk <- function(object, max_risks = 10, show_fixes = TRUE, ...) {
  check_ggplot2()

  risks <- object@risks

  if (length(risks) == 0) {
    df <- data.frame(x = 0.5, y = 0.5)
    p <- ggplot2::ggplot(df, ggplot2::aes(x = .data$x, y = .data$y)) +
      ggplot2::annotate("text", x = 0.5, y = 0.55, label = "VALID",
                        size = 16, fontface = "bold", color = borg_palette["valid"]) +
      ggplot2::annotate("text", x = 0.5, y = 0.42, label = "No risks detected",
                        size = 5, color = "gray40") +
      ggplot2::xlim(0, 1) + ggplot2::ylim(0, 1) +
      ggplot2::theme_void() +
      ggplot2::labs(title = "BORG Risk Assessment")
    return(p)
  }

  if (length(risks) > max_risks) risks <- risks[seq_len(max_risks)]

  df <- data.frame(
    type = vapply(risks, function(r) r$type, character(1)),
    severity = vapply(risks, function(r) r$severity, character(1)),
    description = vapply(risks, function(r) {
      d <- r$description
      if (nchar(d) > 55) paste0(substr(d, 1, 52), "...") else d
    }, character(1)),
    fix = vapply(risks, function(r) {
      f <- .suggest_fix(r$type)
      if (nchar(f) > 50) paste0(substr(f, 1, 47), "...") else f
    }, character(1)),
    n_affected = vapply(risks, function(r) {
      length(r$affected_indices %||% integer(0))
    }, integer(1)),
    stringsAsFactors = FALSE
  )

  # Order by severity (hard first) then by n_affected
  sev_order <- order(df$severity == "hard_violation", df$n_affected, decreasing = TRUE)
  df <- df[sev_order, ]
  df$type <- factor(df$type, levels = rev(df$type))

  label_col <- if (show_fixes) "fix" else "description"

  # Y-axis = n_affected (meaningful), with log scale if range is large
  max_affected <- max(df$n_affected, na.rm = TRUE)
  y_vals <- pmax(df$n_affected, 1)  # floor at 1 for log safety

  sev_colors <- c(hard_violation = "#C0392B", soft_inflation = "#F39C12")
  sev_fills <- c(hard_violation = "#FADBD8", soft_inflation = "#FEF9E7")

  ggplot2::ggplot(df, ggplot2::aes(
    x = .data$type, y = .data$n_affected
  )) +
    # Stem
    ggplot2::geom_segment(
      ggplot2::aes(xend = .data$type, y = 0, yend = .data$n_affected,
                   color = .data$severity),
      linewidth = 1.2
    ) +
    # Head — size encodes severity visually
    ggplot2::geom_point(
      ggplot2::aes(color = .data$severity, fill = .data$severity),
      size = 5, shape = 21, stroke = 1.5
    ) +
    # Label: fix or description
    ggplot2::geom_text(
      ggplot2::aes(label = .data[[label_col]]),
      hjust = 0, nudge_y = max_affected * 0.04, size = 3, color = "gray30"
    ) +
    ggplot2::scale_color_manual(
      values = sev_colors,
      labels = c(hard_violation = "Hard violation", soft_inflation = "Soft inflation")
    ) +
    ggplot2::scale_fill_manual(
      values = sev_fills,
      labels = c(hard_violation = "Hard violation", soft_inflation = "Soft inflation")
    ) +
    ggplot2::coord_flip(clip = "off") +
    ggplot2::labs(
      title = "BORG Risk Assessment",
      subtitle = sprintf(
        "%d hard | %d soft | %s",
        object@n_hard, object@n_soft,
        if (object@is_valid) "VALID" else "INVALID"
      ),
      x = NULL, y = "Affected observations", color = NULL, fill = NULL
    ) +
    borg_theme() +
    ggplot2::guides(fill = "none") +
    ggplot2::theme(
      panel.grid.major.y = ggplot2::element_blank(),
      plot.margin = ggplot2::margin(10, 80, 10, 10)
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
#' @param raster Optional \code{terra::SpatRaster}. When provided with
#'   \code{type = "spatial"}, the first layer is rendered as a background
#'   map behind the fold points. Requires the \pkg{tidyterra} package.
#' @param time Character name of the time column in \code{data}, or a vector of
#'   time values (length = \code{nrow(data)}). Required for \code{type = "temporal"}.
#' @param groups Character name of the grouping column in \code{data}, or a
#'   vector of group labels. Required for \code{type = "groups"}.
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
                                  raster = NULL,
                                  time = NULL,
                                  groups = NULL,
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
  spatial_meta <- attr(object$folds, "spatial_meta")

  switch(type,
    "split" = autoplot_split(train_idx, test_idx, fold),
    "spatial" = autoplot_spatial_single(train_idx, test_idx, data, coords, fold,
                                        spatial_meta = spatial_meta, raster = raster),
    "temporal" = autoplot_temporal(train_idx, test_idx, data, time, fold),
    "groups" = autoplot_groups(train_idx, test_idx, data, groups, fold)
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
#' @param raster Optional \code{terra::SpatRaster}. When provided with
#'   \code{type = "spatial"}, the first layer is rendered as a background
#'   map behind the fold points. Requires the \pkg{tidyterra} package.
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
                              raster = NULL,
                              ...) {
  check_ggplot2()
  type <- match.arg(type)

  folds <- object$folds
  spatial_meta <- attr(folds, "spatial_meta")

  switch(type,
    "folds" = autoplot_folds_tile(folds),
    "spatial" = autoplot_spatial_faceted(folds, data, coords,
                                         spatial_meta = spatial_meta,
                                         raster = raster),
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
  inflation_pct <- abs(object$inflation$estimate) * 100
  p_val <- object$p_value

  # Significance marker
  sig_label <- if (p_val < 0.001) "p < 0.001"
               else if (p_val < 0.01) sprintf("p = %.3f", p_val)
               else sprintf("p = %.2f", p_val)

  switch(type,
    "boxplot" = {
      random_mean <- mean(object$random_cv$metric_value)
      blocked_mean <- mean(object$blocked_cv$metric_value)

      p <- ggplot2::ggplot(combined,
                           ggplot2::aes(x = .data$cv_type, y = .data$metric_value,
                                        fill = .data$cv_type)) +
        # Boxplot with reduced width
        ggplot2::geom_boxplot(
          width = 0.45, outlier.shape = NA, alpha = 0.7,
          color = "gray40", linewidth = 0.4
        ) +
        # Jittered raw points
        ggplot2::geom_jitter(
          width = 0.12, size = 1.8, alpha = 0.5, shape = 16,
          ggplot2::aes(color = .data$cv_type), show.legend = FALSE
        ) +
        # Mean diamond
        ggplot2::stat_summary(
          fun = mean, geom = "point", shape = 23, size = 3.5,
          fill = "white", color = "gray20", stroke = 1
        ) +
        # Inflation arrow annotation
        ggplot2::annotate(
          "segment",
          x = 1.6, xend = 1.6,
          y = blocked_mean, yend = random_mean,
          arrow = ggplot2::arrow(length = ggplot2::unit(0.2, "cm"), ends = "both"),
          color = "gray30", linewidth = 0.6
        ) +
        ggplot2::annotate(
          "label",
          x = 1.6, y = (random_mean + blocked_mean) / 2,
          label = sprintf("%.1f%%", inflation_pct),
          size = 3.2, fontface = "bold",
          fill = "#FEF9E7", color = "#C0392B",
          label.padding = ggplot2::unit(0.2, "lines")
        ) +
        ggplot2::scale_fill_manual(
          values = c(blocked = "#D5F5E3", random = "#FADBD8")
        ) +
        ggplot2::scale_color_manual(
          values = c(blocked = unname(borg_cv_palette["blocked"]),
                     random = unname(borg_cv_palette["random"]))
        ) +
        ggplot2::labs(
          title = "Random vs Blocked Cross-Validation",
          subtitle = sprintf("Inflation: %.1f%% | %s", inflation_pct, sig_label),
          x = NULL, y = metric_label, fill = NULL
        ) +
        borg_theme() +
        ggplot2::guides(fill = ggplot2::guide_legend(override.aes = list(alpha = 1)))
      p
    },
    "density" = {
      ggplot2::ggplot(combined,
                      ggplot2::aes(x = .data$metric_value, fill = .data$cv_type)) +
        ggplot2::geom_density(alpha = 0.35, linewidth = 0.7,
                              ggplot2::aes(color = .data$cv_type)) +
        # Mean lines
        ggplot2::geom_vline(
          xintercept = mean(object$random_cv$metric_value),
          linetype = "dashed", color = unname(borg_cv_palette["random"]), linewidth = 0.6
        ) +
        ggplot2::geom_vline(
          xintercept = mean(object$blocked_cv$metric_value),
          linetype = "dashed", color = unname(borg_cv_palette["blocked"]), linewidth = 0.6
        ) +
        ggplot2::scale_fill_manual(values = c(blocked = "#D5F5E3", random = "#FADBD8")) +
        ggplot2::scale_color_manual(values = c(blocked = unname(borg_cv_palette["blocked"]),
                                                random = unname(borg_cv_palette["random"]))) +
        ggplot2::labs(
          title = "Distribution of CV Estimates",
          subtitle = sprintf("Dashed lines = means | Inflation: %.1f%% | %s",
                             inflation_pct, sig_label),
          x = metric_label, y = "Density", fill = "CV Type", color = "CV Type"
        ) +
        borg_theme()
    },
    "paired" = {
      paired_df <- data.frame(
        random = object$random_cv$metric_value,
        blocked = object$blocked_cv$metric_value
      )

      ggplot2::ggplot(paired_df, ggplot2::aes(x = .data$random, y = .data$blocked)) +
        # Identity line
        ggplot2::geom_abline(slope = 1, intercept = 0,
                             linetype = "dashed", color = "gray70", linewidth = 0.5) +
        # Shaded region where random > blocked (inflation zone)
        ggplot2::annotate(
          "rect",
          xmin = min(paired_df$random) - 0.05,
          xmax = max(paired_df$random) + 0.05,
          ymin = min(paired_df$blocked) - 0.05,
          ymax = min(paired_df$random) - 0.05,
          fill = "#FADBD8", alpha = 0.3
        ) +
        ggplot2::geom_point(color = "#2C3E50", size = 3, alpha = 0.7) +
        ggplot2::labs(
          title = "Paired Comparison",
          subtitle = sprintf(
            "Points below diagonal = inflated random CV | %s", sig_label
          ),
          x = sprintf("Random CV %s", metric_label),
          y = sprintf("Blocked CV %s", metric_label)
        ) +
        borg_theme() +
        ggplot2::coord_equal()
    }
  )
}


# autoplot.BorgDiagnosis ---------------------------------------------------

#' Autoplot Method for BorgDiagnosis Objects
#'
#' Creates a summary panel of detected dependency diagnostics.
#'
#' @param object A \code{\link{BorgDiagnosis}} object.
#' @param type Character. Plot type: \code{"summary"} (default) for dependency
#'   gauge panel, or \code{"variogram"} for empirical semivariogram showing
#'   spatial autocorrelation structure and suggested block size.
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
autoplot.BorgDiagnosis <- function(object, type = c("summary", "variogram"), ...) {
  check_ggplot2()
  type <- match.arg(type)

  if (type == "variogram") {
    return(autoplot_variogram(object))
  }

  # Build diagnostics data frame
  metrics <- data.frame(
    metric = character(0),
    value = numeric(0),
    threshold = numeric(0),
    detected = logical(0),
    stringsAsFactors = FALSE
  )

  if (!is.null(object@spatial$morans_i) && !is.na(object@spatial$morans_i)) {
    metrics <- rbind(metrics, data.frame(
      metric = "Moran's I",
      value = object@spatial$morans_i,
      threshold = 0.1,
      detected = object@spatial$detected, stringsAsFactors = FALSE
    ))
  }
  if (!is.null(object@temporal$acf_lag1) && !is.na(object@temporal$acf_lag1)) {
    metrics <- rbind(metrics, data.frame(
      metric = "ACF lag-1",
      value = object@temporal$acf_lag1,
      threshold = 0.2,
      detected = object@temporal$detected, stringsAsFactors = FALSE
    ))
  }
  if (!is.null(object@clustered$icc) && !is.na(object@clustered$icc)) {
    metrics <- rbind(metrics, data.frame(
      metric = "ICC",
      value = object@clustered$icc,
      threshold = 0.05,
      detected = object@clustered$detected, stringsAsFactors = FALSE
    ))
  }

  if (nrow(metrics) == 0) {
    df <- data.frame(x = 0.5, y = 0.5)
    return(
      ggplot2::ggplot(df, ggplot2::aes(x = .data$x, y = .data$y)) +
        ggplot2::annotate("text", x = 0.5, y = 0.55, label = "No dependencies",
                          size = 8, fontface = "bold", color = borg_palette["valid"]) +
        ggplot2::annotate("text", x = 0.5, y = 0.42,
                          label = "Random CV is appropriate",
                          size = 4, color = "gray40") +
        ggplot2::xlim(0, 1) + ggplot2::ylim(0, 1) +
        ggplot2::theme_void() +
        ggplot2::labs(title = "BORG Diagnosis")
    )
  }

  metrics$metric <- factor(metrics$metric, levels = rev(metrics$metric))
  metrics$status <- ifelse(metrics$detected, "Detected", "Below threshold")
  # Color for the bar fill (threshold region)
  metrics$bar_fill <- ifelse(metrics$detected, "#FADBD8", "#D5F5E3")

  y_max <- max(metrics$value, metrics$threshold) * 1.25

  ggplot2::ggplot(metrics, ggplot2::aes(x = .data$metric)) +
    # Threshold zone (shaded bar from 0 to threshold)
    ggplot2::geom_col(
      ggplot2::aes(y = .data$threshold),
      fill = "gray90", width = 0.5, alpha = 0.8
    ) +
    # Threshold line
    ggplot2::geom_errorbar(
      ggplot2::aes(ymin = .data$threshold, ymax = .data$threshold),
      width = 0.55, linewidth = 0.8, color = "gray50", linetype = "dashed"
    ) +
    # Lollipop stem
    ggplot2::geom_segment(
      ggplot2::aes(xend = .data$metric, y = 0, yend = .data$value,
                   color = .data$status),
      linewidth = 1.5
    ) +
    # Lollipop head
    ggplot2::geom_point(
      ggplot2::aes(y = .data$value, color = .data$status),
      size = 7
    ) +
    # Value label inside point
    ggplot2::geom_text(
      ggplot2::aes(y = .data$value, label = sprintf("%.2f", .data$value)),
      size = 2.5, fontface = "bold", color = "white"
    ) +
    ggplot2::scale_color_manual(
      values = c("Detected" = "#C0392B", "Below threshold" = "#27AE60")
    ) +
    ggplot2::coord_flip(clip = "off") +
    ggplot2::labs(
      title = sprintf("BORG Diagnosis: %s",
                       toupper(object@dependency_type)),
      subtitle = sprintf(
        "Severity: %s | Recommended CV: %s | Dashed line = detection threshold",
        object@severity, object@recommended_cv
      ),
      x = NULL, y = "Dependency strength", color = NULL
    ) +
    borg_theme() +
    ggplot2::theme(
      panel.grid.major.y = ggplot2::element_blank(),
      plot.margin = ggplot2::margin(10, 15, 10, 10)
    )
}


# Internal helpers ---------------------------------------------------------

#' Compute convex hull polygon for a set of 2D points (Internal)
#' @noRd
.hull_polygon <- function(x, y) {
  if (length(x) < 3) return(NULL)
  idx <- grDevices::chull(x, y)
  idx <- c(idx, idx[1])  # close polygon
  data.frame(x = x[idx], y = y[idx])
}


#' Split tile plot for a single fold (Internal)
#' @noRd
autoplot_split <- function(train_idx, test_idx, fold = 1) {
  n <- max(c(train_idx, test_idx))
  role <- rep("excluded", n)
  role[train_idx] <- "train"
  role[test_idx] <- "test"

  overlap <- intersect(train_idx, test_idx)
  role[overlap] <- "overlap"

  n_train <- length(setdiff(train_idx, overlap))
  n_test <- length(setdiff(test_idx, overlap))
  n_overlap <- length(overlap)

  df <- data.frame(
    idx = seq_len(n),
    role = factor(role, levels = c("train", "test", "overlap", "excluded"))
  )

  sub <- sprintf(
    "n = %d | Train: %d (%.0f%%) | Test: %d (%.0f%%)%s",
    n, n_train, 100 * n_train / n,
    n_test, 100 * n_test / n,
    if (n_overlap > 0) sprintf(" | Overlap: %d", n_overlap) else ""
  )

  ggplot2::ggplot(df, ggplot2::aes(x = .data$idx, y = 1, fill = .data$role)) +
    ggplot2::geom_tile(height = 0.6) +
    ggplot2::scale_fill_manual(values = borg_palette) +
    ggplot2::labs(title = sprintf("Fold %d Split", fold),
                  subtitle = sub,
                  x = "Observation index", y = NULL, fill = NULL) +
    borg_theme() +
    ggplot2::theme(
      axis.text.y = ggplot2::element_blank(),
      axis.ticks.y = ggplot2::element_blank(),
      panel.grid = ggplot2::element_blank()
    )
}


#' Spatial scatter for a single fold with convex hulls (Internal)
#' @noRd
autoplot_spatial_single <- function(train_idx, test_idx, data, coords, fold = 1,
                                     spatial_meta = NULL, raster = NULL) {
  if (is.null(data)) {
    stop("'data' is required for spatial autoplot")
  }

  n <- if (inherits(data, "SpatVector")) terra::nrow(data) else nrow(data)
  role <- rep("excluded", n)
  role[train_idx] <- "train"
  role[test_idx] <- "test"
  overlap <- intersect(train_idx, test_idx)
  role[overlap] <- "overlap"
  role_fac <- factor(role, levels = c("train", "test", "overlap", "excluded"))

  # SpatVector path: use tidyterra if available, else convert to sf
  if (inherits(data, "SpatVector") && requireNamespace("terra", quietly = TRUE)) {
    data_copy <- data
    data_copy$role <- role_fac

    if (requireNamespace("tidyterra", quietly = TRUE)) {
      p <- ggplot2::ggplot(data_copy) +
        tidyterra::geom_spatvector(ggplot2::aes(color = .data$role),
                                   size = 1.5, alpha = 0.7) +
        ggplot2::scale_color_manual(values = borg_palette) +
        ggplot2::labs(title = sprintf("Fold %d Spatial Split", fold),
                      color = NULL) +
        borg_theme()
      return(p)
    }

    # Fallback: convert SpatVector -> sf
    if (requireNamespace("sf", quietly = TRUE)) {
      data <- sf::st_as_sf(data)
    } else {
      coord_info <- extract_coords(data)
      return(.build_spatial_point_plot(
        coord_info$x, coord_info$y, role_fac,
        coord_info$coord_names, train_idx, test_idx, fold, spatial_meta,
        raster = raster
      ))
    }
  }

  # sf path
  if (inherits(data, "sf") && requireNamespace("sf", quietly = TRUE)) {
    plot_sf <- data
    plot_sf$role <- role_fac
    p <- ggplot2::ggplot(plot_sf) +
      ggplot2::geom_sf(ggplot2::aes(color = .data$role), size = 1.5, alpha = 0.7) +
      ggplot2::scale_color_manual(values = borg_palette) +
      ggplot2::labs(title = sprintf("Fold %d Spatial Split", fold),
                    color = NULL) +
      borg_theme()
    return(p)
  }

  # data.frame path
  if (is.null(coords)) stop("'coords' required for data.frame spatial plots")
  coord_info <- extract_coords(data, coords)
  .build_spatial_point_plot(
    coord_info$x, coord_info$y, role_fac,
    coord_info$coord_names, train_idx, test_idx, fold, spatial_meta,
    raster = raster
  )
}


#' Build a spatial point plot with convex hulls and optional raster (Internal)
#' @noRd
.build_spatial_point_plot <- function(x, y, role_fac, coord_names,
                                      train_idx, test_idx, fold,
                                      spatial_meta = NULL, raster = NULL) {
  plot_df <- data.frame(x = x, y = y, role = role_fac)

  # Compute convex hulls for train and test sets
  train_hull <- .hull_polygon(x[train_idx], y[train_idx])
  test_hull <- .hull_polygon(x[test_idx], y[test_idx])

  p <- ggplot2::ggplot()

  # Raster background layer (if provided)
  raster_layer_name <- NULL
  if (!is.null(raster)) {
    raster_layer_name <- .add_raster_layer(p, raster)
    if (!is.null(raster_layer_name)) {
      p <- raster_layer_name$plot
      raster_layer_name <- raster_layer_name$name
    }
  }

  # Add convex hull polygons
  if (!is.null(train_hull)) {
    p <- p + ggplot2::geom_polygon(
      data = train_hull, ggplot2::aes(x = .data$x, y = .data$y),
      fill = borg_palette["train"], alpha = 0.08,
      color = borg_palette["train"], linewidth = 0.5, linetype = "dashed",
      inherit.aes = FALSE
    )
  }
  if (!is.null(test_hull)) {
    p <- p + ggplot2::geom_polygon(
      data = test_hull, ggplot2::aes(x = .data$x, y = .data$y),
      fill = borg_palette["test"], alpha = 0.08,
      color = borg_palette["test"], linewidth = 0.5, linetype = "dashed",
      inherit.aes = FALSE
    )
  }

  p <- p +
    ggplot2::geom_point(
      data = plot_df,
      ggplot2::aes(x = .data$x, y = .data$y, color = .data$role),
      size = 1.8, alpha = 0.7
    ) +
    ggplot2::scale_color_manual(values = borg_palette) +
    ggplot2::coord_equal() +
    ggplot2::labs(title = sprintf("Fold %d Spatial Split", fold),
                  x = coord_names[1], y = coord_names[2],
                  color = NULL) +
    borg_theme()

  p
}


#' Add a SpatRaster background layer to a ggplot (Internal)
#'
#' Renders the first layer of a SpatRaster using tidyterra::geom_spatraster().
#' Returns NULL silently if tidyterra is not available.
#'
#' @noRd
.add_raster_layer <- function(p, raster) {
  if (!requireNamespace("terra", quietly = TRUE)) return(NULL)
  if (!requireNamespace("tidyterra", quietly = TRUE)) {
    message("Install 'tidyterra' for raster background: install.packages('tidyterra')")
    return(NULL)
  }

  # Use first layer only
  if (terra::nlyr(raster) > 1) {
    raster <- raster[[1]]
  }

  layer_name <- names(raster)

  p <- p +
    tidyterra::geom_spatraster(data = raster, alpha = 0.6) +
    ggplot2::scale_fill_viridis_c(
      option = "viridis", na.value = "transparent",
      guide = ggplot2::guide_colorbar(
        title = layer_name,
        title.position = "top",
        barwidth = ggplot2::unit(8, "lines"),
        barheight = ggplot2::unit(0.5, "lines"),
        order = 2
      )
    )

  list(plot = p, name = layer_name)
}


#' Faceted spatial map across all folds with convex hulls (Internal)
#' @noRd
autoplot_spatial_faceted <- function(folds, data, coords, spatial_meta = NULL,
                                     raster = NULL) {
  if (is.null(data)) {
    stop("'data' is required for spatial autoplot. Pass the original data.")
  }

  n_folds <- length(folds)

  # SpatVector path: use tidyterra if available
  if (inherits(data, "SpatVector") && requireNamespace("terra", quietly = TRUE)) {
    n <- terra::nrow(data)

    if (requireNamespace("tidyterra", quietly = TRUE) &&
        requireNamespace("sf", quietly = TRUE)) {
      data_sf <- sf::st_as_sf(data)
      rows <- vector("list", n_folds)
      for (i in seq_len(n_folds)) {
        role <- rep("excluded", n)
        role[folds[[i]]$train] <- "train"
        role[folds[[i]]$test] <- "test"
        fold_sf <- data_sf
        fold_sf$role <- role
        fold_sf$fold <- sprintf("Fold %d", i)
        rows[[i]] <- fold_sf
      }
      plot_sf <- do.call(rbind, rows)
      plot_sf$role <- factor(plot_sf$role, levels = c("train", "test", "excluded"))

      return(
        ggplot2::ggplot(plot_sf) +
          ggplot2::geom_sf(ggplot2::aes(color = .data$role),
                           size = 0.8, alpha = 0.7) +
          ggplot2::scale_color_manual(values = borg_palette) +
          ggplot2::facet_wrap(~ fold) +
          ggplot2::labs(title = "Spatial CV Folds", color = NULL) +
          borg_theme()
      )
    }

    coord_info <- extract_coords(data)
    return(.autoplot_spatial_faceted_coords(folds, coord_info, n, n_folds,
                                            spatial_meta = spatial_meta,
                                            raster = raster))
  }

  # sf path
  if (inherits(data, "sf") && requireNamespace("sf", quietly = TRUE)) {
    n <- nrow(data)
    rows <- vector("list", n_folds)
    for (i in seq_len(n_folds)) {
      role <- rep("excluded", n)
      role[folds[[i]]$train] <- "train"
      role[folds[[i]]$test] <- "test"
      fold_sf <- data
      fold_sf$role <- role
      fold_sf$fold <- sprintf("Fold %d", i)
      rows[[i]] <- fold_sf
    }
    plot_sf <- do.call(rbind, rows)
    plot_sf$role <- factor(plot_sf$role, levels = c("train", "test", "excluded"))

    return(
      ggplot2::ggplot(plot_sf) +
        ggplot2::geom_sf(ggplot2::aes(color = .data$role), size = 0.8, alpha = 0.7) +
        ggplot2::scale_color_manual(values = borg_palette) +
        ggplot2::facet_wrap(~ fold) +
        ggplot2::labs(title = "Spatial CV Folds", color = NULL) +
        borg_theme()
    )
  }

  # data.frame path
  if (is.null(coords)) stop("'coords' required for data.frame spatial plots")
  coord_info <- extract_coords(data, coords)
  n <- nrow(data)
  .autoplot_spatial_faceted_coords(folds, coord_info, n, n_folds,
                                   spatial_meta = spatial_meta,
                                   raster = raster)
}


#' Faceted spatial plot with per-fold convex hulls (Internal)
#' @noRd
.autoplot_spatial_faceted_coords <- function(folds, coord_info, n, n_folds,
                                              spatial_meta = NULL,
                                              raster = NULL) {
  rows <- vector("list", n_folds)
  hull_rows <- vector("list", n_folds)

  for (i in seq_len(n_folds)) {
    role <- rep("excluded", n)
    role[folds[[i]]$train] <- "train"
    role[folds[[i]]$test] <- "test"
    fold_label <- sprintf("Fold %d", i)
    rows[[i]] <- data.frame(
      x = coord_info$x,
      y = coord_info$y,
      role = role,
      fold = fold_label,
      stringsAsFactors = FALSE
    )

    # Build convex hull for test set in this fold
    test_hull <- .hull_polygon(coord_info$x[folds[[i]]$test],
                                coord_info$y[folds[[i]]$test])
    if (!is.null(test_hull)) {
      test_hull$fold <- fold_label
      hull_rows[[i]] <- test_hull
    }
  }
  plot_df <- do.call(rbind, rows)
  plot_df$role <- factor(plot_df$role, levels = c("train", "test", "excluded"))

  hull_df <- do.call(rbind, hull_rows)

  p <- ggplot2::ggplot()

  # Raster background layer (if provided)
  if (!is.null(raster)) {
    raster_info <- .add_raster_layer(p, raster)
    if (!is.null(raster_info)) {
      p <- raster_info$plot
    }
  }

  # Add test hull polygons per fold
  if (!is.null(hull_df) && nrow(hull_df) > 0) {
    p <- p + ggplot2::geom_polygon(
      data = hull_df,
      ggplot2::aes(x = .data$x, y = .data$y, group = .data$fold),
      fill = borg_palette["test"], alpha = 0.1,
      color = borg_palette["test"], linewidth = 0.4, linetype = "dashed",
      inherit.aes = FALSE
    )
  }

  p +
    ggplot2::geom_point(
      data = plot_df,
      ggplot2::aes(x = .data$x, y = .data$y, color = .data$role),
      size = 0.8, alpha = 0.7
    ) +
    ggplot2::scale_color_manual(values = borg_palette) +
    ggplot2::facet_wrap(~ fold) +
    ggplot2::coord_equal() +
    ggplot2::labs(
      title = "Spatial CV Folds",
      subtitle = "Dashed outline = test set boundary",
      x = coord_info$coord_names[1],
      y = coord_info$coord_names[2],
      color = NULL
    ) +
    borg_theme()
}


#' Fold tile matrix with annotations (Internal)
#' @noRd
autoplot_folds_tile <- function(folds) {
  n_folds <- length(folds)
  n <- max(vapply(folds, function(f) max(c(f$train, f$test)), integer(1)))

  rows <- vector("list", n_folds)
  size_rows <- vector("list", n_folds)
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
    size_rows[[i]] <- data.frame(
      fold = i,
      n_train = length(folds[[i]]$train),
      n_test = length(folds[[i]]$test),
      stringsAsFactors = FALSE
    )
  }
  plot_df <- do.call(rbind, rows)
  plot_df$role <- factor(plot_df$role, levels = c("train", "test", "excluded"))
  size_df <- do.call(rbind, size_rows)

  # Fold size annotation on the right margin
  size_df$label <- sprintf("%d / %d", size_df$n_train, size_df$n_test)

  ggplot2::ggplot(plot_df, ggplot2::aes(
    x = .data$obs, y = factor(.data$fold), fill = .data$role
  )) +
    ggplot2::geom_tile(color = NA) +
    # Fold size labels on right
    ggplot2::geom_text(
      data = size_df,
      ggplot2::aes(x = n + n * 0.02, y = factor(.data$fold), label = .data$label),
      inherit.aes = FALSE, hjust = 0, size = 2.8, color = "gray40"
    ) +
    ggplot2::scale_fill_manual(values = borg_palette) +
    ggplot2::labs(
      title = "Cross-Validation Fold Structure",
      subtitle = sprintf("%d folds | %d observations | Annotation: train / test count",
                         n_folds, n),
      x = "Observation index", y = "Fold", fill = NULL
    ) +
    borg_theme() +
    ggplot2::theme(
      panel.grid = ggplot2::element_blank(),
      plot.margin = ggplot2::margin(10, 60, 10, 10)
    ) +
    ggplot2::coord_cartesian(clip = "off")
}


#' Temporal split with shaded train/test regions (Internal)
#' @noRd
autoplot_temporal <- function(train_idx, test_idx, data, time, fold = 1) {
  if (is.null(time)) {
    stop("'time' required for temporal autoplot")
  }

  # Resolve time: column name or raw vector
  if (is.character(time) && length(time) == 1 && !is.null(data)) {
    if (!time %in% names(data)) {
      stop(sprintf("Column '%s' not found in data", time))
    }
    time_vals <- data[[time]]
    time_label <- time
  } else {
    time_vals <- time
    time_label <- "Time"
  }

  n <- length(time_vals)
  role <- rep("excluded", n)
  role[train_idx] <- "train"
  role[test_idx] <- "test"

  # Detect look-ahead violations
  max_train_time <- max(time_vals[train_idx], na.rm = TRUE)
  min_test_time <- min(time_vals[test_idx], na.rm = TRUE)
  violations <- test_idx[time_vals[test_idx] < max_train_time]
  role[violations] <- "overlap"

  time_numeric <- as.numeric(time_vals)
  max_train_num <- as.numeric(max_train_time)
  min_test_num <- as.numeric(min_test_time)

  df <- data.frame(
    time = time_numeric,
    role = factor(role, levels = c("train", "test", "overlap", "excluded"))
  )

  time_range <- range(time_numeric, na.rm = TRUE)
  time_pad <- diff(time_range) * 0.02

  # Subtitle
  if (length(violations) > 0) {
    sub_text <- sprintf("WARNING: %d test observations before max train time (look-ahead leak)",
                        length(violations))
  } else if (min_test_num > max_train_num) {
    gap <- min_test_num - max_train_num
    sub_text <- sprintf("Clean separation | Embargo gap: %.1f time units", gap)
  } else {
    sub_text <- "Clean temporal separation"
  }

  p <- ggplot2::ggplot(df, ggplot2::aes(x = .data$time))

  # Shaded train region
  p <- p + ggplot2::annotate(
    "rect",
    xmin = time_range[1] - time_pad, xmax = max_train_num,
    ymin = -Inf, ymax = Inf,
    fill = borg_palette["train"], alpha = 0.08
  )

  # Shaded test region
  p <- p + ggplot2::annotate(
    "rect",
    xmin = min_test_num, xmax = time_range[2] + time_pad,
    ymin = -Inf, ymax = Inf,
    fill = borg_palette["test"], alpha = 0.08
  )

  # Embargo gap (if exists)
  if (min_test_num > max_train_num) {
    p <- p + ggplot2::annotate(
      "rect",
      xmin = max_train_num, xmax = min_test_num,
      ymin = -Inf, ymax = Inf,
      fill = "gray80", alpha = 0.3
    )
  }

  # Cutoff line
  p <- p + ggplot2::geom_vline(
    xintercept = max_train_num,
    linetype = "solid", color = "gray30", linewidth = 0.8
  )

  # Rug marks for individual observations (compact, informative)
  p <- p +
    ggplot2::geom_point(
      ggplot2::aes(y = 0, color = .data$role),
      shape = "|", size = 4, alpha = 0.8
    ) +
    ggplot2::scale_color_manual(
      values = c(train = unname(borg_palette["train"]),
                 test = unname(borg_palette["test"]),
                 overlap = unname(borg_palette["overlap"]),
                 excluded = unname(borg_palette["excluded"])),
      labels = c(train = "Train", test = "Test",
                 overlap = "Look-ahead violation", excluded = "Excluded")
    ) +
    # Region labels — positioned to avoid overlap when leak exists
    ggplot2::annotate(
      "text",
      x = time_range[1] + diff(time_range) * 0.05, y = 0.7,
      label = sprintf("TRAIN (n=%d)", length(train_idx)),
      fontface = "bold", size = 4, hjust = 0,
      color = unname(borg_palette["train"]), alpha = 0.5
    ) +
    ggplot2::annotate(
      "text",
      x = time_range[2] - diff(time_range) * 0.05, y = 0.7,
      label = sprintf("TEST (n=%d)", length(test_idx)),
      fontface = "bold", size = 4, hjust = 1,
      color = unname(borg_palette["test"]), alpha = 0.5
    ) +
    ggplot2::ylim(-0.5, 1) +
    ggplot2::labs(
      title = sprintf("Fold %d Temporal Split", fold),
      subtitle = sub_text,
      x = time_label, y = NULL, color = NULL
    ) +
    borg_theme() +
    ggplot2::theme(
      axis.text.y = ggplot2::element_blank(),
      axis.ticks.y = ggplot2::element_blank(),
      panel.grid.major.y = ggplot2::element_blank(),
      panel.grid.minor.y = ggplot2::element_blank()
    )

  p
}


#' Groups split autoplot (Internal)
#' @noRd
autoplot_groups <- function(train_idx, test_idx, data, groups, fold = 1) {
  if (is.null(groups)) {
    stop("'groups' required for groups autoplot")
  }

  # Resolve groups: column name or raw vector
  if (is.character(groups) && length(groups) == 1 && !is.null(data)) {
    if (!groups %in% names(data)) {
      stop(sprintf("Column '%s' not found in data", groups))
    }
    group_vals <- data[[groups]]
  } else {
    group_vals <- groups
  }

  unique_groups <- unique(group_vals)
  train_groups <- unique(group_vals[train_idx])
  test_groups <- unique(group_vals[test_idx])
  overlap_groups <- intersect(train_groups, test_groups)
  train_only <- setdiff(train_groups, test_groups)
  test_only <- setdiff(test_groups, train_groups)

  # Build per-group summary
  group_data <- data.frame(
    group = unique_groups,
    n_obs = vapply(unique_groups, function(g) sum(group_vals == g), integer(1)),
    assignment = vapply(unique_groups, function(g) {
      if (g %in% overlap_groups) "overlap"
      else if (g %in% train_only) "train"
      else if (g %in% test_only) "test"
      else "excluded"
    }, character(1)),
    stringsAsFactors = FALSE
  )
  group_data$assignment <- factor(group_data$assignment,
                                   levels = c("overlap", "train", "test", "excluded"))
  # Sort: overlapping first, then by size
  group_data <- group_data[order(group_data$assignment, -group_data$n_obs), ]
  group_data$group <- factor(group_data$group, levels = rev(group_data$group))

  # Limit display
  n_display <- nrow(group_data)
  truncated <- FALSE
  if (n_display > 30) {
    group_data <- group_data[seq_len(30), ]
    truncated <- TRUE
  }

  sub <- sprintf(
    "%d groups | %d train | %d test | %d overlapping%s",
    length(unique_groups), length(train_only),
    length(test_only), length(overlap_groups),
    if (truncated) sprintf(" (showing 30 of %d)", n_display) else ""
  )

  ggplot2::ggplot(group_data, ggplot2::aes(
    x = .data$group, y = .data$n_obs, fill = .data$assignment
  )) +
    ggplot2::geom_col(width = 0.7) +
    ggplot2::geom_text(ggplot2::aes(label = .data$n_obs),
                       hjust = -0.2, size = 2.8, color = "gray30") +
    ggplot2::scale_fill_manual(
      values = c(train = unname(borg_palette["train"]),
                 test = unname(borg_palette["test"]),
                 overlap = unname(borg_palette["overlap"]),
                 excluded = unname(borg_palette["excluded"])),
      labels = c(train = "Train only", test = "Test only",
                 overlap = "OVERLAP (leak)", excluded = "Excluded")
    ) +
    ggplot2::coord_flip(clip = "off") +
    ggplot2::labs(
      title = sprintf("Fold %d Group Assignment", fold),
      subtitle = sub,
      x = NULL, y = "Observations", fill = NULL
    ) +
    borg_theme() +
    ggplot2::theme(plot.margin = ggplot2::margin(10, 35, 10, 10))
}


#' Fold sizes bar chart with balance indicator (Internal)
#' @noRd
autoplot_fold_sizes <- function(folds) {
  n_folds <- length(folds)

  train_sizes <- vapply(folds, function(f) length(f$train), integer(1))
  test_sizes <- vapply(folds, function(f) length(f$test), integer(1))

  df <- data.frame(
    fold = rep(seq_len(n_folds), 2),
    set = rep(c("train", "test"), each = n_folds),
    size = c(train_sizes, test_sizes)
  )
  df$set <- factor(df$set, levels = c("train", "test"))

  # Compute balance metric (CV of test sizes)
  test_cv <- if (length(test_sizes) > 1) sd(test_sizes) / mean(test_sizes) else 0
  balance_label <- if (test_cv < 0.1) "well-balanced"
                   else if (test_cv < 0.25) "moderately balanced"
                   else "imbalanced"

  ggplot2::ggplot(df, ggplot2::aes(
    x = factor(.data$fold), y = .data$size, fill = .data$set
  )) +
    ggplot2::geom_col(position = "dodge", width = 0.65) +
    # Size labels on bars
    ggplot2::geom_text(
      ggplot2::aes(label = .data$size),
      position = ggplot2::position_dodge(width = 0.65),
      vjust = -0.3, size = 2.8, color = "gray30"
    ) +
    ggplot2::scale_fill_manual(values = borg_palette[c("train", "test")]) +
    ggplot2::labs(
      title = "Fold Sizes",
      subtitle = sprintf(
        "%d folds | Test set CV: %.1f%% (%s)",
        n_folds, test_cv * 100, balance_label
      ),
      x = "Fold", y = "Observations", fill = NULL
    ) +
    borg_theme()
}


#' Variogram diagnostic plot (Internal)
#' @noRd
autoplot_variogram <- function(object) {
  vario <- object@spatial$variogram
  if (is.null(vario) || !is.data.frame(vario) || nrow(vario) == 0) {
    df <- data.frame(x = 0.5, y = 0.5)
    return(
      ggplot2::ggplot(df, ggplot2::aes(x = .data$x, y = .data$y)) +
        ggplot2::annotate("text", x = 0.5, y = 0.5,
                          label = "No variogram data\n(run borg_diagnose() with coords and target)",
                          size = 5, color = "gray40") +
        ggplot2::xlim(0, 1) + ggplot2::ylim(0, 1) +
        ggplot2::theme_void() +
        ggplot2::labs(title = "BORG Variogram")
    )
  }

  sill <- object@spatial$sill
  range_est <- object@spatial$range_estimate

  p <- ggplot2::ggplot(vario, ggplot2::aes(x = .data$distance, y = .data$semivariance))

  # Suggested block size zone (1x to 1.5x range)
  if (!is.na(range_est)) {
    p <- p + ggplot2::annotate(
      "rect",
      xmin = range_est, xmax = range_est * 1.5,
      ymin = -Inf, ymax = Inf,
      fill = "#D5F5E3", alpha = 0.3
    )
  }

  # Sill line
  if (!is.na(sill)) {
    p <- p + ggplot2::geom_hline(
      yintercept = sill, linetype = "dashed", color = "gray50", linewidth = 0.5
    ) +
    ggplot2::annotate(
      "text", x = max(vario$distance) * 0.95, y = sill,
      label = sprintf("sill = %.3f", sill),
      hjust = 1, vjust = -0.5, size = 3, color = "gray40"
    )
  }

  # Range line
  if (!is.na(range_est)) {
    p <- p + ggplot2::geom_vline(
      xintercept = range_est, linetype = "dashed",
      color = "#C0392B", linewidth = 0.5
    ) +
    ggplot2::annotate(
      "text", x = range_est, y = max(vario$semivariance) * 0.1,
      label = sprintf("range = %.1f", range_est),
      hjust = -0.1, size = 3, color = "#C0392B"
    )
  }

  # Variogram points and line
  p +
    ggplot2::geom_line(color = "#2C3E50", alpha = 0.3, linewidth = 0.5) +
    ggplot2::geom_point(
      ggplot2::aes(size = .data$n_pairs),
      color = "#2C3E50", alpha = 0.8
    ) +
    ggplot2::scale_size_continuous(
      range = c(1.5, 5),
      guide = ggplot2::guide_legend(title = "Pairs")
    ) +
    ggplot2::labs(
      title = "Empirical Semivariogram",
      subtitle = sprintf(
        "Moran's I = %.3f%s | Green zone = suggested block size (%.0f\u2013%.0f)",
        if (!is.na(object@spatial$morans_i)) object@spatial$morans_i else 0,
        if (!is.na(object@spatial$morans_p) && object@spatial$morans_p < 0.05) "***"
        else if (!is.na(object@spatial$morans_p) && object@spatial$morans_p < 0.1) "*"
        else "",
        if (!is.na(range_est)) range_est else 0,
        if (!is.na(range_est)) range_est * 1.5 else 0
      ),
      x = "Distance", y = "Semivariance"
    ) +
    borg_theme()
}
