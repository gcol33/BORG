# ===========================================================================
# BORG Visualization Functions - S3 plot() methods
# ===========================================================================

#' Plot BORG Objects
#'
#' S3 plot method for BORG risk assessment objects.
#'
#' @param x A \code{BorgRisk} object from \code{borg_inspect()} or \code{borg()}.
#' @param title Optional custom plot title.
#' @param max_risks Maximum number of risks to display. Default: 10.
#' @param ... Additional arguments (currently unused).
#'
#' @return Invisibly returns NULL. Called for plotting side effect.
#'
#' @details
#' Displays a visual summary of detected risks:
#' \itemize{
#'   \item Hard violations shown in red
#'   \item Soft inflation risks shown in yellow/orange
#'   \item Green "OK" when no risks detected
#' }
#'
#' @examples
#' # No risks
#' data <- data.frame(x = 1:100, y = 101:200)
#' result <- borg_inspect(data, train_idx = 1:70, test_idx = 71:100)
#' plot(result)
#'
#' # With overlap violation
#' result_bad <- borg_inspect(data, train_idx = 1:60, test_idx = 51:100)
#' plot(result_bad)
#'
#' @export
plot.BorgRisk <- function(x, title = NULL, max_risks = 10, ...) {
  plot_risk_internal(x, title = title, max_risks = max_risks, ...)
}


#' Plot BORG Result Objects
#'
#' S3 plot method for borg_result objects from \code{borg()}.
#'
#' @param x A \code{borg_result} object from \code{borg()}.
#' @param type Character. Plot type: \code{"split"} (default), \code{"risk"},
#'   \code{"temporal"}, or \code{"groups"}.
#' @param fold Integer. Which fold to plot (for split visualization). Default: 1.
#' @param time Column name or values for temporal plots.
#' @param groups Column name or values for group plots.
#' @param title Optional custom plot title.
#' @param ... Additional arguments passed to internal plot functions.
#'
#' @return Invisibly returns NULL. Called for plotting side effect.
#'
#' @examples
#' set.seed(42)
#' data <- data.frame(
#'   x = runif(100, 0, 100),
#'   y = runif(100, 0, 100),
#'   response = rnorm(100)
#' )
#' result <- borg(data, coords = c("x", "y"), target = "response")
#' plot(result)  # Split visualization for first fold
#'
#' @export
plot.borg_result <- function(x,
                              type = c("split", "risk", "temporal", "groups"),
                              fold = 1,
                              time = NULL,
                              groups = NULL,
                              title = NULL,
                              ...) {

  type <- match.arg(type)

  # Risk plot - show the inspection results if available
  if (type == "risk") {
    if (!is.null(x$risk) && inherits(x$risk, "BorgRisk")) {
      return(plot_risk_internal(x$risk, title = title, ...))
    } else {
      stop("No risk assessment available in this borg_result. Use borg_inspect() to create one.")
    }
  }

  # For other types, need fold data
  if (is.null(x$folds) || length(x$folds) == 0) {
    stop("No folds available in borg_result object")
  }

  if (fold > length(x$folds)) {
    stop(sprintf("Fold %d not available (only %d folds)", fold, length(x$folds)))
  }

  train_idx <- x$folds[[fold]]$train
  test_idx <- x$folds[[fold]]$test
  n_total <- max(c(train_idx, test_idx))

  switch(type,
    "split" = plot_split_internal(train_idx, test_idx, n_total,
                                   title = title %||% sprintf("Fold %d Split", fold), ...),
    "temporal" = {
      if (is.null(time)) stop("'time' required for temporal plot")
      plot_temporal_internal(time, train_idx, test_idx,
                              title = title %||% sprintf("Fold %d Temporal", fold), ...)
    },
    "groups" = {
      if (is.null(groups)) stop("'groups' required for groups plot")
      plot_groups_internal(groups, train_idx, test_idx,
                            title = title %||% sprintf("Fold %d Groups", fold), ...)
    }
  )

  invisible(NULL)
}


# Null-coalescing operator
`%||%` <- function(a, b) if (is.null(a)) b else a


# ===========================================================================
# Internal plotting functions
# ===========================================================================

#' @noRd
plot_split_internal <- function(train_idx, test_idx, n_total = NULL,
                                temporal = NULL, groups = NULL,
                                title = "Train/Test Split", ...) {

  if (is.null(n_total)) {
    n_total <- max(c(train_idx, test_idx))
  }

  # Detect overlap
  overlap <- intersect(train_idx, test_idx)

  # Create color vector
  colors <- rep("gray90", n_total)
  colors[train_idx] <- "#2E86AB"  # Blue for train
  colors[test_idx] <- "#E94F37"   # Red for test
  colors[overlap] <- "#F9A03F"    # Orange for overlap

  if (!is.null(temporal)) {
    # Temporal plot: x-axis is time
    x_vals <- as.numeric(temporal)

    oldpar <- par(no.readonly = TRUE)
    on.exit(par(oldpar))

    par(mar = c(4, 4, 3, 1))

    plot(x_vals, rep(1, n_total),
         col = colors, pch = "|", cex = 2,
         xlab = "Time", ylab = "",
         main = title, yaxt = "n",
         xlim = range(x_vals))

    # Add legend
    legend("topright",
           legend = c("Train", "Test", if (length(overlap) > 0) "Overlap"),
           col = c("#2E86AB", "#E94F37", if (length(overlap) > 0) "#F9A03F"),
           pch = 15, bty = "n", cex = 0.8)

    # Add gap indicator if no overlap
    if (length(overlap) == 0) {
      max_train <- max(x_vals[train_idx])
      min_test <- min(x_vals[test_idx])
      if (min_test > max_train) {
        abline(v = (max_train + min_test) / 2, lty = 2, col = "gray50")
        text((max_train + min_test) / 2, 1.1, "gap", cex = 0.7, col = "gray50")
      }
    }

  } else if (!is.null(groups)) {
    # Group plot: faceted by group
    unique_groups <- unique(groups)
    n_groups <- length(unique_groups)

    oldpar <- par(no.readonly = TRUE)
    on.exit(par(oldpar))

    par(mar = c(4, 6, 3, 1))

    # Create y-positions based on group
    y_vals <- match(groups, unique_groups)

    plot(seq_len(n_total), y_vals,
         col = colors, pch = 15, cex = 1.5,
         xlab = "Index", ylab = "",
         main = title, yaxt = "n",
         ylim = c(0.5, n_groups + 0.5))

    axis(2, at = seq_len(n_groups), labels = unique_groups, las = 1)

    legend("topright",
           legend = c("Train", "Test", if (length(overlap) > 0) "Overlap"),
           col = c("#2E86AB", "#E94F37", if (length(overlap) > 0) "#F9A03F"),
           pch = 15, bty = "n", cex = 0.8)

  } else {
    # Simple index plot
    oldpar <- par(no.readonly = TRUE)
    on.exit(par(oldpar))

    par(mar = c(4, 4, 3, 1))

    # Bar plot representation
    barplot(rep(1, n_total), col = colors, border = NA,
            main = title, xlab = "Index", ylab = "",
            space = 0)

    # Add labels
    n_train <- length(train_idx)
    n_test <- length(test_idx)
    n_overlap <- length(overlap)

    legend_text <- c(
      sprintf("Train: %d (%.0f%%)", n_train, 100 * n_train / n_total),
      sprintf("Test: %d (%.0f%%)", n_test, 100 * n_test / n_total)
    )
    legend_cols <- c("#2E86AB", "#E94F37")

    if (n_overlap > 0) {
      legend_text <- c(legend_text,
                       sprintf("Overlap: %d", n_overlap))
      legend_cols <- c(legend_cols, "#F9A03F")
    }

    legend("topright", legend = legend_text, fill = legend_cols,
           bty = "n", cex = 0.8)
  }

  invisible(NULL)
}


#' @noRd
plot_risk_internal <- function(risk, title = NULL, max_risks = 10, ...) {

  if (!inherits(risk, "BorgRisk")) {
    stop("'risk' must be a BorgRisk object")
  }

  risks <- risk@risks

  if (length(risks) == 0) {
    # No risks - show success message
    oldpar <- par(no.readonly = TRUE)
    on.exit(par(oldpar))

    par(mar = c(1, 1, 3, 1))

    plot.new()
    plot.window(xlim = c(0, 1), ylim = c(0, 1))
    title(main = title %||% "BORG Risk Assessment")

    text(0.5, 0.6, "OK", cex = 5, col = "#2E7D32", font = 2)  # Success
    text(0.5, 0.3, "No risks detected", cex = 1.5, col = "#2E7D32")
    text(0.5, 0.15, "Evaluation is valid", cex = 1, col = "gray50")

    return(invisible(NULL))
  }

  # Limit risks displayed
  if (length(risks) > max_risks) {
    risks <- risks[1:max_risks]
    truncated <- TRUE
  } else {
    truncated <- FALSE
  }

  n_risks <- length(risks)

  # Extract risk info
  types <- vapply(risks, function(r) r$type, character(1))
  severities <- vapply(risks, function(r) r$severity, character(1))
  descriptions <- vapply(risks, function(r) {
    desc <- r$description
    if (nchar(desc) > 50) {
      paste0(substr(desc, 1, 47), "...")
    } else {
      desc
    }
  }, character(1))

  # Colors by severity
  colors <- ifelse(severities == "hard_violation", "#C62828", "#F9A825")

  oldpar <- par(no.readonly = TRUE)
  on.exit(par(oldpar))

  par(mar = c(4, 12, 4, 2))

  # Horizontal bar chart
  y_pos <- rev(seq_len(n_risks))

  plot(rep(1, n_risks), y_pos,
       xlim = c(0, 1.5), ylim = c(0.5, n_risks + 0.5),
       type = "n", xaxt = "n", yaxt = "n",
       xlab = "", ylab = "", main = title %||% "BORG Risk Assessment")

  # Draw bars
  rect(0, y_pos - 0.4, 1, y_pos + 0.4, col = colors, border = NA)

  # Add type labels on left
  text(-0.02, y_pos, types, adj = 1, cex = 0.8, xpd = TRUE)

  # Add description labels on bars
  text(0.05, y_pos, descriptions, adj = 0, cex = 0.7, col = "white")

  # Add severity icons on right (use ASCII for compatibility)
  severity_symbols <- ifelse(severities == "hard_violation", "X", "!")
  text(1.1, y_pos, severity_symbols, cex = 1.2, font = 2,
       col = ifelse(severities == "hard_violation", "#C62828", "#F9A825"))

  # Legend
  legend("bottomright",
         legend = c("Hard Violation", "Soft Inflation"),
         fill = c("#C62828", "#F9A825"),
         bty = "n", cex = 0.8)

  # Summary stats
  n_hard <- sum(severities == "hard_violation")
  n_soft <- sum(severities == "soft_inflation")
  status_text <- if (n_hard > 0) "INVALID" else "VALID (with warnings)"
  status_col <- if (n_hard > 0) "#C62828" else "#F9A825"

  mtext(sprintf("Status: %s | Hard: %d | Soft: %d",
                status_text, n_hard, n_soft),
        side = 1, line = 2, cex = 0.9, col = status_col)

  if (truncated) {
    mtext(sprintf("(showing %d of %d risks)", max_risks, length(risk@risks)),
          side = 1, line = 3, cex = 0.7, col = "gray50")
  }

  invisible(NULL)
}


#' @noRd
plot_temporal_internal <- function(temporal, train_idx, test_idx,
                                   title = "Temporal Split", ...) {

  n_total <- length(temporal)
  x_vals <- as.numeric(temporal)

  # Detect issues
  train_times <- x_vals[train_idx]
  test_times <- x_vals[test_idx]

  max_train <- max(train_times, na.rm = TRUE)
  min_test <- min(test_times, na.rm = TRUE)

  # Look-ahead violations (test before train)
  violations <- test_idx[test_times < max_train]
  has_gap <- min_test > max_train && length(violations) == 0

  # Colors
  colors <- rep("gray90", n_total)
  colors[train_idx] <- "#2E86AB"
  colors[test_idx] <- "#E94F37"
  colors[violations] <- "#F9A03F"  # Violations in orange

  oldpar <- par(no.readonly = TRUE)
  on.exit(par(oldpar))

  par(mar = c(5, 4, 4, 2))

  # Plot timeline
  plot(x_vals, rep(0.5, n_total),
       col = colors, pch = 15, cex = 2,
       xlab = if (inherits(temporal, "Date")) "Date" else "Time",
       ylab = "", yaxt = "n",
       main = title,
       ylim = c(0, 1))

  # Add max train line
  abline(v = max_train, lty = 2, col = "#2E86AB", lwd = 2)
  text(max_train, 0.9, "max(train)", pos = 4, cex = 0.7, col = "#2E86AB")

  # Add min test line
  abline(v = min_test, lty = 2, col = "#E94F37", lwd = 2)
  text(min_test, 0.9, "min(test)", pos = 2, cex = 0.7, col = "#E94F37")

  # Gap or overlap annotation
  if (has_gap) {
    gap_size <- min_test - max_train
    rect(max_train, 0.3, min_test, 0.7,
         col = rgb(0.5, 0.8, 0.5, 0.3), border = "#4CAF50")
    text((max_train + min_test) / 2, 0.5,
         sprintf("Gap: %.0f", gap_size), cex = 0.8, col = "#2E7D32")
  } else if (length(violations) > 0) {
    text(mean(range(x_vals)), 0.15,
         sprintf("WARNING: %d test observations before max train time",
                 length(violations)),
         cex = 0.9, col = "#C62828")
  }

  # Legend
  legend_items <- c("Train", "Test")
  legend_cols <- c("#2E86AB", "#E94F37")

  if (length(violations) > 0) {
    legend_items <- c(legend_items, "Look-ahead violation")
    legend_cols <- c(legend_cols, "#F9A03F")
  }

  legend("topright", legend = legend_items, col = legend_cols,
         pch = 15, bty = "n", cex = 0.8)

  invisible(NULL)
}


#' @noRd
plot_spatial_internal <- function(x, y, train_idx, test_idx,
                                  title = "Spatial Split", ...) {

  n_total <- length(x)

  # Detect overlap
  overlap <- intersect(train_idx, test_idx)

  # Colors and shapes
  colors <- rep("gray80", n_total)
  colors[train_idx] <- "#2E86AB"
  colors[test_idx] <- "#E94F37"
  colors[overlap] <- "#F9A03F"

  pch <- rep(1, n_total)
  pch[train_idx] <- 16
  pch[test_idx] <- 17
  pch[overlap] <- 18

  oldpar <- par(no.readonly = TRUE)
  on.exit(par(oldpar))

  par(mar = c(4, 4, 3, 1))

  plot(x, y, col = colors, pch = pch, cex = 1.5,
       xlab = "X (e.g., Longitude)", ylab = "Y (e.g., Latitude)",
       main = title)

  # Add convex hulls if enough points
  if (length(train_idx) >= 3) {
    train_hull <- chull(x[train_idx], y[train_idx])
    train_hull <- c(train_hull, train_hull[1])
    lines(x[train_idx][train_hull], y[train_idx][train_hull],
          col = "#2E86AB", lwd = 2, lty = 2)
  }

  if (length(test_idx) >= 3) {
    test_hull <- chull(x[test_idx], y[test_idx])
    test_hull <- c(test_hull, test_hull[1])
    lines(x[test_idx][test_hull], y[test_idx][test_hull],
          col = "#E94F37", lwd = 2, lty = 2)
  }

  # Legend
  legend("topright",
         legend = c("Train", "Test", if (length(overlap) > 0) "Overlap"),
         col = c("#2E86AB", "#E94F37", if (length(overlap) > 0) "#F9A03F"),
         pch = c(16, 17, if (length(overlap) > 0) 18),
         bty = "n", cex = 0.8)

  invisible(NULL)
}


#' @noRd
plot_groups_internal <- function(groups, train_idx, test_idx,
                                 title = "Group-Based Split",
                                 max_groups = 20, ...) {

  n_total <- length(groups)

  # Get unique groups and their assignments
  unique_groups <- unique(groups)
  n_groups <- length(unique_groups)

  # Determine group assignment
  train_groups <- unique(groups[train_idx])
  test_groups <- unique(groups[test_idx])
  overlap_groups <- intersect(train_groups, test_groups)
  train_only <- setdiff(train_groups, test_groups)
  test_only <- setdiff(test_groups, train_groups)

  # Limit groups for display
  if (n_groups > max_groups) {
    # Prioritize showing overlapping groups
    display_groups <- c(
      overlap_groups,
      head(train_only, max(1, (max_groups - length(overlap_groups)) %/% 2)),
      head(test_only, max(1, (max_groups - length(overlap_groups)) %/% 2))
    )
    display_groups <- head(unique(display_groups), max_groups)
    truncated <- TRUE
  } else {
    display_groups <- unique_groups
    truncated <- FALSE
  }

  n_display <- length(display_groups)

  # Colors for groups
  group_colors <- character(n_display)
  for (i in seq_along(display_groups)) {
    g <- display_groups[i]
    if (g %in% overlap_groups) {
      group_colors[i] <- "#F9A03F"  # Orange for overlap
    } else if (g %in% train_only) {
      group_colors[i] <- "#2E86AB"  # Blue for train
    } else {
      group_colors[i] <- "#E94F37"  # Red for test
    }
  }

  # Count observations per group
  group_counts <- vapply(display_groups, function(g) sum(groups == g), integer(1))

  oldpar <- par(no.readonly = TRUE)
  on.exit(par(oldpar))

  par(mar = c(4, 8, 4, 2))

  # Horizontal bar chart
  y_pos <- rev(seq_len(n_display))

  plot(group_counts, y_pos,
       xlim = c(0, max(group_counts) * 1.2),
       ylim = c(0.5, n_display + 0.5),
       type = "n", yaxt = "n",
       xlab = "Observations", ylab = "",
       main = title)

  # Draw bars
  rect(0, y_pos - 0.4, group_counts, y_pos + 0.4,
       col = rev(group_colors), border = NA)

  # Group labels
  axis(2, at = y_pos, labels = rev(display_groups), las = 1, cex.axis = 0.7)

  # Count labels on bars
  text(group_counts + max(group_counts) * 0.02, y_pos,
       rev(group_counts), adj = 0, cex = 0.7)

  # Legend
  legend("bottomright",
         legend = c("Train only", "Test only",
                    if (length(overlap_groups) > 0) "OVERLAP (leak!)"),
         fill = c("#2E86AB", "#E94F37",
                  if (length(overlap_groups) > 0) "#F9A03F"),
         bty = "n", cex = 0.8)

  # Summary
  summary_text <- sprintf(
    "Groups: %d total | Train: %d | Test: %d | Overlap: %d",
    n_groups, length(train_groups), length(test_groups), length(overlap_groups)
  )
  mtext(summary_text, side = 1, line = 2.5, cex = 0.8)

  if (truncated) {
    mtext(sprintf("(showing %d of %d groups)", n_display, n_groups),
          side = 1, line = 3.5, cex = 0.7, col = "gray50")
  }

  if (length(overlap_groups) > 0) {
    mtext("WARNING: Group leakage detected!", side = 3, line = 0.5,
          cex = 0.9, col = "#C62828", font = 2)
  }

  invisible(NULL)
}
