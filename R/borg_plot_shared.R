# ===========================================================================
# Shared plotting helpers used by both the base-graphics plot() methods
# (borg_plot.R) and the ggplot2 autoplot() methods (borg_autoplot.R)
# ===========================================================================

#' Build a Train/Test/Excluded Role Vector (Internal)
#'
#' Single source of truth for the role-vector construction repeated across
#' every BORG plot method: label each observation as belonging to the
#' training set, the test set, an override category (overlap, look-ahead
#' violation, buffer zone, ...), or excluded entirely.
#'
#' @param train_idx Integer vector of training indices.
#' @param test_idx Integer vector of test indices.
#' @param n Total number of observations.
#' @param overlap_idx Integer vector of indices to relabel as
#'   \code{overlap_label} after the train/test assignment (applied last, so
#'   it takes priority over both). Defaults to
#'   \code{intersect(train_idx, test_idx)}. Pass \code{integer(0)} to skip
#'   this relabeling step entirely.
#' @param overlap_label Label used for \code{overlap_idx}. Default
#'   \code{"overlap"}.
#' @param levels Factor levels, in display order. Default
#'   \code{c("train", "test", overlap_label, "excluded")}.
#' @param as_factor If \code{TRUE} (default), return a factor with
#'   \code{levels}. If \code{FALSE}, return the plain character vector —
#'   useful when the caller will \code{rbind()} several role vectors (e.g.
#'   one per CV fold) before factoring the combined result.
#'
#' @return A factor (or character vector, if \code{as_factor = FALSE}) of
#'   length \code{n}.
#' @noRd
build_role_vector <- function(train_idx, test_idx, n,
                               overlap_idx = intersect(train_idx, test_idx),
                               overlap_label = "overlap",
                               levels = c("train", "test", overlap_label, "excluded"),
                               as_factor = TRUE) {
  role <- rep("excluded", n)
  role[train_idx] <- "train"
  role[test_idx] <- "test"
  if (length(overlap_idx) > 0) {
    role[overlap_idx] <- overlap_label
  }

  if (as_factor) factor(role, levels = levels) else role
}


#' Empty-State Placeholder Plot (Internal)
#'
#' Single source of truth for the "nothing to show" placeholder used across
#' BORG plot methods (no risks detected, no dependencies found, no
#' variogram data, ...). Supports both rendering engines used by the
#' package: \code{engine = "ggplot"} returns a \code{ggplot} object for the
#' \code{autoplot.*} methods; \code{engine = "base"} draws directly via base
#' graphics for the \code{plot.*} methods and returns \code{invisible(NULL)}.
#'
#' @param title Plot title.
#' @param label Main message.
#' @param label_size Size of \code{label} (ggplot \code{size} units for
#'   \code{engine = "ggplot"}, \code{cex} units for \code{engine = "base"}).
#' @param label_color Color of \code{label}.
#' @param fontface Font face of \code{label}: \code{"plain"} or
#'   \code{"bold"}.
#' @param sublabel Optional secondary message shown below \code{label}.
#' @param sublabel_size Size of \code{sublabel}.
#' @param sublabel_color Color of \code{sublabel}.
#' @param note Optional third line shown below \code{sublabel} (e.g.
#'   "Evaluation is valid").
#' @param engine Rendering engine: \code{"ggplot"} (default) or
#'   \code{"base"}.
#'
#' @return A \code{ggplot} object (\code{engine = "ggplot"}) or
#'   \code{invisible(NULL)} (\code{engine = "base"}).
#' @noRd
.empty_plot <- function(title, label, label_size = 5, label_color = "gray40",
                         fontface = "plain", sublabel = NULL, sublabel_size = 5,
                         sublabel_color = "gray40", note = NULL,
                         engine = c("ggplot", "base")) {
  engine <- match.arg(engine)

  if (engine == "base") {
    oldpar <- graphics::par(no.readonly = TRUE)
    on.exit(graphics::par(oldpar))
    graphics::par(mar = c(1, 1, 3, 1))

    graphics::plot.new()
    graphics::plot.window(xlim = c(0, 1), ylim = c(0, 1))
    graphics::title(main = title)

    graphics::text(0.5, if (is.null(sublabel)) 0.5 else 0.6, label,
                   cex = label_size, col = label_color,
                   font = if (fontface == "bold") 2 else 1)

    if (!is.null(sublabel)) {
      graphics::text(0.5, 0.3, sublabel, cex = sublabel_size, col = sublabel_color)
    }
    if (!is.null(note)) {
      graphics::text(0.5, 0.15, note, cex = 1, col = "gray50")
    }

    return(invisible(NULL))
  }

  check_ggplot2()

  df <- data.frame(x = 0.5, y = 0.5)
  p <- ggplot2::ggplot(df, ggplot2::aes(x = .data$x, y = .data$y)) +
    ggplot2::annotate("text", x = 0.5, y = if (is.null(sublabel)) 0.5 else 0.55,
                      label = label, size = label_size, fontface = fontface,
                      color = label_color)

  if (!is.null(sublabel)) {
    p <- p + ggplot2::annotate("text", x = 0.5, y = 0.42, label = sublabel,
                               size = sublabel_size, color = sublabel_color)
  }

  p +
    ggplot2::xlim(0, 1) + ggplot2::ylim(0, 1) +
    ggplot2::theme_void() +
    ggplot2::labs(title = title)
}


#' Classify Groups by Train/Test/Overlap Membership (Internal)
#'
#' Single source of truth for the group-level classification shared by the
#' \code{"groups"} plot type in both \code{\link{plot.borg_result}} (base
#' graphics) and \code{\link[ggplot2]{autoplot.borg_result}} (ggplot2):
#' every unique group is labeled by whether its observations fall in the
#' training set only, the test set only, both (a group-level leak), or
#' neither.
#'
#' @param group_vals Vector of group labels, one per observation.
#' @param train_idx Integer vector of training indices.
#' @param test_idx Integer vector of test indices.
#'
#' @return A list with elements \code{unique_groups}, \code{train_groups},
#'   \code{test_groups}, \code{overlap_groups}, \code{train_only},
#'   \code{test_only}.
#' @noRd
classify_groups <- function(group_vals, train_idx, test_idx) {
  unique_groups <- unique(group_vals)
  train_groups <- unique(group_vals[train_idx])
  test_groups <- unique(group_vals[test_idx])
  overlap_groups <- intersect(train_groups, test_groups)
  train_only <- setdiff(train_groups, test_groups)
  test_only <- setdiff(test_groups, train_groups)

  list(
    unique_groups = unique_groups,
    train_groups = train_groups,
    test_groups = test_groups,
    overlap_groups = overlap_groups,
    train_only = train_only,
    test_only = test_only
  )
}


#' Shared \code{type=} Menu for \code{plot.borg_result} / \code{autoplot.borg_result} (Internal)
#'
#' Single source of truth for the plot-type vocabulary exposed by both
#' \code{\link{plot.borg_result}} and \code{\link[ggplot2]{autoplot.borg_result}}
#' so the two entry points never drift apart. Both methods build their
#' \code{type} argument default from this vector and call
#' \code{match.arg(type, borg_result_plot_types())}.
#'
#' @return Character vector of valid \code{type} values.
#' @noRd
borg_result_plot_types <- function() {
  c("split", "risk", "spatial", "temporal", "groups")
}
