# ===========================================================================
# borg_report() — Self-contained HTML diagnostic report
# ===========================================================================

#' Generate BORG HTML Diagnostic Report
#'
#' Creates a self-contained HTML report with embedded plots summarizing
#' the full BORG analysis: diagnosis, variogram, CV folds, performance,
#' and risk assessment.
#'
#' @param object A \code{borg_workflow} or \code{borg_result} object.
#' @param file Character. Output file path. Default: \code{"borg_report.html"}.
#' @param title Character. Report title.
#' @param open Logical. Open in browser after generation.
#'   Default: \code{TRUE} in interactive sessions.
#'
#' @return Invisible path to the generated HTML file.
#'
#' @details
#' Plots are embedded as base64-encoded PNGs. Requires \pkg{ggplot2}.
#' No rmarkdown or pandoc dependency.
#'
#' @examples
#' \donttest{
#' set.seed(42)
#' d <- data.frame(x = runif(100), y = runif(100), z = rnorm(100))
#' wf <- borg_workflow(d, z ~ x + y, coords = c("x", "y"))
#' borg_report(wf, file = tempfile(fileext = ".html"), open = FALSE)
#' }
#'
#' @export
borg_report <- function(object, file = "borg_report.html",
                          title = "BORG Diagnostic Report",
                          open = interactive()) {

  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package 'ggplot2' required for borg_report()")
  }

  # Extract components
  if (inherits(object, "borg_workflow")) {
    diagnosis <- object$diagnosis
    cv <- object$cv
    performance <- object$performance
    strategy <- object$cv$strategy
  } else if (inherits(object, "borg_result")) {
    diagnosis <- object$diagnosis
    cv <- object$cv
    performance <- NULL
    strategy <- if (!is.null(cv)) cv$strategy else "unknown"
  } else {
    stop("object must be a borg_workflow or borg_result")
  }

  # Generate plots as base64 PNGs
  plots <- list()

  tryCatch({
    plots$diagnosis <- .borg_plot_to_base64(ggplot2::autoplot(diagnosis), 8, 4)
  }, error = function(e) NULL)

  if (!is.null(diagnosis@spatial$variogram)) {
    tryCatch({
      plots$variogram <- .borg_plot_to_base64(
        ggplot2::autoplot(diagnosis, type = "variogram"), 8, 5)
    }, error = function(e) NULL)
  }

  if (!is.null(cv)) {
    tryCatch({
      plots$folds <- .borg_plot_to_base64(ggplot2::autoplot(cv, type = "folds"), 10, 4)
    }, error = function(e) NULL)
    tryCatch({
      plots$sizes <- .borg_plot_to_base64(ggplot2::autoplot(cv, type = "sizes"), 8, 4)
    }, error = function(e) NULL)
  }

  if (!is.null(performance)) {
    tryCatch({
      plots$performance <- .borg_plot_to_base64(ggplot2::autoplot(performance), 8, 5)
    }, error = function(e) NULL)
  }

  # Build HTML
  html <- .borg_build_html(title, diagnosis, cv, performance, strategy, plots)
  writeLines(html, file)

  if (open) utils::browseURL(file)
  message(sprintf("Report saved to %s", file))
  invisible(file)
}


#' @noRd
.borg_plot_to_base64 <- function(p, width = 8, height = 5, dpi = 120) {
  tmp <- tempfile(fileext = ".png")
  on.exit(unlink(tmp))
  ggplot2::ggsave(tmp, p, width = width, height = height, dpi = dpi)
  raw <- readBin(tmp, "raw", file.info(tmp)$size)
  # Base64 encode without external dependency
  paste0("data:image/png;base64,", .base64_encode(raw))
}


#' Simple base64 encoder (Internal, no dependency)
#' @noRd
.base64_encode <- function(raw_bytes) {
  chars <- "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/"
  n <- length(raw_bytes)
  out <- character(0)

  i <- 1L
  while (i <= n) {
    b1 <- as.integer(raw_bytes[i])
    b2 <- if (i + 1L <= n) as.integer(raw_bytes[i + 1L]) else 0L
    b3 <- if (i + 2L <= n) as.integer(raw_bytes[i + 2L]) else 0L

    triple <- bitwShiftL(b1, 16L) + bitwShiftL(b2, 8L) + b3

    out <- c(out,
      substr(chars, bitwAnd(bitwShiftR(triple, 18L), 63L) + 1L,
                     bitwAnd(bitwShiftR(triple, 18L), 63L) + 1L),
      substr(chars, bitwAnd(bitwShiftR(triple, 12L), 63L) + 1L,
                     bitwAnd(bitwShiftR(triple, 12L), 63L) + 1L),
      if (i + 1L <= n)
        substr(chars, bitwAnd(bitwShiftR(triple, 6L), 63L) + 1L,
                       bitwAnd(bitwShiftR(triple, 6L), 63L) + 1L)
      else "=",
      if (i + 2L <= n)
        substr(chars, bitwAnd(triple, 63L) + 1L,
                       bitwAnd(triple, 63L) + 1L)
      else "="
    )
    i <- i + 3L
  }
  paste0(out, collapse = "")
}


#' @noRd
.borg_build_html <- function(title, diagnosis, cv, performance, strategy, plots) {
  timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  dep_type <- diagnosis@dependency_type
  severity <- diagnosis@severity
  n_obs <- diagnosis@n_obs

  sev_class <- if (severity == "none") "valid" else "invalid"

  # Performance section
  perf_html <- ""
  if (!is.null(performance)) {
    mn <- mean(performance$value, na.rm = TRUE)
    sd_v <- stats::sd(performance$value, na.rm = TRUE)
    met <- toupper(performance$metric[1])
    perf_html <- sprintf(
      '<h2>Performance</h2>
       <div class="m"><div class="v">%.4f</div><div class="l">Mean %s</div></div>
       <div class="m"><div class="v">%.4f</div><div class="l">SD</div></div>',
      mn, met, sd_v)
    if (!is.null(plots$performance)) {
      perf_html <- paste0(perf_html, sprintf('\n<img src="%s">', plots$performance))
    }
  }

  img <- function(key) {
    if (!is.null(plots[[key]])) sprintf('<img src="%s">', plots[[key]]) else ""
  }

  sprintf('<!DOCTYPE html>
<html><head><meta charset="UTF-8"><title>%s</title>
<style>
body{font-family:system-ui,sans-serif;max-width:900px;margin:40px auto;padding:0 20px;color:#2c3e50;line-height:1.6}
h1{border-bottom:3px solid #1B4F72;padding-bottom:10px}
h2{color:#1B4F72;margin-top:30px;border-bottom:1px solid #ddd;padding-bottom:5px}
.m{display:inline-block;background:#f8f9fa;border-radius:8px;padding:12px 20px;margin:5px;text-align:center;min-width:120px}
.v{font-size:24px;font-weight:bold;color:#1B4F72}
.l{font-size:12px;color:#666}
.valid{color:#27AE60;font-weight:bold}
.invalid{color:#C0392B;font-weight:bold}
img{max-width:100%%;border-radius:4px;margin:10px 0}
.ft{margin-top:40px;padding-top:20px;border-top:1px solid #ddd;color:#999;font-size:12px}
</style></head><body>
<h1>%s</h1><p>%s</p>
<h2>Summary</h2>
<div class="m"><div class="v">%d</div><div class="l">Observations</div></div>
<div class="m"><div class="v">%s</div><div class="l">Dependency</div></div>
<div class="m"><div class="v %s">%s</div><div class="l">Severity</div></div>
<div class="m"><div class="v">%s</div><div class="l">Strategy</div></div>
<h2>Diagnosis</h2>%s
%s
<h2>CV Folds</h2>%s%s
%s
<div class="ft">Generated by BORG v%s</div>
</body></html>',
  title, title, timestamp, n_obs, dep_type, sev_class, severity, strategy,
  img("diagnosis"),
  if (!is.null(plots$variogram)) paste0("<h2>Variogram</h2>", img("variogram")) else "",
  img("folds"), img("sizes"), perf_html,
  as.character(utils::packageVersion("BORG")))
}
