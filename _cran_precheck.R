# CRAN pre-submission checks
cat("=== CRAN Pre-submission Diagnostics ===\n\n")

setwd("C:/Users/Gilles Colling/Documents/dev/BORG")
Sys.setenv(PATH = paste("C:/Program Files/Pandoc", Sys.getenv("PATH"), sep = ";"))
Sys.setenv(RSTUDIO_PANDOC = "C:/Program Files/Pandoc")

# 1. Regenerate docs
cat("--- Phase 1: Regenerate documentation ---\n")
devtools::document()

# 2. Check for stale man/ files
cat("\n--- Phase 2: Check for stale .Rd files ---\n")
stale <- system("git diff --name-only man/ 2>/dev/null", intern = TRUE)
if (length(stale) > 0) {
  cat("STALE .Rd files detected (need commit before build):\n")
  cat(paste("  ", stale, collapse = "\n"), "\n")
} else {
  cat("All .Rd files up to date.\n")
}

# 3. Check for .Rd files missing \value
cat("\n--- Phase 3: Missing \\value tags ---\n")
rd_files <- list.files("man", pattern = "\\.Rd$", full.names = TRUE)
missing_value <- character()
for (f in rd_files) {
  txt <- readLines(f, warn = FALSE)
  if (!any(grepl("\\\\value\\{", txt))) missing_value <- c(missing_value, basename(f))
}
if (length(missing_value) > 0) {
  cat("MISSING \\value in:\n")
  cat(paste("  ", missing_value, collapse = "\n"), "\n")
} else {
  cat("All", length(rd_files), ".Rd files have \\value tags.\n")
}

# 4. Spelling check
cat("\n--- Phase 4: Spelling ---\n")
if (requireNamespace("spelling", quietly = TRUE)) {
  sp <- spelling::spell_check_package()
  if (nrow(sp) > 0) {
    print(sp)
  } else {
    cat("No misspellings found.\n")
  }
} else {
  cat("SKIP: spelling package not available.\n")
}

# 5. URL check
cat("\n--- Phase 5: URL check ---\n")
if (requireNamespace("urlchecker", quietly = TRUE)) {
  tryCatch({
    urls <- urlchecker::url_check()
    if (nrow(urls) > 0) {
      print(urls)
    } else {
      cat("All URLs OK.\n")
    }
  }, error = function(e) cat("URL check error:", conditionMessage(e), "\n"))
} else {
  cat("SKIP: urlchecker package not available.\n")
}

# 6. DOI validation
cat("\n--- Phase 6: DOI validation ---\n")
desc <- readLines("DESCRIPTION", warn = FALSE)
dois <- unlist(regmatches(desc, gregexpr("(?<=<doi:)[^>]+", desc, perl = TRUE)))
if (length(dois) > 0) {
  for (d in dois) {
    url <- paste0("https://api.crossref.org/works/", d)
    res <- tryCatch(
      readLines(url, n = 1, warn = FALSE),
      error = function(e) NULL,
      warning = function(w) NULL
    )
    cat(sprintf("  %s  doi:%s\n", if (!is.null(res)) "OK" else "FAIL", d))
  }
} else {
  cat("No DOIs found.\n")
}

# 7. Check for relative URLs in vignettes
cat("\n--- Phase 7: Relative URLs in vignettes ---\n")
vigs <- list.files("vignettes", pattern = "\\.Rmd$", full.names = TRUE)
found_rel <- FALSE
for (v in vigs) {
  txt <- readLines(v, warn = FALSE)
  rel <- grep("\\.\\./reference|\\.\\./articles", txt)
  if (length(rel) > 0) {
    cat(sprintf("  WARN: %s has relative URLs on lines: %s\n",
                basename(v), paste(rel, collapse = ", ")))
    found_rel <- TRUE
  }
}
if (!found_rel) cat("No relative URLs found.\n")

cat("\n=== Diagnostics complete ===\n")
