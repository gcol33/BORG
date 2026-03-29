# ===========================================================================
# borg_cache() — Diagnosis caching for iterative workflows
# ===========================================================================

#' Cache and Retrieve BORG Diagnoses
#'
#' Caches \code{\link{BorgDiagnosis}} objects keyed by a hash of the
#' input data, so expensive computations (variograms, distance matrices,
#' autocorrelation tests) are not repeated across iterations.
#'
#' @param data A data frame. Used to compute the cache key.
#' @param diagnosis A \code{BorgDiagnosis} object to cache (for
#'   \code{borg_cache_set}), or NULL.
#' @param coords Character vector. Included in the cache key to
#'   distinguish diagnoses of the same data with different coordinate
#'   columns.
#' @param target Character. Included in the cache key.
#' @param envir Environment for in-memory cache. Default: the package
#'   namespace cache.
#'
#' @return
#' \describe{
#'   \item{\code{borg_cache_get}}{A \code{BorgDiagnosis} or NULL if
#'     not cached.}
#'   \item{\code{borg_cache_set}}{Invisible NULL. Stores the diagnosis.}
#'   \item{\code{borg_cache_clear}}{Invisible NULL. Clears all cached
#'     diagnoses.}
#'   \item{\code{borg_cache_info}}{A data frame with cache key, timestamp,
#'     and data dimensions for all cached entries.}
#' }
#'
#' @examples
#' d <- data.frame(x = runif(100), y = runif(100), z = rnorm(100))
#' diag <- borg_diagnose(d, coords = c("x", "y"), target = "z")
#'
#' # Cache it
#' borg_cache_set(d, diag, coords = c("x", "y"), target = "z")
#'
#' # Retrieve (fast, no recomputation)
#' cached <- borg_cache_get(d, coords = c("x", "y"), target = "z")
#' identical(diag, cached)  # TRUE
#'
#' # Clear all
#' borg_cache_clear()
#'
#' @name borg_cache
NULL

# In-memory cache (package-level environment)
.borg_cache_env <- new.env(parent = emptyenv())

#' @rdname borg_cache
#' @export
borg_cache_get <- function(data, coords = NULL, target = NULL,
                           envir = .borg_cache_env) {
  key <- .cache_key(data, coords, target)
  if (exists(key, envir = envir, inherits = FALSE)) {
    entry <- get(key, envir = envir, inherits = FALSE)
    entry$diagnosis
  } else {
    NULL
  }
}

#' @rdname borg_cache
#' @export
borg_cache_set <- function(data, diagnosis, coords = NULL, target = NULL,
                           envir = .borg_cache_env) {
  if (!inherits(diagnosis, "BorgDiagnosis")) {
    stop("diagnosis must be a BorgDiagnosis object")
  }
  key <- .cache_key(data, coords, target)
  entry <- list(
    diagnosis = diagnosis,
    timestamp = Sys.time(),
    nrow = nrow(data),
    ncol = ncol(data)
  )
  assign(key, entry, envir = envir)
  invisible(NULL)
}

#' @rdname borg_cache
#' @export
borg_cache_clear <- function(envir = .borg_cache_env) {
  rm(list = ls(envir = envir), envir = envir)
  invisible(NULL)
}

#' @rdname borg_cache
#' @export
borg_cache_info <- function(envir = .borg_cache_env) {
  keys <- ls(envir = envir)
  if (length(keys) == 0) {
    return(data.frame(
      key = character(0), timestamp = character(0),
      nrow = integer(0), ncol = integer(0),
      stringsAsFactors = FALSE
    ))
  }

  entries <- lapply(keys, function(k) {
    e <- get(k, envir = envir, inherits = FALSE)
    data.frame(
      key = k,
      timestamp = format(e$timestamp),
      nrow = e$nrow,
      ncol = e$ncol,
      stringsAsFactors = FALSE
    )
  })

  do.call(rbind, entries)
}


#' @noRd
.cache_key <- function(data, coords, target) {
  # Hash based on dimensions + column names + first/last values
  dims <- paste(dim(data), collapse = "x")
  cols <- paste(sort(names(data)), collapse = ",")
  coord_str <- paste(coords %||% "none", collapse = ",")
  target_str <- target %||% "none"

  # Sample a few values for fingerprinting
  n <- nrow(data)
  sample_idx <- unique(c(1, min(2, n), ceiling(n / 2), n))
  sample_vals <- paste(unlist(data[sample_idx, seq_len(min(5, ncol(data)))]),
                       collapse = "|")

  raw <- paste(dims, cols, coord_str, target_str, sample_vals, sep = ":")
  # Simple hash: use nchar + character sums as fingerprint
  chars <- utf8ToInt(raw)
  hash <- sprintf("%x_%x", sum(chars), sum(chars * seq_along(chars)))
  paste0("borg_", hash)
}
