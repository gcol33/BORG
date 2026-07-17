# Head-to-head spatial cross-validation benchmark for the BORG JOSS paper.
#
# Task: predict log(zinc) on the canonical Meuse river dataset (sp::meuse, n = 155)
# with a random forest (ranger), evaluated under six 5-fold CV schemes:
#   random k-fold (the optimistic baseline) and five spatially-blocked schemes,
#   one from each of BORG, blockCV, CAST, spatialsample, and sperrorest.
#
# For each scheme we fit ranger on the training rows of every fold, predict the
# held-out rows, pool the residuals across folds, and report the RMSE. Stochastic
# schemes are repeated over 20 seeds; we report the median and IQR. Single-threaded
# ranger throughout for reproducibility.
#
# Output: paper/bench_results.csv  (one row per CV scheme).
# Reproduce: Rscript paper/bench_spatial_cv.R

suppressMessages({
  library(BORG)
  library(sf)
  library(blockCV)
  library(CAST)
  library(spatialsample)
  library(sperrorest)
  library(ranger)
})

set.seed(2026)

# ---- data --------------------------------------------------------------------
data(meuse, package = "sp")
meuse$lzinc <- log(meuse$zinc)
predictors <- c("dist", "elev", "om", "ffreq", "soil")
meuse <- meuse[stats::complete.cases(meuse[, c("lzinc", predictors)]), ]
n <- nrow(meuse)
form <- stats::as.formula(paste("lzinc ~", paste(predictors, collapse = " + ")))
meuse_sf <- st_as_sf(meuse, coords = c("x", "y"), crs = 28992)

# prediction target: the Meuse floodplain grid (mapping to new locations, not
# interpolation at the sampled points). knndm needs this to reflect the task.
data(meuse.grid, package = "sp")
grid_sf <- st_as_sf(meuse.grid, coords = c("x", "y"), crs = 28992)

# ---- common evaluator --------------------------------------------------------
# folds: list of list(train = <int idx>, test = <int idx>). Returns pooled RMSE.
eval_folds <- function(folds, seed) {
  res <- numeric(0)
  for (f in folds) {
    tr <- f$train; te <- f$test
    if (length(tr) < 5 || length(te) < 1) next
    fit <- ranger(form, data = meuse[tr, , drop = FALSE],
                  num.trees = 500, num.threads = 1, seed = seed)
    pr <- predict(fit, data = meuse[te, , drop = FALSE])$predictions
    res <- c(res, meuse$lzinc[te] - pr)
  }
  sqrt(mean(res^2))
}

# ---- fold generators (normalised to list(train, test)) -----------------------
folds_random <- function(seed) {
  set.seed(seed)
  fold_id <- sample(rep_len(1:5, n))
  lapply(1:5, function(k) list(train = which(fold_id != k), test = which(fold_id == k)))
}

folds_borg <- function(seed) {
  set.seed(seed)
  r <- borg(meuse, coords = c("x", "y"), target = "lzinc", v = 5)
  lapply(r$folds, function(f) list(train = f$train, test = f$test))
}

folds_blockcv <- function(seed) {
  set.seed(seed)
  bcv <- cv_spatial(x = meuse_sf, k = 5, size = 900, selection = "random",
                    iteration = 20, progress = FALSE, plot = FALSE, report = FALSE)
  lapply(bcv$folds_list, function(f) list(train = f[[1]], test = f[[2]]))
}

folds_cast <- function(seed) {
  set.seed(seed)
  kn <- knndm(tpoints = meuse_sf, predpoints = grid_sf, k = 5)
  lapply(seq_along(kn$indx_train), function(i)
    list(train = kn$indx_train[[i]], test = kn$indx_test[[i]]))
}

folds_spatialsample <- function(seed) {
  set.seed(seed)
  sb <- spatial_block_cv(meuse_sf, v = 5)
  lapply(sb$splits, function(s) {
    tr <- s$in_id
    list(train = tr, test = setdiff(seq_len(n), tr))
  })
}

folds_sperrorest <- function(seed) {
  pk <- partition_kmeans(meuse, coords = c("x", "y"), nfold = 5,
                         repetition = 1, seed1 = seed)
  lapply(pk[[1]], function(f) list(train = f$train, test = f$test))
}

# ---- run ---------------------------------------------------------------------
schemes <- list(
  "random k-fold"          = list(gen = folds_random,        spatial = FALSE),
  "BORG spatial_block"     = list(gen = folds_borg,          spatial = TRUE),
  "blockCV cv_spatial"     = list(gen = folds_blockcv,       spatial = TRUE),
  "CAST knndm"             = list(gen = folds_cast,          spatial = TRUE),
  "spatialsample block"    = list(gen = folds_spatialsample, spatial = TRUE),
  "sperrorest kmeans"      = list(gen = folds_sperrorest,    spatial = TRUE)
)

seeds <- 1:20
rows <- list()
for (nm in names(schemes)) {
  gen <- schemes[[nm]]$gen
  rmses <- vapply(seeds, function(s) {
    tryCatch(eval_folds(gen(s), s), error = function(e) { message(nm, " seed ", s, ": ", conditionMessage(e)); NA_real_ })
  }, numeric(1))
  rmses <- rmses[is.finite(rmses)]
  rows[[nm]] <- data.frame(
    scheme      = nm,
    is_spatial  = schemes[[nm]]$spatial,
    rmse_median = median(rmses),
    rmse_q25    = as.numeric(quantile(rmses, 0.25)),
    rmse_q75    = as.numeric(quantile(rmses, 0.75)),
    n_ok        = length(rmses)
  )
  cat(sprintf("%-22s RMSE median = %.4f  (n=%d)\n", nm, median(rmses), length(rmses)))
}
res <- do.call(rbind, rows)

# optimism of random CV relative to the median of the five spatial schemes
spatial_ref <- median(res$rmse_median[res$is_spatial])
res$optimism_pct <- round(100 * (spatial_ref - res$rmse_median) / spatial_ref, 1)

# ---- BORG's automatic diagnosis (what only BORG reports) ---------------------
d <- borg_diagnose(meuse, coords = c("x", "y"), target = "lzinc")
cat("\nBORG diagnosis:\n")
cat(sprintf("  Moran's I = %.3f, p = %.2e, dependency = %s, severity = %s, recommends = %s\n",
            d@spatial$morans_i, d@spatial$morans_p, d@dependency_type, d@severity, d@recommended_cv))

write.csv(res, "paper/bench_results.csv", row.names = FALSE)
cat("\nWrote paper/bench_results.csv\n")
print(res, row.names = FALSE)
