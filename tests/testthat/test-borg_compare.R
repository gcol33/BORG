# ===========================================================================
# Tests for borg_compare_cv() and reporting functions
# ===========================================================================

test_that("borg_compare_cv() runs with spatial data", {
  set.seed(42)
  n <- 100

  # Create spatially autocorrelated data
  spatial_data <- data.frame(
    x = runif(n, 0, 100),
    y = runif(n, 0, 100)
  )
  # Response correlated with location
  spatial_data$response <- spatial_data$x * 0.5 + spatial_data$y * 0.3 + rnorm(n, sd = 5)

  # Run comparison with few repeats for speed

  comparison <- borg_compare_cv(
    data = spatial_data,
    formula = response ~ x + y,
    coords = c("x", "y"),
    v = 3,
    repeats = 3,
    seed = 123,
    verbose = FALSE
  )

  expect_s3_class(comparison, "borg_comparison")
  expect_true("random_cv" %in% names(comparison))
  expect_true("blocked_cv" %in% names(comparison))
  expect_true("inflation" %in% names(comparison))
  expect_true("p_value" %in% names(comparison))

  # Check structure

  expect_equal(nrow(comparison$random_cv), 3)
  expect_equal(nrow(comparison$blocked_cv), 3)
  expect_equal(comparison$repeats, 3)
})


test_that("borg_compare_cv() runs with grouped data", {
  set.seed(42)

  # Create clustered data
  grouped_data <- data.frame(
    site = rep(1:10, each = 10),
    x = rnorm(100)
  )
  # Response varies by site
  site_effects <- rnorm(10, sd = 5)
  grouped_data$response <- site_effects[grouped_data$site] + grouped_data$x + rnorm(100)

  comparison <- borg_compare_cv(
    data = grouped_data,
    formula = response ~ x,
    groups = "site",
    v = 3,
    repeats = 3,
    seed = 456,
    verbose = FALSE
  )

  expect_s3_class(comparison, "borg_comparison")
  expect_equal(comparison$diagnosis@dependency_type, "clustered")
})


test_that("borg_compare_cv() runs with temporal data", {
  set.seed(42)

  # Create temporal data
  n <- 100
  temporal_data <- data.frame(
    date = seq(as.Date("2020-01-01"), by = "day", length.out = n),
    x = rnorm(n)
  )
  # Autocorrelated response
  temporal_data$response <- cumsum(rnorm(n)) + temporal_data$x

  comparison <- borg_compare_cv(
    data = temporal_data,
    formula = response ~ x,
    time = "date",
    v = 3,
    repeats = 3,
    seed = 789,
    verbose = FALSE
  )

  expect_s3_class(comparison, "borg_comparison")
})


test_that("borg_compare_cv() accepts custom model function", {
  set.seed(42)
  n <- 100
  data <- data.frame(
    x = runif(n, 0, 100),
    y = runif(n, 0, 100),
    response = rnorm(n)
  )

  # Custom model function (just lm but wrapped)
  custom_model <- function(formula, data) {
    lm(formula, data = data)
  }

  comparison <- borg_compare_cv(
    data = data,
    formula = response ~ x + y,
    model_fn = custom_model,
    coords = c("x", "y"),
    v = 3,
    repeats = 2,
    verbose = FALSE
  )

  expect_s3_class(comparison, "borg_comparison")
})


test_that("borg_compare_cv() computes different metrics", {
  set.seed(42)
  n <- 100
  data <- data.frame(
    x = runif(n, 0, 100),
    y = runif(n, 0, 100),
    response = rnorm(n)
  )

  # RMSE (default)
  comp_rmse <- borg_compare_cv(
    data = data,
    formula = response ~ x + y,
    metric = "rmse",
    coords = c("x", "y"),
    v = 3,
    repeats = 2,
    verbose = FALSE
  )
  expect_equal(comp_rmse$inflation$metric, "rmse")

  # MAE
  comp_mae <- borg_compare_cv(
    data = data,
    formula = response ~ x + y,
    metric = "mae",
    coords = c("x", "y"),
    v = 3,
    repeats = 2,
    verbose = FALSE
  )
  expect_equal(comp_mae$inflation$metric, "mae")

  # R-squared
  comp_rsq <- borg_compare_cv(
    data = data,
    formula = response ~ x + y,
    metric = "rsq",
    coords = c("x", "y"),
    v = 3,
    repeats = 2,
    verbose = FALSE
  )
  expect_equal(comp_rsq$inflation$metric, "rsq")
})


test_that("print.borg_comparison() works", {
  set.seed(42)
  data <- data.frame(
    x = runif(50, 0, 100),
    y = runif(50, 0, 100),
    response = rnorm(50)
  )

  comparison <- borg_compare_cv(
    data = data,
    formula = response ~ x + y,
    coords = c("x", "y"),
    v = 3,
    repeats = 2,
    verbose = FALSE
  )

  output <- capture.output(print(comparison))
  expect_true(any(grepl("CV Comparison", output)))
  expect_true(any(grepl("Random CV", output)))
  expect_true(any(grepl("Blocked CV", output)))
})


test_that("summary.borg_comparison() works", {
  set.seed(42)
  data <- data.frame(
    x = runif(50, 0, 100),
    y = runif(50, 0, 100),
    response = rnorm(50)
  )

  comparison <- borg_compare_cv(
    data = data,
    formula = response ~ x + y,
    coords = c("x", "y"),
    v = 3,
    repeats = 2,
    verbose = FALSE
  )

  output <- capture.output(summary(comparison))
  expect_true(any(grepl("BORG CV Comparison Summary", output)))
  expect_true(any(grepl("Dependency Diagnosis", output)))
})


test_that("plot.borg_comparison() works", {
  skip_if_not(capabilities("png"))

  set.seed(42)
  data <- data.frame(
    x = runif(50, 0, 100),
    y = runif(50, 0, 100),
    response = rnorm(50)
  )

  comparison <- borg_compare_cv(
    data = data,
    formula = response ~ x + y,
    coords = c("x", "y"),
    v = 3,
    repeats = 3,
    verbose = FALSE
  )

  # Test boxplot
  expect_silent(plot(comparison, type = "boxplot"))

  # Test density
  expect_silent(plot(comparison, type = "density"))

  # Test paired
  expect_silent(plot(comparison, type = "paired"))
})


# ===========================================================================
# Tests for summary() S3 methods
# ===========================================================================

test_that("summary.BorgDiagnosis() generates text for spatial data", {
  set.seed(42)
  data <- data.frame(
    x = runif(100, 0, 100),
    y = runif(100, 0, 100),
    response = rnorm(100)
  )

  diagnosis <- borg_diagnose(data, coords = c("x", "y"), target = "response",
                             verbose = FALSE)

  output <- capture.output(text <- summary(diagnosis, v = 5))

  expect_type(text, "character")
  expect_true(nchar(text) > 50)
  expect_true(grepl("cross-validation", text, ignore.case = TRUE))
  expect_true(grepl("BORG", text))
})


test_that("summary.BorgDiagnosis() generates text for clustered data", {
  set.seed(42)
  data <- data.frame(
    site = rep(1:20, each = 10),
    value = rep(rnorm(20), each = 10) + rnorm(200, sd = 0.5)
  )

  diagnosis <- borg_diagnose(data, groups = "site", target = "value",
                             verbose = FALSE)

  output <- capture.output(text <- summary(diagnosis, v = 5))

  expect_type(text, "character")
  expect_true(grepl("ICC|intraclass|clustered", text, ignore.case = TRUE))
})


test_that("summary.borg_result() works", {
  set.seed(42)
  data <- data.frame(
    x = runif(100, 0, 100),
    y = runif(100, 0, 100),
    response = rnorm(100)
  )

  result <- borg(data, coords = c("x", "y"), target = "response")

  output <- capture.output(text <- summary(result))

  expect_type(text, "character")
  expect_true(nchar(text) > 50)
})


test_that("summary.BorgRisk() works", {
  data <- data.frame(x = 1:100, y = 101:200)

  # With violations
  risk <- borg_inspect(data, train_idx = 1:60, test_idx = 51:100)
  output <- capture.output(summary(risk))
  expect_true(any(grepl("Risk Assessment", output)))
})


# ===========================================================================
# Tests for borg_certificate()
# ===========================================================================

test_that("borg_certificate() creates certificate", {
  set.seed(42)
  data <- data.frame(
    x = runif(100, 0, 100),
    y = runif(100, 0, 100),
    response = rnorm(100)
  )

  diagnosis <- borg_diagnose(data, coords = c("x", "y"), target = "response",
                             verbose = FALSE)

  cert <- borg_certificate(diagnosis, data)

  expect_s3_class(cert, "borg_certificate")
  expect_true("meta" %in% names(cert))
  expect_true("data" %in% names(cert))
  expect_true("diagnosis" %in% names(cert))

  # Check metadata
  expect_true(!is.null(cert$meta$borg_version))
  expect_true(!is.null(cert$meta$timestamp))

  # Check data info
  expect_equal(cert$data$n_observations, 100)
  expect_equal(cert$data$n_features, 3)
})


test_that("print.borg_certificate() works", {
  set.seed(42)
  data <- data.frame(
    x = runif(50, 0, 100),
    y = runif(50, 0, 100),
    response = rnorm(50)
  )

  diagnosis <- borg_diagnose(data, coords = c("x", "y"), target = "response",
                             verbose = FALSE)

  cert <- borg_certificate(diagnosis, data)

  output <- capture.output(print(cert))
  expect_true(any(grepl("BORG Validation Certificate", output)))
  expect_true(any(grepl("Dependency Diagnosis", output)))
})


test_that("borg_certificate() includes comparison when provided", {
  set.seed(42)
  data <- data.frame(
    x = runif(100, 0, 100),
    y = runif(100, 0, 100),
    response = rnorm(100)
  )

  diagnosis <- borg_diagnose(data, coords = c("x", "y"), target = "response",
                             verbose = FALSE)

  comparison <- borg_compare_cv(
    data = data,
    formula = response ~ x + y,
    diagnosis = diagnosis,
    coords = c("x", "y"),
    v = 3,
    repeats = 2,
    verbose = FALSE
  )

  cert <- borg_certificate(diagnosis, data, comparison = comparison)

  expect_true(!is.null(cert$inflation$empirical))
  expect_true(!is.null(cert$inflation$empirical$estimate))
})


# ===========================================================================
# Tests for borg_export()
# ===========================================================================

test_that("borg_export() writes YAML file", {
  set.seed(42)
  data <- data.frame(
    x = runif(50, 0, 100),
    y = runif(50, 0, 100),
    response = rnorm(50)
  )

  diagnosis <- borg_diagnose(data, coords = c("x", "y"), target = "response",
                             verbose = FALSE)

  # Write to temp file
  tmp_file <- tempfile(fileext = ".yaml")
  on.exit(unlink(tmp_file), add = TRUE)

  borg_export(diagnosis, data, tmp_file)

  expect_true(file.exists(tmp_file))

  # Check content
  content <- readLines(tmp_file)
  expect_true(any(grepl("borg_version", content)))
  expect_true(any(grepl("dependency_type", content)))
})


test_that("borg_export() writes JSON file", {
  set.seed(42)
  data <- data.frame(
    x = runif(50, 0, 100),
    y = runif(50, 0, 100),
    response = rnorm(50)
  )

  diagnosis <- borg_diagnose(data, coords = c("x", "y"), target = "response",
                             verbose = FALSE)

  # Write to temp file
  tmp_file <- tempfile(fileext = ".json")
  on.exit(unlink(tmp_file), add = TRUE)

  borg_export(diagnosis, data, tmp_file)

  expect_true(file.exists(tmp_file))

  # Check content
  content <- paste(readLines(tmp_file), collapse = "")
  expect_true(grepl("borg_version", content))
  expect_true(grepl("dependency_type", content))
})


test_that("summary includes comparison when provided", {
  set.seed(42)
  data <- data.frame(
    x = runif(100, 0, 100),
    y = runif(100, 0, 100),
    response = rnorm(100)
  )

  diagnosis <- borg_diagnose(data, coords = c("x", "y"), target = "response",
                             verbose = FALSE)

  comparison <- borg_compare_cv(
    data = data,
    formula = response ~ x + y,
    diagnosis = diagnosis,
    coords = c("x", "y"),
    v = 3,
    repeats = 2,
    verbose = FALSE
  )

  output <- capture.output(text <- summary(diagnosis, comparison = comparison))

  expect_true(grepl("Empirical|empirical", text))
})


test_that("summary respects include_citation option", {
  set.seed(42)
  data <- data.frame(
    x = runif(50, 0, 100),
    y = runif(50, 0, 100),
    response = rnorm(50)
  )

  diagnosis <- borg_diagnose(data, coords = c("x", "y"), target = "response",
                             verbose = FALSE)

  # With citation
  output_with <- capture.output(text_with <- summary(diagnosis, include_citation = TRUE))
  expect_true(grepl("BORG package", text_with))

  # Without citation
  output_without <- capture.output(text_without <- summary(diagnosis, include_citation = FALSE))
  expect_false(grepl("BORG package", text_without))
})
