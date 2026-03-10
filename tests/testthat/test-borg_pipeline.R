# ===========================================================================
# Tests for borg_pipeline()
# ===========================================================================

test_that("borg_pipeline returns correct structure", {
  workflow <- list(
    data = data.frame(x = 1:20, y = rnorm(20)),
    train_idx = 1:15,
    test_idx = 16:20,
    model = lm(y ~ x, data = data.frame(x = 1:15, y = rnorm(15)))
  )

  result <- borg_pipeline(workflow, train_idx = 1:15, test_idx = 16:20,
                           data = workflow$data)

  expect_s3_class(result, "borg_pipeline")
  expect_true(is.list(result$stages))
  expect_s4_class(result$overall, "BorgRisk")
  expect_true(is.numeric(result$n_stages))
  expect_true(is.character(result$leaking_stages))
})


test_that("borg_pipeline detects preprocessing leak in list pipeline", {
  skip_if_not_installed("caret")

  set.seed(42)
  data <- data.frame(
    x1 = rnorm(100, mean = 10, sd = 5),
    x2 = rnorm(100, mean = 50, sd = 20)
  )

  # Leaky: preProcess on full data
  pp_bad <- suppressWarnings(
    caret::preProcess(data, method = c("center", "scale"))
  )

  pipeline <- list(
    data = data,
    preprocess = pp_bad,
    model = lm(x1 ~ x2, data = data[1:70, ])
  )

  result <- borg_pipeline(pipeline, train_idx = 1:70, test_idx = 71:100,
                           data = data)

  expect_true("preprocessing" %in% names(result$stages))
  # Preprocessing stage should detect leak
  expect_false(result$stages$preprocessing@is_valid)
})


test_that("borg_pipeline handles clean pipeline", {
  set.seed(42)
  data <- data.frame(x = 1:20, y = 2 * (1:20) + rnorm(20))
  model <- lm(y ~ x, data = data[1:15, ])

  pipeline <- list(
    data = data,
    model = model
  )

  result <- borg_pipeline(pipeline, train_idx = 1:15, test_idx = 16:20,
                           data = data)

  expect_s3_class(result, "borg_pipeline")
  expect_equal(length(result$leaking_stages), 0)
})


test_that("borg_pipeline prints without error", {
  pipeline <- list(
    data = data.frame(x = 1:20, y = rnorm(20)),
    model = lm(y ~ x, data = data.frame(x = 1:15, y = rnorm(15)))
  )

  result <- borg_pipeline(pipeline, train_idx = 1:15, test_idx = 16:20)
  expect_output(print(result), "BORG Pipeline Validation")
})


test_that("borg_pipeline validates inputs", {
  expect_error(
    borg_pipeline("not a pipeline", train_idx = 1:5, test_idx = 6:10),
    "Unsupported pipeline type"
  )
  expect_error(
    borg_pipeline(list(), train_idx = NULL, test_idx = 1:5),
    "'train_idx' and 'test_idx' are required"
  )
})
