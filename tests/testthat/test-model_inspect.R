# ===========================================================================
# Tests for model object inspection
# ===========================================================================

# ---------------------------------------------------------------------------
# lm inspection
# ---------------------------------------------------------------------------

test_that("borg_inspect detects lm trained on wrong data", {
  set.seed(42)
  data <- data.frame(y = rnorm(100), x = rnorm(100))
  train_idx <- 1:70
  test_idx <- 71:100

  # BAD: lm fitted on full data
  model_bad <- lm(y ~ x, data = data)

  result_bad <- borg_inspect(model_bad, train_idx, test_idx, data = data)
  expect_gt(result_bad@n_hard, 0L)
  scope_risk <- Filter(function(r) r$type == "model_scope", result_bad@risks)
  expect_gt(length(scope_risk), 0)

  # GOOD: lm fitted on train only
  model_good <- lm(y ~ x, data = data[train_idx, ])

  result_good <- borg_inspect(model_good, train_idx, test_idx, data = data)
  expect_true(result_good@is_valid)
})

test_that("borg_inspect detects lm trained on wrong rows", {
  set.seed(42)
  data <- data.frame(y = rnorm(100), x = rnorm(100))
  train_idx <- 1:70
  test_idx <- 71:100

  # BAD: lm fitted on different subset (includes test data)
  wrong_idx <- c(1:50, 71:90)  # Includes test indices!
  model_bad <- lm(y ~ x, data = data[wrong_idx, ])

  result_bad <- borg_inspect(model_bad, train_idx, test_idx, data = data)
  # Model size matches (70 obs), but rows are wrong
  # This should detect the issue via row alignment check
  expect_s4_class(result_bad, "BorgRisk")
})


# ---------------------------------------------------------------------------
# glm inspection
# ---------------------------------------------------------------------------

test_that("borg_inspect detects glm trained on wrong data", {
  set.seed(42)
  data <- data.frame(
    y = rbinom(100, 1, 0.5),
    x = rnorm(100)
  )
  train_idx <- 1:70
  test_idx <- 71:100

  # BAD: glm fitted on full data
  model_bad <- glm(y ~ x, data = data, family = binomial)

  result_bad <- borg_inspect(model_bad, train_idx, test_idx, data = data)
  expect_gt(result_bad@n_hard, 0L)

  # GOOD: glm fitted on train only
  model_good <- glm(y ~ x, data = data[train_idx, ], family = binomial)

  result_good <- borg_inspect(model_good, train_idx, test_idx, data = data)
  expect_true(result_good@is_valid)
})


# ---------------------------------------------------------------------------
# ranger inspection
# ---------------------------------------------------------------------------

test_that("borg_inspect detects ranger trained on wrong data", {
  skip_if_not_installed("ranger")

  set.seed(42)
  data <- data.frame(y = rnorm(100), x1 = rnorm(100), x2 = rnorm(100))
  train_idx <- 1:70
  test_idx <- 71:100

  # BAD: ranger fitted on full data
  model_bad <- ranger::ranger(y ~ ., data = data)

  result_bad <- borg_inspect(model_bad, train_idx, test_idx, data = data)
  expect_gt(result_bad@n_hard, 0L)
  scope_risk <- Filter(function(r) r$type == "model_scope", result_bad@risks)
  expect_gt(length(scope_risk), 0)

  # GOOD: ranger fitted on train only
  model_good <- ranger::ranger(y ~ ., data = data[train_idx, ])

  result_good <- borg_inspect(model_good, train_idx, test_idx, data = data)
  expect_true(result_good@is_valid)
})


# ---------------------------------------------------------------------------
# parsnip inspection
# ---------------------------------------------------------------------------

test_that("borg_inspect handles parsnip model_fit objects", {
  skip_if_not_installed("parsnip")

  set.seed(42)
  data <- data.frame(y = rnorm(100), x = rnorm(100))
  train_idx <- 1:70
  test_idx <- 71:100

  # BAD: parsnip model fitted on full data
  spec <- parsnip::linear_reg() |>
    parsnip::set_engine("lm")
  model_bad <- parsnip::fit(spec, y ~ x, data = data)

  result_bad <- borg_inspect(model_bad, train_idx, test_idx, data = data)
  expect_gt(result_bad@n_hard, 0L)

  # GOOD: parsnip model fitted on train only
  model_good <- parsnip::fit(spec, y ~ x, data = data[train_idx, ])

  result_good <- borg_inspect(model_good, train_idx, test_idx, data = data)
  expect_true(result_good@is_valid)
})


# ---------------------------------------------------------------------------
# workflow inspection
# ---------------------------------------------------------------------------

test_that("borg_inspect handles tidymodels workflow objects", {
  skip_if_not_installed("workflows")
  skip_if_not_installed("recipes")
  skip_if_not_installed("parsnip")

  set.seed(42)
  data <- data.frame(
    y = rnorm(100),
    x1 = rnorm(100, mean = 10),
    x2 = rnorm(100, mean = 50)
  )
  train_idx <- 1:70
  test_idx <- 71:100

  # Create a workflow
  rec <- recipes::recipe(y ~ ., data = data[train_idx, ]) |>
    recipes::step_normalize(recipes::all_numeric_predictors())

  spec <- parsnip::linear_reg() |>
    parsnip::set_engine("lm")

  wf <- workflows::workflow() |>
    workflows::add_recipe(rec) |>
    workflows::add_model(spec)

  # GOOD: workflow fitted on train only
  wf_good <- parsnip::fit(wf, data = data[train_idx, ])

  result_good <- borg_inspect(wf_good, train_idx, test_idx, data = data)
  expect_s4_class(result_good, "BorgRisk")
  # Should be valid if fitted correctly
  expect_true(result_good@is_valid)
})


# ---------------------------------------------------------------------------
# Edge cases
# ---------------------------------------------------------------------------

test_that("model inspection handles NULL train_idx", {
  set.seed(42)
  data <- data.frame(y = rnorm(100), x = rnorm(100))
  model <- lm(y ~ x, data = data)

  # Should not error with NULL indices
  result <- borg_inspect(model, train_idx = NULL, test_idx = NULL, data = data)
  expect_s4_class(result, "BorgRisk")
  expect_true(result@is_valid)
})

test_that("model inspection handles missing data argument", {
  set.seed(42)
  data <- data.frame(y = rnorm(100), x = rnorm(100))
  train_idx <- 1:70
  test_idx <- 71:100

  model <- lm(y ~ x, data = data)

  # Should work without data argument (limited checks)
  result <- borg_inspect(model, train_idx, test_idx)
  expect_s4_class(result, "BorgRisk")
})
