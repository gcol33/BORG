# ===========================================================================
# Tests for borg_guard()
# ===========================================================================

test_that("borg_guard validates required arguments", {
 data <- data.frame(x = 1:10, y = 11:20)

 # Non-data.frame data
 expect_error(
   borg_guard(data = "not a data.frame", train_idx = 1:5, test_idx = 6:10),
   "'data' must be a data.frame"
 )

 # Non-integer train_idx
 expect_error(
   borg_guard(data = data, train_idx = "a", test_idx = 6:10),
   "'train_idx' must be an integer vector"
 )

 # Non-integer test_idx
 expect_error(
   borg_guard(data = data, train_idx = 1:5, test_idx = "b"),
   "'test_idx' must be an integer vector"
 )
})


test_that("borg_guard detects index overlap", {
 data <- data.frame(x = 1:10, y = 11:20)

 expect_error(
   borg_guard(data = data, train_idx = 1:6, test_idx = 5:10),
   "BORG HARD VIOLATION.*overlap"
 )
})


test_that("borg_guard validates out-of-bounds indices", {
 data <- data.frame(x = 1:10, y = 11:20)

 expect_error(
   borg_guard(data = data, train_idx = 1:5, test_idx = 11:15),
   "out-of-bounds"
 )

 expect_error(
   borg_guard(data = data, train_idx = 0:4, test_idx = 6:10),
   "out-of-bounds"
 )
})


test_that("borg_guard returns borg_context object", {
 data <- data.frame(x = 1:10, y = 11:20)

 ctx <- borg_guard(data = data, train_idx = 1:5, test_idx = 6:10)

 expect_s3_class(ctx, "borg_context")
 expect_equal(ctx$mode, "strict")
 expect_equal(length(ctx$train_idx), 5)
 expect_equal(length(ctx$test_idx), 5)
})


test_that("borg_guard accepts valid mode argument", {
 data <- data.frame(x = 1:10, y = 11:20)

 # Valid modes
 ctx_strict <- borg_guard(data, 1:5, 6:10, mode = "strict")
 expect_equal(ctx_strict$mode, "strict")

 ctx_warn <- borg_guard(data, 1:5, 6:10, mode = "warn")
 expect_equal(ctx_warn$mode, "warn")

 ctx_rewrite <- borg_guard(data, 1:5, 6:10, mode = "rewrite")
 expect_equal(ctx_rewrite$mode, "rewrite")

 # Invalid mode
 expect_error(
   borg_guard(data, 1:5, 6:10, mode = "invalid"),
   "'arg' should be one of"
 )
})


test_that("borg_guard validates temporal_col", {
 data <- data.frame(x = 1:10, y = 11:20, time = 1:10)

 # Non-existent column
 expect_error(
   borg_guard(data, 1:5, 6:10, temporal_col = "nonexistent"),
   "'temporal_col'.*not found"
 )

 # Valid column
 ctx <- borg_guard(data, 1:5, 6:10, temporal_col = "time")
 expect_equal(ctx$temporal_col, "time")
})


test_that("borg_guard validates spatial_cols", {
 data <- data.frame(x = 1:10, y = 11:20, lon = runif(10), lat = runif(10))

 # Non-existent columns
 expect_error(
   borg_guard(data, 1:5, 6:10, spatial_cols = c("lng", "latitude")),
   "'spatial_cols' not found"
 )

 # Valid columns
 ctx <- borg_guard(data, 1:5, 6:10, spatial_cols = c("lon", "lat"))
 expect_equal(ctx$spatial_cols, c("lon", "lat"))
})


test_that("borg_guard validates group_col", {
 data <- data.frame(x = 1:10, y = 11:20, group = rep(1:2, each = 5))

 # Non-existent column
 expect_error(
   borg_guard(data, 1:5, 6:10, group_col = "nonexistent"),
   "'group_col'.*not found"
 )

 # Valid column
 ctx <- borg_guard(data, 1:5, 6:10, group_col = "group")
 expect_equal(ctx$group_col, "group")
})


test_that("borg_guard detects group overlap in strict mode", {
 data <- data.frame(
   x = 1:10,
   y = 11:20,
   group = c(1, 1, 2, 2, 3, 3, 4, 4, 1, 2)  # groups 1,2 in both train and test
 )

 expect_error(
   borg_guard(data, 1:4, 5:10, mode = "strict", group_col = "group"),
   "BORG HARD VIOLATION.*Groups"
 )
})


test_that("borg_guard warns on group overlap in warn mode", {
 data <- data.frame(
   x = 1:10,
   y = 11:20,
   group = c(1, 1, 2, 2, 3, 3, 4, 4, 1, 2)  # groups 1,2 in both train and test
 )

 expect_warning(
   borg_guard(data, 1:4, 5:10, mode = "warn", group_col = "group"),
   "BORG HARD VIOLATION.*Groups"
 )
})


test_that("borg_guard detects temporal violations in strict mode", {
 data <- data.frame(
   x = 1:10,
   y = 11:20,
   time = c(1, 2, 3, 4, 5, 2, 3, 4, 6, 7)  # test rows 6-8 predate max train
 )

 expect_error(
   borg_guard(data, 1:5, 6:10, mode = "strict", temporal_col = "time"),
   "BORG HARD VIOLATION.*Temporal"
 )
})


test_that("borg_guard allows valid temporal split", {
 data <- data.frame(
   x = 1:10,
   y = 11:20,
   time = 1:10  # perfectly ordered
 )

 ctx <- borg_guard(data, 1:5, 6:10, mode = "strict", temporal_col = "time")
 expect_s3_class(ctx, "borg_context")
})


test_that("print.borg_context works", {
 data <- data.frame(x = 1:10, y = 11:20)
 ctx <- borg_guard(data, 1:5, 6:10)

 expect_output(print(ctx), "BORG Guarded Context")
 expect_output(print(ctx), "Mode:")
 expect_output(print(ctx), "Train indices:")
})
