# ---- round_clever --------------------------------------------------------
test_that("round_clever returns whole numbers unchanged (was NA)", {
  expect_equal(round_clever(5), 5)
  expect_equal(round_clever(0), 0)
  expect_equal(round_clever(-5), -5)
  expect_equal(round_clever(9L), 9)
})

test_that("round_clever rounds negatives symmetrically (sign must not inflate magnitude)", {
  expect_equal(round_clever(42.7), 40)
  expect_equal(round_clever(-42.7), -40)
})

test_that("round_clever handles large magnitudes without scientific-notation breakage", {
  expect_equal(round_clever(1e20), 1e20)
  expect_equal(round_clever(123456789), 123000000)
  expect_equal(round_clever(12345.6789), 12000)
})

test_that("round_clever preserves the documented example and small decimals", {
  expect_equal(round_clever(1.2346789), 1.23)
  expect_equal(round_clever(c(123456789, 12345.6789, 1.2346789)),
               c(123000000, 12000, 1.23))
})

test_that("round_clever propagates NA and handles empty input", {
  expect_true(is.na(round_clever(NA_real_)))
  expect_identical(round_clever(numeric(0)), numeric(0))
  expect_equal(round_clever(c(5, NA, 42.7)), c(5, NA, 40))
})

test_that("round_clever preserves Inf/NaN and is locale-independent", {
  expect_identical(round_clever(Inf), Inf)
  expect_identical(round_clever(-Inf), -Inf)
  expect_true(is.nan(round_clever(NaN)))
  withr::local_options(OutDec = ",")
  expect_equal(round_clever(12345.6789), 12000)
})

# ---- bucket / bucket2 ----------------------------------------------------
test_that("bucket matches the original strict-quantile semantics", {
  set.seed(1); v <- rnorm(500)
  sp <- quantile(v, probs = seq(0, 1, by = 1/10)[2:11])
  expected <- vapply(v, function(e) min(seq_along(sp)[e < sp], length(sp)), numeric(1))
  expect_equal(bucket(v, 10), unname(expected))
})

test_that("bucket does not crash on NA and returns NA for NA elements", {
  v <- c(1, 5, NA, 9, 2)
  out <- expect_silent(bucket(v, 4))
  expect_true(is.na(out[3]))
  expect_false(anyNA(out[-3]))
})

test_that("bucket2 does not crash on NA", {
  expect_silent(bucket2(c(1, 2, NA, 4)))
})

test_that("bucket/bucket2 return all-NA for all-NA input without error", {
  expect_equal(bucket(c(NA, NA, NA), 5), rep(NA_integer_, 3))
  expect_equal(bucket2(c(NA_real_, NA_real_)), rep(NA_integer_, 2))
})

test_that("bucket2 errors on all-NA splits (bad thresholds), not silently all-NA", {
  expect_error(bucket2(c(1, 2, 3), splits = c(NA, NA)), "threshold")
})

test_that("bucket2 uses strict-< boundary semantics at exact splits", {
  expect_equal(bucket2(c(49, 50, 74, 75), splits = c(50, 75, 100)), c(1, 2, 2, 3))
})

test_that("bucket with round.clever rounds integer splits instead of producing NA buckets", {
  v <- as.numeric(0:9)
  out <- bucket(v, 5, round.clever = TRUE)
  expect_false(anyNA(out))
})

# ---- minmax --------------------------------------------------------------
test_that("minmax clamps to [min, max] (documented behaviour, vectorised)", {
  expect_equal(minmax(c(0, 80, 200), 75, 125), c(75, 80, 125))
})

test_that("minmax equals pmin/pmax clamp on a large vector", {
  set.seed(2); x <- rnorm(1000, 100, 25)
  expect_equal(minmax(x, 75, 125), pmin(pmax(x, 75), 125))
})

test_that("minmax na.value is applied BEFORE clamping (documented two-mode design)", {
  # na.value=-10 with range 75..125: -10 is set then clamped up to 75
  expect_equal(minmax(c(100, NA), 75, 125, na.value = -10), c(100, 75))
  # na.post=-10 bypasses the clamp and survives as a sentinel
  expect_equal(minmax(c(100, NA), 75, 125, na.post = -10), c(100, -10))
})

test_that("minmax returns numeric(0) for empty input (was a list)", {
  expect_identical(minmax(numeric(0), 0, 1), numeric(0))
})

test_that("minmax handles all-NA input quietly and documents reversed-bound behaviour", {
  expect_silent(r <- minmax(c(NA_real_, NA_real_)))
  expect_true(all(is.na(r)))
  expect_equal(minmax(c(NA_real_, NA_real_), na.post = 0), c(0, 0))
  # reversed explicit bounds collapse via pmin(pmax(...)) — documented, not an error
  expect_equal(minmax(c(0, 80, 200), 125, 75), c(75, 75, 75))
})
