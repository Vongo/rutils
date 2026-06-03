# ---- urns ----------------------------------------------------------------
test_that("urns sorts unique non-NA values for the registered atomic types", {
  expect_equal(urns(c(3, 1, 2, 1, NA)), c(1, 2, 3))
  expect_equal(urns(c("b", "a", "b", NA)), c("a", "b"))
  expect_equal(urns(c(TRUE, NA, FALSE, TRUE)), c(FALSE, TRUE))
})

test_that("urns has a default method so factors and Dates no longer error", {
  d <- as.Date(c("2020-01-02", "2020-01-01", "2020-01-01", NA))
  expect_equal(urns(d), as.Date(c("2020-01-01", "2020-01-02")))

  f <- factor(c("b", "a", "a", NA))
  res <- expect_silent(urns(f))   # was: "no applicable method" / is.vector() rejection
  expect_equal(as.character(res), c("a", "b"))
})

# ---- lsh -----------------------------------------------------------------
test_that("lsh works when called inside a function (caller-environment scoping)", {
  f <- function() { aaa <- 1:1000; bbb <- "hello"; lsh(split = FALSE) }
  out <- NULL
  suppressMessages(invisible(capture.output(out <- f())))   # was: Error object 'aaa' not found
  expect_s3_class(out, "data.table")
  expect_setequal(out$name, c("aaa", "bbb"))
  expect_equal(out[name == "bbb", class], "character")
})

test_that("lsh handles an empty environment without error", {
  g <- function() lsh(split = FALSE)
  out <- NULL
  expect_silent(suppressMessages(invisible(capture.output(out <- g()))))
  expect_equal(nrow(out), 0L)
})

test_that("lsh reports the first class for multi-class objects", {
  f <- function() { dt <- data.table::data.table(a = 1); lsh(split = FALSE) }
  out <- NULL
  suppressMessages(invisible(capture.output(out <- f())))
  expect_equal(out[name == "dt", class], "data.table")
})

test_that("lsh(up=TRUE) errors cleanly when pryr is unavailable", {
  skip_if(requireNamespace("pryr", quietly = TRUE))
  f <- function() { x <- 1; lsh(up = TRUE) }
  expect_error(f(), "pryr")
})

test_that("urns.default handles POSIXct (sorted, NA dropped)", {
  t <- as.POSIXct(c("2020-01-02", "2020-01-01", "2020-01-01", NA), tz = "UTC")
  res <- urns(t)
  expect_length(res, 2)
  expect_false(anyNA(res))
  expect_true(res[1] < res[2])
})
