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
  suppressMessages(invisible(capture.output(out <- g())))
  expect_equal(nrow(out), 0L)
})
