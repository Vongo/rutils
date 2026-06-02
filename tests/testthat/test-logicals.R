test_that("na.false treats NA as FALSE and keeps TRUE/FALSE", {
  expect_identical(na.false(NA), FALSE)
  expect_identical(na.false(TRUE), TRUE)
  expect_equal(na.false(c(TRUE, NA, FALSE)), c(TRUE, FALSE, FALSE))
})

test_that("na.true treats NA as TRUE and keeps TRUE/FALSE", {
  expect_identical(na.true(NA), TRUE)
  expect_identical(na.true(FALSE), FALSE)
  expect_equal(na.true(c(TRUE, NA, FALSE)), c(TRUE, TRUE, FALSE))
})

test_that("zero-length input returns the scalar default (guard was dead code)", {
  expect_identical(na.false(logical(0)), FALSE)
  expect_identical(na.true(logical(0)), TRUE)
})

test_that("non-logical input is coerced to logical, honouring the documented contract", {
  expect_identical(na.false(c(1, 0, NA)), c(TRUE, FALSE, FALSE))
  expect_type(na.false(c(1, 0, NA)), "logical")
})
