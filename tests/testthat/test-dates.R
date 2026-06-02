test_that("mondf counts calendar-month boundaries crossed", {
  expect_equal(mondf("2019-01-01", "2019-03-01"), 2)
  expect_equal(mondf("2019-03-01", "2019-01-01", absolute = FALSE), -2)
  expect_equal(mondf("2019-01-01", "2019-03-01", absolute = FALSE), 2)
})

test_that("mondf recycles a length-1 argument (documented vectorised use)", {
  expect_equal(mondf(c("2019-01-01", "2018-01-01"), "2020-01-01"), c(12, 24))
})

test_that("mondf errors on mismatched lengths instead of silently recycling", {
  expect_error(
    mondf(c("2019-01-01", "2018-01-01"), c("2020-01-01", "2020-01-01", "2020-01-01"))
  )
})
