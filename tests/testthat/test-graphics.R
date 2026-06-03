# Plot to a null device so nothing is displayed during tests.
test_that("lines2 uses the actual x range for xlim, not 1:ncol", {
  pdf(NULL); on.exit(grDevices::dev.off())
  lines2(matrix(1:12, nrow = 3), x = c(10, 20, 30, 40), ynames = letters[1:3])
  usr <- graphics::par("usr")
  expect_lte(usr[1], 10)   # plotting region must cover the smallest x
  expect_gte(usr[2], 40)   # ... and the largest x
})

test_that("pie2 builds labels and plots without error", {
  pdf(NULL); on.exit(grDevices::dev.off())
  x <- c(a = 5, b = 3, c = 2)
  expect_error(pie2(x), NA)
})

test_that("plotly_stacked_area fails cleanly when plotly is unavailable", {
  skip_if(requireNamespace("plotly", quietly = TRUE) &&
          requireNamespace("wesanderson", quietly = TRUE))
  expect_error(
    plotly_stacked_area(data.table::data.table(t = 1:3, v = 1:3, g = "a"), "t", "v", "g"),
    "plotly"
  )
})
