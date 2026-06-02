test_that("create_pb has a deterministic default style and does not consume the RNG", {
  set.seed(1); a <- runif(1)
  set.seed(1); pb <- create_pb(5, width = 80); b <- runif(1)
  expect_equal(a, b)                 # RNG stream untouched (was sample()d)
  expect_equal(pb$bar_style, "simple")
  expect_equal(pb$time_style, "cd")
  expect_equal(pb$width, 80)
})

test_that("update_pb does not crash when index exceeds tot_iter (was rep() negative)", {
  pb <- create_pb(10, "simple", "cd", width = 80)
  expect_error(capture.output(update_pb(pb, 15)), NA)
})

test_that("update_pb does not crash at index 0 (division by zero / Inf)", {
  pb <- create_pb(10, "pc", "end", width = 80)
  expect_error(capture.output(update_pb(pb, 0)), NA)
})
