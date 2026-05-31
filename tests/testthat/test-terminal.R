# Regression tests for the non-TTY progress-bar crash.
#
# In cron / CI / piped output, `system("stty size")` fails with
# "Inappropriate ioctl for device". Previously ws() swallowed the resulting
# error and returned warning()'s (character) value, so update_pb() then did
# `terminal_width - time_width` on a string and raised
# "non-numeric argument to binary operator" (Calls: update_pb -> ifelse).
#
# The fix: ws() always returns a positive numeric width, create_pb() returns
# NULL when there is no terminal, and update_pb() no-ops on a NULL bar.

test_that("ws() returns a single positive numeric width, even without a TTY", {
  old <- options(width = getOption("width"))  # ws() mutates width; restore after
  on.exit(options(old), add = TRUE)
  w <- ws()
  expect_true(is.numeric(w))
  expect_length(w, 1)
  expect_false(is.na(w))
  expect_gt(w, 0)
})

test_that("ws() sets the width option to the width it returns", {
  old <- options(width = 70)
  on.exit(options(old), add = TRUE)
  w <- ws()
  expect_identical(getOption("width"), as.integer(w))
})

test_that("ws(ret = FALSE) returns NULL invisibly", {
  old <- options(width = getOption("width"))
  on.exit(options(old), add = TRUE)
  expect_null(ws(ret = FALSE))
})

test_that("update_pb() no-ops on a NULL progress bar", {
  expect_null(update_pb(NULL, 1))
})

test_that("create_pb() + update_pb() never error in any context", {
  # cron / CI: create_pb() returns NULL and update_pb() no-ops.
  # real terminal: the bar renders. Neither path may raise.
  expect_no_error(capture.output({
    pb <- create_pb(5, bar_style = "pc", time_style = "cd")
    for (i in 1:5) update_pb(pb, i)
  }))
})

test_that("create_pb() returns NULL when there is no terminal", {
  skip_if(interactive() || isatty(stdout()), "needs a non-TTY session")
  expect_null(create_pb(10))
})

# The "never error" test above only exercises update_pb()'s NULL path in
# non-interactive runs (CI / R CMD check), because create_pb() returns NULL
# there. Drive the render body directly with a hand-built bar so the
# width/geometry clamps run regardless of TTY.
make_fake_pb <- function(tot_iter = 5, bar_style = "simple", time_style = "cd") {
  list(dep_time = Sys.time() - 10, tot_iter = tot_iter,
       bar_style = bar_style, time_style = time_style)
}

test_that("update_pb() render path never errors across bar/time styles", {
  for (bs in c("simple", "pc")) for (ts in c("cd", "end")) {
    pb <- make_fake_pb(5, bs, ts)
    expect_no_error(capture.output(for (i in 1:5) update_pb(pb, i)))
  }
})

test_that("update_pb() tolerates a zero-length job (Inf progress)", {
  # tot_iter = 0 makes progress = index/0 = Inf; the bar_nb clamp must tame it.
  pb <- make_fake_pb(0, "pc", "cd")
  expect_no_error(capture.output(update_pb(pb, 1)))
})

test_that("update_pb() survives a very narrow terminal (geometry clamp)", {
  # On a real TTY stty reports the actual (wide) width, so force the non-TTY
  # path: ws() then falls back to getOption("width"), pinned here to R's
  # minimum (10). bar_width - time_width goes negative, so the max(1L, ...) /
  # min(bar_nb, bar_width) clamps must keep rep()'s counts non-negative.
  skip_if(isatty(stdout()), "stty reports the real width on a TTY")
  old <- options(width = 10)
  on.exit(options(old), add = TRUE)
  pb <- make_fake_pb(5, "simple", "cd")
  expect_no_error(capture.output(for (i in 1:5) update_pb(pb, i)))
})

test_that("update_pb() survives a non-numeric ws() return (the original crash)", {
  # The reported bug: a stale ws() returned warning()'s *character* value in a
  # non-TTY, and update_pb() then computed `terminal_width - time_width` on a
  # string -> "non-numeric argument to binary operator". The numeric guard in
  # update_pb() must coerce any non-numeric width before the arithmetic.
  local_mocked_bindings(ws = function(...) "Inappropriate ioctl for device")
  pb <- make_fake_pb(5, "simple", "cd")
  expect_no_error(capture.output(for (i in 1:5) update_pb(pb, i)))
})
