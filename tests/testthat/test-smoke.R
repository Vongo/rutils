# Harness smoke test: confirms the package loads and a few stable helpers work.
test_that("package basics work", {
  expect_equal(trim("  hi  "), "hi")
  expect_true("Z" %ni% letters)
  expect_false("z" %ni% letters)
  expect_equal(titlecase_one("adrian"), "Adrian")
})
