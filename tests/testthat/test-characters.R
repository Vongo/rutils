# ---- cry / decry ---------------------------------------------------------
test_that("decry inverts cry in the natural composed form (was broken)", {
  for (k in c(1, 42, 999, 1234)) {
    expect_equal(decry(cry("Hello World 123", k), k), "Hello World 123")
  }
})

test_that("cry/decry also round-trip via an intermediate variable", {
  a <- cry("Love is in the air", 1234)
  expect_equal(decry(a, 1234), "Love is in the air")
  expect_false(identical(a, "Love is in the air"))
})

test_that("cry is deterministic for a given key", {
  expect_equal(cry("abcDEF 123", 5), cry("abcDEF 123", 5))
})

test_that("cry does not clobber the global RNG stream", {
  set.seed(123); before <- runif(2)
  set.seed(123); first <- runif(1)
  invisible(cry("secret message", 7))
  after <- runif(1)
  expect_equal(c(first, after), before)
})

# ---- slug ----------------------------------------------------------------
test_that("slug normalises carets and other non-alphanumerics", {
  expect_equal(slug("a^b^c"), "a-b-c")
  expect_equal(slug("La magie d'Aladin"), "la-magie-d-aladin")
})

# ---- trim ----------------------------------------------------------------
test_that("trim strips non-breaking spaces and tabs, not just ASCII spaces", {
  nbsp <- " "
  expect_equal(trim(paste0(nbsp, nbsp, "hello", nbsp)), "hello")
  expect_equal(trim("\t lorem ipsum \t"), "lorem ipsum")
})

# ---- titlecase_one -------------------------------------------------------
test_that("titlecase_one returns NA for NA input, not 'NANA'", {
  expect_true(is.na(titlecase_one(NA)))
  expect_equal(titlecase_one(c("aDRIAN", NA, "bOb")), c("Adrian", NA, "Bob"))
})

# ---- fetch_safe ----------------------------------------------------------
test_that("fetch_safe warns and returns NULL instead of failing silently (logger=NULL)", {
  expect_warning(res <- fetch_safe("http://example.com", max_attempts = 0), regexp = "Failed to fetch")
  expect_null(res)
})

test_that("fetch_safe surfaces transport errors rather than returning NULL silently", {
  expect_warning(res <- fetch_safe("http://localhost:1/", max_attempts = 1, backoff = 0))
  expect_null(res)
})
