#!/usr/bin/env Rscript
# Dev test runner: load the package from source and run the testthat suite.
# Usage: Rscript dev/run-tests.R [optional-filter]
#
# Uses the "silent" reporter and summarises results here so the run does not
# depend on the optional `praise` package (which testthat loads at random on
# success and which is not installed in this library).
suppressMessages(pkgload::load_all(".", quiet = TRUE, export_all = FALSE))
args <- commandArgs(trailingOnly = TRUE)

res <- testthat::test_dir(
  "tests/testthat",
  filter = if (length(args)) args[1] else NULL,
  reporter = "silent",
  stop_on_failure = FALSE
)

n_pass <- 0L; n_fail <- 0L; n_err <- 0L; n_skip <- 0L; n_warn <- 0L
for (test in res) {
  for (exp in test$results) {
    if (inherits(exp, "expectation_success"))      n_pass <- n_pass + 1L
    else if (inherits(exp, "expectation_failure"))  { n_fail <- n_fail + 1L; bad <- TRUE }
    else if (inherits(exp, "expectation_error"))    { n_err  <- n_err  + 1L; bad <- TRUE }
    else if (inherits(exp, "expectation_skip"))     n_skip <- n_skip + 1L
    else if (inherits(exp, "expectation_warning"))  n_warn <- n_warn + 1L
    else next
    if (inherits(exp, c("expectation_failure", "expectation_error"))) {
      cat(sprintf("\nFAIL [%s] %s\n  %s\n", test$file, test$test,
                  gsub("\n", "\n  ", conditionMessage(exp))))
    }
  }
}

cat(sprintf("\nPASS %d | FAIL %d | ERROR %d | SKIP %d | WARN %d\n",
            n_pass, n_fail, n_err, n_skip, n_warn))
if (n_fail + n_err > 0L) quit(status = 1L, save = "no")
