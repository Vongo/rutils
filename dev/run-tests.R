#!/usr/bin/env Rscript
# Dev test runner: load the package from source and run the testthat suite.
# Usage: Rscript dev/run-tests.R [optional-filter]
suppressMessages(pkgload::load_all(".", quiet = TRUE, export_all = FALSE))
args <- commandArgs(trailingOnly = TRUE)
reporter <- testthat::SummaryReporter$new()
res <- testthat::test_dir(
  "tests/testthat",
  filter = if (length(args)) args[1] else NULL,
  reporter = reporter,
  stop_on_failure = TRUE
)
