.inla_root <- normalizePath(testthat::test_path("..", ".."), mustWork = TRUE)
source(file.path(.inla_root, "tests/reference/frame-comparison.R"), local = TRUE)

inla_test_protocol <- function() {
  testthat::skip_if_not(identical(Sys.getenv("ALERTTOOLS_TEST_INLA"), "true"),
               "set ALERTTOOLS_TEST_INLA=true for the dedicated real-engine suite")
  if (!requireNamespace("INLA", quietly = TRUE)) stop("INLA is mandatory in this suite")
  source(file.path(.inla_root, "tests/reference/inla-validation.R"), local = parent.frame())
}
