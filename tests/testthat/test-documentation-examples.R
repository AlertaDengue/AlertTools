test_that("published in-memory example runs in a clean environment", {
  example_file <- system.file(
    "examples", "in-memory-pipeline.R", package = "AlertTools"
  )
  expect_true(nzchar(example_file))
  example_env <- new.env(parent = globalenv())
  expect_silent(sys.source(example_file, envir = example_env))
  expect_s3_class(example_env$inputs, "alerttools_inputs")
  expect_s3_class(example_env$result, "alerttools_result")
})

test_that("published SQLite example runs without credentials or production", {
  skip_if_not_installed("RSQLite")
  example_file <- system.file(
    "examples", "sqlite-pipeline.R", package = "AlertTools"
  )
  expect_true(nzchar(example_file))
  example_env <- new.env(parent = globalenv())
  expect_silent(sys.source(example_file, envir = example_env))
  expect_s3_class(example_env$inputs, "alerttools_inputs")
  expect_s3_class(example_env$result, "alerttools_result")
})
