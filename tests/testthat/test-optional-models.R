test_that("MEM integration is available when its optional dependency is installed", {
  skip_if_not_installed("mem")
  expect_true(requireNamespace("mem", quietly = TRUE))
  expect_true(is.function(mem::memmodel))
})

test_that("INLA integration is available when its optional dependency is installed", {
  skip_if_not_installed("INLA")
  expect_true(requireNamespace("INLA", quietly = TRUE))
  expect_true(is.function(INLA::inla))
})
