test_that("Rt numerical regression baseline is preserved", {
  withr::local_options(list(lifecycle_verbosity = "quiet"))
  expected <- read_fixture("expected-rt.csv")
  observed <- Rt(
    expected[c("SE", "casos")],
    count = "casos",
    gtdist = "normal",
    meangt = 3,
    sdgt = 1
  )

  expect_named(observed, names(expected))
  expect_equal(nrow(observed), nrow(expected))
  expect_equal(observed$SE, expected$SE)
  expect_equal(observed$casos, expected$casos)
  expect_equal(observed$Rt, expected$Rt, tolerance = 1e-7)
  expect_equal(observed$lwr, expected$lwr, tolerance = 1e-7)
  expect_equal(observed$upr, expected$upr, tolerance = 1e-7)
  expect_equal(observed$p1, expected$p1, tolerance = 1e-7)
})
