test_that("approved dependency and candidate preserve epidemiological outputs", {
  withr::local_options(list(lifecycle_verbosity = "quiet"))
  for (scenario in c("dengue", "chikungunya", "zika", "missing-climate")) {
    report <- compare_epidemiological_fixture(.reference_root, scenario)
    expect_true(all(report$status == "match"), info = scenario)
    expect_equal(sum(report$differences), 0, info = scenario)
  }
})

test_that("comparison fails on numerical, missingness and identity changes", {
  data <- data.frame(SE = c(202101, 202102), casos = c(1, 3), Rt = c(NA, 1.2),
                      id = c("a", "b"))
  changed <- data
  changed$casos[2] <- 4
  changed$Rt[1] <- 0
  changed$id[1] <- "different"
  report <- compare_epidemiological_frames(data, changed)
  expect_setequal(report$metric[report$status == "mismatch"], c("casos", "Rt", "id"))
  expect_error(compare_epidemiological_frames(data, changed[-1, ]), "cardinality")
})
