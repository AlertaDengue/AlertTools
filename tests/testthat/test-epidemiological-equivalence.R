test_that("reference integrity is portable across text line endings", {
  first <- withr::local_tempfile(fileext = ".R")
  second <- withr::local_tempfile(fileext = ".R")
  writeBin(charToRaw("value <- 1\n"), first)
  writeBin(charToRaw("value <- 1\r\n"), second)
  expect_identical(reference_snapshot_md5(first), reference_snapshot_md5(second))
  writeBin(charToRaw("value <- 2\r\n"), second)
  expect_false(identical(reference_snapshot_md5(first), reference_snapshot_md5(second)))
})

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
  expect_true(all(compare_epidemiological_frames(data, data[2:1, ])$status == "match"))
  expect_error(compare_epidemiological_frames(data, data[c(1, 1), ]), "keys.*unique")
})
