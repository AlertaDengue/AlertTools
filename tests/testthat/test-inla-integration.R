test_that("real INLA preserves repeated and municipal-parallel results", {
  inla_test_protocol()
  fixture <- inla_validation_fixture(.inla_root)
  for (threads in c(1L, 2L)) {
    serial <- run_inla_fixture(fixture, threads = threads)
    concurrent <- run_inla_fixture(fixture, workers = 2L, threads = threads)
    report <- compare_inla_sections(inla_result_sections(serial),
                                     inla_result_sections(concurrent))
    expect_true(all(report$status == "match"), info = paste("threads", threads))
    if (threads == 1L) {
      repeated <- run_inla_fixture(fixture, threads = threads)
      expect_equal(repeated$data, serial$data, tolerance = 1e-7)
      expect_identical(repeated$alerts, serial$alerts)
    }
    directory <- Sys.getenv("ALERTTOOLS_INLA_EVIDENCE")
    if (nzchar(directory)) {
      utils::write.csv(report, file.path(directory, paste0("parallel-threads-", threads, ".csv")),
                         row.names = FALSE)
      saveRDS(list(fixture = fixture, serial = serial, concurrent = concurrent),
                file.path(directory, paste0("parallel-threads-", threads, ".rds")))
    }
  }
})

test_that("terminated municipal workers cannot be mistaken for complete output", {
  inla_test_protocol()
  if (.Platform$OS.type != "unix") stop("The dedicated process-termination scenario requires Unix")
  expect_warning(expect_error(AlertTools:::.map_workers(1:2, function(i) {
    if (i == 2L) tools::pskill(Sys.getpid(), signal = 9L)
    42
  }, workers = 2L), "did not return a result"), "did not deliver a result")
})
