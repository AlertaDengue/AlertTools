test_that("real INLA agrees with pinned calculations at the historical fitting budget", {
  inla_test_protocol()
  source(file.path(.inla_root, "tests/reference/inla-reference-validation.R"), local = TRUE)
  fixture <- inla_validation_fixture(.inla_root, cities = 3304557)
  # The immutable historical fit requests four threads. Compare at that budget;
  # changing fitting threads is separately measured, not assumed bit-identical.
  candidate <- run_inla_fixture(fixture, threads = 4L)
  reference <- inla_reference_sections(.inla_root, fixture)
  report <- compare_inla_sections(reference, inla_result_sections(candidate))
  expect_true(all(report$status == "match"))
  directory <- Sys.getenv("ALERTTOOLS_INLA_EVIDENCE")
  if (nzchar(directory)) {
    utils::write.csv(report, file.path(directory, "reference-comparison.csv"), row.names = FALSE)
    saveRDS(list(fixture = fixture, reference = reference, candidate = candidate),
              file.path(directory, "reference-comparison.rds"))
  }
})
