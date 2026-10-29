test_that("legacy warnings start at 1.3.0 and execute the lifecycle emitter", {
  withr::local_options(lifecycle_verbosity = "warning")
  for (version in c("1.2.0", "1.2.9", "1.3.0", "1.4.0")) {
    local_mocked_bindings(.alerttools_version = function() package_version(version))
    invoke <- function() Rt(read_fixture("cases.csv"), count = "casos",
                           gtdist = "normal", meangt = 3, sdgt = 1)
    if (package_version(version) < package_version("1.3.0")) {
      expect_no_warning(invoke())
    } else {
      expect_warning(invoke(), "deprecated")
    }
  }
})
