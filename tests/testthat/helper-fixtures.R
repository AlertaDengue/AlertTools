.fixture_directory <- normalizePath(testthat::test_path("fixtures"), mustWork = TRUE)

fixture_path <- function(name) {
  file.path(.fixture_directory, name)
}

read_fixture <- function(name, ...) {
  utils::read.csv(fixture_path(name), na.strings = "NA", ...)
}

expect_cases_contract <- function(observed, expected) {
  testthat::expect_s3_class(observed, "data.frame")
  testthat::expect_named(
    observed,
    c("SE", "cidade", "CID10", "casos", "cas_prov", "cas_lab",
      "localidade", "nome", "pop")
  )
  testthat::expect_equal(nrow(observed), nrow(expected))
  testthat::expect_false(anyDuplicated(observed[c("SE", "cidade", "CID10")]) > 0)
  testthat::expect_true(all(vapply(
    observed[c("SE", "cidade", "casos", "cas_prov", "cas_lab", "localidade", "pop")],
    is.numeric,
    logical(1)
  )))
  testthat::expect_type(observed$CID10, "character")
  testthat::expect_type(observed$nome, "character")
  testthat::expect_false(anyNA(observed[c("SE", "cidade", "CID10", "casos",
                                         "cas_prov", "cas_lab", "localidade")]))
  testthat::expect_equal(observed$SE, sort(observed$SE))
  testthat::expect_equal(lapply(observed, unname), lapply(expected, unname))
}
