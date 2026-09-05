test_that("getCases has a deterministic SQLite integration contract", {
  withr::local_options(list(lifecycle_verbosity = "quiet"))
  withr::local_dir(withr::local_tempdir())
  connection <- sqlite_fixture_connection(environment())
  expected <- read_fixture("expected-fetch-cases.csv", stringsAsFactors = FALSE)

  observed <- getCases(
    cities = 3304557,
    firstday = as.Date("2020-12-20"),
    lastday = as.Date("2021-01-09"),
    cid10 = "A90",
    dataini = "notific",
    datasource = connection
  )

  expect_cases_contract(observed, expected)
  expect_false(file.exists("caselist.RData"))
  expect_s3_class(attr(observed, "case_records"), "data.frame")
  expect_equal(nrow(attr(observed, "case_records")), 4)
})

test_that("getCases reproduces its contract on ephemeral PostgreSQL", {
  withr::local_options(list(lifecycle_verbosity = "quiet"))
  withr::local_dir(withr::local_tempdir())
  connection <- postgres_fixture_connection(environment())
  expected <- read_fixture("expected-fetch-cases.csv", stringsAsFactors = FALSE)

  observed <- getCases(
    cities = 3304557,
    firstday = as.Date("2020-12-20"),
    lastday = as.Date("2021-01-09"),
    cid10 = "A90",
    dataini = "notific",
    datasource = connection
  )

  expect_cases_contract(observed, expected)
})

test_that("getCases rejects invalid user input before querying", {
  withr::local_options(list(lifecycle_verbosity = "quiet"))
  connection <- sqlite_fixture_connection(environment())

  expect_error(
    getCases(cities = "3304557", datasource = connection),
    "cities should be a vector of numeric geocodes"
  )
  expect_error(
    getCases(cities = 3304557, dataini = "invalid", datasource = connection),
    "dataini should"
  )
  expect_error(
    getCases(cities = 3304557, cid10 = "B00", datasource = connection),
    "Unknown CID-10"
  )
})
