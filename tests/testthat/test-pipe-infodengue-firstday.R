test_that("pipe_infodengue preserves firstday defaults and historical overrides", {
  withr::local_options(list(lifecycle_verbosity = "quiet"))
  expect_identical(eval(formals(pipe_infodengue)$firstday), as.Date("2018-01-01"))
  connection <- sqlite_fixture_connection(environment())
  calls <- list()
  climate_starts <- list()
  local_mocked_bindings(
    fetch_alert_parameters = function(conn, geocodes, disease) {
      data.frame(municipio_geocodigo = geocodes)
    },
    fetch_climate = function(conn, geocodes, climate_vars, start_date, end_date) {
      climate_starts[[length(climate_starts) + 1L]] <<- start_date
      data.frame()
    },
    fetch_cases = function(conn, geocodes, disease, start_date, end_date,
                           case_date, complete_tail, verbose) {
      calls[[length(calls) + 1L]] <<- list(
        conn = conn, geocodes = geocodes, disease = disease,
        start_date = start_date, end_date = end_date, case_date = case_date,
        complete_tail = complete_tail
      )
      stop("captured fetch_cases call")
    },
    .package = "AlertTools"
  )

  report_end <- as.Date("2026-08-01")
  expect_error(pipe_infodengue(2201919, finalday = report_end,
                               datasource = connection), "captured fetch_cases call")
  expect_identical(calls[[1L]]$start_date, as.Date("2018-01-01"))
  expect_identical(calls[[1L]]$end_date, report_end)
  expect_identical(calls[[1L]]$conn, connection)

  historical_start <- as.Date("2010-01-03")
  expect_error(pipe_infodengue(2201919, finalday = report_end,
                               datasource = connection, firstday = historical_start),
               "captured fetch_cases call")
  expect_identical(calls[[2L]]$start_date, historical_start)
  expect_identical(calls[[2L]]$geocodes, 2201919)
  expect_identical(calls[[2L]]$disease, "A90")
  expect_identical(calls[[2L]]$case_date, "notification")

  # Preserve the positional arguments of both master and the release candidate.
  expect_error(pipe_infodengue(2201919, "A90", 202630, report_end,
                               201001, "none", NULL, FALSE, connection, NA,
                               "notific", 1L, NULL, FALSE, report_end),
               "captured fetch_cases call")
  expect_identical(calls[[3L]]$start_date, as.Date("2018-01-01"))
  expect_identical(calls[[3L]]$end_date, epiweek_start(202630) + 6L)
  expect_identical(climate_starts, rep(list(epiweek_start(201001)), 3L))
})
