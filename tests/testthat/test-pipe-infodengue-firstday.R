test_that("pipe_infodengue keeps the source default and forwards an override", {
  expect_identical(eval(formals(pipe_infodengue)$firstday), as.Date("2018-01-01"))

  calls <- list()
  local_mocked_bindings(
    read.parameters = function(cities, cid10) {
      data.frame(municipio_geocodigo = cities)
    },
    getClima = function(...) data.frame(),
    getCases = function(cities, lastday, firstday, cid10, type,
                        dataini, completetail) {
      calls[[length(calls) + 1L]] <<- list(
        cities = cities, lastday = lastday, firstday = firstday,
        cid10 = cid10, type = type, dataini = dataini,
        completetail = completetail
      )
      stop("captured getCases call")
    },
    .package = "AlertTools"
  )

  report_end <- as.Date("2026-08-01")
  expect_error(pipe_infodengue(2201919, finalday = report_end),
               "captured getCases call")
  expect_identical(calls[[1L]]$firstday, as.Date("2018-01-01"))
  expect_identical(calls[[1L]]$lastday, report_end)

  historical_start <- as.Date("2010-01-03")
  expect_error(pipe_infodengue(2201919, finalday = report_end,
                               firstday = historical_start),
               "captured getCases call")
  expect_identical(calls[[2L]]$firstday, historical_start)
  expect_identical(calls[[2L]]$cid10, "A90")
  expect_identical(calls[[2L]]$type, "all")

  # The original positional arguments still end at dataini.
  expect_error(pipe_infodengue(2201919, "A90", 202630, report_end,
                               201001, "none", NULL, FALSE, NULL, NA,
                               "notific"), "captured getCases call")
  expect_identical(calls[[3L]]$firstday, as.Date("2018-01-01"))
})
