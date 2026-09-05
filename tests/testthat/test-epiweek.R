test_that("calendar behavior is preserved across epidemiological year boundary", {
  expected <- read_fixture("expected-epiweeks.csv", colClasses = "character")
  dates <- as.Date(expected$date)
  weeks <- as.numeric(expected$SE)

  expect_equal(vapply(dates, episem, numeric(1)), weeks)
  expect_equal(data2SE(expected$date, format = "%Y-%m-%d"), weeks)

  starts <- SE2date(unique(weeks))
  expected_starts <- expected[!duplicated(weeks), c("SE", "start_date")]
  expect_equal(starts$SE, as.numeric(expected_starts$SE))
  expect_equal(as.character(starts$ini), expected_starts$start_date)
  expect_equal(seqSE(202052, 202101)$SE, c(202052, 202053, 202101))
})

test_that("epidemiological calendar functions preserve types and boundaries", {
  expect_equal(epiYear(202010)$eyear, 2019)
  expect_equal(epiYear(202041)$eyear, 2020)
  expect_equal(epiYear(202030, cut = 31)$eyear, 2019)
  expect_equal(epiYear(202031, cut = 31)$eyear, 2020)

  converted <- data2SE(c("03-04-2013", "07-01-2019", "01-03-1998"),
                       format = "%d-%m-%Y")
  expect_type(converted, "double")
  expect_true(all(nchar(as.character(converted)) == 6))
  expect_equal(data2SE("03-01-1998", format = "%d-%m-%Y"), 199753)

  expect_equal(episem(as.Date("2015-01-01")), 201453)
  expect_equal(episem(as.Date("2015-01-01"), separa = "-"), "2014-53")
  expect_equal(episem(as.Date("2015-01-01"), retorna = "Y"), 2014)
  expect_equal(episem(as.Date("2015-01-01"), retorna = "W"), 53)
  expect_true(is.na(episem(NA)))
  expect_equal(episem(as.Date("2017-01-01")), 201701)
  expect_equal(episem(as.Date("2016-01-01")), 201552)
  expect_equal(vapply(2020:2022, lastepiweek, numeric(1)), c(53, 52, 52))
})

test_that("date conversion results retain their tabular contract", {
  starts <- SE2date(c(202001, 202002))
  expect_s3_class(starts, "data.frame")
  expect_named(starts, c("SE", "ini"))
  expect_equal(nrow(starts), 2)
  expect_equal(starts$SE, c(202001, 202002))
  expect_equal(as.character(starts$ini[1]), "2019-12-29")

  days <- daySEday(c("2015-12-23", "2015-10-23", "2022-10-16"))
  expect_named(days, c("SE", "ini"))
  expect_equal(nrow(days), 3)
  expect_equal(days$SE[1], 201551)
  expect_equal(as.character(days$ini[1]), "2015-12-23")
  expect_equal(as.character(daySEday(202001)$ini), "2019-12-29")
})

test_that("algorithmic calendar is equivalent to the legacy SE table", {
  legacy <- AlertTools:::SE
  valid <- as.integer(format(legacy$Inicio, "%w")) == 0L &
    as.numeric(legacy$Termino - legacy$Inicio) == 6
  expect_equal(which(!valid), which(legacy$SE == 201815))
  expect_equal(as.numeric(as_epiweek(legacy$Inicio[valid])), legacy$SE[valid])
  expect_equal(epiweek_start(legacy$SE[valid]), legacy$Inicio[valid])
  expect_equal(epiweek_start(legacy$SE[valid]) + 6L, legacy$Termino[valid])
  expect_equal(epiweek_start(201815), as.Date("2018-04-08"))
})

test_that("algorithmic calendar has no fixed annual boundary", {
  weeks <- epiweek_seq(190052, 210101)
  starts <- epiweek_start(weeks)

  expect_gt(length(weeks), 10000)
  expect_equal(as.numeric(diff(starts)), rep(7, length(starts) - 1L))
  expect_equal(as_epiweek(starts), weeks)
  expect_equal(as_epiweek(starts + 6L), weeks)
  future <- as.Date("2150-07-14")
  future_start <- epiweek_start(as_epiweek(future))
  expect_true(future_start <= future && future <= future_start + 6L)
})

test_that("epidemiological week validation rejects impossible values", {
  expect_error(epiweek_start(202054), "Invalid epidemiological week")
  expect_error(epiweek_seq(202101, 202053), "must not be after")
  expect_error(epiweek_start(202101.5), "finite integers")
  expect_true(is.na(as_epiweek(as.Date(NA))))
  expect_true(is.na(epiweek_start(NA_real_)))
})
