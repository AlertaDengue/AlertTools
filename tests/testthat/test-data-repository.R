test_that("repository-backed readers expose stable domain schemas", {
  withr::local_options(list(lifecycle_verbosity = "quiet"))
  connection <- sqlite_fixture_connection(environment())

  cities <- getCidades(uf = "Rio de Janeiro", datasource = connection)
  expect_named(cities, c("municipio_geocodigo", "cidade", "regional", "regional_id",
                         "macroregional", "macroregional_id", "uf"))
  expect_equal(cities$municipio_geocodigo, 3304557)
  expect_equal(getRegionais(uf = "Rio de Janeiro", datasource = connection),
               "Regional Fixture")
  expect_equal(getRegionais(uf = "Rio de Janeiro", macroreg = TRUE,
                            datasource = connection), "Macro Fixture")

  parameters <- read.parameters(3304557, datasource = connection)
  expect_equal(nrow(parameters), 1)
  expect_equal(parameters$codmodelo, "Af")
  expect_named(getWUstation(3304557, datasource = connection),
               c("municipio_geocodigo", "codigo_estacao_wu", "estacao_wu_sec"))
  regional_link <- AlertTools:::.repo_regional_health(connection, 3304557)
  expect_named(regional_link, c("id", "nome_regional", "municipio_geocodigo",
                                "codigo_estacao_wu", "estacao_wu_sec"))
})

test_that("bound values prevent SQL injection through repository filters", {
  withr::local_options(list(lifecycle_verbosity = "quiet"))
  connection <- sqlite_fixture_connection(environment())

  expect_error(
    getCidades(uf = "Rio de Janeiro' OR 1=1 --", datasource = connection),
    "found no city"
  )
  expect_error(
    read.parameters(cities = c(3304557, NA_real_), datasource = connection)
  )
  expect_equal(DBI::dbGetQuery(connection, "SELECT COUNT(*) AS n FROM Municipio")$n, 1)
})

test_that("case-record readers share date and disease normalization", {
  withr::local_options(list(lifecycle_verbosity = "quiet"))
  connection <- sqlite_fixture_connection(environment())

  records <- getCaseslist(
    3304557, firstday = as.Date("2020-12-20"), lastday = as.Date("2021-01-09"),
    datasource = connection
  )
  expect_equal(nrow(records), 4)
  expect_true(all(vapply(records[c("dt_notific", "dt_sin_pri", "dt_digita")],
                         inherits, logical(1), what = "Date")))

  delays <- getdelaydata(3304557, nyears = 1, lastday = as.Date("2021-01-09"),
                         datasource = connection)
  expect_named(delays, c("municipio_geocodigo", "dt_notific", "dt_sin_pri", "dt_digita"))
  expect_equal(nrow(delays), 4)

  weekly <- read.cases(2020, 2021, mun_list = 3304557, datasource = connection)
  expect_named(weekly, c("municipio_geocodigo", "SE", "casos"))
  expect_equal(sum(weekly$casos), 4)
  expect_false(anyDuplicated(weekly[c("municipio_geocodigo", "SE")]) > 0)
})

test_that("climate and tweet repositories filter dates and preserve ordering", {
  withr::local_options(list(lifecycle_verbosity = "quiet"))
  connection <- sqlite_fixture_connection(environment())

  climate <- getClima(
    3304557, vars = c("temp_min", "temp_max", "temp_med", "umid_min",
                      "umid_med", "umid_max"),
    iniSE = 202052, finalday = as.Date("2021-01-02"), datasource = connection
  )
  expect_named(climate, c("geocodigo", "SE", "temp_min", "temp_max", "temp_med",
                          "umid_min", "umid_med", "umid_max"))
  expect_equal(climate$SE, c(202052, 202053))
  expect_equal(climate$temp_min, c(21, 25))

  station <- getWU("TEST", vars = c("temp_min", "temp_med"), iniSE = 202052,
                   finalday = as.Date("2021-01-02"), datasource = connection)
  expect_equal(station$SE, c(202052, 202053))
  expect_equal(station$temp_min, c(21, 25))

  tweets <- getTweet(3304557, lastday = as.Date("2020-12-27"), datasource = connection)
  selected <- tweets[tweets$SE %in% c(202052, 202053), ]
  expect_equal(selected$tweet, c(2, 3))
})

test_that("parameter writes implement explicit conflict policies", {
  withr::local_options(list(lifecycle_verbosity = "quiet"))
  connection <- sqlite_fixture_connection(environment())
  original <- parameter_fixture()

  ignored <- write_parameters(3304557, "A90", transform(original, limiar_preseason = 20),
                              datasource = connection, conflict = "ignore")
  expect_equal(ignored$limiar_preseason, 10)

  updated <- write_parameters(3304557, "A90", transform(original, limiar_preseason = 20),
                              datasource = connection, conflict = "update")
  expect_equal(updated$limiar_preseason, 20)

  expect_error(
    write_parameters(3304557, "A90", original, datasource = connection, conflict = "error"),
    "UNIQUE constraint failed"
  )
  expect_equal(read.parameters(3304557, datasource = connection)$limiar_preseason, 20)
})

test_that("batch write rolls back every row after a conflict", {
  connection <- sqlite_fixture_connection(environment())
  rows <- rbind(
    transform(parameter_fixture(), municipio_geocodigo = 3550308, limiar_preseason = 30),
    transform(parameter_fixture(), municipio_geocodigo = 3304557, limiar_preseason = 40)
  )

  expect_error(
    AlertTools:::.repo_write_rows(
      connection, "parameters", rows,
      key = c("municipio_geocodigo", "cid10"), conflict = "error"
    ),
    "UNIQUE constraint failed"
  )
  observed <- DBI::dbGetQuery(connection, "SELECT * FROM parameters ORDER BY municipio_geocodigo")
  expect_equal(nrow(observed), 1)
  expect_equal(observed$limiar_preseason, 10)
})

test_that("station association updates are batched and transactional", {
  connection <- sqlite_fixture_connection(environment())
  update <- data.frame(
    municipio_geocodigo = 3304557,
    primary_station = "NEW1",
    secondary_station = "NEW2"
  )

  expect_invisible(setWUstation(update, UF = "Rio de Janeiro", datasource = connection))
  observed <- getWUstation(3304557, datasource = connection)
  expect_equal(observed$codigo_estacao_wu, "NEW1")
  expect_equal(observed$estacao_wu_sec, "NEW2")
})

test_that("alert history writes are batched, idempotent and conflict-aware", {
  withr::local_options(list(lifecycle_verbosity = "quiet"))
  connection <- sqlite_fixture_connection(environment())
  DBI::dbExecute(connection, paste(
    "CREATE TABLE Historico_alerta (",
    '"SE" INTEGER, "data_iniSE" TEXT, casos_est REAL, casos_est_min REAL,',
    'casos_est_max REAL, casos REAL, casprov REAL, municipio_geocodigo INTEGER,',
    'p_rt1 REAL, p_inc100k REAL, "Localidade_id" INTEGER, nivel INTEGER, id TEXT,',
    'versao_modelo TEXT, municipio_nome TEXT, tweet REAL, "Rt" REAL, pop REAL,',
    'tempmin REAL, tempmed REAL, tempmax REAL, umidmin REAL, umidmed REAL,',
    'umidmax REAL, receptivo INTEGER, transmissao INTEGER, nivel_inc INTEGER,',
    'UNIQUE ("SE", municipio_geocodigo, "Localidade_id"))'
  ))
  history <- data.frame(
    SE = c(202052, 202053), data_iniSE = as.Date(c("2020-12-20", "2020-12-27")),
    CID10 = "A90", casos_est = c(1, 2), casos_est_min = c(1, 2),
    casos_est_max = c(1, 2), casos = c(1, 2), casprov = c(1, 2),
    municipio_geocodigo = 3304557, p_rt1 = c(0.1, 0.2), p_inc100k = c(10, 20),
    Localidade_id = 0, nivel = c(1, 2), id = c("a", "b"),
    versao_modelo = "2026-09-04", municipio_nome = "Municipio Fixture",
    Rt = c(0.8, 1.2), pop = 10000, tweet = c(2, 3), receptivo = c(0, 1),
    transmissao = c(0, 1), nivel_inc = c(0, 1), temp_min = c(20, 24),
    temp_med = c(24, 28), temp_max = c(28, 32), umid_min = c(40, 44),
    umid_med = c(60, 64), umid_max = c(80, 84)
  )

  expect_invisible(write_alerta(history, datasource = connection, conflict = "update"))
  expect_equal(DBI::dbGetQuery(connection, "SELECT COUNT(*) AS n FROM Historico_alerta")$n, 2)

  changed <- transform(history, casos_est = casos_est + 10)
  expect_invisible(write_alerta(changed, datasource = connection, conflict = "update"))
  observed <- DBI::dbGetQuery(connection,
                              'SELECT casos_est FROM Historico_alerta ORDER BY "SE"')
  expect_equal(observed$casos_est, c(11, 12))
  expect_error(write_alerta(history, datasource = connection, conflict = "error"),
               "UNIQUE constraint failed")
  expect_equal(DBI::dbGetQuery(connection, "SELECT COUNT(*) AS n FROM Historico_alerta")$n, 2)
})

test_that("SQL file export quotes literals instead of interpolating user values", {
  withr::local_options(list(lifecycle_verbosity = "quiet"))
  path <- withr::local_tempfile(fileext = ".sql")
  history <- data.frame(
    SE = 202052, data_iniSE = as.Date("2020-12-20"), CID10 = "A90",
    casos_est = 1, casos_est_min = 1, casos_est_max = 1, casos = 1, casprov = 1,
    municipio_geocodigo = 3304557, p_rt1 = 0.1, p_inc100k = 10,
    Localidade_id = 0, nivel = 1, id = "a", versao_modelo = "2026-09-04",
    municipio_nome = "D'Angelo); DROP TABLE x; --", Rt = 0.8, pop = 10000,
    tweet = 2, receptivo = 0, transmissao = 0, nivel_inc = 0,
    temp_min = 20, temp_med = 24, temp_max = 28,
    umid_min = 40, umid_med = 60, umid_max = 80
  )

  expect_invisible(write_alerta(history, writetofile = TRUE, arq = path))
  sql <- paste(readLines(path, warn = FALSE), collapse = "\n")
  expect_match(sql, "D''Angelo", fixed = TRUE)
  expect_match(sql, "INSERT INTO \"Municipio\".\"Historico_alerta\"", fixed = TRUE)
})

test_that("database functions never fall back to a global con object", {
  withr::local_options(list(lifecycle_verbosity = "quiet"))
  connection <- sqlite_fixture_connection(environment())
  assign("con", connection, envir = .GlobalEnv)
  withr::defer(rm("con", envir = .GlobalEnv), envir = environment())

  database_functions <- list(getCases, getCaseslist, getClima, getWU, getTweet,
                             getCidades, getRegionais, read.parameters,
                             write_parameters, getWUstation, setWUstation,
                             read.cases, getdelaydata, pipe_infodengue,
                             write_alerta, applyGenTimeDist)
  expect_true(all(vapply(database_functions, function(fun) {
    identical(formals(fun)$datasource, quote(expr = ))
  }, logical(1))))
  expect_error(getCases(3304557),
               "valid DBI connection|datasource.*missing|argument.*datasource",
               ignore.case = TRUE)
})
