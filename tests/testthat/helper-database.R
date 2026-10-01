notification_fixture <- function() {
  data <- read_fixture("notifications.csv", stringsAsFactors = FALSE)
  date_columns <- c("dt_notific", "dt_sin_pri", "dt_digita")
  data[date_columns] <- lapply(data[date_columns], as.Date)
  data
}

municipality_fixture <- function() {
  data.frame(
    nome = "Municipio Fixture",
    populacao = 10000,
    geocodigo = 3304557,
    regional = "Regional Fixture",
    id_regional = 7,
    macroregional = "Macro Fixture",
    macroregional_id = 70,
    uf = "Rio de Janeiro",
    stringsAsFactors = FALSE
  )
}

parameter_fixture <- function() {
  data <- read_fixture("parameters.csv", stringsAsFactors = FALSE)
  data$codigo_estacao_wu <- "TEST"
  data$estacao_wu_sec <- "ALT1"
  data
}

station_climate_fixture <- function() {
  data.frame(
    Estacao_wu_estacao_id = rep("TEST", 4),
    data_dia = as.Date(c("2020-12-20", "2020-12-21", "2020-12-27", "2020-12-28")),
    temp_min = c(20, 22, 24, 26),
    temp_med = c(24, 25, 26, 27),
    stringsAsFactors = FALSE
  )
}

municipal_climate_fixture <- function() {
  data.frame(
    geocode = rep(3304557, 4),
    date = as.Date(c("2020-12-20", "2020-12-21", "2020-12-27", "2020-12-28")),
    temp_min = c(20, 22, 24, 26), temp_max = c(28, 30, 32, 34),
    temp_med = c(24, 26, 28, 30), umid_min = c(40, 42, 44, 46),
    umid_med = c(60, 62, 64, 66), umid_max = c(80, 82, 84, 86),
    precip_tot = c(0, 2, 4, 6), precip_max = c(0, 2, 4, 6)
  )
}

tweet_fixture <- function() {
  data.frame(
    Municipio_geocodigo = c(3304557, 3304557),
    data_dia = as.Date(c("2020-12-20", "2020-12-27")),
    numero = c(2, 3)
  )
}

sqlite_fixture_connection <- function(test_env = parent.frame()) {
  testthat::skip_if_not_installed("RSQLite")
  connection <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  withr::defer(DBI::dbDisconnect(connection), envir = test_env)

  notifications <- notification_fixture()
  date_columns <- c("dt_notific", "dt_sin_pri", "dt_digita")
  notifications[date_columns] <- lapply(
    notifications[date_columns],
    function(value) as.numeric(value)
  )

  DBI::dbWriteTable(connection, "Notificacao", notifications)
  DBI::dbWriteTable(connection, "Municipio", municipality_fixture())
  DBI::dbWriteTable(connection, "parameters", parameter_fixture())
  DBI::dbExecute(
    connection,
    "CREATE UNIQUE INDEX parameters_key ON parameters (municipio_geocodigo, cid10)"
  )

  station_climate <- station_climate_fixture()
  station_climate$data_dia <- as.numeric(station_climate$data_dia)
  DBI::dbWriteTable(connection, "wu", station_climate)

  municipal_climate <- municipal_climate_fixture()
  municipal_climate$date <- as.numeric(municipal_climate$date)
  DBI::dbWriteTable(connection, "copernicus_bra", municipal_climate)

  tweets <- tweet_fixture()
  tweets$data_dia <- as.numeric(tweets$data_dia)
  DBI::dbWriteTable(connection, "tweet", tweets)

  DBI::dbWriteTable(connection, "regional_saude", data.frame(
    id = 1, nome_regional = "Regional Fixture", municipio_geocodigo = 3304557,
    codigo_estacao_wu = "TEST", estacao_wu_sec = "ALT1"
  ))
  connection
}

create_alert_history_table <- function(connection) {
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
}

postgres_fixture_connection <- function(test_env = parent.frame()) {
  testthat::skip_if_not(
    identical(Sys.getenv("ALERTTOOLS_TEST_POSTGRES"), "true"),
    "set ALERTTOOLS_TEST_POSTGRES=true only for an ephemeral test database"
  )
  testthat::skip_if_not_installed("RPostgres")

  connection <- DBI::dbConnect(
    RPostgres::Postgres(),
    dbname = Sys.getenv("PGDATABASE", "alerttools_test"),
    host = Sys.getenv("PGHOST", "127.0.0.1"),
    port = as.integer(Sys.getenv("PGPORT", "5432")),
    user = Sys.getenv("PGUSER", "postgres"),
    password = Sys.getenv("PGPASSWORD", "postgres")
  )
  withr::defer(DBI::dbDisconnect(connection), envir = test_env)

  database_name <- DBI::dbGetInfo(connection)$dbname
  if (!identical(database_name, "alerttools_test")) {
    stop("PostgreSQL integration tests require the dedicated alerttools_test database.")
  }

  DBI::dbExecute(connection, 'DROP SCHEMA IF EXISTS "Municipio" CASCADE')
  DBI::dbExecute(connection, 'DROP SCHEMA IF EXISTS "Dengue_global" CASCADE')
  DBI::dbExecute(connection, 'CREATE SCHEMA "Municipio"')
  DBI::dbExecute(connection, 'CREATE SCHEMA "Dengue_global"')
  withr::defer(
    {
      DBI::dbExecute(connection, 'DROP SCHEMA IF EXISTS "Municipio" CASCADE')
      DBI::dbExecute(connection, 'DROP SCHEMA IF EXISTS "Dengue_global" CASCADE')
    },
    envir = test_env
  )

  DBI::dbWriteTable(
    connection,
    DBI::Id(schema = "Municipio", table = "Notificacao"),
    notification_fixture()
  )
  DBI::dbWriteTable(
    connection,
    DBI::Id(schema = "Dengue_global", table = "Municipio"),
    municipality_fixture()
  )
  connection
}
