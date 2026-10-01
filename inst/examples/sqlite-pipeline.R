library(AlertTools)

if (!requireNamespace("RSQLite", quietly = TRUE)) {
  stop("Install RSQLite to run this example.")
}

conn <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")

weeks <- epiweek_seq(202501, 202512)
dates <- epiweek_start(weeks)
case_counts <- c(0, 1, 2, 4, 8, 13, 18, 15, 9, 5, 2, 1)
notification_dates <- rep(dates, case_counts)

notifications <- data.frame(
  municipio_geocodigo = 3304557,
  cid10_codigo = "A90",
  dt_notific = notification_dates,
  dt_sin_pri = notification_dates,
  dt_digita = notification_dates,
  ano_notif = as.integer(format(notification_dates, "%Y")),
  se_notif = as_epiweek(notification_dates) %% 100,
  classi_fin = 1,
  criterio = 1
)
date_columns <- c("dt_notific", "dt_sin_pri", "dt_digita")
notifications[date_columns] <- lapply(
  notifications[date_columns], as.numeric
)

municipality <- data.frame(
  geocodigo = 3304557,
  nome = "Município sintético",
  populacao = 100000
)

climate <- data.frame(
  geocode = 3304557,
  date = as.numeric(dates),
  temp_min = c(20, 20, 21, 22, 23, 24, 24, 23, 22, 21, 20, 20),
  temp_med = c(25, 25, 26, 27, 28, 29, 29, 28, 27, 26, 25, 25),
  temp_max = c(30, 30, 31, 32, 33, 34, 34, 33, 32, 31, 30, 30),
  umid_min = 45,
  umid_med = 65,
  umid_max = 85
)

parameters_table <- data.frame(
  municipio_geocodigo = 3304557,
  limiar_preseason = 10,
  limiar_posseason = 5,
  limiar_epidemico = 100,
  varcli = "temp_min",
  clicrit = 22,
  varcli2 = NA_character_,
  clicrit2 = NA_real_,
  cid10 = "A90",
  codmodelo = "Af"
)

DBI::dbWriteTable(conn, "Notificacao", notifications)
DBI::dbWriteTable(conn, "Municipio", municipality)
DBI::dbWriteTable(conn, "copernicus_bra", climate)
DBI::dbWriteTable(conn, "parameters", parameters_table)

inputs <- fetch_alert_inputs(
  conn = conn,
  geocodes = 3304557,
  disease = "dengue",
  start_week = 202501,
  report_week = 202512
)
parameters <- fetch_alert_parameters(
  conn = conn,
  geocodes = 3304557,
  disease = "dengue"
)
result <- run_alert_pipeline(inputs, parameters, nowcast = "none")

DBI::dbDisconnect(conn)

stopifnot(
  inherits(inputs, "alerttools_inputs"),
  inherits(result, "alerttools_result"),
  nrow(result$data) == length(weeks)
)
