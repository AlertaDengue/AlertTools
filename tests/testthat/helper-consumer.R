consumer_fixture_database <- function(backend, test_env = parent.frame()) {
  conn <- if (backend == "postgres") postgres_fixture_connection(test_env) else
    sqlite_fixture_connection(test_env)
  if (backend == "postgres") {
    DBI::dbExecute(conn, 'CREATE SCHEMA "weather"')
    withr::defer(DBI::dbExecute(conn, 'DROP SCHEMA "weather" CASCADE'), envir = test_env)
  }
  codes <- c(3304557, 3303302)
  combine <- function(factory, column) do.call(rbind, lapply(codes, function(code) {
    value <- factory()
    value[[column]] <- code
    value
  }))
  notifications <- combine(notification_fixture, "municipio_geocodigo")
  notifications <- do.call(rbind, lapply(c("A90", "A92.0", "A92.8"), function(disease) {
    value <- notifications
    value$cid10_codigo <- disease
    value
  }))
  climate <- combine(municipal_climate_fixture, "geocode")
  if (backend == "sqlite") {
    notifications[c("dt_notific", "dt_sin_pri", "dt_digita")] <-
      lapply(notifications[c("dt_notific", "dt_sin_pri", "dt_digita")], as.numeric)
    climate$date <- as.numeric(climate$date)
  }
  parameters <- combine(parameter_fixture, "municipio_geocodigo")
  chik <- parameters
  chik$cid10 <- "A92.0"
  tables <- list(notifications = notifications, municipal_climate = climate,
                 municipalities = combine(municipality_fixture, "geocodigo"),
                 parameters = rbind(parameters, chik))
  for (key in names(tables)) {
    DBI::dbWriteTable(conn, consumer_table_id(conn, key), tables[[key]], overwrite = TRUE)
  }
  conn
}

consumer_table_id <- function(conn, key) {
  backend <- AlertTools:::.db_backend(conn)
  location <- AlertTools:::.db_table_config[[key]][[backend]]
  if (backend == "postgres") DBI::Id(schema = location[[1]], table = location[[2]]) else
    DBI::Id(table = location[[1]])
}

load_pinned_consumer <- function(conn, reference = FALSE, fail_state = FALSE,
                                  failed_cities = numeric()) {
  directory <- file.path(.reference_root, "tests/reference/consumer-27a03f26")
  manifest <- utils::read.csv(file.path(directory, "manifest.csv"))
  stopifnot(identical(reference_snapshot_md5(file.path(directory, manifest$file)),
                      manifest$md5))
  consumer <- new.env(parent = asNamespace("AlertTools"))
  sys.source(file.path(directory, "functions.R"), consumer)
  consumer$events <- character()
  consumer$log_msg <- function(..., level = "INFO") {
    consumer$events <- c(consumer$events, paste0(level, ": ", paste0(..., collapse = "")))
    invisible(NULL)
  }
  consumer$report_end_date <- as.Date("2021-02-13")
  consumer$report_epiweek <- 202106
  consumer$window_weeks <- 52
  inject_fault <- function(cities) {
    if ((fail_state && length(cities) > 1L) || any(cities %in% failed_cities)) {
      stop("Injected consumer recovery test failure.")
    }
  }
  if (!reference) {
    consumer$pipe_infodengue <- function(cities, ...) {
      inject_fault(cities)
      AlertTools::pipe_infodengue(cities, ...)
    }
  } else {
    parameters <- AlertTools:::.repo_parameters(conn, c(3304557, 3303302))
    legacy <- load_epidemiological_reference(.reference_root, parameters)
    consumer$tabela_historico <- legacy$tabela_historico
    consumer$write_alerta <- legacy$write_alerta
    consumer$pipe_infodengue <- function(cities, cid10, nowcasting, finalday,
                                         narule, iniSE, dataini, completetail) {
      inject_fault(cities)
      # Controlled DB boundary: both versions read the same snapshot through DBI.
      # Historical calculation, conversion and SQL writer bodies are unchanged.
      parameters <- AlertTools:::.fetch_alert_parameters_impl(cities, cid10, conn)
      cases <- AlertTools::fetch_cases(conn, cities, cid10, as.Date("2018-01-01"),
                                       finalday, "symptom_onset", completetail)
      climate <- AlertTools::fetch_climate(conn, cities,
                                           AlertTools::epiweek_start(iniSE), finalday)
      input <- dplyr::left_join(cases, climate,
                                 by = c("cidade" = "geocodigo", "SE" = "SE"))
      input$tweet <- NA_real_
      input$inc <- input$casos / input$pop * 1e5
      values <- lapply(cities, function(city) suppressMessages(legacy$calc.alerta(
        city, list(input, AlertTools::as_epiweek(finalday), nowcasting, parameters, NULL)
      )))
      names(values) <- cities
      values
    }
  }
  consumer
}

consumer_known_warnings <- function(code) {
  withCallingHandlers(code, warning = function(condition) {
    if (grepl("Using dengue parameters", conditionMessage(condition), fixed = TRUE)) {
      invokeRestart("muffleWarning")
    }
  })
}

consumer_physical_history <- function(history) {
  value <- history[setdiff(names(history), "CID10")]
  names(value) <- sub("^(temp|umid)_", "\\1", names(value))
  rownames(value) <- NULL
  value
}

consumer_history_table <- function(conn, history, key) {
  id <- consumer_table_id(conn, key)
  if (!DBI::dbExistsTable(conn, id)) {
    DBI::dbWriteTable(conn, id, consumer_physical_history(history)[0, ])
    keys <- paste(DBI::dbQuoteIdentifier(conn,
                  c("SE", "municipio_geocodigo", "Localidade_id")), collapse = ", ")
    backend <- AlertTools:::.db_backend(conn)
    suffix <- switch(key, alert_dengue = "", alert_chik = "_chik", alert_zika = "_zika")
    name <- as.character(DBI::dbQuoteIdentifier(conn, paste0("alertas_unicos", suffix)))
    table <- as.character(DBI::dbQuoteIdentifier(conn, id))
    command <- if (backend == "postgres") paste("ALTER TABLE", table,
      "ADD CONSTRAINT", name, "UNIQUE (", keys, ")") else
      paste("CREATE UNIQUE INDEX", name, "ON", table, "(", keys, ")")
    DBI::dbExecute(conn, command)
  }
  DBI::dbExecute(conn, paste("DELETE FROM", DBI::dbQuoteIdentifier(conn, id)))
  id
}

consumer_ordered_rows <- function(value) {
  value <- value[order(value$municipio_geocodigo, value$SE), , drop = FALSE]
  # SQLite stores DBI dates as numeric days; PostgreSQL returns Date.
  if (is.numeric(value$data_iniSE)) value$data_iniSE <- as.Date(value$data_iniSE,
                                                               origin = "1970-01-01")
  rownames(value) <- NULL
  value
}

consumer_publication <- function(consumer, result, directory, disease) {
  dir.create(directory, recursive = TRUE, showWarnings = FALSE)
  res <- list()
  suffix <- switch(disease, A90 = "den", A92.0 = "chik", A92.8 = "zika")
  res[[paste0("ale.", suffix)]] <- result$ale
  res[[paste0("restab.", suffix)]] <- result$restab
  state_file <- file.path(directory, "state.RData")
  save(res, file = state_file)
  decoded <- consumer$load_state_result(state_file)
  consumer$sql_dir <- directory
  consumer$restab_den <- if (disease == "A90") result$restab else NULL
  consumer$restab_chik <- if (disease == "A92.0") result$restab else NULL
  consumer$restab_zika <- if (disease == "A92.8") result$restab else NULL
  suppressMessages(sys.source(file.path(.reference_root,
                      "tests/reference/consumer-27a03f26/publication.R"), consumer))
  sql <- file.path(directory, switch(disease, A90 = "output_dengue.sql",
                                     A92.0 = "output_chik.sql", A92.8 = "output_zika.sql"))
  list(state = state_file, sql = sql, decoded = decoded,
       history = decoded[[paste0("restab.", suffix)]],
       data = consumer$normalize_ale(decoded[[paste0("ale.", suffix)]]))
}

record_consumer_evidence <- function(backend, name, report, candidate, reference) {
  output <- Sys.getenv("ALERTTOOLS_CONSUMER_EVIDENCE", "")
  if (!nzchar(output)) return(invisible(NULL))
  output <- file.path(output, backend, name)
  dir.create(output, recursive = TRUE, showWarnings = FALSE)
  utils::write.csv(report, file.path(output, "comparison.csv"), row.names = FALSE)
  file.copy(candidate$state, file.path(output, "candidate.RData"), overwrite = TRUE)
  file.copy(reference$state, file.path(output, "reference.RData"), overwrite = TRUE)
  file.copy(candidate$sql, file.path(output, "candidate.sql"), overwrite = TRUE)
  file.copy(reference$sql, file.path(output, "reference.sql"), overwrite = TRUE)
  invisible(NULL)
}
