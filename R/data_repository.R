# Internal database boundary -------------------------------------------------

.db_table_config <- list(
  notifications = list(postgres = c("Municipio", "Notificacao"), sqlite = "Notificacao"),
  municipalities = list(postgres = c("Dengue_global", "Municipio"), sqlite = "Municipio"),
  station_catalog = list(postgres = c("Municipio", "Estacao_wu"), sqlite = "estacao_wu"),
  station_climate = list(postgres = c("Municipio", "Clima_wu"), sqlite = "wu"),
  municipal_climate = list(postgres = c("weather", "copernicus_bra"), sqlite = "copernicus_bra"),
  tweets = list(postgres = c("Municipio", "Tweet"), sqlite = "tweet"),
  parameters = list(postgres = c("Dengue_global", "parameters"), sqlite = "parameters"),
  regional_health = list(postgres = c("Dengue_global", "regional_saude"), sqlite = "regional_saude"),
  alert_dengue = list(postgres = c("Municipio", "Historico_alerta"), sqlite = "Historico_alerta"),
  alert_chik = list(postgres = c("Municipio", "Historico_alerta_chik"), sqlite = "Historico_alerta_chik"),
  alert_zika = list(postgres = c("Municipio", "Historico_alerta_zika"), sqlite = "Historico_alerta_zika")
)

.db_validate_connection <- function(conn) {
  if (missing(conn) || is.null(conn) || !inherits(conn, "DBIConnection") ||
      !DBI::dbIsValid(conn)) {
    stop("A valid DBI connection must be supplied in `conn`/`datasource`.", call. = FALSE)
  }
  invisible(conn)
}

.db_backend <- function(conn) {
  .db_validate_connection(conn)
  if (inherits(conn, "SQLiteConnection")) return("sqlite")
  if (inherits(conn, c("PqConnection", "PostgreSQLConnection"))) return("postgres")
  stop("Unsupported database backend; use SQLite or PostgreSQL.", call. = FALSE)
}

.db_table <- function(conn, key) {
  config <- .db_table_config[[key]]
  if (is.null(config)) stop("Unknown repository table: ", key, call. = FALSE)
  location <- config[[.db_backend(conn)]]
  identifier <- if (length(location) == 2L) {
    DBI::Id(schema = location[[1]], table = location[[2]])
  } else {
    DBI::Id(table = location[[1]])
  }
  as.character(DBI::dbQuoteIdentifier(conn, identifier))
}

.db_columns <- function(conn, columns) {
  paste(as.character(DBI::dbQuoteIdentifier(conn, columns)), collapse = ", ")
}

.db_placeholders <- function(conn, count, offset = 0L) {
  if (.db_backend(conn) == "postgres") {
    paste0("$", seq_len(count) + offset)
  } else {
    rep("?", count)
  }
}

.db_in_clause <- function(conn, values, offset = 0L) {
  if (!length(values)) stop("Repository filters must not be empty.", call. = FALSE)
  paste0("(", paste(.db_placeholders(conn, length(values), offset), collapse = ", "), ")")
}

.db_date_param <- function(conn, value) {
  value <- as.Date(value)
  if (length(value) != 1L || is.na(value)) stop("Dates must be valid scalar values.", call. = FALSE)
  if (.db_backend(conn) == "sqlite") as.numeric(value) else value
}

.db_normalize_dates <- function(data, columns) {
  for (column in intersect(columns, names(data))) {
    if (!inherits(data[[column]], "Date")) {
      data[[column]] <- if (is.numeric(data[[column]])) {
        as.Date(data[[column]], origin = "1970-01-01")
      } else {
        as.Date(data[[column]])
      }
    }
  }
  data
}

.normalize_cid <- function(cid10) {
  if (identical(cid10, "A90")) return(list(canonical = "A90", values = "A90"))
  if (cid10 %in% c("A92", "A920", "A92.0")) {
    return(list(canonical = "A92.0", values = c("A92", "A920", "A92.0")))
  }
  if (cid10 %in% c("A92.8", "A928")) {
    return(list(canonical = "A92.8", values = c("A92.8", "A928")))
  }
  stop("Unknown CID-10: ", cid10, call. = FALSE)
}

.repo_notifications <- function(conn, cities, cids, firstday = NULL,
                                lastday = NULL, start_year = NULL,
                                end_year = NULL, columns = "*") {
  .db_validate_connection(conn)
  cities <- as.numeric(cities)
  if (!length(cities) || anyNA(cities)) stop("`cities` must contain geocodes.", call. = FALSE)
  if (!length(cids) || anyNA(cids)) stop("`cids` must contain disease codes.", call. = FALSE)

  selected <- if (identical(columns, "*")) "*" else .db_columns(conn, columns)
  params <- c(as.list(cities), as.list(as.character(cids)))
  city_filter <- .db_in_clause(conn, cities)
  cid_filter <- .db_in_clause(conn, cids, length(cities))
  where <- c(
    paste0(.db_columns(conn, "municipio_geocodigo"), " IN ", city_filter),
    paste0(.db_columns(conn, "cid10_codigo"), " IN ", cid_filter)
  )
  offset <- length(params)
  if (!is.null(firstday)) {
    where <- c(where, paste0(.db_columns(conn, "dt_digita"), " >= ",
                             .db_placeholders(conn, 1L, offset)))
    params <- c(params, list(.db_date_param(conn, firstday)))
    offset <- offset + 1L
  }
  if (!is.null(lastday)) {
    where <- c(where, paste0(.db_columns(conn, "dt_digita"), " <= ",
                             .db_placeholders(conn, 1L, offset)))
    params <- c(params, list(.db_date_param(conn, lastday)))
    offset <- offset + 1L
  }
  if (!is.null(start_year)) {
    where <- c(where, paste0(.db_columns(conn, "ano_notif"), " >= ",
                             .db_placeholders(conn, 1L, offset)))
    params <- c(params, list(as.integer(start_year)))
    offset <- offset + 1L
  }
  if (!is.null(end_year)) {
    where <- c(where, paste0(.db_columns(conn, "ano_notif"), " <= ",
                             .db_placeholders(conn, 1L, offset)))
    params <- c(params, list(as.integer(end_year)))
  }

  sql <- paste("SELECT", selected, "FROM", .db_table(conn, "notifications"),
               "WHERE", paste(where, collapse = " AND "))
  result <- DBI::dbGetQuery(conn, sql, params = unname(params))
  .db_normalize_dates(result, c("dt_notific", "dt_sin_pri", "dt_digita"))
}

.repo_municipalities <- function(conn, cities = NULL, uf = NULL,
                                 regional = NULL, macroregional = NULL,
                                 columns = c("geocodigo", "nome", "populacao")) {
  .db_validate_connection(conn)
  filters <- list()
  params <- list()
  add_filter <- function(column, values) {
    offset <- length(params)
    filters <<- c(filters, paste0(.db_columns(conn, column), " IN ",
                                  .db_in_clause(conn, values, offset)))
    params <<- c(params, as.list(values))
  }
  if (!is.null(cities)) add_filter("geocodigo", as.numeric(cities))
  if (!is.null(uf)) add_filter("uf", as.character(uf))
  if (!is.null(regional)) add_filter("regional", as.character(regional))
  if (!is.null(macroregional)) add_filter("macroregional", as.character(macroregional))

  sql <- paste("SELECT", .db_columns(conn, columns), "FROM",
               .db_table(conn, "municipalities"))
  if (length(filters)) sql <- paste(sql, "WHERE", paste(filters, collapse = " AND "))
  DBI::dbGetQuery(conn, sql, params = unname(params))
}

.repo_parameters <- function(conn, cities, cid10 = NULL) {
  .db_validate_connection(conn)
  params <- as.list(as.numeric(cities))
  filters <- paste0(.db_columns(conn, "municipio_geocodigo"), " IN ",
                    .db_in_clause(conn, cities))
  if (!is.null(cid10)) {
    filters <- c(filters, paste0(.db_columns(conn, "cid10"), " = ",
                                 .db_placeholders(conn, 1L, length(params))))
    params <- c(params, list(as.character(cid10)))
  }
  sql <- paste("SELECT * FROM", .db_table(conn, "parameters"), "WHERE",
               paste(filters, collapse = " AND "))
  DBI::dbGetQuery(conn, sql, params = unname(params))
}

.repo_station_links <- function(conn, cities) {
  params <- as.list(as.numeric(cities))
  sql <- paste(
    "SELECT", .db_columns(conn, c("municipio_geocodigo", "codigo_estacao_wu", "estacao_wu_sec")),
    "FROM", .db_table(conn, "parameters"),
    "WHERE", paste0(.db_columns(conn, "municipio_geocodigo"), " IN ",
                     .db_in_clause(conn, cities))
  )
  DBI::dbGetQuery(conn, sql, params = unname(params))
}

.repo_station_catalog <- function(conn, stations) {
  if (.db_backend(conn) == "sqlite") {
    return(data.frame(estacao_id = as.character(stations), nome = as.character(stations)))
  }
  sql <- paste("SELECT", .db_columns(conn, c("estacao_id", "nome")), "FROM",
               .db_table(conn, "station_catalog"), "WHERE",
               paste0(.db_columns(conn, "estacao_id"), " IN ", .db_in_clause(conn, stations)))
  DBI::dbGetQuery(conn, sql, params = as.list(as.character(stations)))
}

.repo_station_climate <- function(conn, stations, lastday) {
  station_column <- "Estacao_wu_estacao_id"
  params <- c(as.list(as.character(stations)), list(.db_date_param(conn, lastday)))
  sql <- paste(
    "SELECT * FROM", .db_table(conn, "station_climate"), "WHERE",
    paste0(.db_columns(conn, station_column), " IN ", .db_in_clause(conn, stations)),
    "AND", paste0(.db_columns(conn, "data_dia"), " <= ",
                   .db_placeholders(conn, 1L, length(stations)))
  )
  result <- DBI::dbGetQuery(conn, sql, params = unname(params))
  .db_normalize_dates(result, "data_dia")
}

.repo_municipal_climate <- function(conn, cities, columns, firstday, lastday) {
  params <- c(as.list(as.numeric(cities)), list(.db_date_param(conn, firstday),
                                                .db_date_param(conn, lastday)))
  offset <- length(cities)
  sql <- paste(
    "SELECT", .db_columns(conn, columns), "FROM", .db_table(conn, "municipal_climate"),
    "WHERE", paste0(.db_columns(conn, "geocode"), " IN ", .db_in_clause(conn, cities)),
    "AND", paste0(.db_columns(conn, "date"), " >= ", .db_placeholders(conn, 1L, offset)),
    "AND", paste0(.db_columns(conn, "date"), " <= ", .db_placeholders(conn, 1L, offset + 1L))
  )
  result <- DBI::dbGetQuery(conn, sql, params = unname(params))
  .db_normalize_dates(result, "date")
}

.repo_tweets <- function(conn, cities, lastday) {
  params <- c(as.list(as.numeric(cities)), list(.db_date_param(conn, lastday)))
  city_column <- "Municipio_geocodigo"
  sql <- paste(
    "SELECT", .db_columns(conn, c(city_column, "data_dia", "numero")),
    "FROM", .db_table(conn, "tweets"), "WHERE",
    paste0(.db_columns(conn, city_column), " IN ", .db_in_clause(conn, cities)),
    "AND", paste0(.db_columns(conn, "data_dia"), " <= ",
                   .db_placeholders(conn, 1L, length(cities)))
  )
  result <- DBI::dbGetQuery(conn, sql, params = unname(params))
  .db_normalize_dates(result, "data_dia")
}

.repo_regional_health <- function(conn, cities) {
  columns <- c("id", "nome_regional", "municipio_geocodigo",
               "codigo_estacao_wu", "estacao_wu_sec")
  sql <- paste("SELECT", .db_columns(conn, columns), "FROM",
               .db_table(conn, "regional_health"), "WHERE",
               paste0(.db_columns(conn, "municipio_geocodigo"), " IN ",
                      .db_in_clause(conn, cities)))
  DBI::dbGetQuery(conn, sql, params = as.list(as.numeric(cities)))
}

.repo_write_rows <- function(conn, table, data, key, conflict = c("error", "update", "ignore")) {
  .db_validate_connection(conn)
  conflict <- match.arg(conflict)
  if (!is.data.frame(data) || !nrow(data)) stop("`data` must contain at least one row.", call. = FALSE)
  if (!all(key %in% names(data))) stop("Conflict keys are missing from `data`.", call. = FALSE)

  target <- .db_table(conn, table)
  columns <- names(data)
  quoted_columns <- as.character(DBI::dbQuoteIdentifier(conn, columns))
  temp_name <- "alerttools_stage"
  temp_id <- DBI::Id(table = temp_name)
  temp_table <- as.character(DBI::dbQuoteIdentifier(conn, temp_id))

  DBI::dbWithTransaction(conn, {
    DBI::dbWriteTable(conn, temp_id, data, temporary = TRUE, overwrite = TRUE)
    on.exit(try(DBI::dbRemoveTable(conn, temp_id), silent = TRUE), add = TRUE)
    sql <- paste("INSERT INTO", target, paste0("(", paste(quoted_columns, collapse = ", "), ")"),
                 "SELECT", paste(quoted_columns, collapse = ", "), "FROM", temp_table,
                 "WHERE 1 = 1")
    if (conflict != "error") {
      quoted_key <- as.character(DBI::dbQuoteIdentifier(conn, key))
      sql <- paste(sql, "ON CONFLICT", paste0("(", paste(quoted_key, collapse = ", "), ")"))
      if (conflict == "ignore") {
        sql <- paste(sql, "DO NOTHING")
      } else {
        updates <- setdiff(columns, key)
        if (!length(updates)) stop("Update policy requires at least one non-key column.", call. = FALSE)
        quoted_updates <- as.character(DBI::dbQuoteIdentifier(conn, updates))
        assignments <- paste0(quoted_updates, " = excluded.", quoted_updates)
        sql <- paste(sql, "DO UPDATE SET", paste(assignments, collapse = ", "))
      }
    }
    DBI::dbExecute(conn, sql)
  })
}

.repo_update_station_links <- function(conn, data) {
  .db_validate_connection(conn)
  required <- c("municipio_geocodigo", "codigo_estacao_wu", "estacao_wu_sec")
  if (!is.data.frame(data) || !nrow(data) || !all(required %in% names(data))) {
    stop("Station link data has an invalid schema.", call. = FALSE)
  }
  data <- data[required]
  target <- .db_table(conn, "parameters")
  stage_id <- DBI::Id(table = "alerttools_station_stage")
  stage <- as.character(DBI::dbQuoteIdentifier(conn, stage_id))
  city <- .db_columns(conn, "municipio_geocodigo")
  primary <- .db_columns(conn, "codigo_estacao_wu")
  secondary <- .db_columns(conn, "estacao_wu_sec")

  DBI::dbWithTransaction(conn, {
    DBI::dbWriteTable(conn, stage_id, data, temporary = TRUE, overwrite = TRUE)
    on.exit(try(DBI::dbRemoveTable(conn, stage_id), silent = TRUE), add = TRUE)
    sql <- paste(
      "UPDATE", target, "SET",
      paste0(primary, " = stage.", primary, ", ", secondary, " = stage.", secondary),
      "FROM", stage, "AS stage WHERE",
      paste0(target, ".", city, " = stage.", city)
    )
    DBI::dbExecute(conn, sql)
  })
}
