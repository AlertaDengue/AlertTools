# Public API -----------------------------------------------------------------

.normalize_disease <- function(disease) {
  if (length(disease) != 1L || !is.character(disease) || is.na(disease)) {
    stop("`disease` must be one non-missing character value.", call. = FALSE)
  }
  key <- tolower(trimws(disease))
  if (key %in% c("dengue", "a90")) return("A90")
  if (key %in% c("chik", "chikungunya", "a92", "a920", "a92.0")) return("A92.0")
  if (key %in% c("zika", "a928", "a92.8")) return("A92.8")
  stop("Unknown disease: ", disease, call. = FALSE)
}

.validate_geocodes <- function(geocodes) {
  if (!is.numeric(geocodes) || !length(geocodes) || anyNA(geocodes)) {
    stop("`geocodes` must be a non-empty numeric vector.", call. = FALSE)
  }
  unique(vapply(geocodes, sevendigitgeocode, numeric(1)))
}

.resolve_api_period <- function(start_week = NULL, report_week = NULL,
                                start_date = NULL, end_date = NULL) {
  if (is.null(start_week) && is.null(start_date)) {
    stop("Supply `start_week` or `start_date`.", call. = FALSE)
  }
  if (is.null(report_week) && is.null(end_date)) {
    stop("Supply `report_week` or `end_date`.", call. = FALSE)
  }
  if (!is.null(start_date)) {
    start_date <- as.Date(start_date)
    if (length(start_date) != 1L || is.na(start_date)) {
      stop("`start_date` must be one valid date.", call. = FALSE)
    }
  }
  if (!is.null(end_date)) {
    end_date <- as.Date(end_date)
    if (length(end_date) != 1L || is.na(end_date)) {
      stop("`end_date` must be one valid date.", call. = FALSE)
    }
  }
  if (!is.null(start_week)) {
    .validate_epiweek(start_week)
    if (length(start_week) != 1L || is.na(start_week)) {
      stop("`start_week` must be one epidemiological week.", call. = FALSE)
    }
  }
  if (!is.null(report_week)) {
    .validate_epiweek(report_week)
    if (length(report_week) != 1L || is.na(report_week)) {
      stop("`report_week` must be one epidemiological week.", call. = FALSE)
    }
  }
  resolved_start_week <- if (is.null(start_week)) as_epiweek(start_date) else as.numeric(start_week)
  resolved_report_week <- if (is.null(report_week)) as_epiweek(end_date) else as.numeric(report_week)
  if (!is.null(start_date) && as_epiweek(start_date) != resolved_start_week) {
    stop("`start_date` and `start_week` refer to different weeks.", call. = FALSE)
  }
  if (!is.null(end_date) && as_epiweek(end_date) != resolved_report_week) {
    stop("`end_date` and `report_week` refer to different weeks.", call. = FALSE)
  }
  resolved_start_date <- if (is.null(start_date)) epiweek_start(resolved_start_week) else start_date
  resolved_end_date <- if (is.null(end_date)) epiweek_start(resolved_report_week) + 6L else end_date
  if (resolved_start_date > resolved_end_date ||
      epiweek_start(resolved_start_week) > epiweek_start(resolved_report_week)) {
    stop("The start of the period must not be after its end.", call. = FALSE)
  }
  list(
    start_week = resolved_start_week, report_week = resolved_report_week,
    start_date = resolved_start_date, end_date = resolved_end_date
  )
}

.normalize_population <- function(population) {
  if (is.null(population)) return(NULL)
  if (!is.data.frame(population)) stop("`population` must be a data frame.", call. = FALSE)
  code <- intersect(c("cidade", "geocode", "municipio_geocodigo"), names(population))
  value <- intersect(c("pop", "populacao", "population"), names(population))
  if (!length(code) || !length(value)) {
    stop("`population` must contain a geocode and population column.", call. = FALSE)
  }
  normalized <- data.frame(
    cidade = as.numeric(population[[code[[1]]]]),
    pop = as.numeric(population[[value[[1]]]])
  )
  if ("year" %in% names(population)) {
    normalized$year <- as.integer(population$year)
    normalized <- normalized[order(normalized$cidade, normalized$year), , drop = FALSE]
    normalized <- normalized[!duplicated(normalized$cidade, fromLast = TRUE), , drop = FALSE]
    normalized$year <- NULL
  }
  if (anyNA(normalized) || any(normalized$pop <= 0) || anyDuplicated(normalized$cidade)) {
    stop("`population` must have one positive value per geocode.", call. = FALSE)
  }
  normalized
}

#' Construct validated inputs for the alert pipeline
#'
#' `cases` has one row per municipality, disease, and epidemiological week and
#' must contain `SE`, `cidade`, `CID10`, `casos`, `cas_prov`, `cas_lab`,
#' `localidade`, and `nome`. It must also contain `pop`, unless `population` is
#' supplied. `climate` has one row per municipality and week, keyed by
#' `geocodigo` and `SE`. Rows are ordered by municipality and week.
#'
#' @param cases Weekly case data frame.
#' @param climate Weekly climate data frame.
#' @param population Optional population data with a geocode and population
#'   column. When years are present, the latest value per municipality is used.
#' @param case_records Optional individual notification records used by
#'   Bayesian nowcasting.
#' @param metadata Named list describing input provenance.
#' @return An experimental `alerttools_inputs` object.
#' @section Lifecycle:
#' This interface is experimental while operational consumers migrate from the
#' legacy API.
#' @export
new_alert_inputs <- function(cases, climate, population = NULL,
                             case_records = NULL, metadata = list()) {
  .validate_columns(
    cases,
    c("SE", "cidade", "CID10", "casos", "cas_prov", "cas_lab", "localidade", "nome"),
    "cases"
  )
  .validate_columns(climate, c("SE", "geocodigo"), "climate")
  if (!is.numeric(cases$SE) || !is.numeric(cases$cidade) ||
      !is.numeric(climate$SE) || !is.numeric(climate$geocodigo) ||
      !all(vapply(cases[c("casos", "cas_prov", "cas_lab")], is.numeric, logical(1)))) {
    stop("Case and climate keys and counts must be numeric.", call. = FALSE)
  }
  .validate_epiweek(c(cases$SE, climate$SE))
  if (!is.null(case_records) && !is.data.frame(case_records)) {
    stop("`case_records` must be NULL or a data frame.", call. = FALSE)
  }
  if (!is.list(metadata) || (length(metadata) && is.null(names(metadata)))) {
    stop("`metadata` must be a named list.", call. = FALSE)
  }

  population <- .normalize_population(population)
  if (!"pop" %in% names(cases)) cases$pop <- NA_real_
  if (!is.null(population)) {
    matched <- match(cases$cidade, population$cidade)
    replace <- is.na(cases$pop)
    cases$pop[replace] <- population$pop[matched[replace]]
  }
  if (anyNA(cases$pop) || any(cases$pop <= 0)) {
    stop("Every case row must have a positive population.", call. = FALSE)
  }
  if (anyDuplicated(cases[c("cidade", "CID10", "SE")])) {
    stop("`cases` keys (`cidade`, `CID10`, `SE`) must be unique.", call. = FALSE)
  }
  if (anyDuplicated(climate[c("geocodigo", "SE")])) {
    stop("`climate` keys (`geocodigo`, `SE`) must be unique.", call. = FALSE)
  }
  cases <- cases[order(cases$cidade, cases$CID10, cases$SE), , drop = FALSE]
  climate <- climate[order(climate$geocodigo, climate$SE), , drop = FALSE]
  rownames(cases) <- NULL
  rownames(climate) <- NULL

  structure(
    list(cases = cases, climate = climate, population = population,
         case_records = case_records, metadata = metadata),
    class = "alerttools_inputs"
  )
}

#' @export
print.alerttools_inputs <- function(x, ...) {
  cat("<alerttools_inputs>\n")
  cat("  case rows:", nrow(x$cases), "\n")
  cat("  climate rows:", nrow(x$climate), "\n")
  cat("  municipalities:", length(unique(x$cases$cidade)), "\n")
  invisible(x)
}

#' Fetch weekly cases
#'
#' @param conn A valid DBI connection.
#' @param geocodes Numeric seven-digit municipality codes.
#' @param disease Disease name (`"dengue"`, `"chikungunya"`, or `"zika"`) or
#'   equivalent CID-10 code.
#' @param start_date,end_date Explicit inclusive date interval.
#' @param case_date Date used for aggregation.
#' @param complete_tail Value used for weeks beyond the latest report.
#' @param verbose Whether to emit progress messages.
#' @return A data frame ordered by `cidade` and `SE`, with one row per
#'   `cidade`, `CID10`, and `SE`. Columns are `SE`, `cidade`, `CID10`, `casos`,
#'   `cas_prov`, `cas_lab`, `localidade`, `nome`, and `pop`.
#' @export
fetch_cases <- function(conn, geocodes, disease = "dengue", start_date,
                        end_date, case_date = c("notification", "symptom_onset"),
                        complete_tail = NA, verbose = FALSE) {
  .db_validate_connection(conn)
  geocodes <- .validate_geocodes(geocodes)
  case_date <- match.arg(case_date)
  .fetch_cases_impl(
    cities = geocodes, firstday = as.Date(start_date), lastday = as.Date(end_date),
    cid10 = .normalize_disease(disease),
    dataini = if (case_date == "notification") "notific" else "sinpri",
    completetail = complete_tail, type = "all", datasource = conn, verbose = verbose
  )
}

#' Fetch individual case records
#'
#' @inheritParams fetch_cases
#' @return A data frame with individual notification records. It includes
#'   `municipio_geocodigo`, `dt_notific`, `dt_sin_pri`, and `dt_digita`; date
#'   columns use class `Date`.
#' @export
fetch_case_records <- function(conn, geocodes, disease = "dengue",
                               start_date, end_date) {
  .db_validate_connection(conn)
  .fetch_case_records_impl(
    cities = .validate_geocodes(geocodes), firstday = as.Date(start_date),
    lastday = as.Date(end_date), cid10 = .normalize_disease(disease), datasource = conn
  )
}

#' Fetch weekly municipal climate data
#'
#' @param climate_vars Climate columns to fetch.
#' @inheritParams fetch_cases
#' @return A data frame ordered and uniquely keyed by `geocodigo` and `SE`,
#'   followed by the requested climate columns.
#' @export
fetch_climate <- function(conn, geocodes, start_date, end_date,
                          climate_vars = c("temp_min", "temp_max", "temp_med",
                                           "umid_min", "umid_med", "umid_max")) {
  .db_validate_connection(conn)
  .fetch_climate_impl(
    cities = .validate_geocodes(geocodes), vars = climate_vars,
    iniSE = as_epiweek(as.Date(start_date)), finalday = as.Date(end_date),
    datasource = conn
  )
}

#' Fetch all inputs required by the alert pipeline
#'
#' Exactly one representation of each boundary is sufficient: use weeks, dates,
#' or both when they refer to the same epidemiological weeks.
#'
#' @inheritParams fetch_cases
#' @param start_week First epidemiological week.
#' @param report_week Last epidemiological week included in the report.
#' @param climate_vars Climate columns to fetch.
#' @return An experimental `alerttools_inputs` object.
#' @section Lifecycle:
#' This database orchestration interface is experimental.
#' @export
fetch_alert_inputs <- function(conn, geocodes, disease = "dengue",
                               start_week = NULL, report_week = NULL,
                               start_date = NULL, end_date = NULL,
                               case_date = c("notification", "symptom_onset"),
                               complete_tail = NA,
                               climate_vars = c("temp_min", "temp_max", "temp_med",
                                                "umid_min", "umid_med", "umid_max"),
                               verbose = FALSE) {
  period <- .resolve_api_period(start_week, report_week, start_date, end_date)
  geocodes <- .validate_geocodes(geocodes)
  cid10 <- .normalize_disease(disease)
  cases <- fetch_cases(
    conn, geocodes, cid10, period$start_date, period$end_date,
    case_date = match.arg(case_date), complete_tail = complete_tail, verbose = verbose
  )
  records <- attr(cases, "case_records")
  attr(cases, "case_records") <- NULL
  climate <- fetch_climate(
    conn, geocodes, period$start_date, period$end_date,
    climate_vars = climate_vars
  )
  new_alert_inputs(
    cases, climate, case_records = records,
    metadata = c(period, list(geocodes = geocodes, disease = cid10, source = "database"))
  )
}

#' Fetch alert parameters
#'
#' @inheritParams fetch_cases
#' @return A data frame with one row per `municipio_geocodigo` and `cid10`, in
#'   requested geocode order. Pipeline columns include `limiar_preseason`,
#'   `limiar_epidemico`, `varcli`, `clicrit`, and `codmodelo`.
#' @export
fetch_alert_parameters <- function(conn, geocodes, disease = "dengue") {
  .db_validate_connection(conn)
  geocodes <- .validate_geocodes(geocodes)
  cid10 <- .normalize_disease(disease)
  result <- .repo_parameters(conn, geocodes, cid10)
  if (!nrow(result) || !all(geocodes %in% result$municipio_geocodigo)) {
    stop("Alert parameters were not found for every requested geocode and disease.",
         call. = FALSE)
  }
  result <- result[order(match(result$municipio_geocodigo, geocodes)), , drop = FALSE]
  rownames(result) <- NULL
  result
}

#' Run the alert pipeline
#'
#' This experimental entry point performs computation only. It never reads from
#' or writes to a database.
#'
#' @param inputs An `alerttools_inputs` object.
#' @param parameters Alert parameter data frame, keyed by municipality and disease.
#' @param report_week Explicit report week. Defaults to the value recorded by
#'   `fetch_alert_inputs()`.
#' @param nowcast Either `"none"` or `"bayesian"`.
#' @param workers Positive worker count; defaults to one.
#' @param seed Optional deterministic seed.
#' @param verbose Whether to emit progress messages.
#' @return An `alerttools_result` object ordered by municipality and week, with
#'   `data`, `alerts`, `parameters`, `metadata`, and `diagnostics` fields.
#' @section Lifecycle:
#' This orchestration interface is experimental.
#' @export
run_alert_pipeline <- function(inputs, parameters, report_week = NULL,
                               nowcast = c("none", "bayesian"), workers = 1L,
                               seed = NULL, verbose = FALSE) {
  if (!inherits(inputs, "alerttools_inputs")) {
    stop("`inputs` must be created by new_alert_inputs() or fetch_alert_inputs().",
         call. = FALSE)
  }
  if (is.null(report_week)) report_week <- inputs$metadata$report_week
  if (is.null(report_week)) stop("Supply an explicit `report_week`.", call. = FALSE)
  alerttools_pipeline(
    cases = inputs$cases, climate = inputs$climate, parameters = parameters,
    case_records = inputs$case_records, report_week = report_week,
    nowcasting = match.arg(nowcast), workers = workers, seed = seed, verbose = verbose
  )
}

#' Convert an alert result to the persistence schema
#'
#' @param result An `alerttools_result` object.
#' @param version_date Explicit model-version date.
#' @param start_week,end_week Optional inclusive week filter.
#' @return A data frame keyed by `SE`, `municipio_geocodigo`, and `Localidade_id`.
#' @export
as_alert_history <- function(result, version_date, start_week = NULL, end_week = NULL) {
  if (!inherits(result, "alerttools_result")) {
    stop("`result` must be an alerttools_result.", call. = FALSE)
  }
  args <- list(obj = result, versao = as.Date(version_date))
  if (!is.null(start_week)) args$iniSE <- start_week
  if (!is.null(end_week)) args$lastSE <- end_week
  do.call(.as_alert_history_impl, args)
}

#' Write alert results explicitly
#'
#' @param conn A valid DBI connection.
#' @param result An `alerttools_result` object or an alert-history data frame.
#' @param version_date Explicit model-version date, required when `result` is an
#'   `alerttools_result`.
#' @param conflict Conflict policy: `"update"`, `"error"`, or `"ignore"`.
#' @param start_week,end_week Optional inclusive week filter.
#' @param verbose Whether to emit a persistence message.
#' @return `result`, invisibly.
#' @export
write_alert_results <- function(conn, result, version_date = NULL,
                                conflict = c("update", "error", "ignore"),
                                start_week = NULL, end_week = NULL,
                                verbose = FALSE) {
  .db_validate_connection(conn)
  conflict <- match.arg(conflict)
  history <- if (inherits(result, "alerttools_result")) {
    if (is.null(version_date)) {
      stop("`version_date` is required for an alerttools_result.", call. = FALSE)
    }
    as_alert_history(result, version_date, start_week, end_week)
  } else {
    if (!is.data.frame(result)) stop("`result` must be an alerttools_result or data frame.", call. = FALSE)
    result
  }
  .write_alert_results_impl(history, datasource = conn, conflict = conflict,
                            verbose = verbose)
  invisible(result)
}

#' Upsert alert parameters
#'
#' @param conn A valid DBI connection.
#' @param parameters Parameter data frame keyed by `municipio_geocodigo` and `cid10`.
#' @param conflict Conflict policy: `"update"`, `"error"`, or `"ignore"`.
#' @return The persisted parameter rows.
#' @export
upsert_alert_parameters <- function(conn, parameters,
                                    conflict = c("update", "error", "ignore")) {
  .db_validate_connection(conn)
  .validate_columns(parameters, c("municipio_geocodigo", "cid10"), "parameters")
  if (any(!vapply(as.character(parameters$cid10), function(value) {
    identical(.normalize_disease(value), value)
  }, logical(1)))) {
    stop("`parameters$cid10` must contain canonical CID-10 codes.", call. = FALSE)
  }
  if (anyDuplicated(parameters[c("municipio_geocodigo", "cid10")])) {
    stop("Parameter keys must be unique.", call. = FALSE)
  }
  conflict <- match.arg(conflict)
  .repo_write_rows(conn, "parameters", parameters,
                   key = c("municipio_geocodigo", "cid10"), conflict = conflict)
  observed <- lapply(split(parameters, parameters$cid10), function(rows) {
    .repo_parameters(conn, rows$municipio_geocodigo, rows$cid10[[1]])
  })
  dplyr::bind_rows(observed)
}

# Focused snake_case aliases for pure computational responsibilities.

#' Estimate the effective reproductive number
#'
#' @param data In-memory weekly data.
#' @param count Name of the count column.
#' @param distribution Generation-time distribution.
#' @param mean_generation_time,sd_generation_time Generation-time parameters.
#' @param ... Additional arguments passed to `Rt()`.
#' @return `data` with `Rt`, interval, and probability columns.
#' @export
estimate_rt <- function(data, count = "casos", distribution = "normal",
                        mean_generation_time = 3, sd_generation_time = 1, ...) {
  .estimate_rt_impl(data, count = count, gtdist = distribution,
                    meangt = mean_generation_time, sdgt = sd_generation_time, ...)
}

#' Nowcast weekly cases
#'
#' @param data In-memory weekly case data for one municipality.
#' @param case_records Optional individual notification records.
#' @param method Either `"none"` or `"bayesian"`.
#' @param report_week Explicit report week.
#' @param workers Positive worker count.
#' @param seed Optional deterministic seed.
#' @param verbose Whether to emit progress messages.
#' @param ... Additional nowcasting arguments.
#' @return The weekly data with adjusted-count columns.
#' @export
nowcast_cases <- function(data, case_records = NULL,
                          method = c("none", "bayesian"), report_week,
                          workers = 1L, seed = NULL, verbose = FALSE, ...) {
  .nowcast_cases_impl(data, datas = case_records, method = match.arg(method),
                      nowSE = report_week, workers = workers, seed = seed,
                      verbose = verbose, ...)
}

#' Define alert rules
#'
#' @param rule Rule identifier.
#' @param values Named parameter vector.
#' @param delays Number of weeks used by delayed criteria.
#' @return A validated list of alert criteria.
#' @export
define_alert_rules <- function(rule, values, delays = 3) {
  .define_alert_rules_impl(rule = rule, values = values, delays = delays)
}

#' Classify epidemiological alerts
#'
#' @param data In-memory epidemiological indicator data.
#' @param rules Rules returned by `define_alert_rules()`.
#' @param missing Whether missing climate values activate the legacy missing-data rule.
#' @param minimum_history Minimum history used by the classifier.
#' @return An object of class `alerta`.
#' @export
classify_alerts <- function(data, rules, missing = FALSE, minimum_history = 8) {
  .classify_alerts_impl(data, crit = rules, miss = missing, dy = minimum_history)
}

# Legacy wrappers ------------------------------------------------------------

.deprecate_api <- function(old, replacement) {
  lifecycle::deprecate_warn("1.1.0", old, replacement, user_env = parent.frame(2L))
}

#' Legacy Rt estimator
#' @param obj,count,gtdist,meangt,sdgt,CI,alpha,a0,b0 Legacy arguments.
#' @export
Rt <- function(obj, count = "casos", gtdist, meangt, sdgt, CI = "beta",
               alpha = .95, a0 = 2, b0 = 3) {
  .deprecate_api("Rt()", "estimate_rt()")
  .estimate_rt_impl(obj, count, gtdist, meangt, sdgt, CI, alpha, a0, b0)
}

#' Legacy nowcasting interface
#' @param obj,datas,method,pdig,Dmax,nyears,datasource,nowSE,safelimit,seed,workers,verbose,... Legacy arguments.
#' @export
adjustIncidence <- function(obj, datas = NULL, method = "none",
                            pdig = stats::plnorm((1:20) * 7, 2.5016, 1.1013),
                            Dmax = 10, nyears = 2, datasource = NULL, nowSE,
                            safelimit = 5, seed = NULL, workers = 1L,
                            verbose = FALSE, ...) {
  .deprecate_api("adjustIncidence()", "nowcast_cases()")
  .nowcast_cases_impl(obj, datas, method, pdig, Dmax, nyears, datasource,
                      nowSE, safelimit, seed, workers, verbose, ...)
}

#' Legacy alert-rule constructor
#' @param rule,values,delays Legacy arguments.
#' @export
setCriteria <- function(rule = NULL, values = NULL, delays = 3) {
  .deprecate_api("setCriteria()", "define_alert_rules()")
  .define_alert_rules_impl(rule, values, delays)
}

#' Legacy alert classifier
#' @param obj,crit,miss,dy Legacy arguments.
#' @export
fouralert <- function(obj, crit, miss = "last", dy = 4) {
  .deprecate_api("fouralert()", "classify_alerts()")
  .classify_alerts_impl(obj, crit, miss, dy)
}

#' Legacy history converter
#' @param obj,iniSE,lastSE,type,versao,parameters Legacy arguments.
#' @export
tabela_historico <- function(obj, iniSE, lastSE, type = "all", versao,
                             parameters = if (inherits(obj, "alerttools_result")) obj$parameters else attr(obj, "parameters")) {
  .deprecate_api("tabela_historico()", "as_alert_history()")
  args <- list(obj = obj, type = type, parameters = parameters)
  if (!missing(iniSE)) args$iniSE <- iniSE
  if (!missing(lastSE)) args$lastSE <- lastSE
  if (!missing(versao)) args$versao <- versao
  do.call(.as_alert_history_impl, args)
}

#' Legacy case reader
#' @inheritParams fetch_cases
#' @param cities,lastday,firstday,cid10,dataini,completetail,type,datasource Legacy arguments.
#' @export
getCases <- function(cities, lastday = Sys.Date(), firstday = as.Date("2018-01-01"),
                     cid10 = "A90", dataini = "notific", completetail = NA,
                     type = "notified", datasource, verbose = FALSE) {
  .deprecate_api("getCases()", "fetch_cases()")
  .fetch_cases_impl(cities, lastday, firstday, cid10, dataini, completetail,
                    type, datasource, verbose)
}

#' Legacy individual-case reader
#' @param cities,lastday,firstday,cid10,datasource Legacy arguments.
#' @export
getCaseslist <- function(cities, lastday = Sys.Date(), firstday = as.Date("2022-01-01"),
                         cid10 = "A90", datasource) {
  .deprecate_api("getCaseslist()", "fetch_case_records()")
  .fetch_case_records_impl(cities, lastday, firstday, cid10, datasource)
}

#' Legacy climate reader
#' @param cities,vars,finalday,iniSE,lastSE,datasource Legacy arguments.
#' @export
getClima <- function(cities, vars = c("temp_min", "temp_max", "temp_med", "umid_min", "ampT",
                                      "umid_med", "umid_max", "precip_tot", "precip_max"),
                     finalday = Sys.Date(), iniSE = 201501, lastSE, datasource) {
  .deprecate_api("getClima()", "fetch_climate()")
  args <- list(cities = cities, vars = vars, finalday = finalday,
               iniSE = iniSE, datasource = datasource)
  if (!missing(lastSE)) args$lastSE <- lastSE
  do.call(.fetch_climate_impl, args)
}

#' Legacy alert-parameter reader
#' @param cities,cid10,datasource Legacy arguments.
#' @export
read.parameters <- function(cities, cid10 = "A90", datasource) {
  .deprecate_api("read.parameters()", "fetch_alert_parameters()")
  .fetch_alert_parameters_impl(cities, cid10, datasource)
}

#' Legacy alert-parameter writer
#' @param city,cid10,params,overwrite,datasource,conflict Legacy arguments.
#' @export
write_parameters <- function(city, cid10, params, overwrite = FALSE, datasource,
                             conflict = c("ignore", "update", "error")) {
  .deprecate_api("write_parameters()", "upsert_alert_parameters()")
  .write_parameters_impl(city, cid10, params, overwrite, datasource, conflict)
}

#' Legacy alert-result writer
#' @param d,writetofile,datasource,arq,conflict Legacy arguments.
#' @export
write_alerta <- function(d, writetofile = FALSE, datasource, arq = "output.sql",
                         conflict = c("update", "error", "ignore")) {
  .deprecate_api("write_alerta()", "write_alert_results()")
  args <- list(d = d, writetofile = writetofile, arq = arq, conflict = conflict)
  if (!missing(datasource)) args$datasource <- datasource
  do.call(.write_alert_results_impl, args)
}
