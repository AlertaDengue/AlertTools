# Pure epidemiological pipeline ----------------------------------------------

.validate_workers <- function(workers) {
  if (length(workers) != 1L || !is.numeric(workers) || is.na(workers) ||
      workers < 1 || workers != floor(workers)) {
    stop("`workers` must be a positive integer.", call. = FALSE)
  }
  as.integer(workers)
}

.with_seed <- function(seed, code) {
  if (is.null(seed)) return(force(code))
  if (length(seed) != 1L || !is.numeric(seed) || is.na(seed)) {
    stop("`seed` must be NULL or a non-missing numeric scalar.", call. = FALSE)
  }
  had_seed <- exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  if (had_seed) old_seed <- get(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  on.exit({
    if (had_seed) {
      assign(".Random.seed", old_seed, envir = .GlobalEnv)
    } else if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) {
      rm(".Random.seed", envir = .GlobalEnv)
    }
  }, add = TRUE)
  set.seed(as.integer(seed))
  force(code)
}

.map_workers <- function(x, fun, workers = 1L) {
  workers <- .validate_workers(workers)
  if (workers == 1L || .Platform$OS.type == "windows") return(lapply(x, fun))
  parallel::mclapply(x, fun, mc.cores = workers, mc.preschedule = TRUE)
}

.validate_columns <- function(data, required, name, allow_empty = FALSE) {
  if (!is.data.frame(data)) stop("`", name, "` must be a data frame.", call. = FALSE)
  missing_columns <- setdiff(required, names(data))
  if (length(missing_columns)) {
    stop("`", name, "` is missing columns: ", paste(missing_columns, collapse = ", "),
         call. = FALSE)
  }
  if (!allow_empty && !nrow(data)) stop("`", name, "` must not be empty.", call. = FALSE)
  invisible(data)
}

#' Calculate incidence from in-memory vectors
#'
#' @param count Numeric vector of case counts.
#' @param population Numeric vector of positive population values.
#' @param scale Incidence scale, conventionally 100,000 inhabitants.
#' @return A numeric incidence vector.
#' @export
calculate_incidence <- function(count, population, scale = 100000) {
  if (!is.numeric(count) || !is.numeric(population) || length(count) != length(population)) {
    stop("`count` and `population` must be numeric vectors of equal length.", call. = FALSE)
  }
  if (length(scale) != 1L || !is.numeric(scale) || is.na(scale) || scale <= 0) {
    stop("`scale` must be a positive numeric scalar.", call. = FALSE)
  }
  if (any(count < 0, na.rm = TRUE)) {
    stop("`count` must not contain negative values.", call. = FALSE)
  }
  if (any(population <= 0, na.rm = TRUE)) {
    stop("`population` must contain only positive values.", call. = FALSE)
  }
  count / population * scale
}

#' Construct an AlertTools pipeline result
#'
#' @param data In-memory epidemiological data frame.
#' @param alerts In-memory alert-index data frame.
#' @param parameters Parameter data frame used by the computation.
#' @param metadata Named list describing the run.
#' @param diagnostics Named list of diagnostic information.
#' @return An object of class `alerttools_result`.
#' @export
new_alerttools_result <- function(data, alerts, parameters,
                                  metadata = list(), diagnostics = list()) {
  .validate_columns(data, character(), "data", allow_empty = TRUE)
  .validate_columns(alerts, character(), "alerts", allow_empty = TRUE)
  .validate_columns(parameters, character(), "parameters", allow_empty = TRUE)
  if (anyDuplicated(names(data)) || anyDuplicated(names(alerts)) ||
      anyDuplicated(names(parameters))) {
    stop("Result data frames must not contain duplicate column names.", call. = FALSE)
  }
  if (nrow(data) != nrow(alerts)) {
    stop("`data` and `alerts` must have the same number of rows.", call. = FALSE)
  }
  if (!is.list(metadata) || (length(metadata) && is.null(names(metadata)))) {
    stop("`metadata` must be a named list.", call. = FALSE)
  }
  if (!is.list(diagnostics) || (length(diagnostics) && is.null(names(diagnostics)))) {
    stop("`diagnostics` must be a named list.", call. = FALSE)
  }
  structure(
    list(data = data, alerts = alerts, parameters = parameters,
         metadata = metadata, diagnostics = diagnostics),
    class = "alerttools_result"
  )
}

#' Run the pure in-memory AlertTools pipeline
#'
#' This function performs no database or file access and does not depend on the
#' current working directory.
#'
#' @param cases Weekly case data using the `getCases()` domain schema.
#' @param climate Weekly municipal climate data with `SE` and `geocodigo`.
#' @param parameters Alert parameters, one row per municipality and disease.
#' @param case_records Optional individual notification records for Bayesian
#'   nowcasting.
#' @param report_week Epidemiological week through which to compute results.
#' @param nowcasting Either `"none"` or `"bayesian"`.
#' @param workers Positive number of workers. The default is portable and serial.
#' @param seed Optional deterministic seed for stochastic nowcasting.
#' @param verbose Whether orchestration should emit progress messages.
#' @return An `alerttools_result` object.
#' @export
alerttools_pipeline <- function(cases, climate, parameters, case_records = NULL,
                                report_week = max(cases$SE),
                                nowcasting = c("none", "bayesian"),
                                workers = 1L, seed = NULL, verbose = FALSE) {
  nowcasting <- match.arg(nowcasting)
  workers <- .validate_workers(workers)
  if (!is.logical(verbose) || length(verbose) != 1L || is.na(verbose)) {
    stop("`verbose` must be TRUE or FALSE.", call. = FALSE)
  }
  .validate_columns(
    cases,
    c("SE", "cidade", "CID10", "casos", "cas_prov", "cas_lab",
      "localidade", "nome", "pop"),
    "cases"
  )
  .validate_columns(climate, c("SE", "geocodigo"), "climate")
  .validate_columns(
    parameters,
    c("municipio_geocodigo", "limiar_preseason", "limiar_epidemico",
      "varcli", "clicrit", "cid10", "codmodelo"),
    "parameters"
  )
  if (length(report_week) != 1L || !is.numeric(report_week) || is.na(report_week)) {
    stop("`report_week` must be one epidemiological week.", call. = FALSE)
  }
  .validate_epiweek(report_week)
  if (nowcasting == "bayesian" && is.null(case_records)) {
    stop("Bayesian nowcasting requires in-memory `case_records`.", call. = FALSE)
  }

  input <- dplyr::left_join(cases, climate, by = c("cidade" = "geocodigo", "SE" = "SE"))
  if (!"tweet" %in% names(input)) input$tweet <- NA_real_
  input$inc <- calculate_incidence(input$casos, input$pop)
  cities <- unique(as.numeric(parameters$municipio_geocodigo))
  if (!all(cities %in% input$cidade)) {
    stop("Parameters contain municipalities absent from `cases`.", call. = FALSE)
  }
  if (!all(parameters$cid10 %in% input$CID10)) {
    stop("Parameters contain diseases absent from `cases`.", call. = FALSE)
  }

  compute_city <- function(city) {
    city_data <- input[input$cidade == city & input$SE <= report_week, , drop = FALSE]
    city_parameters <- parameters[parameters$municipio_geocodigo == city, , drop = FALSE]
    if (nrow(city_parameters) != 1L) {
      stop("Each municipality must have exactly one parameter row.", call. = FALSE)
    }
    records <- if (is.null(case_records)) NULL else {
      case_records[case_records$municipio_geocodigo == city, , drop = FALSE]
    }
    city_seed <- if (is.null(seed)) NULL else seed + match(city, cities) - 1L
    adjusted <- nowcast_cases(
      city_data, case_records = records, method = nowcasting, report_week = report_week,
      seed = city_seed, workers = workers, verbose = verbose
    )
    estimated <- estimate_rt(adjusted, count = "tcasesmed", distribution = "normal",
                             mean_generation_time = 3, sd_generation_time = 1)
    estimated$inc <- calculate_incidence(estimated$tcasesmed, estimated$pop)
    values <- structure(as.character(city_parameters[1, ]), names = names(city_parameters))
    criteria <- define_alert_rules(rule = city_parameters$codmodelo[[1]], values = values)
    alert <- classify_alerts(estimated, rules = criteria,
                             missing = "last", minimum_history = 4)
    list(data = alert$data, alerts = alert$indices, criteria = criteria)
  }

  computed <- .with_seed(seed, .map_workers(cities, compute_city, workers))
  data <- dplyr::bind_rows(lapply(computed, `[[`, "data"))
  alerts <- dplyr::bind_rows(lapply(computed, `[[`, "alerts"))
  diagnostics <- list(
    cities = length(cities),
    rows = nrow(data),
    missing_climate = sum(!stats::complete.cases(input[setdiff(names(climate), c("SE", "geocodigo"))]))
  )
  metadata <- list(
    report_week = as.numeric(report_week), nowcasting = nowcasting,
    workers = workers, seed = seed, deterministic = nowcasting == "none" || !is.null(seed)
  )
  new_alerttools_result(data, alerts, parameters, metadata, diagnostics)
}

#' @export
print.alerttools_result <- function(x, ...) {
  cat("<alerttools_result>\n")
  cat("  rows:", nrow(x$data), "\n")
  cat("  municipalities:", x$diagnostics$cities %||% length(unique(x$data$cidade)), "\n")
  cat("  report week:", x$metadata$report_week %||% NA, "\n")
  invisible(x)
}

#' @export
summary.alerttools_result <- function(object, ...) {
  levels <- if ("level" %in% names(object$alerts)) {
    table(factor(object$alerts$level, levels = 1:4), useNA = "ifany")
  } else integer()
  list(
    rows = nrow(object$data),
    municipalities = object$diagnostics$cities %||% length(unique(object$data$cidade)),
    alert_levels = levels,
    metadata = object$metadata,
    diagnostics = object$diagnostics
  )
}

#' @export
as.data.frame.alerttools_result <- function(x, row.names = NULL, optional = FALSE, ...) {
  duplicate <- intersect(names(x$data), names(x$alerts))
  alerts <- x$alerts[setdiff(names(x$alerts), duplicate)]
  as.data.frame(cbind(x$data, alerts), row.names = row.names, optional = optional)
}

`%||%` <- function(x, y) if (is.null(x)) y else x
