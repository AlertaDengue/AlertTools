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

.canonicalize_municipality_key <- function(data, aliases, name) {
  .validate_columns(data, character(), name)
  if (anyDuplicated(names(data))) {
    stop("`", name, "` must not contain duplicate column names.", call. = FALSE)
  }
  canonical <- "municipio_geocodigo"
  present_aliases <- intersect(aliases, names(data))
  if (canonical %in% names(data)) {
    for (alias in present_aliases) {
      if (!identical(as.character(data[[canonical]]), as.character(data[[alias]]))) {
        stop("`", name, "` contains conflicting municipality identifiers.",
             call. = FALSE)
      }
    }
    data[present_aliases] <- NULL
  } else {
    if (length(present_aliases) != 1L) {
      stop("`", name, "` must contain `municipio_geocodigo`.", call. = FALSE)
    }
    data[[canonical]] <- data[[present_aliases[[1L]]]]
    data[present_aliases[[1L]]] <- NULL
  }
  data
}

.canonicalize_pipeline_geocodes <- function(cases, climate) {
  list(
    cases = .canonicalize_municipality_key(cases, "cidade", "cases"),
    climate = .canonicalize_municipality_key(climate, "geocodigo", "climate")
  )
}

.validate_unique_keys <- function(data, keys, name) {
  if (anyNA(data[keys])) {
    stop("`", name, "` keys must not contain missing values.", call. = FALSE)
  }
  if (anyDuplicated(data[keys])) {
    stop("`", name, "` keys (", paste(keys, collapse = ", "),
         ") must be unique.", call. = FALSE)
  }
  invisible(data)
}

.validate_pipeline_inputs <- function(cases, climate, parameters = NULL,
                                      require_population = TRUE) {
  case_columns <- c("SE", "municipio_geocodigo", "CID10", "casos", "cas_prov",
                    "cas_lab", "localidade", "nome")
  if (require_population) case_columns <- c(case_columns, "pop")
  .validate_columns(cases, case_columns, "cases")
  .validate_columns(climate, c("SE", "municipio_geocodigo"), "climate")
  numeric_case_columns <- c("SE", "municipio_geocodigo", "casos", "cas_prov", "cas_lab")
  if (!all(vapply(cases[numeric_case_columns], is.numeric, logical(1))) ||
      !is.numeric(climate$SE) || !is.numeric(climate$municipio_geocodigo) ||
      !is.character(cases$CID10) || anyNA(cases$CID10)) {
    stop("Case and climate keys and counts must use valid numeric and disease types.",
         call. = FALSE)
  }
  if (require_population &&
      (!is.numeric(cases$pop) || anyNA(cases$pop) || any(cases$pop <= 0))) {
    stop("Every case row must have a positive population.", call. = FALSE)
  }
  if (any(cases$casos < 0) || any(cases$cas_prov < 0) || any(cases$cas_lab < 0)) {
    stop("Case counts must not be negative.", call. = FALSE)
  }
  .validate_epiweek(c(cases$SE, climate$SE))
  .validate_unique_keys(cases, c("municipio_geocodigo", "CID10", "SE"), "cases")
  .validate_unique_keys(climate, c("municipio_geocodigo", "SE"), "climate")

  if (!is.null(parameters)) {
    .validate_columns(
      parameters,
      c("municipio_geocodigo", "limiar_preseason", "limiar_epidemico",
        "varcli", "clicrit", "cid10", "codmodelo"),
      "parameters"
    )
    if (anyDuplicated(names(parameters)) || !is.numeric(parameters$municipio_geocodigo) ||
        !is.character(parameters$cid10) || anyNA(parameters$cid10)) {
      stop("`parameters` must have valid municipality and disease identifiers.",
           call. = FALSE)
    }
    .validate_unique_keys(parameters, c("municipio_geocodigo", "cid10"), "parameters")
  }
  invisible(list(cases = cases, climate = climate, parameters = parameters))
}

.filter_case_records_for_group <- function(records, municipality, disease,
                                           geocode_column, disease_column) {
  if (is.null(records)) return(NULL)
  keep <- records[[geocode_column]] == municipality
  if (!is.null(disease_column)) {
    keep <- keep & records[[disease_column]] == disease
  }
  records[!is.na(keep) & keep, , drop = FALSE]
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
#' @param cases Weekly case data keyed by `municipio_geocodigo`, disease, and
#'   epidemiological week. The legacy `cidade` alias is accepted at the input
#'   boundary.
#' @param climate Weekly municipal climate data keyed by `SE` and
#'   `municipio_geocodigo`. The legacy `geocodigo` alias is accepted at the input
#'   boundary.
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
  canonical <- .canonicalize_pipeline_geocodes(cases, climate)
  cases <- canonical$cases
  climate <- canonical$climate
  cases$CID10 <- vapply(cases$CID10, .normalize_disease, character(1))
  parameters$cid10 <- vapply(as.character(parameters$cid10), .normalize_disease,
                             character(1))
  .validate_pipeline_inputs(cases, climate, parameters)
  # Keep the established calculation and result schema stable while accepting
  # canonical identifiers at the public API boundary.
  cases$cidade <- cases$municipio_geocodigo
  cases$municipio_geocodigo <- NULL
  climate$geocodigo <- climate$municipio_geocodigo
  climate$municipio_geocodigo <- NULL
  if (length(report_week) != 1L || !is.numeric(report_week) || is.na(report_week)) {
    stop("`report_week` must be one epidemiological week.", call. = FALSE)
  }
  .validate_epiweek(report_week)
  if (nowcasting == "bayesian" && is.null(case_records)) {
    stop("Bayesian nowcasting requires in-memory `case_records`.", call. = FALSE)
  }
  record_geocode <- NULL
  record_disease <- NULL
  if (!is.null(case_records)) {
    if (!is.data.frame(case_records)) {
      stop("`case_records` must be a data frame.", call. = FALSE)
    }
    record_geocode <- intersect(
      c("municipio_geocodigo", "cidade", "geocodigo"), names(case_records)
    )
    record_disease <- intersect(c("cid10_codigo", "cid10", "CID10"), names(case_records))
    if (length(record_geocode) != 1L || anyDuplicated(names(case_records))) {
      stop("`case_records` must have one municipality identifier column.", call. = FALSE)
    }
    record_geocode <- record_geocode[[1L]]
    if (length(record_disease) > 1L) {
      stop("`case_records` must have at most one disease identifier column.",
           call. = FALSE)
    }
    if (length(record_disease)) {
      record_disease <- record_disease[[1L]]
      case_records[[record_disease]] <- vapply(
        as.character(case_records[[record_disease]]), .normalize_disease, character(1)
      )
    } else {
      record_disease <- NULL
    }
  }

  input <- dplyr::left_join(cases, climate, by = c("cidade" = "geocodigo", "SE" = "SE"))
  if (!"tweet" %in% names(input)) input$tweet <- NA_real_
  input$inc <- calculate_incidence(input$casos, input$pop)
  groups <- unique(data.frame(
    municipio_geocodigo = as.numeric(input$cidade), cid10 = input$CID10,
    stringsAsFactors = FALSE
  ))
  parameter_groups <- data.frame(
    municipio_geocodigo = as.numeric(parameters$municipio_geocodigo),
    cid10 = parameters$cid10, stringsAsFactors = FALSE
  )
  group_key <- function(x) paste(x$municipio_geocodigo, x$cid10, sep = "\r")
  if (!all(group_key(groups) %in% group_key(parameter_groups))) {
    stop("Parameters are missing for one or more municipality-disease combinations.",
         call. = FALSE)
  }
  if (!all(group_key(parameter_groups) %in% group_key(groups))) {
    stop("Parameters contain municipality-disease combinations absent from `cases`.",
         call. = FALSE)
  }
  groups <- groups[order(groups$municipio_geocodigo, groups$cid10), , drop = FALSE]
  rownames(groups) <- NULL

  if (!is.null(case_records) && is.null(record_disease)) {
    group_counts <- table(groups$municipio_geocodigo)
    record_cities <- unique(as.character(case_records[[record_geocode]]))
    if (any(group_counts[names(group_counts) %in% record_cities] > 1L)) {
      stop("`case_records` must identify disease when a municipality has multiple diseases.",
           call. = FALSE)
    }
  }

  compute_group <- function(group_index) {
    group <- groups[group_index, , drop = FALSE]
    city <- group$municipio_geocodigo[[1L]]
    disease <- group$cid10[[1L]]
    city_data <- input[input$cidade == city & input$CID10 == disease &
                         input$SE <= report_week, , drop = FALSE]
    city_parameters <- parameters[
      parameters$municipio_geocodigo == city & parameters$cid10 == disease,
      , drop = FALSE
    ]
    if (nrow(city_parameters) != 1L) {
      stop("Each municipality-disease combination must have exactly one parameter row.",
           call. = FALSE)
    }
    records <- .filter_case_records_for_group(
      case_records, city, disease, record_geocode, record_disease
    )
    group_seed <- if (is.null(seed)) NULL else seed + group_index - 1L
    adjusted <- nowcast_cases(
      city_data, case_records = records, method = nowcasting, report_week = report_week,
      seed = group_seed, workers = workers, verbose = verbose
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

  computed <- .with_seed(seed, .map_workers(seq_len(nrow(groups)), compute_group, workers))
  data <- dplyr::bind_rows(lapply(computed, `[[`, "data"))
  alerts <- dplyr::bind_rows(lapply(computed, `[[`, "alerts"))
  diagnostics <- list(
    cities = length(unique(groups$municipio_geocodigo)),
    municipality_disease_groups = nrow(groups),
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
