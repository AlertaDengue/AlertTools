# Real-engine reference adapter; immutable historical functions are not modified.
inla_reference_sections <- function(root, fixture, version_date = as.Date("2026-10-29")) {
  reference <- load_epidemiological_reference(root, fixture$parameters)
  reference$inla <- INLA::inla
  reference$f <- INLA::f
  reference$epiweek <- lubridate::epiweek
  reference$epiyear <- lubridate::epiyear
  groups <- fixture$parameters[order(fixture$parameters$municipio_geocodigo,
                                     fixture$parameters$cid10), ]
  output <- lapply(seq_len(nrow(groups)), function(i) {
    parameter <- groups[i, , drop = FALSE]
    city <- parameter$municipio_geocodigo
    disease <- parameter$cid10
    data <- fixture$cases[fixture$cases$cidade == city & fixture$cases$CID10 == disease, ]
    data <- dplyr::left_join(data, fixture$climate,
                              by = c("cidade" = "geocodigo", "SE" = "SE"))
    data$tweet <- NA_real_
    data$inc <- data$casos / data$pop * 1e5
    records <- fixture$case_records[fixture$case_records$municipio_geocodigo == city &
                                       fixture$case_records$cid10_codigo == disease, ]
    group_seed <- fixture$seed + i - 1L
    # The unchanged historical sampler has no seed argument. Only its external
    # engine binding is adapted to the candidate's explicit seed/thread protocol.
    reference$inla.posterior.sample <- function(n, result, ...) {
      set.seed(group_seed)
      INLA::inla.posterior.sample(n, result, seed = group_seed,
                                  num.threads = "1:1", parallel.configs = FALSE, ...)
    }
    old <- AlertTools:::.with_seed(group_seed, suppressMessages(reference$calc.alerta(
      city, list(data, fixture$report_week, "bayesian", parameter, records)
    )))
    old_data <- tail(old$data, 8)
    if (!any(old_data$tcasesmed != old_data$casos)) stop("Reference skipped real INLA")
    list(data = old$data, alerts = old$indices,
         history = suppressMessages(reference$tabela_historico(
           setNames(list(old), as.character(city)), versao = version_date)))
  })
  setNames(lapply(c("data", "alerts", "history"), function(section) {
    dplyr::bind_rows(lapply(output, `[[`, section))
  }), c("data", "alerts", "history"))
}
