# Controlled real-engine protocol shared by tests and benchmark workers.
# No operational data or database connection is used.
inla_validation_fixture <- function(root, cities = c(3303302, 3304557),
                                     diseases = c("A90", "A92.0")) {
  read <- function(name) utils::read.csv(file.path(root, "tests/testthat/fixtures", name))
  report_week <- 202106
  end <- AlertTools::epiweek_start(report_week) + 6
  groups <- expand.grid(municipio_geocodigo = cities, cid10 = diseases,
                         stringsAsFactors = FALSE)
  groups <- groups[order(groups$municipio_geocodigo, groups$cid10), ]
  records <- cases <- climates <- parameters <- vector("list", nrow(groups))
  for (i in seq_len(nrow(groups))) {
    grid <- expand.grid(week = 0:59, delay = 0:4)
    grid <- grid[rep(seq_len(nrow(grid)), 2 + (grid$week + grid$delay + i) %% 3), ]
    onset <- AlertTools::epiweek_start(report_week) - grid$week * 7 + 2
    entered <- onset + grid$delay * 7
    records[[i]] <- data.frame(
      municipio_geocodigo = groups$municipio_geocodigo[i],
      cid10_codigo = groups$cid10[i], dt_sin_pri = onset,
      dt_notific = onset, dt_digita = entered
    )
    records[[i]] <- records[[i]][entered <= end, ]
    weeks <- AlertTools::as_epiweek(records[[i]]$dt_sin_pri)
    counts <- table(weeks)
    value <- read("cases.csv")[rep(1, length(counts)), ]
    value$SE <- as.numeric(names(counts))
    value$cidade <- groups$municipio_geocodigo[i]
    value$CID10 <- groups$cid10[i]
    value$casos <- as.numeric(counts)
    value$cas_prov <- value$casos
    value$cas_lab <- 0
    value$pop <- 100000
    cases[[i]] <- value
    climate <- read("climate.csv")[rep(1, nrow(value)), ]
    climate$SE <- value$SE
    climate$geocodigo <- value$cidade
    climates[[i]] <- climate
    parameter <- read("parameters.csv")
    parameter$municipio_geocodigo <- groups$municipio_geocodigo[i]
    parameter$cid10 <- groups$cid10[i]
    parameters[[i]] <- parameter
  }
  bind <- function(x) { value <- do.call(rbind, x); rownames(value) <- NULL; value }
  list(cases = bind(cases), climate = unique(bind(climates)),
       parameters = bind(parameters), case_records = bind(records),
       report_week = report_week, seed = 814L)
}

run_inla_fixture <- function(fixture, workers = 1L, threads = 1L) {
  result <- do.call(AlertTools::alerttools_pipeline,
                     c(fixture, list(nowcasting = "bayesian",
                                     municipality_workers = workers, inla_threads = threads)))
  for (city in unique(result$data$cidade)) {
    for (disease in unique(result$data$CID10[result$data$cidade == city])) {
      data <- result$data[result$data$cidade == city & result$data$CID10 == disease, ]
      # A successful call that silently returned original counts is not INLA evidence.
      recent <- tail(data, 8)
      if (!any(recent$tcasesmed != recent$casos) ||
          any(!is.finite(recent$tcasesmed)) ||
          any(recent$tcasesICmin > recent$tcasesICmax, na.rm = TRUE)) {
        stop("Real Bayesian estimates were not produced for ", city, "/", disease)
      }
    }
  }
  result
}

inla_result_sections <- function(result, version_date = as.Date("2026-10-29")) {
  history <- lapply(seq_len(nrow(result$parameters)), function(i) {
    parameter <- result$parameters[i, , drop = FALSE]
    rows <- result$data$cidade == parameter$municipio_geocodigo &
      result$data$CID10 == parameter$cid10
    single <- AlertTools::new_alerttools_result(result$data[rows, ],
                result$alerts[rows, ], parameter)
    AlertTools::tabela_historico(AlertTools:::.as_legacy_alert_objects(single),
                                 versao = version_date)
  })
  list(data = result$data, alerts = result$alerts,
       history = dplyr::bind_rows(history))
}

compare_inla_sections <- function(reference, candidate) {
  # Fixed before running: 1e-7 absolute tolerance; exact NA/text/alert categories.
  do.call(rbind, lapply(names(reference), function(section) {
    report <- compare_epidemiological_frames(reference[[section]], candidate[[section]],
                                               tolerance = 1e-7)
    cbind(section = section, report)
  }))
}
