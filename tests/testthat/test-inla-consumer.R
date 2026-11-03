test_that("real Bayesian consumer fallback isolates failures and preserves artifacts", {
  inla_test_protocol()
  fixture <- inla_validation_fixture(.inla_root, diseases = "A90")
  consumer <- new.env(parent = asNamespace("AlertTools"))
  manifest <- utils::read.csv(file.path(.inla_root,
                                         "tests/reference/consumer-27a03f26/manifest.csv"))
  files <- file.path(.inla_root, "tests/reference/consumer-27a03f26", manifest$file)
  expect_identical(reference_snapshot_md5(files), manifest$md5)
  sys.source(file.path(.inla_root, "tests/reference/consumer-27a03f26/functions.R"), consumer)
  consumer$log_msg <- function(...) invisible(NULL)
  consumer$report_epiweek <- fixture$report_week
  consumer$window_weeks <- 200
  consumer$report_end_date <- epiweek_start(fixture$report_week) + 6
  fail_state <- TRUE
  failed_cities <- numeric()
  consumer$tabela_historico <- function(...) tabela_historico(..., versao = as.Date("2026-10-29"))
  consumer$pipe_infodengue <- function(cities, ...) {
    if ((fail_state && length(cities) > 1L) || any(cities %in% failed_cities)) {
      stop("Injected failure before a municipality fit")
    }
    selected <- fixture
    selected$cases <- fixture$cases[fixture$cases$cidade %in% cities, ]
    selected$climate <- fixture$climate[fixture$climate$geocodigo %in% cities, ]
    selected$parameters <- fixture$parameters[fixture$parameters$municipio_geocodigo %in% cities, ]
    selected$case_records <- fixture$case_records[
      fixture$case_records$municipio_geocodigo %in% cities, ]
    AlertTools:::.as_legacy_alert_objects(run_inla_fixture(selected))
  }
  cities <- sort(unique(fixture$cases$cidade))
  run <- function() consumer$run_disease_pipeline(cities, "A90", "bayesian", "dengue", "RJ")
  res <- run()
  expect_setequal(res$restab$municipio_geocodigo, cities)
  expect_true(any(res$restab$casos_est != res$restab$casos))
  directory <- withr::local_tempdir()
  save(res, file = file.path(directory, "state.RData"))
  expect_identical(consumer$load_state_result(file.path(directory, "state.RData")), res)
  write_alerta(res$restab, writetofile = TRUE, arq = file.path(directory, "state.sql"))
  expect_true(file.exists(file.path(directory, "state.sql")))
  failed_cities <- cities[[1L]]
  partial <- run()
  expect_setequal(partial$restab$municipio_geocodigo, cities[-1L])
  failed_cities <- cities
  expect_error(run(), "All municipality executions failed")
  evidence <- Sys.getenv("ALERTTOOLS_INLA_EVIDENCE")
  if (nzchar(evidence)) file.copy(list.files(directory, full.names = TRUE), evidence, overwrite = TRUE)
})
