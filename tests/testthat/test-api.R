test_that("new_alert_inputs validates, normalizes, and orders public schemas", {
  cases <- read_fixture("cases.csv")
  climate <- read_fixture("climate.csv")
  population <- unique(data.frame(
    municipio_geocodigo = cases$cidade, populacao = cases$pop, year = 2025
  ))
  cases$pop <- NULL
  inputs <- new_alert_inputs(
    cases[sample(nrow(cases)), ], climate[sample(nrow(climate)), ], population,
    metadata = list(source = "fixture")
  )

  expect_s3_class(inputs, "alerttools_inputs")
  expect_named(inputs, c("cases", "climate", "population", "case_records", "metadata"))
  expect_equal(inputs$cases$SE, sort(inputs$cases$SE))
  expect_equal(inputs$climate$SE, sort(inputs$climate$SE))
  expect_true(all(inputs$cases$pop == 10000))
  expect_output(printed <- print(inputs), "<alerttools_inputs>", fixed = TRUE)
  expect_identical(printed, inputs)

  duplicate <- rbind(cases, cases[1, ])
  duplicate$pop <- 10000
  expect_error(new_alert_inputs(duplicate, climate), "keys.*unique")
  expect_error(new_alert_inputs(cases, climate), "positive population")
})

test_that("run_alert_pipeline is equivalent to the approved pure pipeline", {
  cases <- read_fixture("cases.csv")
  climate <- read_fixture("climate.csv")
  parameters <- read_fixture("parameters.csv", stringsAsFactors = FALSE)
  inputs <- new_alert_inputs(cases, climate, metadata = list(report_week = 202106))

  expected <- alerttools_pipeline(
    cases, climate, parameters, report_week = 202106,
    nowcasting = "none", workers = 1, seed = 42
  )
  observed <- run_alert_pipeline(
    inputs, parameters, nowcast = "none", workers = 1, seed = 42
  )
  expect_identical(observed, expected)
  expect_error(run_alert_pipeline(list(), parameters), "new_alert_inputs")
})

test_that("database API fetches a complete, explicitly bounded input bundle", {
  connection <- sqlite_fixture_connection(environment())
  inputs <- fetch_alert_inputs(
    conn = connection, geocodes = 3304557, disease = "dengue",
    start_week = 202052, report_week = 202101
  )
  parameters <- fetch_alert_parameters(connection, 3304557, "A90")

  expect_s3_class(inputs, "alerttools_inputs")
  expect_equal(inputs$metadata$start_date, as.Date("2020-12-20"))
  expect_equal(inputs$metadata$end_date, as.Date("2021-01-09"))
  expect_equal(inputs$metadata$disease, "A90")
  expect_equal(nrow(inputs$case_records), 4)
  expect_equal(parameters$municipio_geocodigo, 3304557)
  expect_equal(parameters$cid10, "A90")
  expect_error(
    fetch_alert_inputs(connection, 3304557, start_week = 202052),
    "report_week.*end_date"
  )
  expect_error(
    fetch_alert_inputs(connection, 3304557, start_week = 202052,
                       report_week = 202101, end_date = as.Date("2021-01-10")),
    "different weeks"
  )
})

test_that("new persistence API writes results and parameters explicitly", {
  connection <- sqlite_fixture_connection(environment())
  create_alert_history_table(connection)
  cases <- read_fixture("cases.csv")
  climate <- read_fixture("climate.csv")
  parameters <- read_fixture("parameters.csv", stringsAsFactors = FALSE)
  result <- run_alert_pipeline(
    new_alert_inputs(cases, climate), parameters,
    report_week = 202106, nowcast = "none"
  )

  expect_invisible(write_alert_results(
    connection, result, version_date = as.Date("2026-09-04"), conflict = "update"
  ))
  expect_equal(DBI::dbGetQuery(connection, "SELECT COUNT(*) AS n FROM Historico_alerta")$n, 8)
  expect_error(write_alert_results(connection, result), "version_date")

  withr::local_options(list(lifecycle_verbosity = "warning"))
  history <- as_alert_history(result, as.Date("2026-09-04"))
  expect_warning(
    write_alerta(history, datasource = connection, conflict = "update"),
    "write_alert_results"
  )

  changed <- transform(parameter_fixture(), limiar_preseason = 23)
  persisted <- upsert_alert_parameters(connection, changed, conflict = "update")
  expect_equal(persisted$limiar_preseason, 23)
})

test_that("legacy wrappers warn and preserve argument and result adapters", {
  withr::local_options(list(lifecycle_verbosity = "warning"))
  connection <- sqlite_fixture_connection(environment())

  modern_cases <- fetch_cases(
    connection, 3304557, "dengue",
    as.Date("2020-12-20"), as.Date("2021-01-09")
  )
  expect_warning(
    legacy_cases <- getCases(
      cities = 3304557, cid10 = "A90", firstday = as.Date("2020-12-20"),
      lastday = as.Date("2021-01-09"), dataini = "notific", datasource = connection
    ),
    "fetch_cases"
  )
  expect_identical(legacy_cases, modern_cases)
  expect_warning(
    legacy_records <- getCaseslist(
      3304557, firstday = as.Date("2020-12-20"),
      lastday = as.Date("2021-01-09"), datasource = connection
    ),
    "fetch_case_records"
  )
  expect_identical(
    legacy_records,
    fetch_case_records(connection, 3304557, start_date = as.Date("2020-12-20"),
                       end_date = as.Date("2021-01-09"))
  )
  expect_warning(
    legacy_climate <- getClima(
      3304557, vars = c("temp_min", "temp_max"), iniSE = 202052,
      finalday = as.Date("2021-01-02"), datasource = connection
    ),
    "fetch_climate"
  )
  expect_identical(
    legacy_climate,
    fetch_climate(connection, 3304557, as.Date("2020-12-20"),
                  as.Date("2021-01-02"), c("temp_min", "temp_max"))
  )

  modern_parameters <- fetch_alert_parameters(connection, 3304557, "dengue")
  expect_warning(
    legacy_parameters <- read.parameters(3304557, cid10 = "A90", datasource = connection),
    "fetch_alert_parameters"
  )
  expect_identical(legacy_parameters, modern_parameters)
  expect_warning(
    write_parameters(3304557, "A90", modern_parameters, overwrite = TRUE,
                     datasource = connection),
    "upsert_alert_parameters"
  )
  expect_warning(
    expect_error(
      pipe_infodengue(3304557, datarelatorio = 202101, datasource = NULL),
      "valid DBI connection"
    ),
    "run_alert_pipeline"
  )
})

test_that("snake_case computational aliases preserve baseline results", {
  withr::local_options(list(lifecycle_verbosity = "quiet"))
  cases <- read_fixture("cases.csv")
  criteria_values <- structure(
    as.character(read_fixture("parameters.csv", stringsAsFactors = FALSE)[1, ]),
    names = names(read_fixture("parameters.csv", stringsAsFactors = FALSE))
  )
  expect_identical(
    estimate_rt(cases, count = "casos", distribution = "normal",
                mean_generation_time = 3, sd_generation_time = 1),
    Rt(cases, count = "casos", gtdist = "normal", meangt = 3, sdgt = 1)
  )
  expect_identical(
    nowcast_cases(cases, method = "none", report_week = 202106),
    adjustIncidence(cases, method = "none", nowSE = 202106)
  )
  rules <- define_alert_rules("Af", criteria_values)
  expect_identical(rules, setCriteria("Af", criteria_values))
})

test_that("legacy computational wrappers identify replacements and remain equivalent", {
  withr::local_options(list(lifecycle_verbosity = "warning"))
  cases <- read_fixture("cases.csv")
  climate <- read_fixture("climate.csv")
  parameters <- read_fixture("parameters.csv", stringsAsFactors = FALSE)

  modern_rt <- estimate_rt(cases, count = "casos", distribution = "normal",
                           mean_generation_time = 3, sd_generation_time = 1)
  expect_warning(
    legacy_rt <- Rt(cases, count = "casos", gtdist = "normal", meangt = 3, sdgt = 1),
    "estimate_rt"
  )
  expect_identical(legacy_rt, modern_rt)

  modern_nowcast <- nowcast_cases(cases, method = "none", report_week = 202106)
  expect_warning(
    legacy_nowcast <- adjustIncidence(cases, method = "none", nowSE = 202106),
    "nowcast_cases"
  )
  expect_identical(legacy_nowcast, modern_nowcast)

  values <- structure(as.character(parameters[1, ]), names = names(parameters))
  modern_rules <- define_alert_rules("Af", values)
  expect_warning(legacy_rules <- setCriteria("Af", values), "define_alert_rules")
  expect_identical(legacy_rules, modern_rules)

  result <- run_alert_pipeline(
    new_alert_inputs(cases, climate), parameters,
    report_week = 202106, nowcast = "none"
  )
  modern_alert <- classify_alerts(result$data, modern_rules,
                                   missing = "last", minimum_history = 4)
  expect_warning(
    legacy_alert <- fouralert(result$data, modern_rules, miss = "last", dy = 4),
    "classify_alerts"
  )
  expect_identical(legacy_alert, modern_alert)
  modern_history <- as_alert_history(result, as.Date("2026-09-04"))
  expect_warning(
    legacy_history <- tabela_historico(result, versao = as.Date("2026-09-04")),
    "as_alert_history"
  )
  expect_identical(legacy_history, modern_history)
})
