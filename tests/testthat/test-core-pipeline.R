test_that("pure pipeline runs deterministically from in-memory fixtures", {
  withr::local_dir(withr::local_tempdir())
  cases <- read_fixture("cases.csv")
  climate <- read_fixture("climate.csv")
  parameters <- read_fixture("parameters.csv", stringsAsFactors = FALSE)
  expected <- read_fixture("expected-pipeline.csv", stringsAsFactors = FALSE)
  before <- list.files(all.files = TRUE)

  first <- alerttools_pipeline(
    cases, climate, parameters,
    report_week = 202106, nowcasting = "none", workers = 1, seed = 42
  )
  second <- alerttools_pipeline(
    cases, climate, parameters,
    report_week = 202106, nowcasting = "none", workers = 1, seed = 42
  )

  expect_s3_class(first, "alerttools_result")
  expect_named(first, c("data", "alerts", "parameters", "metadata", "diagnostics"))
  expect_identical(first, second)
  expect_equal(list.files(all.files = TRUE), before)
  expect_equal(nrow(first$data), nrow(cases))
  expect_equal(nrow(first$alerts), nrow(cases))
  observed <- first$data[c("SE", "cidade", "CID10", "casos", "tcasesmed",
                           "inc", "Rt", "lwr", "upr", "p1")]
  observed$level <- first$alerts$level
  expect_equal(observed, expected, tolerance = 1e-6)
  expect_equal(first$metadata$workers, 1L)
  expect_true(first$metadata$deterministic)
  expect_equal(first$diagnostics$missing_climate, 0)
})

test_that("pipeline keeps diseases separate within the same municipality", {
  cases <- read_fixture("cases.csv")
  chik_cases <- cases
  chik_cases$CID10 <- "A92"
  chik_cases$casos <- chik_cases$casos + 1
  chik_cases$cas_prov <- chik_cases$cas_prov + 1
  cases <- rbind(cases, chik_cases)
  climate <- read_fixture("climate.csv")
  parameters <- read_fixture("parameters.csv", stringsAsFactors = FALSE)
  chik_parameters <- parameters
  chik_parameters$cid10 <- "A92.0"
  parameters <- rbind(parameters, chik_parameters)

  result <- alerttools_pipeline(
    cases, climate, parameters, report_week = 202106, nowcasting = "none"
  )

  expect_equal(nrow(result$data), 16)
  expect_equal(result$diagnostics$cities, 1L)
  expect_equal(result$diagnostics$municipality_disease_groups, 2L)
  expect_setequal(unique(result$data$CID10), c("A90", "A92.0"))
  expect_equal(as.integer(table(result$data$CID10)), c(8L, 8L))

  history <- as_alert_history(result, as.Date("2026-09-04"))
  expect_equal(nrow(history), 16)
  expect_setequal(unique(history$CID10), c("A90", "A92.0"))
  expect_equal(anyDuplicated(history[c("municipio_geocodigo", "CID10", "SE")]), 0L)
  expect_equal(length(unique(history$id)), 16)
  connection <- sqlite_fixture_connection(environment())
  expect_error(
    write_alert_results(connection, result, version_date = as.Date("2026-09-04")),
    "one disease per call"
  )

  expect_error(
    alerttools_pipeline(cases, climate, parameters[-2, ], report_week = 202106),
    "missing for one or more municipality-disease"
  )
  duplicate_parameters <- rbind(parameters, parameters[1, ])
  expect_error(
    alerttools_pipeline(cases, climate, duplicate_parameters, report_week = 202106),
    "parameters.*keys.*unique"
  )

  records <- read_fixture("notifications.csv", stringsAsFactors = FALSE)
  records_other_disease <- records
  records_other_disease$cid10_codigo <- "A92.0"
  mixed_records <- rbind(records, records_other_disease)
  normalized_records <- mixed_records
  normalized_records$cid10_codigo <- vapply(
    normalized_records$cid10_codigo, AlertTools:::.normalize_disease, character(1)
  )
  dengue_records <- AlertTools:::.filter_case_records_for_group(
    normalized_records, 3304557, "A90", "municipio_geocodigo", "cid10_codigo"
  )
  chik_records <- AlertTools:::.filter_case_records_for_group(
    normalized_records, 3304557, "A92.0", "municipio_geocodigo", "cid10_codigo"
  )
  expect_true(all(dengue_records$cid10_codigo == "A90"))
  expect_true(all(chik_records$cid10_codigo == "A92.0"))
  expect_error(
    alerttools_pipeline(cases, climate, parameters, case_records =
                          mixed_records[c("municipio_geocodigo", "dt_notific",
                                          "dt_sin_pri", "dt_digita")],
                        report_week = 202106),
    "must identify disease when a municipality has multiple diseases"
  )
})

test_that("alerttools_result methods expose a stable result contract", {
  withr::local_options(list(lifecycle_verbosity = "quiet"))
  cases <- read_fixture("cases.csv")
  climate <- read_fixture("climate.csv")
  parameters <- read_fixture("parameters.csv", stringsAsFactors = FALSE)
  result <- alerttools_pipeline(cases, climate, parameters, report_week = 202106)

  expect_output(printed <- print(result), "<alerttools_result>", fixed = TRUE)
  expect_identical(printed, result)
  overview <- summary(result)
  expect_named(overview, c("rows", "municipalities", "alert_levels", "metadata", "diagnostics"))
  expect_equal(overview$rows, 8)
  expect_equal(sum(overview$alert_levels), 8)
  flattened <- as.data.frame(result)
  expect_s3_class(flattened, "data.frame")
  expect_equal(nrow(flattened), 8)
  expect_true(all(c("SE", "cidade", "Rt", "level") %in% names(flattened)))

  history <- tabela_historico(result, versao = as.Date("2026-09-04"))
  expect_s3_class(history, "data.frame")
  expect_equal(nrow(history), 8)
  expect_true(all(c("data_iniSE", "municipio_geocodigo", "nivel_inc") %in% names(history)))
  expect_equal(nrow(tabela_historico(result)), 8)
})

test_that("alerttools_result constructor handles empty and invalid inputs", {
  empty <- new_alerttools_result(
    data.frame(), data.frame(), data.frame(),
    metadata = list(source = "fixture"), diagnostics = list(rows = 0L)
  )
  expect_s3_class(empty, "alerttools_result")
  expect_equal(nrow(as.data.frame(empty)), 0)

  expect_error(
    new_alerttools_result(data.frame(x = 1), data.frame(), data.frame(),
                          metadata = list(x = 1), diagnostics = list(x = 1)),
    "same number of rows"
  )
  expect_error(
    new_alerttools_result(data.frame(), data.frame(), data.frame(),
                          metadata = list(), diagnostics = unname(list(1))),
    "diagnostics.*named list"
  )
  duplicate_names <- structure(data.frame(x = 1, y = 2), names = c("x", "x"))
  expect_error(
    new_alerttools_result(duplicate_names, data.frame(level = 1), data.frame()),
    "duplicate column names"
  )
})

test_that("incidence is a pure validated calculation", {
  expect_equal(calculate_incidence(c(0, 10, NA), c(1000, 2000, 3000)),
               c(0, 500, NA))
  expect_error(calculate_incidence(-1, 1000), "negative")
  expect_error(calculate_incidence(1, 0), "positive")
  expect_error(calculate_incidence(1:2, 1000), "equal length")
})

test_that("pure pipeline validates schemas, workers and nowcasting inputs", {
  cases <- read_fixture("cases.csv")
  climate <- read_fixture("climate.csv")
  parameters <- read_fixture("parameters.csv", stringsAsFactors = FALSE)

  expect_error(alerttools_pipeline(cases[-1], climate, parameters),
               "cases.*missing columns")
  expect_error(alerttools_pipeline(cases, climate[-1], parameters),
               "climate.*missing columns")
  expect_error(alerttools_pipeline(cases, climate, parameters, workers = 0),
               "positive integer")
  expect_error(alerttools_pipeline(cases, climate, parameters,
                                   nowcasting = "bayesian"),
               "requires in-memory.*case_records")
})

test_that("seed scoping does not mutate caller random state", {
  set.seed(123)
  before <- .Random.seed
  value <- AlertTools:::.with_seed(999, stats::runif(3))
  expect_length(value, 3)
  expect_identical(.Random.seed, before)
})

test_that("portable worker controls are validated", {
  expect_error(GenTimeDist(rep(25, 60), workers = 0), "positive integer")
  expect_error(GenTimeDist(c(25, NA), smooth = "raw"), "finite numeric")
  expect_error(GenTimeDist(rep(25, 60), smooth = "other"), "sinusoidal.*raw")
  serial <- GenTimeDist(rep(25, 60), smooth = "raw", workers = 1)
  expect_s3_class(serial, "generationtime")
  expect_equal(dim(serial), c(11, 60))
})

test_that("computational entry points contain no database or file access", {
  functions <- list(alerttools_pipeline, adjustIncidence, bayesnowcasting,
                    Rt, setCriteria, fouralert, GenTimeDist,
                    AlertTools:::applymem)
  forbidden <- "DBI::|dbGetQuery|dbExecute|read\\.|writeLines|save\\(|load\\(|setwd\\("
  bodies <- vapply(functions, function(fun) paste(deparse(body(fun)), collapse = "\n"),
                   character(1))
  expect_false(any(grepl(forbidden, bodies)))
})
