check_consumer_contract <- function(backend, test_env = parent.frame()) {
  withr::local_options(list(lifecycle_verbosity = "quiet"), .local_envir = test_env)
  conn <- consumer_fixture_database(backend, test_env)
  had_connection <- exists("con", envir = .GlobalEnv, inherits = FALSE)
  if (had_connection) previous_connection <- get("con", envir = .GlobalEnv)
  assign("con", conn, envir = .GlobalEnv)
  withr::defer({
    if (had_connection) assign("con", previous_connection, envir = .GlobalEnv) else
      rm("con", envir = .GlobalEnv)
  }, envir = test_env)
  scenarios <- list(
    state = list(disease = "A90", fail_state = FALSE, failed_cities = numeric()),
    recovery = list(disease = "A90", fail_state = TRUE, failed_cities = numeric()),
    isolation = list(disease = "A90", fail_state = TRUE, failed_cities = 3303302),
    chikungunya = list(disease = "A92.0", fail_state = FALSE, failed_cities = numeric()),
    zika = list(disease = "A92.8", fail_state = FALSE, failed_cities = numeric())
  )
  directory <- withr::local_tempdir(.local_envir = test_env)
  for (name in names(scenarios)) {
    scenario <- scenarios[[name]]
    make <- function(reference) load_pinned_consumer(conn, reference,
                 scenario$fail_state, scenario$failed_cities)
    candidate <- make(FALSE)
    reference <- make(TRUE)
    run <- function(consumer) consumer_known_warnings(consumer$run_disease_pipeline(
      c(3304557, 3303302), scenario$disease, "none", name, "RJ"
    ))
    observed <- run(candidate)
    expected <- run(reference)
    observed_publication <- consumer_publication(candidate, observed,
                             file.path(directory, name, "candidate"), scenario$disease)
    expected_publication <- consumer_publication(reference, expected,
                             file.path(directory, name, "reference"), scenario$disease)
    report <- compare_epidemiological_frames(expected_publication$history,
                                              observed_publication$history)
    expect_true(all(report$status == "match"), info = paste(backend, name))
    data_report <- compare_epidemiological_frames(expected_publication$data,
                                                   observed_publication$data)
    expect_true(all(data_report$status == "match"), info = paste(backend, name, "RData"))
    expect_true(file.exists(observed_publication$sql))
    expect_named(observed_publication$decoded, names(expected_publication$decoded))
    if (scenario$fail_state) expect_true(any(grepl("fallback", candidate$events)))
    if (length(scenario$failed_cities)) {
      expect_false(any(observed$restab$municipio_geocodigo %in% scenario$failed_cities))
    }
    key <- switch(scenario$disease, A90 = "alert_dengue", A92.0 = "alert_chik",
                    A92.8 = "alert_zika")
    table <- consumer_history_table(conn, observed$restab, key)
    suppressMessages(write_alerta(observed$restab, datasource = conn))
    suppressMessages(write_alerta(observed$restab, datasource = conn))
    stored <- consumer_ordered_rows(DBI::dbReadTable(conn, table))
    physical <- consumer_ordered_rows(consumer_physical_history(observed$restab))
    expect_true(all(compare_epidemiological_frames(physical, stored)$status == "match"))
    if (backend == "postgres") {
      for (sql in c(expected_publication$sql, observed_publication$sql)) {
        DBI::dbExecute(conn, paste("DELETE FROM", DBI::dbQuoteIdentifier(conn, table)))
        DBI::dbExecute(conn, paste(readLines(sql), collapse = "\n"), immediate = TRUE)
        replayed <- consumer_ordered_rows(DBI::dbReadTable(conn, table))
        expect_true(all(compare_epidemiological_frames(physical, replayed)$status == "match"),
                    info = paste(backend, name, basename(dirname(sql))))
      }
    }
    record_consumer_evidence(backend, name, report, observed_publication,
                              expected_publication)
  }
  failed <- load_pinned_consumer(conn, fail_state = TRUE,
                                  failed_cities = c(3304557, 3303302))
  expect_error(failed$run_disease_pipeline(c(3304557, 3303302), "A90", "none", "dengue", "RJ"),
               "All municipality executions failed")
}

test_that("pinned consumer preserves state, recovery and artifacts on SQLite", {
  check_consumer_contract("sqlite", environment())
})

test_that("pinned consumer replays reference and candidate SQL on PostgreSQL", {
  check_consumer_contract("postgres", environment())
})

test_that("SQL export retains numeric precision instead of formatting a character matrix", {
  snapshots <- epidemiological_snapshots(.reference_root, "dengue")
  history <- snapshots$candidate$history[1, ]
  history$p_rt1 <- 0.12345678912345
  sql_file <- withr::local_tempfile()
  suppressMessages(write_alerta(history, writetofile = TRUE, arq = sql_file))
  sql <- readLines(sql_file)
  header <- sub("^.*?\\((.*?)\\) VALUES.*$", "\\1", sql, perl = TRUE)
  columns <- gsub('"', "", strsplit(header, ", ", fixed = TRUE)[[1]], fixed = TRUE)
  values <- sub("^.*?\\) VALUES \\((.*?)\\) ON CONFLICT.*$", "\\1", sql, perl = TRUE)
  values <- strsplit(values, ", ", fixed = TRUE)[[1]]
  value <- gsub("^'|'$", "", values[match("p_rt1", columns)])
  expect_equal(as.numeric(value), history$p_rt1,
                tolerance = 1e-12)
})
