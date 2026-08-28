library(assertthat)
library(dplyr)
library(purrr)
library(stringr)

source(testthat::test_path("..", "..", "R", "utility_tools.R"))
source(testthat::test_path("..", "..", "R", "alert_functions.R"))

historical_alert_input <- function(include_tweet = TRUE) {
  data <- data.frame(
    cidade = 3304557,
    Localidade_id = 0,
    SE = 202401,
    nome = "Rio de Janeiro",
    CID10 = "A90",
    casos = 10,
    tcasesmed = 12,
    tcasesICmin = 8,
    tcasesICmax = 16,
    cas_prov = 9,
    p1 = 0.5,
    localidade = NA,
    Rt = 1.2,
    pop = 1000000,
    temp_min = 20,
    temp_med = 25,
    temp_max = 30,
    umid_min = 40,
    umid_med = 50,
    umid_max = 60
  )
  if (include_tweet) data$tweet <- 42

  structure(
    list(
      data = data,
      indices = data.frame(cytrue = 1, cotrue = 1, level = 2)
    ),
    class = "alerta"
  )
}

test_that("tabela_historico excludes legacy tweet input", {
  alert_environment <- environment(tabela_historico)
  original_read_parameters <- get("read.parameters", envir = alert_environment)
  original_SE2date <- get("SE2date", envir = alert_environment)
  assign("read.parameters", function(...) {
    data.frame(
      municipio_geocodigo = 3304557,
      limiar_preseason = 5,
      limiar_epidemico = 15
    )
  }, envir = alert_environment)
  on.exit(assign("read.parameters", original_read_parameters,
                 envir = alert_environment), add = TRUE)
  assign("SE2date", function(se) {
    data.frame(SE = se, ini = as.Date("2024-01-01"))
  }, envir = alert_environment)
  on.exit(assign("SE2date", original_SE2date, envir = alert_environment),
          add = TRUE)

  with_tweet <- tabela_historico(historical_alert_input(TRUE))
  without_tweet <- tabela_historico(historical_alert_input(FALSE))

  expect_false("tweet" %in% names(with_tweet))
  expect_false("tweet" %in% names(without_tweet))
  expect_true(all(c("CID10", "casos_est", "Rt", "nivel_inc") %in% names(with_tweet)))
})

historical_alert_row <- function(cid10) {
  data.frame(
    SE = 202401,
    data_iniSE = as.Date("2024-01-01"),
    casos_est = 12,
    casos_est_min = 8,
    casos_est_max = 16,
    casos = 10,
    casprov = 9,
    municipio_geocodigo = 3304557,
    p_rt1 = 0.5,
    p_inc100k = 1.2,
    Localidade_id = 0,
    nivel = 2,
    id = "33045570202401",
    versao_modelo = "2024-01-01",
    municipio_nome = "Rio de Janeiro",
    Rt = 1.2,
    pop = 1000000,
    receptivo = 1,
    transmissao = 1,
    nivel_inc = 1,
    temp_min = 20,
    temp_med = 25,
    temp_max = 30,
    umid_min = 40,
    umid_med = 50,
    umid_max = 60,
    CID10 = cid10
  )
}

test_that("write_alerta SQL omits tweet for every disease table", {
  expected_tables <- c(
    A90 = "Historico_alerta",
    "A92.0" = "Historico_alerta_chik",
    "A92.8" = "Historico_alerta_zika"
  )

  for (cid10 in names(expected_tables)) {
    sql_file <- tempfile(fileext = ".sql")
    write_alerta(historical_alert_row(cid10), writetofile = TRUE, arq = sql_file)
    sql <- paste(readLines(sql_file, warn = FALSE), collapse = "\n")

    expect_false(grepl("\\btweet\\b", sql, ignore.case = TRUE))
    expect_match(sql, expected_tables[[cid10]], fixed = TRUE)
    expect_match(sql, "casos_est", fixed = TRUE)
    expect_match(sql, "\\\"Rt\\\"")
  }
})
