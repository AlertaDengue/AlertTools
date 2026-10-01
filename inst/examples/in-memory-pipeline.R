library(AlertTools)

weeks <- epiweek_seq(202501, 202512)
case_counts <- c(0, 1, 2, 4, 8, 13, 18, 15, 9, 5, 2, 1)

cases <- data.frame(
  SE = weeks,
  cidade = 3304557,
  CID10 = "A90",
  casos = case_counts,
  cas_prov = case_counts,
  cas_lab = floor(case_counts / 2),
  localidade = 0,
  nome = "Município sintético",
  pop = 100000
)

climate <- data.frame(
  SE = weeks,
  geocodigo = 3304557,
  temp_min = c(20, 20, 21, 22, 23, 24, 24, 23, 22, 21, 20, 20),
  temp_med = c(25, 25, 26, 27, 28, 29, 29, 28, 27, 26, 25, 25),
  temp_max = c(30, 30, 31, 32, 33, 34, 34, 33, 32, 31, 30, 30),
  umid_min = 45,
  umid_med = 65,
  umid_max = 85
)

parameters <- data.frame(
  municipio_geocodigo = 3304557,
  limiar_preseason = 10,
  limiar_posseason = 5,
  limiar_epidemico = 100,
  varcli = "temp_min",
  clicrit = 22,
  varcli2 = NA_character_,
  clicrit2 = NA_real_,
  cid10 = "A90",
  codmodelo = "Af"
)

inputs <- new_alert_inputs(cases = cases, climate = climate)
result <- run_alert_pipeline(
  inputs = inputs,
  parameters = parameters,
  report_week = 202512,
  nowcast = "none",
  workers = 1
)

stopifnot(
  inherits(inputs, "alerttools_inputs"),
  inherits(result, "alerttools_result"),
  nrow(result$data) == length(weeks)
)
