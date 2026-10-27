#' AlertTools: utilities for the InfoDengue alert system
#'
#' AlertTools retrieves and organizes epidemiological and climate data,
#' adjusts notification series, estimates epidemiological indicators, and
#' calculates alert levels. Database access is isolated behind an internal DBI
#' repository, while computational functions operate on in-memory data frames.
#'
#' @keywords internal
#' @importFrom assertthat assert_that
#' @importFrom graphics legend lines
#' @importFrom dplyr "%>%" across all_of arrange bind_rows case_when filter full_join
#'   group_by join_by left_join mutate n rename select summarise
#'   summarize
#' @importFrom purrr map transpose
#' @importFrom stats arima dnorm fitted lm median pbeta plnorm predict
#'   qbeta quantile rnbinom setNames
#' @importFrom stringr str_c str_replace_all
#' @importFrom tibble rowid_to_column tibble
#' @importFrom tidyr drop_na gather replace_na spread
#' @importFrom utils tail
#' @importFrom zoo na.approx rollapply
#' @aliases AlertTools-package
#' @name AlertTools
"_PACKAGE"

utils::globalVariables(c(
  ".", "alerta", "ano_notif", "ano_sinpri", "cas_desc", "cas_lab",
  "cas_prov", "casos", "Casos", "casos_est", "CID10", "cidade",
  "classi_fin", "cotrue", "criterio", "cytrue", "data_dia",
  "dataini", "Date", "Delay", "delay_epiweek", "dt_digita",
  "dt_digita_epiweek", "dt_digita_epiyear", "dt_sin_pri",
  "dt_sinpri_aux", "dt_sinpri_epiweek", "dt_sinpri_epiyear",
  "dt_sinpri_week", "estacao", "Estacao_wu_estacao_id", "geocode",
  "geocodigo", "level", "localidade", "mun", "municipio_geocodigo",
  "Municipio_geocodigo", "nome", "numero", "p1", "pop", "populacao",
  "pos", "pre", "se_notif", "se_sin_pri", "tcasesICmax",
  "tcasesICmin", "tcasesmed", "temp_max", "temp_med", "temp_min",
  "Time", "tweet", "umid_max", "umid_med", "umid_min", "veryhigh",
  "write.parameters", "year"
))
