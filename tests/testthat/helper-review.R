review_result <- function(city = 3304557, threshold = 10) {
  cases <- read_fixture("cases.csv")
  cases$cidade <- city
  climate <- read_fixture("climate.csv")
  climate$geocodigo <- city
  parameters <- read_fixture("parameters.csv", stringsAsFactors = FALSE)
  parameters$municipio_geocodigo <- city
  parameters$limiar_preseason <- threshold
  alerttools_pipeline(cases, climate, parameters, report_week = 202106)
}
