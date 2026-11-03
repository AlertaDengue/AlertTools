# Run from the repository root. Each configuration has an isolated R process.
# Supervisor uses Python 3 and Unix ps; no production database is used.
args <- commandArgs(trailingOnly = TRUE)
if (length(args) && args[[1]] == "--worker") {
  devtools::load_all(quiet = TRUE)
  read <- function(name) utils::read.csv(file.path("tests/testthat/fixtures", name))
  cases <- read("cases.csv")
  climate <- read("climate.csv")
  parameters <- read("parameters.csv")
  copies <- lapply(seq_len(200), function(i) {
    code <- 3304557 + i
    list(cases = transform(cases, cidade = code),
         climate = transform(climate, geocodigo = code),
         parameters = transform(parameters, municipio_geocodigo = code))
  })
  combine <- function(name) do.call(rbind, lapply(copies, `[[`, name))
  inputs <- new_alert_inputs(combine("cases"), combine("climate"))
  parameters <- combine("parameters")
  timing <- system.time(result <- run_alert_pipeline(
    inputs, parameters, report_week = 202106, nowcast = "none", seed = 42,
    municipality_workers = as.integer(args[[2]]), inla_threads = as.integer(args[[3]])
  ))
  utils::write.csv(data.frame(elapsed_seconds = unname(timing[["elapsed"]]),
               input_bytes = as.numeric(object.size(list(inputs, parameters))),
               rows = nrow(result$data), platform = R.version$platform,
               r_version = as.character(getRversion())), args[[4]], row.names = FALSE)
} else {
  output <- if (length(args)) args[[1]] else "benchmark-concurrency.csv"
  status <- system2("python3", c("tools/benchmark-concurrency.py", shQuote(output)))
  if (status != 0L) stop("Concurrency benchmark failed.")
}
