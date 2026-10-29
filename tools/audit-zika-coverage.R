# Read-only operational audit. Connection settings are supplied through PG*.
args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 2L || !nzchar(Sys.getenv("PGDATABASE"))) {
  stop("Usage: PGDATABASE=... Rscript tools/audit-zika-coverage.R geocodes.csv report.csv")
}
input <- utils::read.csv(args[[1]])
if (!"municipio_geocodigo" %in% names(input)) {
  stop("Input CSV must contain municipio_geocodigo.")
}
geocodes <- input$municipio_geocodigo
if (!is.numeric(geocodes) || !length(geocodes) || anyNA(geocodes) ||
    any(!is.finite(geocodes) | geocodes != floor(geocodes)) ||
    any(geocodes < 1000000 | geocodes > 9999999) || anyDuplicated(geocodes)) {
  stop("Input must contain unique, nonmissing seven-digit numeric municipality codes.")
}
if (file.exists("DESCRIPTION") &&
    identical(unname(read.dcf("DESCRIPTION")[1, "Package"]), "AlertTools")) {
  devtools::load_all(quiet = TRUE)
}
if (!requireNamespace("RPostgres", quietly = TRUE)) stop("RPostgres must be installed.")
outputs <- c(args[[2]], paste0(args[[2]], c(".metadata.csv", ".sources.csv")))
if (any(file.exists(outputs))) stop("Output already exists; choose a new path to preserve audit evidence.")
conn <- DBI::dbConnect(RPostgres::Postgres())
tryCatch({
  DBI::dbBegin(conn)
  DBI::dbExecute(conn, "SET TRANSACTION READ ONLY")
  report <- AlertTools:::.alert_parameter_coverage(conn, geocodes)
  utils::write.csv(report, args[[2]], row.names = FALSE)
  metadata <- data.frame(audited_at_utc = format(Sys.time(), tz = "UTC", usetz = TRUE),
                           package_version = as.character(packageVersion("AlertTools")),
                           input_md5 = unname(tools::md5sum(args[[1]])),
                           municipalities = length(geocodes), read_only = TRUE)
  utils::write.csv(metadata, paste0(args[[2]], ".metadata.csv"), row.names = FALSE)
  sources <- list.files("R", pattern = "\\.R$", full.names = TRUE)
  if (length(sources)) {
    utils::write.csv(data.frame(file = sources, md5 = unname(tools::md5sum(sources))),
                       paste0(args[[2]], ".sources.csv"), row.names = FALSE)
  }
  DBI::dbRollback(conn)
}, finally = DBI::dbDisconnect(conn))
