# Read-only operational audit. Connection settings are supplied through PG*.
args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 2L || !nzchar(Sys.getenv("PGDATABASE"))) {
  stop("Usage: PGDATABASE=... Rscript tools/audit-zika-coverage.R geocodes.csv report.csv")
}
geocodes <- utils::read.csv(args[[1]])$municipio_geocodigo
conn <- DBI::dbConnect(RPostgres::Postgres())
tryCatch({
  DBI::dbBegin(conn)
  DBI::dbExecute(conn, "SET TRANSACTION READ ONLY")
  report <- AlertTools:::.alert_parameter_coverage(conn, geocodes)
  utils::write.csv(report, args[[2]], row.names = FALSE)
  DBI::dbRollback(conn)
}, finally = DBI::dbDisconnect(conn))
