args <- commandArgs(trailingOnly = TRUE)
output <- if (length(args)) args[[1]] else "consumer-evidence"
if (!identical(Sys.getenv("ALERTTOOLS_TEST_POSTGRES"), "true") ||
    !identical(Sys.getenv("PGDATABASE"), "alerttools_test")) {
  stop("Requires the disposable alerttools_test PostgreSQL database and explicit test opt-in.")
}
if (!requireNamespace("RPostgres", quietly = TRUE)) stop("RPostgres must be installed.")
dir.create(output, recursive = TRUE, showWarnings = FALSE)
Sys.setenv(ALERTTOOLS_CONSUMER_EVIDENCE = normalizePath(output))
results <- devtools::test(stop_on_failure = FALSE)
summary <- as.data.frame(results)
atomic <- vapply(summary, is.atomic, logical(1))
utils::write.csv(summary[atomic], file.path(output, "test-results.csv"), row.names = FALSE)
sources <- list.files("R", pattern = "\\.R$", full.names = TRUE)
utils::write.csv(data.frame(file = sources, md5 = unname(tools::md5sum(sources))),
                 file.path(output, "candidate-sources.csv"), row.names = FALSE)
file.copy("tests/reference/consumer-27a03f26/manifest.csv",
           file.path(output, "consumer-manifest.csv"), overwrite = TRUE)
file.copy("tests/reference/e460b3b8/manifest.csv",
           file.path(output, "reference-manifest.csv"), overwrite = TRUE)
writeLines(sub("[[:blank:]]+$", "", capture.output(sessionInfo())),
             file.path(output, "environment.txt"))
postgres <- grepl("consumer.*PostgreSQL", summary$test)
if (sum(postgres) != 1L || any(summary$skipped[postgres])) {
  stop("The pinned consumer PostgreSQL contract was not executed.")
}
if (any(summary$failed > 0L | summary$error | summary$warning > 0L)) {
  stop("Package or consumer integration checks failed; see the uploaded evidence.")
}
