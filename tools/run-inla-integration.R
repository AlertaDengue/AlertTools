args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 1L) stop("Usage: Rscript tools/run-inla-integration.R output-directory")
if (!requireNamespace("INLA", quietly = TRUE)) stop("INLA must be installed; skipping is not permitted")
output <- args[[1L]]
dir.create(output, recursive = TRUE, showWarnings = FALSE)
Sys.setenv(ALERTTOOLS_TEST_INLA = "true", ALERTTOOLS_INLA_EVIDENCE = normalizePath(output))
writeLines(sub("[[:blank:]]+$", "", capture.output(sessionInfo())),
             file.path(output, "environment.txt"))
sources <- c(list.files("R", pattern = "\\.R$", full.names = TRUE),
             list.files("tests/reference", pattern = "^inla-.*[.]R$", full.names = TRUE),
             list.files("tests/testthat", pattern = "^test-inla-.*[.]R$", full.names = TRUE),
             "tests/reference/frame-comparison.R")
utils::write.csv(data.frame(file = sources, md5 = unname(tools::md5sum(sources))),
                   file.path(output, "candidate-sources.csv"), row.names = FALSE)
results <- devtools::test(filter = "inla-(integration|reference|consumer)", stop_on_failure = FALSE)
summary <- as.data.frame(results)
utils::write.csv(summary[vapply(summary, is.atomic, logical(1))],
                   file.path(output, "test-results.csv"), row.names = FALSE)
if (nrow(summary) != 4L || any(summary$skipped) ||
    any(summary$failed > 0 | summary$error | summary$warning > 0)) {
  stop("Real INLA integration did not pass all four required scenarios; inspect the evidence")
}
cat("Real INLA integration passed: all required scenarios executed.\n")
