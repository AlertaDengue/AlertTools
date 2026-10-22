args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 1L) stop("Usage: Rscript tools/compare-epidemiology.R output-directory")
devtools::load_all(quiet = TRUE)
source("tests/reference/comparison.R")
output <- args[[1]]
dir.create(output, recursive = TRUE, showWarnings = FALSE)
report <- do.call(rbind, lapply(c("dengue", "chikungunya", "zika", "missing-climate"),
  function(scenario) compare_epidemiological_fixture(getwd(), scenario)))
utils::write.csv(report, file.path(output, "comparison.csv"), row.names = FALSE)
sources <- list.files("R", pattern = "\\.R$", full.names = TRUE)
utils::write.csv(data.frame(file = sources, md5 = unname(tools::md5sum(sources))),
                 file.path(output, "candidate-sources.csv"), row.names = FALSE)
writeLines(c("Reference: e460b3b8c15ab28e8755f482d576cf7280803b34",
             "Consumer: 27a03f26a9bf366597ef135e7da8d2f620ea9a8e",
             "Mode: none; synthetic data; fixed version date 2026-10-22; seed 42",
             "Absolute tolerance: 1e-7; character fields and missingness: exact",
             "Operational Bayesian/scientific approval: pending",
             capture.output(sessionInfo())), file.path(output, "environment.txt"))
if (any(report$status != "match")) stop("Epidemiological comparison failed; inspect comparison.csv")
cat(nrow(report), "metrics matched across four controlled scenarios.\n")
