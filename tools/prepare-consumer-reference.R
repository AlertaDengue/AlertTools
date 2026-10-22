# Input is the raw main_BR.R from the pinned consumer revision, downloaded read-only.
args <- commandArgs(trailingOnly = TRUE)
source("tests/reference/comparison.R", local = TRUE)
if (length(args) != 1L) stop("Usage: Rscript tools/prepare-consumer-reference.R main_BR.R")
blob <- system2("git", c("hash-object", shQuote(args[[1]])), stdout = TRUE)
stopifnot(identical(blob, "863937b5b77389c5341a103522bb59c8a75d5789"))
original <- as.list(parse(args[[1]], keep.source = FALSE))
names <- c("safe_gc", "combine_ale_parts", "run_city_pipeline",
           "run_disease_pipeline_by_city", "run_disease_pipeline", "load_state_result",
           "append_part", "normalize_restab", "normalize_ale", "bind_parts", "cap_max")
functions <- Filter(function(node) {
  is.call(node) && identical(node[[1]], as.name("<-")) && is.symbol(node[[2]]) &&
    as.character(node[[2]]) %in% names && is.call(node[[3]]) &&
    identical(node[[3]][[1]], as.name("function"))
}, original)
stopifnot(setequal(vapply(functions, function(node) as.character(node[[2]]), ""), names))
publication <- Filter(function(node) {
  is.call(node) && identical(node[[1]], as.name("if")) &&
    any(vapply(c("restab_den", "restab_chik", "restab_zika"), function(name) {
      identical(node[[2]], parse(text = paste0("!is.null(", name, ")"))[[1]])
    }, logical(1)))
}, original)
stopifnot(length(publication) == 3L)
destination <- "tests/reference/consumer-27a03f26"
dir.create(destination, recursive = TRUE, showWarnings = FALSE)
write <- function(nodes, name) {
  path <- file.path(destination, name)
  lines <- c("# Unmodified expressions from AlertaDengueAnalise/main/main_BR.R",
             "# Commit 27a03f26a9bf366597ef135e7da8d2f620ea9a8e",
             unlist(lapply(nodes, deparse, width.cutoff = 100)))
  writeLines(sub("[[:blank:]]+$", "", lines), path)
  stopifnot(identical(as.list(parse(path, keep.source = FALSE)), nodes))
}
write(functions, "functions.R")
write(publication, "publication.R")
files <- c("functions.R", "publication.R")
utils::write.csv(data.frame(file = files,
  consumer_commit = "27a03f26a9bf366597ef135e7da8d2f620ea9a8e",
  source = "main/main_BR.R", source_git_blob = blob,
  approved_alerttools = "e460b3b8c15ab28e8755f482d576cf7280803b34",
  md5 = reference_snapshot_md5(file.path(destination, files))),
  file.path(destination, "manifest.csv"), row.names = FALSE)
