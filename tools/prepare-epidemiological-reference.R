# Explicit regeneration from the consumer's immutable approved dependency.
# Never called by the comparison or by tests to update expected results.
reference <- "e460b3b8c15ab28e8755f482d576cf7280803b34"
source("tests/reference/comparison.R", local = TRUE)
destination <- "tests/reference/e460b3b8"
dir.create(destination, recursive = TRUE, showWarnings = FALSE)
selected <- list(
  "R/alert_functions.R" = c("setCriteria", "fouralert", "calc.alerta", "tabela_historico",
                             "write_alerta"),
  "R/Rt_functions.R" = c("Rt", "betaconf", "ll"),
  "R/adjust_incidence_functions.R" = c("adjustIncidence", "bayesnowcasting"),
  "R/nowcasting_fun.R" = c("ff", "gg", "nowcasting", "nowcast.INLA"),
  "R/utility_tools.R" = c("SE2date", "episem", "data2SE", "sevendigitgeocode")
)
manifest <- lapply(names(selected), function(path) {
  object <- paste0(reference, ":", path)
  original <- parse(text = system2("git", c("show", object), stdout = TRUE),
                    keep.source = FALSE)
  definitions <- Filter(function(node) {
    is.call(node) && as.character(node[[1]]) %in% c("<-", "=") &&
      is.symbol(node[[2]]) && as.character(node[[2]]) %in% selected[[path]] &&
      is.call(node[[3]]) && identical(node[[3]][[1]], as.name("function"))
  }, as.list(original))
  stopifnot(setequal(vapply(definitions, function(x) as.character(x[[2]]), ""),
                     selected[[path]]))
  output <- file.path(destination, basename(path))
  lines <- c(paste("# Unmodified function expressions from", object),
             unlist(lapply(definitions, deparse, width.cutoff = 100)))
  writeLines(sub("[[:blank:]]+$", "", lines), output)
  stopifnot(identical(as.list(parse(output, keep.source = FALSE)), definitions))
  data.frame(file = basename(path), source = path,
             git_blob = system2("git", c("rev-parse", object), stdout = TRUE),
             reference_commit = reference,
             functions = paste(selected[[path]], collapse = ";"))
})
object <- paste0(reference, ":R/sysdata.rda")
stopifnot(system2("git", c("show", object),
                  stdout = file.path(destination, "sysdata.rda")) == 0L)
manifest[[length(manifest) + 1L]] <- data.frame(
  file = "sysdata.rda", source = "R/sysdata.rda",
  git_blob = system2("git", c("rev-parse", object), stdout = TRUE),
  reference_commit = reference, functions = "SE"
)
manifest <- do.call(rbind, manifest)
manifest$md5 <- reference_snapshot_md5(file.path(destination, manifest$file))
utils::write.csv(manifest, file.path(destination, "manifest.csv"),
                 row.names = FALSE)
