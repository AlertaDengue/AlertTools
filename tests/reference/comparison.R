# Shared comparison code for package tests and the standalone evidence command.
reference_snapshot_md5 <- function(files) {
  vapply(files, function(file) {
    if (!grepl("\\.R$", file)) return(unname(tools::md5sum(file)))
    canonical <- paste0(paste(readLines(file, encoding = "UTF-8", warn = FALSE),
                               collapse = "\n"), "\n")
    temporary <- tempfile()
    on.exit(unlink(temporary), add = TRUE)
    writeBin(charToRaw(enc2utf8(canonical)), temporary)
    unname(tools::md5sum(temporary))
  }, character(1), USE.NAMES = FALSE)
}

load_epidemiological_reference <- function(root, parameters) {
  directory <- file.path(root, "tests/reference/e460b3b8")
  manifest <- utils::read.csv(file.path(directory, "manifest.csv"))
  observed <- reference_snapshot_md5(file.path(directory, manifest$file))
  if (!identical(observed, manifest$md5)) {
    stop("Reference snapshot integrity failure: ",
         paste(manifest$file[observed != manifest$md5], collapse = ", "))
  }
  reference <- new.env(parent = asNamespace("AlertTools"))
  for (file in manifest$file[grepl("\\.R$", manifest$file)]) {
    sys.source(file.path(directory, file), reference)
  }
  load(file.path(directory, "sysdata.rda"), reference)
  # Only the historical database parameter lookup is replaced by controlled data.
  # All calculation and history-conversion function bodies remain unchanged.
  reference$read.parameters <- function(cities, cid10 = "A90", ...) {
    if (cid10 == "A92.8") cid10 <- "A90"
    parameters[parameters$municipio_geocodigo %in% cities &
                 parameters$cid10 == cid10, , drop = FALSE]
  }
  reference
}

epidemiological_fixture <- function(root, scenario) {
  read <- function(name) utils::read.csv(file.path(root, "tests/testthat/fixtures", name))
  cases <- read("cases.csv")
  climate <- read("climate.csv")
  parameters <- read("parameters.csv")
  disease <- switch(scenario, chikungunya = "A92.0", zika = "A92.8", "A90")
  cases$CID10 <- disease
  if (scenario == "chikungunya") parameters$cid10 <- disease
  if (scenario == "missing-climate") {
    climate <- climate[climate$SE != 202101, ]
    climate$temp_min[climate$SE == 202104] <- NA_real_
  }
  list(cases = cases, climate = climate, parameters = parameters, disease = disease)
}

epidemiological_snapshots <- function(root, scenario, version_date = as.Date("2026-10-22")) {
  fixture <- epidemiological_fixture(root, scenario)
  reference <- load_epidemiological_reference(root, fixture$parameters)
  input <- dplyr::left_join(fixture$cases, fixture$climate,
                            by = c("cidade" = "geocodigo", "SE" = "SE"))
  input$tweet <- NA_real_
  input$inc <- input$casos / input$pop * 1e5
  old <- lapply(unique(input$cidade), function(city) {
    suppressMessages(reference$calc.alerta(
      city, list(input, 202106, "none", fixture$parameters, NULL)
    ))
  })
  names(old) <- unique(input$cidade)
  parameters <- fixture$parameters
  parameters$cid10 <- fixture$disease
  candidate <- AlertTools::alerttools_pipeline(
    fixture$cases, fixture$climate, parameters,
    report_week = 202106, nowcasting = "none", seed = 42
  )
  legacy_candidate <- AlertTools:::.as_legacy_alert_objects(candidate)
  list(
    reference = list(data = dplyr::bind_rows(lapply(old, `[[`, "data")),
                     alerts = dplyr::bind_rows(lapply(old, `[[`, "indices")),
                     history = suppressMessages(reference$tabela_historico(old,
                                                  versao = version_date))),
    candidate = list(data = candidate$data, alerts = candidate$alerts,
                     history = AlertTools::tabela_historico(legacy_candidate,
                                                             versao = version_date))
  )
}

compare_epidemiological_frames <- function(reference, candidate, tolerance = 1e-7) {
  if (nrow(reference) != nrow(candidate) ||
      !setequal(names(reference), names(candidate))) {
    stop("Epidemiological output schema/cardinality mismatch.")
  }
  do.call(rbind, lapply(names(reference), function(column) {
    old <- reference[[column]]
    new <- candidate[[column]]
    both_na <- is.na(old) & is.na(new)
    numeric <- is.numeric(old) && is.numeric(new)
    if (numeric) {
      finite <- is.finite(old) & is.finite(new)
      equal <- both_na | (finite & abs(old - new) <= tolerance) |
        (is.infinite(old) & is.infinite(new) & sign(old) == sign(new))
      maximum <- if (any(finite)) max(abs(old[finite] - new[finite])) else 0
    } else {
      equal <- both_na | (!is.na(old) & !is.na(new) & as.character(old) == as.character(new))
      maximum <- NA_real_
    }
    equal[is.na(equal)] <- FALSE
    data.frame(metric = column, rows = length(old),
               differences = sum(!equal), max_abs_difference = maximum,
               tolerance = if (numeric) tolerance else 0,
               status = if (all(equal)) "match" else "mismatch")
  }))
}

compare_epidemiological_fixture <- function(root, scenario) {
  snapshots <- epidemiological_snapshots(root, scenario)
  report <- lapply(names(snapshots$reference), function(section) {
    result <- compare_epidemiological_frames(snapshots$reference[[section]],
                                              snapshots$candidate[[section]])
    cbind(scenario = scenario, section = section, result)
  })
  do.call(rbind, report)
}
