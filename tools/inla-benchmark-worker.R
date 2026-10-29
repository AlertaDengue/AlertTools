args <- commandArgs(trailingOnly = TRUE)
devtools::load_all(quiet = TRUE)
source("tests/reference/frame-comparison.R")
source("tests/reference/inla-validation.R")
if (length(args) == 2L && args[[1L]] == "--compare") {
  directory <- args[[2L]]
  runs <- utils::read.csv(file.path(directory, "runs.csv"))
  reports <- lapply(seq_len(nrow(runs)), function(i) {
    row <- runs[i, ]
    baseline <- runs[runs$states == 1L & runs$workers == 1L &
                       runs$threads == row$threads & runs$repetition == 1L, ]
    if (nrow(baseline) != 1L) stop("Missing serial baseline for thread budget")
    expected <- inla_result_sections(readRDS(file.path(directory, baseline$directory, "state-1.rds")))
    do.call(rbind, lapply(seq_len(row$states), function(state) {
      observed <- inla_result_sections(readRDS(file.path(directory, row$directory,
                                                          paste0("state-", state, ".rds"))))
      cbind(run = row$directory, state = state, compare_inla_sections(expected, observed))
    }))
  })
  report <- do.call(rbind, reports)
  utils::write.csv(report, file.path(directory, "within-budget-comparison.csv"), row.names = FALSE)
  first <- runs[runs$states == 1L & runs$workers == 1L & runs$repetition == 1L, ]
  if (nrow(first) > 1L) {
    expected <- inla_result_sections(readRDS(file.path(directory, first$directory[[1]], "state-1.rds")))
    cross <- do.call(rbind, lapply(2:nrow(first), function(i) {
      observed <- inla_result_sections(readRDS(file.path(directory, first$directory[[i]], "state-1.rds")))
      cbind(baseline_threads = first$threads[[1]], threads = first$threads[[i]],
            compare_inla_sections(expected, observed))
    }))
    utils::write.csv(cross, file.path(directory, "cross-thread-comparison.csv"), row.names = FALSE)
    if (any(cross$status[cross$metric %in% c("level", "nivel")] != "match")) {
      stop("Changing the INLA thread budget changed alert levels; investigate before deployment")
    }
  }
  if (any(report$status != "match")) stop("Concurrent/repeated runs changed results within a thread budget")
} else {
  if (length(args) != 4L) stop("Usage: workers threads result.rds summary.csv")
  fixture <- inla_validation_fixture(getwd(), diseases = "A90")
  timing <- system.time(result <- run_inla_fixture(fixture,
                      workers = as.integer(args[[1L]]), threads = as.integer(args[[2L]])))
  saveRDS(result, args[[3L]])
  utils::write.csv(data.frame(elapsed_fit_seconds = unname(timing[["elapsed"]]),
    rows = nrow(result$data), records = nrow(fixture$case_records), seed = fixture$seed,
    r_version = as.character(getRversion()), inla_version = as.character(packageVersion("INLA")),
    platform = R.version$platform), args[[4L]], row.names = FALSE)
}
