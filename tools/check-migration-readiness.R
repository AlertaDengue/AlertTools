args <- commandArgs(trailingOnly = TRUE)
for_removal <- "--for-removal" %in% args

registry_path <- file.path("docs", "refactoring", "consumer-registry.csv")
policy_path <- file.path("docs", "refactoring", "transition-policy.dcf")

if (!file.exists(registry_path) || !file.exists(policy_path)) {
  stop("Run this script from the AlertTools repository root.", call. = FALSE)
}

registry <- utils::read.csv(
  registry_path,
  stringsAsFactors = FALSE,
  check.names = FALSE,
  na.strings = ""
)
policy <- as.list(read.dcf(policy_path)[1, ])

legacy_api <- c(
  "getCases", "getCaseslist", "getClima", "read.parameters",
  "pipe_infodengue", "Rt", "adjustIncidence", "setCriteria", "fouralert",
  "tabela_historico", "write_alerta", "write_parameters"
)

r_files <- list.files("R", pattern = "\\.[Rr]$", full.names = TRUE)
internal_legacy_calls <- character()
for (r_file in r_files) {
  lines <- readLines(r_file, warn = FALSE)
  for (line_number in seq_along(lines)) {
    line <- lines[[line_number]]
    if (grepl("^\\s*#", line) || grepl("\\.deprecate_api\\(", line)) {
      next
    }
    code_line <- sub("\\s+#.*$", "", line)
    for (api in legacy_api) {
      call_pattern <- paste0("\\b", gsub("\\.", "\\\\.", api), "\\s*\\(")
      definition_pattern <- paste0(
        "^\\s*", gsub("\\.", "\\\\.", api), "\\s*<-\\s*function\\b"
      )
      if (grepl(call_pattern, code_line) && !grepl(definition_pattern, code_line)) {
        internal_legacy_calls <- c(
          internal_legacy_calls,
          paste0(r_file, ":", line_number, ": ", trimws(line))
        )
      }
    }
  }
}

if (length(internal_legacy_calls)) {
  stop(
    "Legacy API calls reintroduced in package code:\n",
    paste(unique(internal_legacy_calls), collapse = "\n"),
    call. = FALSE
  )
}

required_columns <- c(
  "consumer_id", "repository", "path", "legacy_api", "criticality",
  "owner", "status", "last_checked", "evidence", "blocker", "rollback"
)
missing_columns <- setdiff(required_columns, names(registry))
if (length(missing_columns)) {
  stop(
    "Consumer registry is missing columns: ",
    paste(missing_columns, collapse = ", "),
    call. = FALSE
  )
}

allowed_status <- c("discovered", "planned", "migrating", "validated", "retired")
invalid_status <- setdiff(unique(registry$status), allowed_status)
if (length(invalid_status)) {
  stop("Invalid consumer status: ", paste(invalid_status, collapse = ", "), call. = FALSE)
}

allowed_criticality <- c("critical", "high", "medium", "low")
invalid_criticality <- setdiff(unique(registry$criticality), allowed_criticality)
if (length(invalid_criticality)) {
  stop(
    "Invalid consumer criticality: ",
    paste(invalid_criticality, collapse = ", "),
    call. = FALSE
  )
}

if (anyDuplicated(registry$consumer_id)) {
  stop("Consumer IDs must be unique.", call. = FALSE)
}

if (any(!nzchar(registry$rollback))) {
  stop("Every consumer must have a rollback plan.", call. = FALSE)
}

blocking <- registry[
  registry$criticality %in% c("critical", "high") &
    !registry$status %in% c("validated", "retired"),
  ,
  drop = FALSE
]

cat("Migration inventory:", nrow(registry), "consumers\n")
cat("Internal operational legacy calls: 0\n")
cat("Critical/high consumers pending:", nrow(blocking), "\n")
cat("Transition policy:", policy$Status, "\n")

if (nrow(blocking)) {
  cat("Pending IDs:", paste(blocking$consumer_id, collapse = ", "), "\n")
}

if (for_removal) {
  policy_approved <- identical(unname(policy$Status), "approved")
  if (!policy_approved || nrow(blocking)) {
    stop(
      "Legacy API removal is blocked: approve the support policy and validate ",
      "or retire every critical/high consumer.",
      call. = FALSE
    )
  }
  cat("Legacy API removal gate: READY\n")
} else {
  cat("Registry integrity: OK\n")
  if (nrow(blocking)) {
    cat("Legacy API removal gate: BLOCKED (expected during transition)\n")
  }
}
