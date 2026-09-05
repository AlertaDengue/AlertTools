if (!requireNamespace("styler", quietly = TRUE)) {
  stop("Install the suggested package 'styler' before running this script.")
}

files <- c(
  "R/api.R",
  "R/core_pipeline.R",
  "R/data_repository.R",
  "R/epiweek.R",
  "tests/testthat/test-api.R",
  "tests/testthat/test-core-pipeline.R",
  "tests/testthat/test-data-repository.R",
  "tests/testthat/test-epiweek.R"
)

missing_files <- files[!file.exists(files)]
if (length(missing_files)) {
  stop("Run tools/style.R from the package root. Missing: ",
       paste(missing_files, collapse = ", "))
}

styler::style_file(files, strict = TRUE)

message(
  "Review and commit style-only changes separately from functional changes, ",
  "then rerun devtools::test() and devtools::check()."
)
