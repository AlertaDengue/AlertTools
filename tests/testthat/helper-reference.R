.reference_root <- normalizePath(testthat::test_path("..", ".."), mustWork = TRUE)
source(file.path(.reference_root, "tests/reference/frame-comparison.R"), local = TRUE)
source(file.path(.reference_root, "tests/reference/comparison.R"), local = TRUE)
