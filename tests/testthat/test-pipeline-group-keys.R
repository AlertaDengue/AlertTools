test_that("municipality-disease mismatches identify the offending pair", {
  cases <- read_fixture("cases.csv")
  climate <- read_fixture("climate.csv")
  parameters <- read_fixture("parameters.csv", stringsAsFactors = FALSE)
  wrong <- parameters
  wrong$cid10 <- "A92.0"
  expect_error(alerttools_pipeline(cases, climate, wrong, report_week = 202106),
               "3304557.*A90")
  expect_error(alerttools_pipeline(cases, climate, rbind(parameters, wrong),
                                  report_week = 202106), "3304557.*A92.0")
})

