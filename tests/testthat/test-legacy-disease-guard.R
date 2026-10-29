test_that("legacy adapter rejects mixed or inconsistent disease results", {
  result <- review_result()
  mixed <- result
  mixed$data$CID10[1] <- "A92.0"
  expect_error(AlertTools:::.as_legacy_alert_objects(mixed), "exactly one disease")
  inconsistent <- result
  inconsistent$parameters$cid10 <- "A92.0"
  expect_error(AlertTools:::.as_legacy_alert_objects(inconsistent), "exactly one disease")
  expect_s3_class(AlertTools:::.as_legacy_alert_objects(result)[[1]], "alerta")
})

