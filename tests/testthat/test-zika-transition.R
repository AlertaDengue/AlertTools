test_that("modern Zika lookup reports complete, partial and absent coverage", {
  conn <- sqlite_fixture_connection(environment())
  codes <- c(3304557, 3303302)
  dengue <- parameter_fixture()[rep(1, 2), ]
  dengue$municipio_geocodigo <- codes
  DBI::dbWriteTable(conn, "parameters", dengue, overwrite = TRUE)
  zika <- dengue
  zika$cid10 <- "A92.8"
  zika$limiar_epidemico <- c(30, 40)

  expect_error(fetch_alert_parameters(conn, codes, "zika"),
               "A92.8.*3304557, 3303302")
  expect_identical(AlertTools:::.alert_parameter_coverage(conn, codes)$status,
                   c("legacy_only", "legacy_only"))
  DBI::dbWriteTable(conn, "parameters", zika[1, ], append = TRUE)
  expect_error(fetch_alert_parameters(conn, codes, "zika"), "A92.8.*3303302")
  expect_identical(AlertTools:::.alert_parameter_coverage(conn, codes)$status,
                   c("specific", "legacy_only"))
  DBI::dbWriteTable(conn, "parameters", zika[2, ], append = TRUE)
  expect_equal(fetch_alert_parameters(conn, rev(codes), "zika")$limiar_epidemico,
               c(40, 30))
  DBI::dbWriteTable(conn, "parameters", zika[1, ], append = TRUE)
  expect_error(fetch_alert_parameters(conn, codes, "zika"), "keys.*unique")
  expect_identical(AlertTools:::.alert_parameter_coverage(conn, codes)$status,
                   c("duplicate", "specific"))
})

test_that("legacy Zika fallback preserves dengue rules and Zika result identity", {
  conn <- sqlite_fixture_connection(environment())
  notifications <- notification_fixture()
  notifications$cid10_codigo <- "A92.8"
  notifications[c("dt_notific", "dt_sin_pri", "dt_digita")] <-
    lapply(notifications[c("dt_notific", "dt_sin_pri", "dt_digita")], as.numeric)
  DBI::dbWriteTable(conn, "Notificacao", notifications, overwrite = TRUE)
  withr::local_options(list(lifecycle_verbosity = "quiet"))

  run <- function() pipe_infodengue(
    3304557, cid10 = "A92.8", datarelatorio = 202106, nowcasting = "none",
    firstday = as.Date("2020-12-20"), datasource = conn, completetail = 0
  )
  expect_warning(legacy <- run(), "Using dengue parameters")
  expect_true(all(legacy[[1]]$data$CID10 == "A92.8"))
  expect_true(all(attr(legacy, "parameters")$cid10 == "A92.8"))
  zika <- parameter_fixture()
  zika$cid10 <- "A92.8"
  zika$limiar_epidemico <- 999
  DBI::dbWriteTable(conn, "parameters", zika, append = TRUE)
  expect_warning(with_specific <- run(), "Using dengue parameters")
  expect_equal(with_specific, legacy)
  expect_equal(fetch_alert_parameters(conn, 3304557, "zika")$limiar_epidemico, 999)
})
