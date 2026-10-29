test_that("multiple municipality-disease groups equal isolated calculations", {
  first <- review_result()
  second <- review_result(3303302, 100)
  second$data$CID10 <- "A92.0"
  second$parameters$cid10 <- "A92.0"
  cases <- rbind(first$data, second$data)
  climate <- read_fixture("climate.csv")
  other <- climate
  other$geocodigo <- 3303302
  climate <- rbind(climate, other)
  parameters <- rbind(first$parameters, second$parameters)
  # Use original case columns; recompute every indicator from the same inputs.
  columns <- names(read_fixture("cases.csv"))
  cases <- cases[columns]
  joint <- alerttools_pipeline(cases[nrow(cases):1, ], climate[nrow(climate):1, ],
                               parameters[2:1, ], report_week = 202106)
  isolated <- lapply(seq_len(nrow(parameters)), function(i) {
    p <- parameters[i, , drop = FALSE]
    alerttools_pipeline(cases[cases$cidade == p$municipio_geocodigo &
                               cases$CID10 == p$cid10, ],
                         climate[climate$geocodigo == p$municipio_geocodigo, ],
                         p, report_week = 202106)
  })
  sort_rows <- function(x) {
    x <- x[order(x$cidade, x$CID10, x$SE), ]
    rownames(x) <- NULL
    x
  }
  expect_equal(sort_rows(joint$data),
               sort_rows(dplyr::bind_rows(lapply(isolated, `[[`, "data"))))
  history <- as_alert_history(joint, as.Date("2026-10-29"))
  expected <- dplyr::bind_rows(lapply(isolated, as_alert_history,
                                    version_date = as.Date("2026-10-29")))
  history <- history[order(history$id), ]
  expected <- expected[order(expected$id), ]
  rownames(history) <- rownames(expected) <- NULL
  # Multi-disease histories deliberately append the CID10 to their IDs.
  expect_equal(history[setdiff(names(history), "id")], expected[setdiff(names(expected), "id")])
})
