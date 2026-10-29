test_that("municipal fallback preserves distinct parameters without database reads", {
  results <- list(review_result(), review_result(3303302, 100))
  parts <- lapply(results, AlertTools:::.as_legacy_alert_objects)
  combined <- do.call(c, parts)
  expect_null(attr(combined, "parameters"))
  local_mocked_bindings(
    .fetch_alert_parameters_impl = function(...) stop("Unexpected database lookup")
  )
  date <- as.Date("2026-10-29")
  expected <- dplyr::bind_rows(lapply(parts, tabela_historico, versao = date))
  observed <- tabela_historico(combined, versao = date)
  expect_equal(observed, expected)
  expect_equal(nrow(observed), sum(vapply(results, function(x) nrow(x$data), integer(1))))
  expect_equal(AlertTools:::.legacy_alert_parameters(combined)$limiar_preseason,
               c(10, 100))

  missing <- combined
  attr(missing[[2]], "parameters") <- NULL
  expect_error(tabela_historico(missing, versao = date), "requires.*parameters")
  conflict <- combined
  conflicting <- attr(conflict[[1]], "parameters")
  conflicting$limiar_preseason <- 999
  attr(conflict[[2]], "parameters") <- conflicting
  expect_error(tabela_historico(conflict, versao = date), "keys must be unique")
})

