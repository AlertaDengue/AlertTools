test_that("setCriteria validates inputs and returns the legacy structure", {
  withr::local_options(list(lifecycle_verbosity = "quiet"))
  values <- c(
    varcli = "temp_min", clicrit = "22",
    limiar_preseason = "10", limiar_epidemico = "100"
  )

  expect_error(setCriteria(rule = "XX"), "setcriteria: rule unknown.")
  expect_error(setCriteria(), "if rule is null, values must be provided")

  criteria <- setCriteria(rule = "Af", values = values)
  expect_named(criteria, c("crity", "crito", "critr"))
  expect_equal(vapply(criteria, class, character(1)),
               c(crity = "character", crito = "character", critr = "character"))
  expect_equal(lengths(criteria), c(crity = 3L, crito = 3L, critr = 3L))
})

test_that("alert rules preserve output structure and delayed levels", {
  withr::local_options(list(lifecycle_verbosity = "quiet"))
  cases <- read_fixture("cases.csv")
  climate <- read_fixture("climate.csv")
  parameters <- read_fixture("parameters.csv", stringsAsFactors = FALSE)
  expected <- read_fixture("expected-alerts.csv")

  input <- merge(cases, climate, by.x = c("SE", "cidade"),
                 by.y = c("SE", "geocodigo"), sort = FALSE)
  input <- input[match(cases$SE, input$SE), ]
  input$inc <- input$casos / input$pop * 100000
  input$p1 <- c(0, 0.5, 0.96, 0.97, 0.98, 0.9, 0.4, 0.2)

  values <- c(
    varcli = parameters$varcli,
    clicrit = as.character(parameters$clicrit),
    limiar_preseason = as.character(parameters$limiar_preseason),
    limiar_epidemico = as.character(parameters$limiar_epidemico)
  )
  criteria <- setCriteria(rule = parameters$codmodelo, values = values)
  observed <- fouralert(input, crit = criteria)

  expect_s3_class(observed, "alerta")
  expect_named(observed, c("data", "indices", "crit", "n"))
  expect_equal(nrow(observed$indices), nrow(input))
  expect_equal(observed$indices, expected)
  expect_equal(observed$n, 4)
  expect_equal(observed$crit, criteria)
})
