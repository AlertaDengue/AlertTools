test_that("synthetic fixtures preserve tabular schemas and keys", {
  cases <- read_fixture("cases.csv", stringsAsFactors = FALSE)
  climate <- read_fixture("climate.csv")
  population <- read_fixture("population.csv")
  parameters <- read_fixture("parameters.csv", stringsAsFactors = FALSE)

  expect_named(cases, c("SE", "cidade", "CID10", "casos", "cas_prov", "cas_lab",
                        "localidade", "nome", "pop"))
  expect_named(climate, c("SE", "geocodigo", "temp_min", "temp_med", "temp_max",
                          "umid_min", "umid_med", "umid_max"))
  expect_named(population, c("geocode", "year", "pop"))
  expect_named(parameters, c("municipio_geocodigo", "limiar_preseason",
                             "limiar_posseason", "limiar_epidemico", "varcli",
                             "clicrit", "varcli2", "clicrit2", "cid10", "codmodelo"))
  expect_true(all(cases$nome == "Municipio Fixture"))
  expect_true(all(cases$CID10 == "A90"))
  expect_false(anyDuplicated(cases[c("SE", "cidade", "CID10")]) > 0)
  expect_false(anyDuplicated(climate[c("SE", "geocodigo")]) > 0)
  expect_false(anyDuplicated(population[c("geocode", "year")]) > 0)
  expect_false(anyDuplicated(parameters[c("municipio_geocodigo", "cid10")]) > 0)
})
