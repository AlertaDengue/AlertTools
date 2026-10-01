test_that("municipal geocode conversion preserves regular and exceptional cases", {
  expect_equal(sevendigitgeocode(330455), 3304557)
  expect_equal(sevendigitgeocode(355030), 3550308)
  expect_equal(sevendigitgeocode(3304557), 3304557)
  expect_equal(sevendigitgeocode(261153), 2611533)
  expect_equal(sevendigitgeocode(220191), 2201919)
  expect_error(sevendigitgeocode(123),
               "this funtion receives 6 digits geocodes only", fixed = TRUE)
})

test_that("missing values are filled by the selected deterministic rule", {
  values <- c(1, 2, NA, 4, 5)
  expect_equal(nafill(values, rule = "zero"), c(1, 2, 0, 4, 5))
  expect_equal(nafill(values, rule = "linear"), c(1, 2, 3, 4, 5))
})

test_that("temperature extrapolation preserves length and removes tail gaps", {
  values <- sin(seq(0, 10, length.out = 100)) + 20
  values[91:100] <- NA

  expect_message(observed <- temp.predict(values), "temperature predicted 10 steps ahead")
  expect_length(observed, 100)
  expect_false(anyNA(observed))
})

test_that("database functions validate absent connections and malformed input", {
  withr::local_options(list(lifecycle_verbosity = "quiet"))
  expect_error(getRegionais(uf = "Rio de Janeiro", datasource = NULL))
  expect_error(getRegionais(), "getRegionais: please specify uf")
  expect_error(getCidades(uf = "Rio de Janeiro", datasource = NULL))
  expect_error(getCidades(), "getCidades: specify uf's full name")
  expect_error(read.parameters(cities = 330455, datasource = NULL))
  expect_error(write_parameters(city = 3304557, cid10 = "A90", params = list()),
               "params should be a data.frame")
  expect_error(write_parameters(city = 3304557, cid10 = "A90",
                                params = data.frame(), datasource = NULL))
  expect_error(getWUstation(cities = 3304557, datasource = NULL))
  expect_error(setWUstation(st = data.frame(), UF = "Rio de Janeiro", datasource = NULL))
})
