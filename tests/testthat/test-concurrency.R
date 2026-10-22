parallel_fixture <- function() {
  cases <- read_fixture("cases.csv")
  climate <- read_fixture("climate.csv")
  parameters <- read_fixture("parameters.csv", stringsAsFactors = FALSE)
  second_cases <- cases
  second_cases$cidade <- 3303302
  second_climate <- climate
  second_climate$geocodigo <- 3303302
  second_parameters <- parameters
  second_parameters$municipio_geocodigo <- 3303302
  list(cases = rbind(cases, second_cases), climate = rbind(climate, second_climate),
       parameters = rbind(parameters, second_parameters), report_week = 202106)
}

test_that("municipal processes and INLA threads are independently propagated", {
  outer <- NULL
  inner <- integer()
  original <- nowcast_cases
  local_mocked_bindings(
    .map_workers = function(x, fun, workers) {
      outer <<- workers
      lapply(x, fun)
    },
    nowcast_cases = function(..., inla_threads) {
      inner <<- c(inner, inla_threads)
      original(..., inla_threads = inla_threads)
    }
  )
  args <- c(parallel_fixture(), list(municipality_workers = 4, inla_threads = 2))
  result <- do.call(alerttools_pipeline, args)
  expect_identical(outer, 4L)
  expect_identical(inner, c(2L, 2L))
  expect_identical(result$metadata$municipality_workers, 4L)
  expect_identical(result$metadata$inla_threads, 2L)
})

test_that("serial and parallel pipelines preserve epidemiological output", {
  args <- c(parallel_fixture(), list(seed = 42))
  serial <- do.call(alerttools_pipeline, c(args, list(municipality_workers = 1)))
  concurrent <- do.call(alerttools_pipeline, c(args, list(municipality_workers = 2)))
  expect_equal(concurrent$data, serial$data, tolerance = 1e-7)
  expect_equal(concurrent$alerts, serial$alerts)
  expect_identical(concurrent$parameters, serial$parameters)
  alias <- do.call(alerttools_pipeline, c(args, list(workers = 2)))
  expect_identical(alias, concurrent)
  expect_identical(alias$metadata$inla_threads, 1L)
  expect_error(do.call(alerttools_pipeline, c(args, list(workers = 2,
                    municipality_workers = 3))), "Conflicting")
  expect_error(do.call(alerttools_pipeline, c(args, list(inla_threads = Inf))),
               "inla_threads.*positive integer")
})

test_that("INLA fitting and posterior sampling respect explicit thread budgets", {
  skip_if_not_installed("INLA")
  withr::local_seed(814)
  fit_args <- sample_args <- NULL
  local_mocked_bindings(
    inla = function(...) {
      fit_args <<- list(...)
      list(fit = TRUE)
    },
    inla.posterior.sample = function(n, result, ...) {
      sample_args <<- list(...)
      rep(list(list(latent = c(0, 0), hyperpar = 1)), n)
    }, .package = "INLA"
  )
  data <- data.frame(Date = as.Date(c("2021-01-03", "2021-01-10")),
                     Casos = c(NA_real_, NA_real_))
  fit <- AlertTools:::nowcast.INLA(data, Casos ~ 1, inla_threads = 3)
  expect_identical(fit_args$num.threads, "3:1")
  before <- .Random.seed
  first <- AlertTools:::nowcasting(fit, data, 2, as.Date("2021-01-10"),
                                    seed = 42, samples = 5, inla_threads = 3)
  second <- AlertTools:::nowcasting(fit, data, 2, as.Date("2021-01-10"),
                                     seed = 42, samples = 5, inla_threads = 3)
  expect_identical(first, second)
  expect_identical(.Random.seed, before)
  expect_identical(sample_args$seed, 42L)
  expect_identical(sample_args$num.threads, "1:1")
  expect_false(sample_args$parallel.configs)
})
