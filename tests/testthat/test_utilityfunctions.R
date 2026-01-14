library(assertthat)
library(tidyverse)

####================================================
# Testing functions episem, data2SE, SE2date, seqSE. epiyear
####================================================

# epiYear
test_that("epiYear correctly identifies epidemiological years", {
      # Default cut = 41
      expect_equal(epiYear(202010)$eyear, 2019) # Before cut
      expect_equal(epiYear(202041)$eyear, 2020) # At cut
      expect_equal(epiYear(202045)$eyear, 2020) # After cut
      
      # Custom cut
      expect_equal(epiYear(202030, cut = 31)$eyear, 2019)
      expect_equal(epiYear(202031, cut = 31)$eyear, 2020)
})

# data2SE
out1 <- data2SE(c("03-04-2013","07-01-2019","01-03-1998"),format="%d-%m-%Y")

test_that("data2SE output should be numeric", {
      expect_type(out1, "double")
})

test_that("data2SE output should have 6 digits when converted to numeric", {
      expect_true(all(nchar(as.character(out1)) == 6))
})

test_that("data2SE for 01-03-1998 should reflect epiweek 53 of 1997", {
      # 1998-03-01 is a Sunday.
      # episem("1998-03-01") -> let's see
      expect_equal(data2SE("03-01-1998", format="%d-%m-%Y"), 199753)
})

# SE2date
test_that("SE2date output structure and values", {
      # SE2date relies on the 'SE' object being present (usually from sysdata.rda)
      # We assume it is loaded via devtools::load_all()
      out_se2d <- SE2date(se = c(202001, 202002))
      expect_equal(dim(out_se2d), c(2, 2))
      expect_equal(out_se2d$SE[1], 202001)
      # Week 1 of 2020 started on 2019-12-29
      expect_equal(as.character(out_se2d$ini[1]), "2019-12-29")
})

# daySEday
test_that("daySEday output structure and values", {
      out_dsd <- daySEday(x = c("2015-12-23", "2015-10-23", "2022-10-16"))
      expect_equal(dim(out_dsd), c(3, 2))
      expect_equal(out_dsd$SE[1], 201551)
      expect_equal(as.character(out_dsd$ini[1]), "2015-12-23")
      
      # Testing with numeric input
      out_dsd_num <- daySEday(x = 202001)
      expect_equal(as.character(out_dsd_num$ini[1]), "2019-12-29")
})

# episem
test_that("episem output types and formats", {
      # numeric output
      out_num <- episem(x= as.Date("2015-01-01", format="%Y-%m-%d"), separa = '')
      expect_type(out_num, "double")
      expect_equal(out_num, 201453)
      
      # character output
      out_char <- episem(x= as.Date("2015-01-01", format="%Y-%m-%d"), separa = '-')
      expect_type(out_char, "character")
      expect_equal(out_char, "2014-53")
      
      # year only
      expect_equal(episem(x= as.Date("2015-01-01", format="%Y-%m-%d"), retorna = "Y"), 2014)
      
      # week only
      expect_equal(episem(x= as.Date("2015-01-01", format="%Y-%m-%d"), retorna = "W"), 53)
      
      # edge cases
      expect_true(is.na(episem(NA)))
      expect_equal(episem(as.Date("2017-01-01")), 201701) # Jan 1st 2017 was Sunday
      expect_equal(episem(as.Date("2016-01-01")), 201552) # Jan 1st 2016 was Friday
})

# lastepiweek
test_that("lastepiweek identifies the correct last week", {
      expect_equal(lastepiweek(2020), 53)
      expect_equal(lastepiweek(2021), 52)
      expect_equal(lastepiweek(2022), 52)
})

# seqSE
test_that("seqSE generates correct sequence", {
      sq <- seqSE(202001, 202003)
      expect_equal(nrow(sq), 3)
      expect_equal(sq$SE, c(202001, 202002, 202003))
})

# sevendigitgeocode
test_that("sevendigitgeocode works for regular and special cases", {
      # Regular cases
      expect_equal(sevendigitgeocode(330455), 3304557)
      expect_equal(sevendigitgeocode(355030), 3550308) # São Paulo
      
      # Already 7 digits
      expect_equal(sevendigitgeocode(3304557), 3304557)
      
      # Pathological cases
      expect_equal(sevendigitgeocode(261153), 2611533) # Recife
      expect_equal(sevendigitgeocode(220191), 2201919)
      
      # Errors
      expect_error(sevendigitgeocode(123), "this funtion receives 6 digits geocodes only")
})

# nafill
test_that("nafill works correctly", {
      v <- c(1, 2, NA, 4, 5)
      expect_equal(nafill(v, rule = "zero"), c(1, 2, 0, 4, 5))
      expect_equal(nafill(v, rule = "linear"), c(1, 2, 3, 4, 5))
})

# temp.predict
test_that("temp.predict extrapolates values", {
      # Create a seasonal-like series with NAs at the end
      v <- sin(seq(0, 10, length.out = 100)) + 20
      v_with_nas <- v
      v_with_nas[91:100] <- NA
      
      # Mocking message to avoid output clutter
      expect_message(out <- temp.predict(v_with_nas), "temperature predicted 10 steps ahead")
      expect_equal(length(out), 100)
      expect_false(any(is.na(out)))
})

####==================================
# getRegional e getCidades (Database dependent) ----
####==================================

test_that("Database functions handle missing connection or bad inputs", {
      # Since we don't have a real DB connection, we verify they fail as expected
      # getRegionais
      expect_error(getRegionais(uf = "Rio de Janeiro", datasource = NULL))
      expect_error(getRegionais(), "getRegionais: please specify uf")
      
      # getCidades
      expect_error(getCidades(uf = "Rio de Janeiro", datasource = NULL))
      expect_error(getCidades(), "getCidades: specify uf's full name")
      
      # read.parameters
      # Should fail because it tries to convert cities to 7 digits then query
      expect_error(read.parameters(cities = 330455, datasource = NULL))
      
      # write_parameters
      expect_error(write_parameters(city = 3304557, cid10 = "A90", params = list()), "params should be a data.frame")
      expect_error(write_parameters(city = 3304557, cid10 = "A90", params = data.frame(), datasource = NULL))
      
      # getWUstation
      expect_error(getWUstation(cities = 3304557, datasource = NULL))
      
      # setWUstation
      expect_error(setWUstation(st = data.frame(), UF = "Rio de Janeiro", datasource = NULL))
})
