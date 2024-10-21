setwd("../..")

####================================================
# Testing functions episem, data2SE, SE2date, seqSE. epiyear
####================================================

# data2SE
out1 <- data2SE(c("03-04-2013","07-01-2019","01-03-1998"),format="%d-%m-%Y")

test_that("dataSE output should be numeric", {
      expect_true(class(out1) == "numeric")
})

test_that("dataSE output should have 6 digits", {
      expect_true(nchar(as.numeric(out1[1])) == 6)
})

test_that("dataSE for 01-03-1998 should be 199753", {
      expect_true(data2SE("03-01-1998",format="%d-%m-%Y") == 199753)
})

# SE2date
out1 <- SE2date(se = c(202001:202209))
test_that("SE2date output should have 2 columns", {
      expect_true(dim(out1)[2] == 2)
})

test_that("SE2date output SE should be of format 201001", {
      expect_true((out1)[1,1] == 202001)
})

test_that("SE2date output ini should be the first day (sunday)", {
      expect_true((out1)[1,2] == "2019-12-29")
})

# daySEday

out1 <- daySEday(x = c("2015-12-23", "2015-10-23", "2022-10-16"))
out1

test_that("daySEday output should have 2 columns", {
      expect_true(dim(out1)[2] == 2)
})

test_that("daySEday output SE should be of format 201001", {
      expect_true((out1)[1,1] == 201551)
})

test_that("daySEday output ini should be the first day (sunday)", {
      expect_true((out1)[1,2] == "2015-12-23")
})

# episem
out1 <- episem(x= as.Date("2015-01-01", format="%Y-%m-%d", separa = ''))
test_that("episem output should be numeric if separa = ''", {
      expect_true(class(out1) == "numeric")
})

out2 <- episem(x= as.Date("2015-01-01", format="%Y-%m-%d"), separa = '-')
test_that("episem output should be a character if separa != ''", {
      expect_true(class(out2) == "character")
})

out3 <- episem(x= as.Date("2015-01-01", format="%Y-%m-%d"), separa = '-')
test_that("episem output gave the wrong result''", {
      expect_true(out3 == "2014-53")
})

out4 <- episem(x= as.Date("2015-01-01", format="%Y-%m-%d"), retorna = "Y")
test_that("episem output gave the wrong result''", {
      expect_true(out4 == 2014)
})

rm(out1, out2, out3, out4)




####==================================
# getRegional e getCidades ----
####==================================

rr <- getRegionais(3304557, uf = "Rio de Janeiro")

cc <- getCidades(regional = "Metropolitana I", uf="Rio de Janeiro",datasource=con)
test_that("getCidades: produce required output",{
      expect_named(cc, c("municipio_geocodigo","nome","nome_regional","nome_macroreg","uf"),ignore.order = TRUE)})
