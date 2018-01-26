setwd("../..")

####================================================
# Testing functions episem, data2SE, SE2date, seqSE
####================================================

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


