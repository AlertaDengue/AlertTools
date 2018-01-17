setwd("../..")

## testando conexão com o banco 
con <-DenguedbConnect()
test_that("sql connection exists", {
      expect_equal(class(con)[1], "PostgreSQLConnection")
})

geoc = 330240 # cidade para teste

####======================================
# Testing function getCases
####======================================
dC0 = getCases(city = geoc, datasource=con)
test_that("output getCases has the required columns", {
#  expect_true(all(c("bairro", "SE", "casos") %in% names(dC0)))
  expect_true(all(c("localidade", "SE", "casos","nome","pop") %in% names(dC0)))
})

### =====================================
# Testing the adjustIncidence function
### =====================================
dC1<-adjustIncidence(obj=dC0)

test_that("output of adjustIncidence has the required columns.", {
  expect_true(all(c("localidade", "SE", "casos", "pdig", "tcasesICmin", 
                    "tcasesmed", "tcasesICmax") %in% names(dC1)))
})

### =====================
# Testing Rt functions
### =====================
# different ways to run it
dC11<-Rt(obj = dC0, count = "casos", gtdist="normal", meangt=3, sdgt = 1)
dC12<-Rt(obj = dC1, count = "tcasesmed", gtdist="normal", meangt=3, sdgt = 1)

test_that("output of Rt has the required columns.", {
  expect_true(all(c("localidade", "SE", "casos", "Rt", "lwr", "upr", "p1") %in% names(dC11)))
  expect_true(all(c("localidade", "SE", "casos", "Rt", "lwr", "upr", "p1") %in% names(dC12)))
})


test_that("Rt is always zero or positive.", {
      expect_true(min(dC11$Rt>0, na.rm=TRUE) >= 0)
})

# ----------------------
# Testing getTweet function
# ----------------------
dT01 = getTweet(city = geoc, lastday = Sys.Date(), datasource = con) 

test_that("output of getTweet has the required columns.", {
  expect_true(all(c("cidade", "SE", "tweet") %in% names(dT01)))
})


# ----------------------
# Testing getWU function
# ----------------------
dW01 = getWU(stations = 'SBAF', datasource=con) #default var is temp_min

test_that("output of getWU has the required columns.", {
  expect_true(all(c("SE", "estacao","temp_min") %in% names(dW01)))
})

dW02 = getWU(stations = 'SBAF', vars=c("temp_min","temp_max"), datasource=con)
test_that("output of getWU has the required columns.", {
      expect_true(all(c("SE", "estacao","temp_min","temp_max") %in% names(dW02)))
})

#testando mais de uma estacao que existe
dW03 = getWU(stations = c('SBAF', "SBRJ"), vars=c("temp_min","temp_max"), datasource=con)
test_that("output of getWU has the solicited stations.", {
      expect_true(all(c('SBAF', "SBRJ") %in% as.character(unique(dW03$estacao))))
})

#testando uma estacao que nao existe e outra q existe
dW04 = getWU(stations = c('ABCD', "SBRJ"), vars=c("temp_min","temp_max"), datasource=con)
test_that("output of getWU has the solicited stations.", {
      expect_true(all(c("SBRJ") %in% as.character(unique(dW04$estacao))))
})

#testando uma estacao que nao existe 
dW05 = getWU(stations = c('ABCD'), vars=c("temp_min","temp_max"), datasource=con)
test_that("output of getWU should be NULL pq estacao nao existe.", {
      expect_null(dW05)
})

# ---------------------
# Testing mergedata 
d0<- mergedata(cases = dC1,tweet = dT01, climate = dW02)
d1<- mergedata(tweet = dT01, climate = dW02)
d2<- mergedata(cases = dC1, climate = dW02)
d3<- mergedata(cases = dC1, tweet = dT01)

test_that("output of merging is a non empty data.frame.", {
  expect_gt(dim(d0)[1], 0)
  expect_gt(dim(d1)[1], 0)
  expect_gt(dim(d2)[1], 0)
  expect_gt(dim(d3)[1], 0)
})

test_that("output has the minimum set of columns.", {
      expect_true(all(c("cidade", "SE", "estacao","tweet","casos") %in% names(d0)))
      expect_true(all(c("cidade", "SE", "estacao","tweet") %in% names(d1)))
      expect_true(all(c("cidade", "SE", "estacao","casos") %in% names(d2)))
      expect_true(all(c("cidade", "SE", "tweet","casos") %in% names(d3)))
})


