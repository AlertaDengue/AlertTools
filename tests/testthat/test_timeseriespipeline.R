setwd("../..")
require(assertthat)
require(tidyverse)

## testando conexão com o banco 
#con <-DenguedbConnect()  #
#test_that("sql connection exists", {
#      expect_equal(class(con)[1], "PostgreSQLConnection")
#})

geoc = c(3304557, 3205200) # cidade para teste

# =====================================
## Testing function getCases ==========
#======================================
# args:cities, lastday = Sys.Date(), cid10 = "A90", dataini = "notific",
#datasource=con

# common use
dC0 = getCases(cities = geoc, firstday = as.Date("2023-01-01"), 
               dataini = "sinpri", datasource=con)

test_that("getCases: produces required input error messages", {
      expect_error(getCases(cities = "Rio"),"cities should be a vector of numeric geocodes")
      #expect_error(getCases(cities=222222), "one or more cities not found")
      #expect_error_getCases(cities = c(3302403, 999999), "one or more cities not found")
      #expect_error(getCases(cities = 3302403, cid10 = "A99"),"cid10 unknown")
      #expect_error(getCases(cities = 3302403, cid10 = "dengue"),"cid10 should be a code, p.e. A92")
      #expect_error(getCases(cities = 3302403, lastday = "20-12-2019"),"check lastday's format") 
      #expect_error(getCases(cities = 3302403, lastday = "1980-12-01"),"lastday should be 2010-01-01 or more recent")
      #expect_error(getCases(cities = 3302403, dataini = "abc"),"check dataini argument, it seems incorrect")
})


#test_that("getcases: produce required warnings",{
      #expect_message(getCases(cities = 3302403, lastday = "2050-12-31"),"this date is greater than the lastday, returning the most recent data available")       
#})

#test_that("getcases: produce required output",{
#      classes <- sapply(dC0,class)
#      expect_equal(sapply(dC0,class), c("SE"="numeric","cidade"="numeric",
#                                        "CID10"="character","casos"="numeric",
#                                        "localidade"="numeric","nome"="character",
#                                        "pop"="numeric"))
#      expect_equal(sum(sapply(dC0, anyNA)),0)  # no NAs
      
#})

## 

## =====================================
# Testing getWU function ===============
#  =====================================
# args: vars = "temp_min", finalday = Sys.Date(), datasource=con
sta <- c('SBAF','SBRJ')
wuvars <- c("temp_min","temp_max","temp_med","umid_min","umid_med","umid_max",
                  "pressao_min","pressao_med","pressao_max")

dW01 = getWU(stations = sta, vars = wuvars, datasource=con) #default var is temp_min

test_that("getWU: produces required input error messages", {
      #expect_error(getWU(stations = 'SBGL', lastday = "20-12-2019"),"check lastday's format") 
      #expect_error(getWU(stations = 'SBGL', lastday = "1980-12-01"),"lastday should be 2010-01-01 or more recent")
})

test_that("output of getWU has the required columns.", {
      expect_named(dW01, c("estacao","SE", wuvars),ignore.order = TRUE)
      expect_equal(sapply(dW01,class), c("estacao"="character",SE="numeric", 
                                         "temp_min"="numeric","temp_med"="numeric","temp_max"="numeric",
                                         "umid_min"="numeric","umid_med"="numeric","umid_max"="numeric",
                                         "pressao_min"="numeric","pressao_med"="numeric","pressao_max"="numeric"))
      expect_equal(sum(sapply(dW01[,c("estacao","SE")], anyNA)),0)  # no NAs
})

### =====================================
# Testing the adjustIncidence function 
### =====================================

# check consistency between data used in getCases and nowcasting functions 
# both must use similar filters
require(INLA)

dC0a <- dC0 %>% filter(cidade == geoc[1])
dC2 <- adjustIncidence(dC0a, method = "bayesian", nyears = 1, nowSE = max(dC0a$SE))
# args: obj, method = "", pdig = plnorm((1:20)*7, 2.5016, 1.1013), 
# Dmax=12, nyears = 3, datasource = con, lastSE=NA

test_that("adjustIncidence: produce required output",{
      required_output = c("casos","tcasesICmin","tcasesmed","tcasesICmax") 
      # bayesian
      expect_true(all(required_output %in% names(dC2)))
})

test_that("adjustIncidence: tcasesmed >= casos",{
      expect_true(all(dC2$tcasesmed >= dC2$casos, na.rm = TRUE))
})

# Testing the internal bayesian adjust incidence functions 
### =====================================
#q0 <- getdelaydata(cities=geoc, years=2016:2017, datasource=con)
#q1 <- getdelaydata(cities=330455, years=2017, datasource=con)

#res0 = delaycalc(q0)
#test_that("delayCalc: runnoff matrix contains all valid cases", {
#      expect_true(sum(res0$delay.tbl$Notifications) == nrow(q0))
#})

# incluir teste se a sequencia de semanas epidemiologicas esta correta

#out0<-fitDelay.inla(res0)
#delay <- prob.inc(out0, plotar = F)

#test_that("prob.inc: returns a matrix", {
#      expect_true(class(delay) == "matrix")
#})

#test_that("prob.inc: returns a non-empty matrix", {
#      expect_true(all(is.na(delay)) == FALSE)
#})


### =====================
# Testing Rt functions
### =====================
# Rt com dist normal sem temp dep
# args: count = "casos", gtdist, meangt, sdgt, CI = "beta", alpha = .95, a0 = 2 , b0 = 3
# different ways to run it
dC00 <- dC0 %>% 
      Rt(., count = "casos", gtdist="normal", meangt=3, sdgt = 1)
dC10<-dC1 %>% 
      Rt(., count = "tcasesmed", gtdist="normal", meangt=3, sdgt = 1)

test_that("output of Rt has the required columns.", {
  expect_true(all(c("SE", "casos", "Rt", "lwr", "upr") %in% names(dC00)))
  expect_true(all(c("SE", "casos", "Rt", "lwr", "upr") %in% names(dC10)))
})

test_that("Rt: produce valid estimates",{
      # casos
      expect_true(min(dC00$Rt, na.rm=TRUE) >= 0)
      expect_true(min(dC00$lwr, na.rm=TRUE) >= 0)
      expect_true(min(dC00$upr, na.rm=TRUE) >= 0)
      # casosest
      expect_true(min(dC10$Rt, na.rm=TRUE) >= 0)
      expect_true(min(dC10$lwr, na.rm=TRUE) >= 0)
      expect_true(min(dC10$upr, na.rm=TRUE) >= 0)
})

# ----------------------
# Testing getTweet function
# ----------------------
#dT01 = getTweet(city = geoc, lastday = Sys.Date(), datasource = con) 

#test_that("output of getTweet has the required columns.", {
#  expect_true(all(c("cidade", "SE", "tweet") %in% names(dT01)))
#})




