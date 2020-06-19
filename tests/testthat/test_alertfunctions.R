## Testing the alert_functions ####

setwd("../..")

# cities for testing
geoc = c(3302403, 3205200)

# 
# Testing setcriteria --------------------------
# 
# common use
val = c(varcli ="temp_min", "clicrit"="22","limiar_preseason"="10","limiar_epidemico"="100")
criteria = setCriteria(rule="Af",values=val)

test_that("setcriteria: produces required input error messages", {
      expect_error(setCriteria(rule="XX"),"setcriteria: rule unknown.")
      expect_error(setCriteria(), "if rule is null, values must be provided")
      expect_error(setCriteria(rule = "Aw", values = val), "setcriteria: Af requires temp_min and Aw requires umid_max")
})

test_that("setcriteria: produce required output",{
      expect_named(criteria, c("crity", "crito", "critr"))
      classes <- sapply(criteria,class)
      expect_equal(sapply(criteria,class), c("character", "character","character"))
      expect_equal(sapply(criteria,length), c(3,3,3))
})


# 
# Testing the fouralert function ------------------
# 


cas = getCases(cities = geoc[1], cid10 = "A90", datasource=con) %>% 
      Rt(count = "casos",gtdist="normal", meangt=3, sdgt = 1) %>%
      mutate(inc = casos/pop*100000)
cli = getWU(stations = 'SBGL', vars=c("temp_min"), datasource=con) %>%
      mutate(temp_min = nafill(temp_min, rule = "arima")) 
ale <- plyr::join_all(list(cas,cli),by="SE") 
resf <- fouralert(ale, crit = criteria)

test_that("fouralert: produces required input error messages", {
      #expect_error(fouralert(cities = "Rio"),"cities should be a vector of numeric geocodes")
      #expect_error(getCases(cities=222222), "one or more cities not found")
})

test_that("fouralert: produces the required output", {
      expect_named(resf, c("data", "indices", "crit", "n"))
      expect_equal(sapply(resf,class), c(data = "data.frame", indices = "data.frame",
                                         crit = "list", n = "numeric"))
      expect_equal(class(resf),"alerta")
})

#critgy <- c("temp_min > 22 | tweet > 80", 3, 3)
#alerta <- twoalert(d0, cy = critgy)

#test_that("twoalert has the minimum set of items.", {
#      expect_true(all(c("data", "indices", "rules","n") %in% names(alerta)))
#})

# Testing alerio2 ----------------------

# common use
params <- c(varcli ="temp_min", clicrit=22, limiar_epidemico=100, limiar_preseason = 14.15)
criter <- setCriteria(rule="Af", values = params)
alerio <- alertaRio(naps = 0:2, crit = criter, se=201304, delaymethod="fixedprob")

test_that("alertrio: produces the required output", {
      expect_named(alerio, c("APS 1", "APS 2.1", "APS 2.2"))
      expect_named(alerio[[1]], c("data", "indices", "crit", "n"))
      expect_named(alerio[[1]]$data, c("localidadeid","SE","casos","localidade","populacao",
                                       "cidade","nome","CID10","tcasesICmin","tcasesmed",
                                       "tcasesICmax","pdig","Rt","lwr","upr","p1","inc",
                                       "estacao","temp_min","tweet"))
      expect_named(alerio[[1]]$indices, c("cytrue","nytrue","cotrue","notrue","crtrue",
                                          "nrtrue","level"))
      expect_equal(class(alerio[[1]]), "alerta")
})


