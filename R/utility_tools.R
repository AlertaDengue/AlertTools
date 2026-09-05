# PROJETO ALERTA DENGUE -------------------------------------
# Funcoes auxiliadoras para formatacao dados de clima do Alerta dengue
# Claudia Codeco 2015 - 2020
# -----------------------------------------------------------


# epiYear ---------------------------------------------------------------------
#'@description Find to which epidemiological year belongs a given epidemiological week.
#'@title Define Epidemiological Year.
#'@export
#'@param se numeric vector with epidemiological weeks to be converted
#'@param cut epidemiological week that separates consecutive epidemiological years. Default = 41
#'@return vector of epidemiological years.
#'@examplesIf FALSE
#'epiYear(se = 201012)
#'epiYear(se = 201012:201522)

epiYear <- function(se, cut = 41){

  d <- tibble(se = se)
  d %>%
    mutate(year = floor(se/100),
           eweek = se - year*100,
           eyear = case_when(
             eweek < cut ~ year - 1,  # if se < cut, eYear = previous calendar Year
             TRUE ~ year              # if se >= cut, eYear = current calendar Year
           ))
}


# data2SE ---------------------------------------------------------------------
#'@description Find to which epidemiological week belongs a given day. Uses episem function
#'(formula generated data).
#'@title Define Epidemiological Week.
#'@export
#'@param days string vector with dates to be converted
#'@param format date format
#'@return numeric vector with the epidemiological weeks.
#'@examplesIf FALSE
#'data2SE("01-02-2020",format="%d-%m-%Y")
#'data2SE("12-02-2008",format="%d-%m-%Y")
#'data2SE(c("03-04-2013","07-01-2019"),format="%d-%m-%Y")

data2SE <- function(days, format = "%d/%m/%Y"){
  as.numeric(as_epiweek(as.Date(as.character(days), format = format)))
}

# episem ---------------------------------------------------------------------
#' @description Find to which epidemiological week belongs a given day
#' @author Oswaldo Cruz
#' @title Define Epidemiological Week.
#' @export
#' @param x date to be converted (class Date)
#' @param format date format used when `x` is not already a `Date`.
#' @param separa symbol between year and week
#' @param retorna What should be return, if epidemiological year and week ('YW'),
#' epi. year only ('Y') or epi. week only ('W').
#'   Default: 'YW'.
#' @return epidemiological week or year. If separa = '', the output is numeric;
#' otherwise is a character.
#' @examplesIf FALSE
#' episem(x= as.Date("2018-12-31", format="%Y-%m-%d"))
#' episem(x= as.Date("2015-01-01", format="%Y-%m-%d"), separa='-')
#' episem(x= as.Date("2015-01-01", format="%Y-%m-%d"), retorna='Y')

episem <- function(x, format="%Y-%m-%d", separa='', retorna='YW') {
      dates <- if (inherits(x, "Date")) x else as.Date(x, format = format)
      weeks <- as_epiweek(dates)
      year <- floor(weeks / 100)
      week <- weeks %% 100
      assert_that(retorna %in% c("YW", "Y", "W"),
                  msg = "episem: retorna must be YW, Y or W")
      out <- if (retorna == "Y") year else if (retorna == "W") week else weeks
      if (retorna == "YW" && nzchar(separa)) {
            out <- ifelse(is.na(weeks), NA_character_, sprintf("%04d%s%02d", year, separa, week))
      }
      if (nzchar(separa)) as.character(out) else as.numeric(out)
}

#' lastepiweek -----------------------------------
#' @description Calculate number of year's last epidemiological week using Brazilian standard.
#' @name lastepiweek
#' @author Marcelo F Gomes
#' @param ano Year
#' @keywords internal
#' @examplesIf FALSE
#' lastepiweek(2018)

lastepiweek <- function(ano){
      as.numeric((.epi_year_start(as.integer(ano) + 1L) - .epi_year_start(ano)) / 7)
}

# SE2date ---------------------------------------------------------------------
#'@description Return the first day of the Epidemiological Week
#'@title Return the first day of the Epidemiological Week
#'@export
#'@param se numeric vector with epidemiological weeks, for example 201420.
#'@return data.frame with SE and first day.
#'@examplesIf FALSE
#'SE2date(se=202612)
#'SE2date(se = c(202001:202209))

SE2date <- function(se){
      if(!is.numeric(se)) stop("se should be numeric or integer")
      data.frame(SE = se, ini = epiweek_start(se))
}

# daySEday---------------------------------------------------------------------
#'@description Return the first day of the Epidemiological Week and vice-versa
#'@title Return the first day of the Epidemiological Week and vice-versa
#'@export
#'@param x numeric vector with epidemiological weeks , format 201945, or date
#'@param format date format used for character input.
#'@return data.frame with SE and first day.
#'@examplesIf FALSE
#'daySEday(x=202612)
#'daySEday(x = c(202541:202604))
#'daySEday(x = c("2015-12-23", "2015-10-23", "2026-10-16"))

daySEday <- function(x, format = "%Y-%m-%d"){
      if (is.numeric(x)) return(data.frame(SE = x, ini = epiweek_start(x)))
      dates <- if (inherits(x, "Date")) x else as.Date(x, format = format)
      data.frame(SE = as.numeric(as_epiweek(dates)), ini = dates)
}


# seqSE ---------------------------------------------------------------------
#'@description Creates a sequence of epidemiological weeks and respective initial and final days
#'@title Sequence of epidemiological weeks.
#'@export
#'@param from first week in format 201401
#'@param to first week in format 201401
#'@return data.frame with the epidemiological weeks and corresponding extreme days.
#'WARNING: only works from 2010 to 2024.
#'@examplesIf FALSE
#'seqSE(202442, 202510)

seqSE <- function(from, to){
      weeks <- epiweek_seq(from, to)
      starts <- epiweek_start(weeks)
      data.frame(Ano = floor(weeks / 100), SE = as.numeric(weeks),
                 Inicio = starts, Termino = starts + 6L)
}


# sevendigitgeocode ---------------------------------------------------------------------
#'@description  calculates the verification digit of brazilian municipalities. Required
#'to convert 6 digits to 7 digits geocodes.
#'@title convert 6 to 7 digits geocodes.
#'@export
#'@param dig Six- or seven-digit municipality geocode.
#'@return 7 digits municipality geocode.
#'@examplesIf FALSE
#'sevendigitgeocode(330455)
#'sevendigitgeocode(3304557)
#'sevendigitgeocode(261153)

sevendigitgeocode <- function(dig){
      ndig = nchar(as.character(dig))
      if (ndig == 7) return(dig)
      if (ndig!=6) stop("this funtion receives 6 digits geocodes only")

      # there are 9 cities with inconsistent seventh digit.
      digspatologicos <- c(220191, 220198, 220225, 261153, 311783, 315213, 430587, 520396, 520393)
      if(dig %in% digspatologicos){
            pos <- which(digspatologicos == dig)
            dig7 <- c(2201919,2201988,2202251,2611533,3117836,3152131,4305871,5203962,5203939)[pos]
      return(dig7)
      }
      # for all the remaining, there is this rule
      peso <- c(1, 2, 1, 2, 1, 2, 0)
      soma <- 0
      digchar <- strsplit(as.character(dig),"")[[1]]
      ndig <- length(digchar)

      for (i in 1:6){
            valor <- as.integer(digchar[i]) * peso[i]
            nvalor <- ifelse(valor < 10, valor, trunc(valor/10) + valor%%10)
            soma <- soma + nvalor
      }
      dv <- ifelse(soma%%10 == 0, 0, 10 - (soma%%10))
      return(dig*10+dv)
}


# nafill ------------------------------------
#'@description  collection of imputation procedures
#'@title methods to substitute NAs. Use the function na.approx from package zoo.
#'@export
#'@param v vector with missing elements.
#'@param rule rule for filling the missing cells. "zero" just fills them with 0; "linear"
#' interpolate using zoo::na.approx. In this case, the tails are not filled. If "arima", then it interpolates using
#' linear and extrapolates using arima (calling AlertTools::temp.predict)
#'@param maxgap maximum number of consecutive NAs to fill. Longer gaps will be left unchanged. Only works for rule = "zero"
#' or "linear"
#'@param verbose Whether to emit information about missing values.
#'@return vector
#'@examplesIf FALSE
#'# Interpolation:
#'v <- c(1,2,3,NA,5,6,NA,NA,9,10,NA,NA)
#'nafill(v, rule = "zero")
#'nafill(v, rule = "linear")
#'# Inter using linear and Extrapolation using arima
#'cliSBCB <- getWU(station = "SBCB")
#'summary(cliSBCB)
#'cliSBCB <- getWU(station = "SBCB") %>%
#'           mutate(nafill("temp_min", rule = "arima"))

nafill <- function(v, rule, maxgap = 4, verbose = F){
      Nna = sum(is.na(v))
      if (verbose == T) message(paste("number of weeks with missing data is ", Nna))
      if(sum(is.na(v))!=0) {
            miss <- which(is.na(v))
            if (rule == "zero"){v[miss]<-0}
            if (rule == "linear") {v <- zoo::na.approx(v, method = "linear", maxgap = maxgap, na.rm=FALSE)}
            if (rule == "arima") v <- temp.predict(v)
      }
      v
}

# temp.predict ------------------------------------
#'@description  function for extrapolating temperature using arima
#'@title Fit arima to fill in missing data at the end of temperature time series.
#'@export
#'@param v vector with temperature data.
#'@param plotar Whether to plot observed and predicted values.
#'@return vector with replaced NA.
#'@examplesIf FALSE
#'head(cliSBCB)
#'temp.predict(v=cli[,3], plotar = T)

temp.predict <- function(v, plotar = FALSE){
      Nv=length(v) # tamanho total da serie
      datarange <- range(which(!is.na(v)))
      # tamanho do tail de na:
      Nna = Nv - datarange[2]

      x <- zoo::na.approx(v)

      if(Nna > 0){

            # Para saber os coeficientes da parte ARIMA atraves de criterios de selecao automatica:
            #automatica:
            c.a <- forecast::auto.arima(x, max.p=5, max.q=5, max.P=5, max.Q=5)$arma
            # Modelo considerando a sazonalidade, e a parte ARIMA sugerida anteriormente:
            modelo.sarima <- arima(zoo::na.approx(v),order=c.a[c(1,6,2)],seasonal=list(order=c(c.a[3],1,c.a[4]),period=52))

            message(paste("temperature predicted", Nna, "steps ahead"  ))
            predito<-predict(modelo.sarima,n.ahead=Nna)$pred

            if (plotar == T){
                  fitado <- stats::fitted(modelo.sarima)
                  # Plot para ver o desempenho do modelo in/outsample
                  plot(c(fitado,predito),col="orange",type="l",ylab="")
                  lines(x,type="l")
                  legend("bottomleft",c("Observado","Estimado"),col=c("black","orange"),lty=1)
            }

            # juntando dados com predito
            v[(datarange[2]+1):Nv] <-predito
      }
      v[datarange[1]:datarange[2]] <- x

      v
}


# getRegionais ------------------------------------
#'@description  consult database to get list of regionais
#'@title get list of regionais.
#'@export
#'@param uf full name of the state.
#'@param cities cities' geocodes
#'@param sortedby the options are: 'a' alphabetically, 'id' regional id number (only valid for regional),
#'if available
#'@param macroreg TRUE if getRegionais should return macroreg instead of reg. Default: False
#'@param output if "names" returns only a vector with names of regionais or macros.
#'If "complete" , returns municipalities and their regs
#'@param datasource name of the database
#'@return vector with names of the regionais.
#'@examplesIf FALSE
#'getRegionais(uf="Rio de Janeiro")
#'head(getRegionais(uf="Rio de Janeiro",output="complete",macroreg = TRUE))
#'head(getRegionais(uf="Rio de Janeiro",output="complete"))
#'getRegionais(cities = c(3304128,3306107,3300159), uf="Rio de Janeiro")
#'getRegionais(cities = c(3304128,3306107,3300159), uf="Rio de Janeiro", macroreg = TRUE)
#'getRegionais(uf="Rio de Janeiro", sortedby = 'id')

getRegionais <- function(cities, uf, sortedby = "a", macroreg = FALSE,
                         datasource, output = "names"){

      assert_that(!missing(uf),
                  msg = "getRegionais: please specify uf. Ex. uf = \"Ceara\" ")

      .db_validate_connection(datasource)
      d <- .repo_municipalities(
            datasource,
            cities = if (!missing(cities)) cities else NULL,
            uf = uf,
            columns = c("geocodigo", "nome", "regional", "id_regional",
                        "macroregional", "macroregional_id", "uf")
      )
        assert_that(nrow(d) > 0,
                    msg = (paste("getRegionais:
                                 Database does not have the health areas for ", uf)))

      names(d) <- c("municipio_geocodigo","cidade","regional","codigo_regional",
                    "macroregional","codigo_macroregional","uf")

      if(!missing(cities)) {
            assert_that(nrow(d) == length(cities),
            msg = (paste("getRegionais: Database does not have the health
                         districts for all listed cities in", uf)))
      }

      if(output == "names" & macroreg == TRUE) return(unique(d$macroregional))
      if(output == "names" & macroreg == FALSE) return(unique(d$regional))

      return(d)
}


# getCidades ------------------------------------
#'@description  consult database to get list of cities for regional, macroregional or uf.
#'@title get list of cities.
#'@export
#'@param uf full name of the state.
#'@param regional full name of the regional. Use getRegionais() to obtain the correct spelling.
#'@param macroregional full name of the macroregional.
#'@param datasource name of the database
#'@return vector with names of the cities.
#'@examplesIf FALSE
#'getCidades(regional = "Metropolitana I", uf="Rio de Janeiro")
#'getCidades(uf="Acre")
#'getCidades(uf="Maranhao", macroregional = "NORTE")

getCidades <- function(regional, macroregional, uf, datasource){

      if(missing(uf)) stop("getCidades: specify uf's full name. Ex: Sao Paulo")
      .db_validate_connection(datasource)
      d <- .repo_municipalities(
            datasource,
            uf = uf,
            regional = if (!missing(regional)) regional else NULL,
            macroregional = if (!missing(macroregional)) macroregional else NULL,
            columns = c("geocodigo", "nome", "regional", "id_regional",
                        "macroregional", "macroregional_id", "uf")
      )
      assert_that(nrow(d)>0, msg = "getCidades: found no city")
      names(d) <- c("municipio_geocodigo", "cidade", "regional", "regional_id",
                    "macroregional","macroregional_id","uf")
      return(d)

}


# write_parameters ------------------------------------
#'@description  Write the alert parameters for each city into the database, to be used in the update.alert.
#'Currently, the parameters are: "limiar_preseason", "limiar_posseason",
#'"limiar_epidemico,"varcli", "varcli2", "clicrit", "clicrit2" , "cid10", "codmodelo".
#' City must be already in the regionais table.
#'@title City's parameterization.
#'@noRd
#'@param params vector of the names of the params to be inserted in the table. Limiar is given as incidence.
#'It can be a subset of the default.
#'@param city Seven-digit municipality geocode.
#'@param cid10 Disease code.
#'@param overwrite Whether to replace existing parameters.
#'@param datasource Database connection.
#'@param conflict Conflict policy: `ignore`, `update`, or `error`. `overwrite = TRUE`
#'selects `update` for backwards compatibility.
#'@return the new line in the parameters table
#'@examplesIf FALSE
#'pars = data.frame(municipio_geocodigo = 3506003,limiar_preseason = 4.50243, limiar_posseason = 3.962566,
#'limiar_epidemico = 67.72364, varcli = "temp_min", clicrit = 22, cid10 = "A90", codmodelo = "Af")
#'res = write_parameters(params$municipio_geocodigo, params$cid10, params = pars)

.write_parameters_impl <- function(city, cid10, params, overwrite = FALSE, datasource,
                             conflict = c("ignore", "update", "error")) {
      assert_that(is.data.frame(params),
                  msg = "write_parameters: params should be a data.frame")
      assert_that(nrow(params) == 1,
                  msg = "write_parameters write one line only")
      assert_that(cid10 %in% c("A90", "A92.0", "A92.8"),
                  msg = paste("write_parameters: not prepared for cid10 =", cid10))
      assert_that(all(c("municipio_geocodigo", "cid10") %in% names(params)),
                  msg = "write.parameters: params must contain municipio_geocodigo, cid10")
      .db_validate_connection(datasource)

      conflict <- match.arg(conflict)
      if (isTRUE(overwrite)) conflict <- "update"
      assert_that(as.numeric(params$municipio_geocodigo) == as.numeric(city) &&
                    as.character(params$cid10) == as.character(cid10),
                  msg = "write_parameters: keys in params must match city and cid10")

      .repo_write_rows(
            datasource, "parameters", params,
            key = c("municipio_geocodigo", "cid10"), conflict = conflict
      )
      .repo_parameters(datasource, city, cid10)
}

# read.parameters ------------------------------------
#'@description  Read the alert parameters for a set of cities from the database, to be used in the infodengue pipeline.
#'Currently, the parameters are: "limiar_preseason" (pre-season incidence threshold calculated using MEM),
#'"limiar_posseason" (pos-season incidence threshold), "limiar_epidemico"(epidemic threshold), "varcli" (name of the critical
#'meteorological variable), "clicrit" (critical value of the meteorological variable), "cid10",
#'"codmodelo" (name of the heuristic decision model, see serCriteria()). These parameters are specified when the city is initiated
#'in the pipeline.
#'@title Get city-level alert parameters for the infodengue pipeline.
#'@noRd
#'@param cities cities' geocodes. Tip: find them using getCidades().
#'@param cid10 Dengue = "A90" (default), Chik = "A92.0", Zika = "A92.8"
#'@param datasource SQL connection to the database
#'@return dataframe with all parameters
#'@examplesIf FALSE
#'read.parameters(cities = 3118601, cid10 = "A90")
#'cid <- getCidades(regional = "Norte",uf = "Rio de Janeiro")
#'read.parameters(cities = cid$municipio_geocodigo, cid10 = "A90")

.fetch_alert_parameters_impl <- function(cities, cid10 = "A90", datasource){

      cities <- sapply(cities, function(x) sevendigitgeocode(x))
      if(!cid10 %in% c("A90", "A92.0")) {
        warning("Parameter table currently supports only A90 (dengue) and A92.0 (chikungunya). Using dengue parameters (A90).")
        cid10 <- "A90"
      }


      # specific rule: Espirito Santo only has dengue parameters
      if(any(grepl("^32", cities)) && cid10 != "A90") {
        warning("Parameters for Espirito Santo are only available for dengue (A90). Using A90.")
        cid10 <- "A90"
      }

      .db_validate_connection(datasource)
      dd <- .repo_parameters(datasource, cities, cid10)

      assert_that(all(cities %in% dd$municipio_geocodigo),msg = ("check if cities and cid10 are in the parameter table"))

      return(dd)
}

# getWUstation ------------------------------------------
#'@description  Get the meteorological stations associated with one or more cities
#'@title get meteorological stations
#'@export
#'@param cities vector with geocodes
#'@param datasource connection to the project database
#'@return data.frame
#'@examplesIf FALSE
#'getWUstation(cities = 3304557)
#'cidades <- getCidades(regional = "Sete Lagoas", uf = "Minas Gerais")
#'getWUstation(cities = cidades$municipio_geocodigo)

getWUstation <- function(cities, datasource){
  .db_validate_connection(datasource)
  .repo_station_links(datasource, cities)
}

# setWUstation ------------------------------------------
#'@description  Set primary and secondary meteorological stations associated
#'with one or more cities of the same state
#'@title set meteorological stations
#'@export
#'@param st data.frame containing municipio_geocodigo, primary_station,
#'secondary_station
#'@param UF name of the state.Ex. "Rio de Janeiro"
#'@param datasource connection to the project database
#'@examplesIf FALSE
#'# NOT RUN
#'wudata = data.frame(municipio_geocodigo = 3107802, primary_station = "SBIP",
#'secondary_station = "SBGV")
#'setWUstation(wudata, UF = "Minas Gerais")
#'getWUstation(cities =wudata$municipio_geocodigo)

setWUstation <- function(st, UF, datasource){

      ncities <- nrow(st)

      # checking inputs
      assert_that(class(st) == "data.frame",
                  msg = "setWUstation: st should be a data.frame")

      assert_that(all(names(st) %in% c("municipio_geocodigo" , "primary_station",
                                 "secondary_station")),
                  msg = "setWUstation: st should contain columns municipio_geocodigo,
                        primary_station, secondary_station")

      .db_validate_connection(datasource)

      # check if city is already in the system (Regional table)
      cities_table <- getCidades(uf = UF, datasource = datasource)
      cities_in <- st$municipio_geocodigo %in% cities_table$municipio_geocodigo

      assert_that(sum(cities_in)==ncities,
                  msg = paste("geocodes", st$municipio_geocodigo[cities_in == FALSE] ,
                              "not implemented in Infodengue.") )

      updates <- data.frame(
            municipio_geocodigo = st$municipio_geocodigo,
            codigo_estacao_wu = st$primary_station,
            estacao_wu_sec = st$secondary_station,
            stringsAsFactors = FALSE
      )
      invisible(.repo_update_station_links(datasource, updates))
}
