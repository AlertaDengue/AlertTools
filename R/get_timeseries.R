# PROJETO ALERTA DENGUE -------------------------------------
# FUNCOES PARA ORGANIZAR SERIES
#TEMPORAIS A PARTIR DOS DADOS BRUTOS CLAUDIA CODECO - 2015

# getWU --------------------------------------------------------
#'@description Create weekly time series from meteorological station data in server taking the mean of the daily values.
#'@title Get Climate Data
#'@export
#'@param stations station code (4 digits).
#'@param vars vector with meteorological variables. Available variables: "temp_min" (default),
#'"temp_max","temp_med","data_dia","umid_min","umid_med","umid_max","pressao_min","pressao_med","pressao_max"
#'@param finalday last day. Default is the last available. Format = Y-m-d.
#'@param iniSE First epidemiological week to return.
#'@param datasource Use "data/WUdata.rda" to use test dataset. Use the connection to the Postgresql server if using project data. See also DenguedbConnect
#' to open the database connection.
#'@return data.frame with the weekly data (cidade estacao data temp_min tmed tmax umin umed umax pressaomin pressaomed pressaomax)
#'@examplesIf FALSE
#'# NOT USE: connection <- dbConnect(RSQLite::SQLite(), "../../AlertaDengueAnalise/mydengue.sqlite")
#'res = getWU(stations = c('SBRJ','SBGL'), vars="temp_min", iniSE = 201201)
#'res = getWU(stations = 'SBRJ', vars=c("temp_min", "temp_med"))
#'tail(res)

getWU <- function(stations, vars = "temp_min", finalday = Sys.Date(), iniSE = 201001,
                  datasource) {

      # validade climate variables
      wuvars <- c("temp_min","temp_max","temp_med","umid_min","umid_med","umid_max",
                  "pressao_min","pressao_med","pressao_max")
      if(any(!(vars %in% wuvars))) stop("wu climate variable(s) unknown or mispecified")

      .db_validate_connection(datasource)
      stations <- unique(as.character(stations))
      catalog <- .repo_station_catalog(datasource, stations)
      foundsta <- intersect(stations, as.character(catalog$estacao_id))
      d <- .repo_station_climate(datasource, stations, finalday)
      foundsta <- intersect(foundsta, unique(as.character(d$Estacao_wu_estacao_id)))
      notfoundsta <- setdiff(stations, foundsta)
      if (length(notfoundsta)) {
        warning(paste("station", notfoundsta, "does not exist in the database."))
      }
      if (!length(foundsta)) stop("'stations' unknown. Check their names.")
      message("these are the wu stations used: ", paste(foundsta, collapse = ", "))
      d <- d[d$Estacao_wu_estacao_id %in% foundsta, , drop = FALSE]


      # agregando vars climaticas por semana (ignora NAs)
      d1 = d %>%
            mutate(estacao = Estacao_wu_estacao_id) %>%
            mutate(SE = data2SE(data_dia, format = "%Y-%m-%d")) %>% # creating column SE
            group_by(estacao,SE)  %>%
            summarise(across(all_of(vars), ~ mean(.x, na.rm = TRUE)), .groups = "drop")

      # criar serie temporal-----------------------------------------
        st <-  expand.grid(estacao = stations,
                        SE = seqSE(from = iniSE, to = max(d1$SE))$SE,
                        stringsAsFactors = FALSE) %>%
                left_join(.,d1,by = c("estacao", "SE")) %>%
            arrange(estacao,SE)
    st

}

#bestWU -----------------------------------------------------------------
#'@description Receives a set of meteorological data and return the most complete for
#' a given city.If both are good, the first is returned.
#'@export
#'@title Chooses the most updated meteorological data for a city from a set of two choices
#'@param series list of the competing time series.
#'See examples.
#'@param var name of the meteorological variable
#'@return data.frame with the best series
#'@examplesIf FALSE
#'# NOT USE: connection <- dbConnect(RSQLite::SQLite(), "../../AlertaDengueAnalise/mydengue.sqlite")
#'series1 = getWU(stations = 'SBCP',vars="temp_min", datasource= connection)
#'series2 = getWU(stations = 'SBME',vars="temp_min", datasource= connection)
#'res = bestWU(list(series1, series2),var="temp_min")
#'tail(res)

bestWU <- function(series,var){

      if(missing(var))stop("bestWU: please specify a valid meteorological variable")
      if(class(series)!="list") stop("bestWU: WU data must be in a list")

      # prop missing data
      propNA <- sapply(series,function(x,v=var) sum(is.na(x[,v]))/nrow(x))
      # last date with data
      if(sum(propNA == 1) == 1) {
            ser <- series[[which(propNA != 1)]]
            emptysta <- unique(series[[which(propNA == 1)]]$estacao)
            sta <- unique(ser$estacao)
            message(paste("WU station",emptysta, "has no data. Using", sta ))

            return(ser)
            }

      if(sum(propNA) == 2) { #none has data
            message("WARNING: Both stations without data. Returning the first one with NAs")
            ser <- series[[1]]
            return(ser)
      } else { # both has data
            lastdate <- sapply(series, function(x,v=var) x$SE[max(which(is.na(x[,v])==FALSE))])
            return(series[[which.max(lastdate)[1]]])
            }
}

# getClima --------------------------------------------------------
#'@description Create weekly climate time series from satellite data in server
#'taking the mean of the daily values. Data source: ERA5.
#'@title Get Climate Data
#'@noRd
#'@param cities list of geocodes.
#'@param vars vector with meteorological variables. Minimum set of vars
#'"temp_min", "temp_max". Default: all of them
#'@param iniSE first epiweek. Default is 012015
#'@param lastSE last epiweek. Optional. Default is the last available
#'@param finalday alternative to lastSE. Default is the last day available. Format = Y-m-d.
#'@param datasource a valid database connection
#'@return data.frame with the weekly data (cidade data temp_min tmed tmax umin umed umax pressaomin pressaomed pressaomax)
#'@examplesIf FALSE
#'res = getClima(cities = c(3304557), vars=c("temp_min","temp_max") , iniSE = 202201)
#'res = getClima(cities = c(3304557, 3200300), iniSE = 202403)
#'tail(res)

.fetch_climate_impl <- function(cities, vars = c("temp_min","temp_max","temp_med","umid_min","ampT",
                                      "umid_med","umid_max", "precip_tot","precip_max"),
                     finalday = Sys.Date(), iniSE = 201501, lastSE, datasource) {

  allowed_vars <- c("temp_min", "temp_max", "temp_med", "umid_min", "ampT",
                    "umid_med", "umid_max", "precip_tot", "precip_max")
  if(any(!(vars %in% allowed_vars))) stop("climate variable(s) unknown or mispecified")
  if("ampT" %in% vars) {varsred = vars[-which(vars == "ampT")]} else
  {varsred = vars}

  assert_that(all(c("temp_min","temp_max") %in% varsred), msg="getClima: vars should
                                                           include temp_min, temp_max. Check it!")
  # dates
  #if(missing(lastSE)) lastSE <- epiweek(finalday, format = "Y-m-d")
  iniday <- SE2date(iniSE)$ini

  .db_validate_connection(datasource)
  cities <- sapply(cities, sevendigitgeocode)
  varscomplete <- c("geocode", "date", varsred)
  d <- .repo_municipal_climate(datasource, cities, varscomplete, iniday, finalday)

  # agregando vars climaticas por semana
  d1 <- d %>%
    mutate(SE = data2SE(date, format = "%Y-%m-%d")) %>% # creating column SE
    group_by(geocode,SE)  %>%
    summarise(across(all_of(varsred), ~ mean(.x, na.rm = TRUE)), .groups = "drop") %>%
    rename(geocodigo = geocode)

  d1$geocodigo <- as.numeric(d1$geocodigo)

  if("ampT" %in% vars) d1$ampT = d1$temp_max - d1$temp_min


  # check output-----------------------------------------
  assert_that(all(c("geocodigo","SE") %in% names (d1)), msg = "debug getClima required")

  d1

}

# getPop --------------------------------------------------------
#'@description Get population time series, using the brpop package.
#'Currently has estimates from 2000 to 2021
#'@title Get population data at municipal level.
#'@export
#'@param cities list of geocodes.
#'@param iniY first year. Default is 2010
#'@param endY last year. Default is the last available
#'@return tibble with columns geocode, year, pop
#'@examplesIf FALSE
#'cities <- getCidades(uf = "Paraná")$municipio_geocodigo
#'pop <- getPop(cities, iniY = 2018, endY = 2019)
#'tail(pop)

getPop <- function(cities, iniY = 2010, endY) {
      if (!requireNamespace("brpop", quietly = TRUE)) {
            stop("getPop requires the optional package 'brpop'.", call. = FALSE)
      }

      # check input
      assert_that(all(c(iniY) >= 2000),
                  msg = "getPop: check dates, getPop only has data from 2000 to 2021.")

      assert_that(all(c(iniY,endY) <= 2021),
                  msg = "getPop: check dates, getPop only has data from 2000 to 2021.")

      cities6 <- sapply(cities, function(x) floor(x/10))

      x <- brpop::mun_pop_totals() %>%
            filter(mun %in% cities6 & year >= iniY & year <= endY) %>%
            select(geocode6 = mun,
                   year,
                   pop)

      x$geocode <- sapply(x$geocode6, function(x) sevendigitgeocode(x))

      # check output
      assert_that(all(cities %in% x$geocode),
                  msg = "getPop: not all cities with pop data. check geocodes")

      x[,c("geocode", "year", "pop")]
}

# GetCases --------------------------------------------------------------
#'@description Create weekly time series from case data from server. The source is the SINAN.
#'@title Get Case Data and aggregate per week and area
#'@noRd
#'@param cities cities' geocode.
#'@param lastday last day. Default is the last available. Format: "yyyy-mm-dd"
#'@param completetail if sinan data is older than final_day, fill in the tail with NA (default) or 0.
#'@param dataini "notific" if data aggregated by notification date or "sinpri" if data aggregated
#' if aggregated by date of first symptoms
#'@param firstday is the first date of the time series to be produced.  Format: "yyyy-mm-dd"
#'@param cid10 cid 10 code. Dengue = "A90" (default), Chik = "A92.0", Zika = "A92.8",
#'@param type case definition. Default = "notified". Other options: "probable",
#'"lab_confirmed", "all". All means returning the three counts.
#'@param datasource PostgreSQLConnection to project database.
#'@param verbose Whether to emit progress messages.
#'@return data.frame with the data aggregated per week according to disease onset date.
#'Notice that the names of the columns and the number of columns will change according to type.
#'To recover the original function behavior, use the default type.
#'@examplesIf FALSE
#'# NOT USE: connection <- dbConnect(RSQLite::SQLite(), "../../AlertaDengueAnalise/mydengue.sqlite")
#'d <- getCases(cities =  3106200, dataini = "sinpri", type = "all") # dengue
#'d <- getCases(cities = 3300936, completetail = 0) # dengue
#'d <- getCases(cities = 3304557, cid10="A92.0") # chikungunya, until last day available
#'cid <- getCidades(regional = "Norte",uf = "Rio de Janeiro")
#'d <- getCases(cities = cid$municipio_geocodigo[1:2],
#'firstday = as.Date("2023-01-01"), dataini = "sinpri")
#'tail(d)

.fetch_cases_impl <- function(cities, lastday = Sys.Date(), firstday = as.Date("2018-01-01"),
                     cid10 = "A90", dataini = "notific", completetail = NA,
                     type = "notified", datasource, verbose = FALSE) {

      assert_that(class(cities) %in% c("integer","numeric"),
                  msg = "cities should be a vector of numeric geocodes")

      assert_that(dataini %in% c("sinpri", "notific"), msg="getCases: dataini should
                                                           be sinpri or notific. Check it!")
      cities <- sapply(cities, function(x) sevendigitgeocode(x))

      .db_validate_connection(datasource)
      firstday <- as.Date(firstday)
      lastday <- as.Date(lastday)
      assert_that(!is.na(firstday) && !is.na(lastday) && firstday <= lastday,
                  msg = "getCases: firstday must not be after lastday")
      disease <- .normalize_cid(cid10)
      cid10 <- disease$canonical
      dd <- .repo_notifications(datasource, cities, disease$values, firstday, lastday)
      if(nrow(dd)==0) stop("getCases found no data")
      varglobais <- .repo_municipalities(datasource, cities = cities)

      # definindo a data para calculo da semana
      if(dataini == "notific"){
            if (isTRUE(verbose)) message("cases aggregated by notification date")
            dd <- dd %>%
                  mutate(SE = ano_notif*100+se_notif)
            }
      if(dataini == "sinpri"){
            # fixing wrong dt_sinpri's using the median time to notification (3days)
            #w <- (dd$dt_notific - dd$dt_sin_pri) > 60
            #lw <- sum(w, na.rm = TRUE); plw <- round((lw / nrow(dd) * 100), digits = 2)

            #message(paste("there are", lw, "(",plw ,"%)","cases with implausible dt_sinpri. Imputed with dt_notific"))
            #dd$dt_sin_pri[which(w==TRUE)] <- dd$dt_notific[which(w==TRUE)] - 3  # median delay

            # remove cases with wrong dt_sin_pri
            # this condition must be equal in the bayesnowcasting function

            dd$ininotif <- dd$dt_notific - dd$dt_sin_pri
            wrongdates <- which(dd$ininotif > 365 | dd$ininotif < 0 | is.na(dd$dt_sin_pri))  # tirar o 30?
            if(length(wrongdates) > 0) {
            if (isTRUE(verbose)) message(length(wrongdates), " records with invalid symptom dates")
            dd <- dd[-wrongdates,]
            }

            # calculating  epiweek from dt_sin_pri
            dd$se_sin_pri <- lubridate::epiweek(as.Date(dd$dt_sin_pri, format = "%Y-%m-%d"))
            dd <- dd %>%
                  mutate(ano_sinpri = lubridate::epiyear(dt_sin_pri),
                         SE = ano_sinpri*100+se_sin_pri)
            if (isTRUE(verbose)) message("cases aggregated by symptom onset date")

            }

      # identificando os casos de acordo com a definicao
      #dd$tipo <- NA
      #dd$tipo[dd$classi_fin == 5] <- "discarded"
      #dd$tipo[dd$classi_fin != 5] <- "probable"

      # contando os casos de acordo com a definicao

     casos <- dd %>%
            group_by(municipio_geocodigo, SE) %>%
            summarise(
                  casos = length(classi_fin),
                  cas_desc = sum(classi_fin == 5, na.rm = TRUE),
                  cas_lab = sum(classi_fin != 5 & criterio == 1 , na.rm = TRUE)) %>%
           mutate(cas_prov = casos - cas_desc)

      # criando serie
      lastSE <- data2SE(lastday, format = "%Y-%m-%d")
      firstSE <- data2SE(firstday, format = "%Y-%m-%d")

      sem <-  expand.grid(municipio_geocodigo = cities,
                          SE = seqSE(from = firstSE, to = lastSE)$SE)

      st <- left_join(sem, casos, by = c("municipio_geocodigo", "SE")) %>%
            arrange(municipio_geocodigo, SE) %>%
            mutate(localidade = 0) %>%  # para uso qdo tiver divisao submunicipal
            mutate(geocodigo = municipio_geocodigo) %>%
            mutate(CID10 = cid10)%>%
            left_join(.,varglobais,"geocodigo") %>%
            select(SE, cidade = municipio_geocodigo,CID10, casos, cas_prov,
                   cas_lab, localidade, nome, pop=populacao)

      # substitute NA for zero to indicate that no case was reported that week
      st$casos[(is.na(st$casos))] <- 0
      st$cas_prov[(is.na(st$cas_prov))] <- 0
      st$cas_lab[(is.na(st$cas_lab))] <- 0

      if(any(is.na(st$pop)))
            warning("getCases function failed to import pop data for one or more cities", cities)

      # choosing what to return
      #if(type == "notified") return(subset(st, select = -c(cas_prov, cas_lab)))
      #if(type == "probable") return(subset(st, select = -c(casos, cas_lab)))
      #if(type == "lab_confirmed") return(subset(st, select = -c(casos, cas_prov)))
      #if(type == "all") return(st)
      attr(st, "case_records") <- dd
      st
}


# read.cases --------------------------------------------------------------
#' @title Function to extract case count for covered municipalities
#' @description Function \code{read.cases} extract notification data from database
#' and aggregate by epiweek. Used by mem.
#' @export
#' @param start_year first year of the time series
#' @param end_year last year of the time series
#' @param cid10 cid 10 code. Dengue = "A90" (default), Chik = "A92.0", Zika = "A92.8"
#' @param datasource Infodengue connection
#' @param mun_list vector with the municipalities' 7 digit geocodes
#' @author Marcelo F C Gomes
#' @examplesIf FALSE
#' dd <- read.cases(2019, 2020, mun_list = c(4108304, 3300936))
#' dd <- read.cases(2019, 2020, cid10 = "A92.0", mun_list = c(4108304, 3300936))

read.cases <- function(start_year, end_year, cid10 = "A90", datasource, mun_list){
  .db_validate_connection(datasource)
  assert_that(length(start_year) == 1L && length(end_year) == 1L &&
                is.numeric(start_year) && is.numeric(end_year) && start_year <= end_year,
              msg = "read.cases: invalid year interval")
  disease <- .normalize_cid(cid10)
  df.cases.weekly <- .repo_notifications(
    datasource,
    cities = sapply(mun_list, sevendigitgeocode),
    cids = disease$values,
    start_year = start_year,
    end_year = end_year,
    columns = c("dt_notific", "se_notif", "ano_notif", "municipio_geocodigo")
  )
  if (!nrow(df.cases.weekly)) stop("read.cases found no data", call. = FALSE)
      # In the database we have each single notification. So we'll have to aggregate
      #later on We'll use an object name "weekly" from the start since we'll overwrite
      #it with the aggregate later.



      # Auxiliary function to correctly generate SE in the format YYYYWW from columns
      # notification week (WW) and current year. Since dates in the begining(end) of an year
      # can belong to epidemiological weeks from the previous(year), we must check the week and month
      # of the notification. If the epiweek (w) is 52 or 53 and the month is 01 (Jan), then it is still
      # from an epiweek of the previous year. Example: a case from 2016-01-01 is of epiweek 2001553.
      # If, on the other hand, epiweek is 1 and the month is 12 (Dec), then the epiweek is already
      # of the next year. Example: 2014-12-31 is of epiweek 201501.
      f.se <- function(w, m, y){
            if (w > 51 & m=='01'){
                  y <- y-1
            } else if (w == 1 & m == '12'){
                  y <- y+1
            }
            return(as.integer(y*100 + w))
      }

      df.cases.weekly$SE <- mapply(function(w, m, y) f.se(w,m,y) , df.cases.weekly$se_notif,
                                   format(df.cases.weekly$dt_notific, '%m'), df.cases.weekly$ano_notif)

      # Aggregate by municipio_geocodigo and SE.
      # Using table is faster than using aggregateby.notified.cases.R function
      df.cases.weekly <- data.frame(table(df.cases.weekly[, c('municipio_geocodigo', 'SE')]), stringsAsFactors = FALSE)
      names(df.cases.weekly) <- c('municipio_geocodigo', 'SE', 'casos')

      # Fill all epiweeks:
      df.epiweeks <- data.frame(municipio_geocodigo=integer(), SE=integer())
      # List of locations:
      mun_list <- unique(df.cases.weekly$municipio_geocodigo)
      df.cases.weekly$SE <- as.integer((levels(df.cases.weekly$SE))[df.cases.weekly$SE])
      effec_start_year <- min(floor(df.cases.weekly$SE/100))
      years.list <- c(effec_start_year:end_year)
      for (mun in mun_list){

            for (y in years.list){
                  epiweeks <- c()
                  lweek <- as.integer(lastepiweek(y))
                  for (w in c(1:lweek)){
                        epiweeks <- c(epiweeks, as.integer(paste0(y, sprintf('%02d', w))))
                  }
                  df.epiweeks <- rbind(df.epiweeks, data.frame(list(municipio_geocodigo=mun, SE=epiweeks)))
            }

      }

      df.cases.weekly <- merge(df.epiweeks, df.cases.weekly, by=c('municipio_geocodigo', 'SE'), all.x=T)
      df.cases.weekly[is.na(df.cases.weekly)] <- 0
      df.cases.weekly$municipio_geocodigo <- as.integer(df.cases.weekly$municipio_geocodigo)

      return(df.cases.weekly)
}


# getTweet --------------------------------------------------------------
#'@description Create weekly time series from tweeter data from server. The
#'source of this data is the Observatorio da Dengue (UFMG).
#'@title Get Tweeter Data
#'@export
#'@param cities cities's geocode. Use getCidades()
#'@param cid10 default is A90 (dengue). If not dengue, returns NA
#'@param lastday last day. Default is the last available.
#'@param datasource Use the connection to the Postgresql server for using project data.
#'@return data.frame with weekly counts of people tweeting on dengue.
#'@examplesIf FALSE
#'# NOT USE: connection <- dbConnect(RSQLite::SQLite(), "../../AlertaDengueAnalise/mydengue.sqlite")
#'tw <- getTweet(cities = c(3302205,3200300), lastday = "2014-03-01")
#'tw <- getTweet(cities = 3304557, finalday = "2016-03-01")
#'cid <- getCidades(regional = "Norte",uf = "Rio de Janeiro")
#'tw <- getTweet(cities = cid$municipio_geocodigo)
#'tail(tw)

getTweet <- function(cities, lastday = Sys.Date(), cid10 = "A90", datasource) {

      cities <- sapply(cities, function(x) sevendigitgeocode(x))

      # get tweets on dengue
      if (cid10 == "A90"){

            .db_validate_connection(datasource)
            tw <- .repo_tweets(datasource, cities, as.Date(lastday))

      } else {stop(paste("there is no tweet for", cid10,"in the database"))}

      # no tweets found for these cities
      if(nrow(tw) == 0){
            message(paste("cidade(s)",cities,"nunca tweetou sobre dengue"))
            tw <- expand.grid(Municipio_geocodigo = cities,
                              SE = seqSE(from = 201001,
                                         to = data2SE(lastday,
                                                      format = "%Y-%m-%d"))$SE)
            tw$tweet <- 0
            return(tw)
      }

      # checking if tweets were partially found
      tots = tapply(tw$numero,tw$Municipio_geocodigo,sum)
      if (any(tots==0)) message(paste("cidade(s)",cities[which(tots==0)],"nunca tweetou sobre dengue"))

      # Counting number of tweets per SE and city
      tw <- tw %>%  #
            mutate(SE = data2SE(data_dia, format = "%Y-%m-%d")) # creating column SE

      sem <-  expand.grid(Municipio_geocodigo = cities,
                          SE = seqSE(from = 201001, to = data2SE(lastday,
                                                                 format = "%Y-%m-%d"))$SE)
      st <- full_join(sem,tw,by = c("Municipio_geocodigo", "SE")) %>%
            arrange(Municipio_geocodigo,SE) %>%
            group_by(Municipio_geocodigo,SE)  %>%
            summarize(tweet = sum(numero, na.rm = TRUE))  %>%
            select(Municipio_geocodigo, SE, tweet)

      return(as.data.frame(st))

}

# GetCaseslist --------------------------------------------------------------
#'@description Get case data from server.
#'@title Get Case Data
#'@noRd
#'@param cities cities' geocode.
#'@param lastday last day. Default is the last available. Format: "yyyy-mm-dd"
#'@param firstday is the first date of the time series to be produced.  Format: "yyyy-mm-dd"
#'@param cid10 cid 10 code. Dengue = "A90" (default), Chik = "A92.0", Zika = "A92.8",
#'@param datasource PostgreSQLConnection to project database.
#'@return data.frame with the data aggregated per week according to disease onset date.
#'Notice that the names of the columns and the number of columns will change according to type.
#'To recover the original function behavior, use the default type.
#'@examplesIf FALSE
#'# NOT USE: connection <- dbConnect(RSQLite::SQLite(), "../../AlertaDengueAnalise/mydengue.sqlite")
#'d <- getCaseslist(cities = c(4209102, 3304557),firstday = as.Date("2024-01-01")) # dengue
#'d <- getCaseslist(cities = 3304557, cid10="A92.0") # chikungunya, until last day available
#'cid <- getCidades(regional = "Norte",uf = "Rio de Janeiro")
#'d <- getCaseslist(cities = 3304557, firstday = as.Date("2023-01-01"),dataini)
#'tail(d)

.fetch_case_records_impl <- function(cities, lastday = Sys.Date(), firstday = as.Date("2022-01-01"), cid10 = "A90",
                     datasource) {

      assert_that(class(cities) %in% c("integer","numeric"),
                  msg = "cities should be a vector of numeric geocodes")

      cities <- sapply(cities, function(x) sevendigitgeocode(x))
      .db_validate_connection(datasource)
      firstday <- as.Date(firstday)
      lastday <- as.Date(lastday)
      assert_that(!is.na(firstday) && !is.na(lastday) && firstday <= lastday,
                  msg = "getCaseslist: firstday must not be after lastday")
      disease <- .normalize_cid(cid10)
      dd <- .repo_notifications(datasource, cities, disease$values, firstday, lastday)
      if(nrow(dd)==0) stop("getCaseslist found no data")

      return(dd)
}
