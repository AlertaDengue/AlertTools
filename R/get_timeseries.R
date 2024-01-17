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
#'@param datasource Use "data/WUdata.rda" to use test dataset. Use the connection to the Postgresql server if using project data. See also DenguedbConnect
#' to open the database connection. 
#'@return data.frame with the weekly data (cidade estacao data temp_min tmed tmax umin umed umax pressaomin pressaomed pressaomax)
#'@examples
#'NOT USE: con <- dbConnect(RSQLite::SQLite(), "../../AlertaDengueAnalise/mydengue.sqlite")
#'res = getWU(stations = c('SBRJ','SBGL'), vars="temp_min", iniSE = 201201)
#'res = getWU(stations = 'SBRJ', vars=c("temp_min", "temp_med"))
#'tail(res)

getWU <- function(stations, vars = "temp_min", finalday = Sys.Date(), iniSE = 201001,
                  datasource=con) {
      
      # validade climate variables
      wuvars <- c("temp_min","temp_max","temp_med","umid_min","umid_med","umid_max",
                  "pressao_min","pressao_med","pressao_max")
      if(any(!(vars %in% wuvars))) stop("wu climate variable(s) unknown or mispecified")

      # check if all stations exist
      sqlstations = paste("'", str_c(stations, collapse = "','"),"'", sep="")
      
      
      if(class(datasource) == "PostgreSQLConnection"){
        checkStationComm <- paste("SELECT estacao_id, nome FROM 
                                \"Municipio\".\"Estacao_wu\" WHERE  estacao_id 
                                IN (", sqlstations, ")",sep="")
        stanames <- dbGetQuery(datasource,checkStationComm)  
        if(nrow(stanames)==0) stop("'stations' unknown. Check their names.")
        notfoundsta <- stations[!(stations %in% stanames$estacao_id)]
        if(length(notfoundsta) > 0) warning(paste("station",notfoundsta, 
                                                  "does not exist in the database."))
        
        # Geting the data from the available stations
        message(paste("are the wu stations used", cat(stanames$nome)))
        sqlstations = paste("'", str_c(stanames$estacao_id, collapse = "','"),"'", sep="")
        
        comando <- paste("SELECT * from \"Municipio\".\"Clima_wu\" WHERE 
                        \"Estacao_wu_estacao_id\" IN  (", sqlstations, ") AND 
                         data_dia <= '",finalday,"'",sep="")
        
        d <- dbGetQuery(datasource,comando) 
      }
      
      if(class(datasource) == "SQLiteConnection"){
        allsta <- unique(dbGetQuery(datasource, 'SELECT Estacao_wu_estacao_id FROM wu')$Estacao_wu_estacao_id)
        notfoundsta <- stations[!(stations %in% allsta)]
        if(length(notfoundsta) > 0) warning(paste("station",notfoundsta, 
                                                  "does not exist in the database."))
        
        # Geting the data from the available stations
        foundsta <- stations[stations %in% allsta]
        if(length(foundsta) > 0){ 
          message(paste("these are the wu stations used", cat(foundsta)))
          finaldaynum <- as.numeric(finalday) 
          sqlstations = paste("'", str_c(foundsta, collapse = "','"),"'", sep="")
          comando <- paste("SELECT * from wu WHERE 
                        \"Estacao_wu_estacao_id\" IN  (", sqlstations, ")" ,sep="")
          
          d <- dbGetQuery(datasource, comando)
          
          # fixing dates
          d$data_dia <- as.Date(d$data_dia, origin = "1970-01-01")
          d <- d %>%
            filter(d$data_dia <= finalday)
          } 
      }
      
      
      # agregando vars climaticas por semana (ignora NAs)
      d1 = d %>% 
            mutate(estacao = Estacao_wu_estacao_id) %>% 
            mutate(SE = data2SE(data_dia, format = "%Y-%m-%d")) %>% # creating column SE
            group_by(estacao,SE)  %>%
            summarise_at(vars(vars),list(mean),na.rm=TRUE) 
      
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
#'@examples
#'NOT USE: con <- dbConnect(RSQLite::SQLite(), "../../AlertaDengueAnalise/mydengue.sqlite")
#'series1 = getWU(stations = 'SBCP',vars="temp_min", datasource= con)
#'series2 = getWU(stations = 'SBME',vars="temp_min", datasource= con)
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
#'@export
#'@param cities list of geocodes.
#'@param vars vector with meteorological variables. Minimum set of vars 
#'"temp_min", "temp_max". Default: all of them
#'@param iniSE first epiweek. Default is 012015 
#'@param lastSE last epiweek. Optional. Default is the last available
#'@param finalday alternative to lastSE. Default is the last day available. Format = Y-m-d. 
#'@param datasource a valid database connection 
#'@return data.frame with the weekly data (cidade data temp_min tmed tmax umin umed umax pressaomin pressaomed pressaomax)
#'@examples
#'NOT USE: con <- dbConnect(RSQLite::SQLite(), "../../AlertaDengueAnalise/mydengue.sqlite")
#'res = getClima(cities = c(3304557), vars=c("temp_min","temp_max") , iniSE = 201801)
#'res = getClima(cities = c(3304557, 3200300), iniSE = 202003)
#'tail(res)

getClima <- function(cities, vars = c("temp_min","temp_max","temp_med","umid_min",
                                      "umid_med","umid_max", "precip_tot","precip_max"),
                     finalday = Sys.Date(), iniSE = 201501, lastSE, datasource=con) {
      
      # validate climate variables
      if(any(!(vars %in% vars))) stop("climate variable(s) unknown or mispecified")
      
      assert_that(all(c("temp_min","temp_max") %in% vars), msg="getClima: vars should 
                                                           include temp_min, temp_max. Check it!")
      # dates
      #if(missing(lastSE)) lastSE <- epiweek(finalday, format = "Y-m-d")
      iniday <- SE2date(iniSE)$ini
      
      if(class(datasource) == "PostgreSQLConnection"){
            sqlcity = paste("'", str_c(cities, collapse = "','"),"'", sep="")
            varscomplete = c("geocodigo", "date", vars)
            sqlvars = paste("", str_c(varscomplete, collapse = ","), sep="")
            
            comando <- paste("SELECT ", sqlvars ," from \"weather\".\"copernicus_brasil\" 
                        WHERE geocodigo IN  (", sqlcity, ") AND 
                         date <= '",finalday,"' AND date >= '", iniday, "'",sep="")
            
            d <- dbGetQuery(datasource,comando) 
      }
      
      # agregando vars climaticas por semana
      vars <- c(vars, "ampT") # calculating daily thermal amplitude
      d1 <- d %>% 
            mutate(SE = data2SE(date, format = "%Y-%m-%d")) %>% # creating column SE
            mutate(ampT = temp_max - temp_min) %>% 
            group_by(geocodigo,SE)  %>%
            summarise_at(vars(vars),list(mean),na.rm=TRUE) 
      
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
#'@param lastY last year. Default is the last available
#'@return tibble with columns geocode, year, pop 
#'@examples
#'cities <- getCidades(uf = "Paraná")$municipio_geocodigo
#'pop <- getPop(cities, iniY = 2018, endY = 2019)
#'tail(pop)

getPop <- function(cities, iniY = 2010, endY) {
      
      # check input
      assert_that(all(c(iniY) >= 2000), 
                  msg = "getPop: check dates, getPop only has data from 2000 to 2021.")
      
      assert_that(all(c(iniY,endY) <= 2021), 
                  msg = "getPop: check dates, getPop only has data from 2000 to 2021.")
      
      require(brpop)
      cities6 <- sapply(cities, function(x) floor(x/10))

      x <- mun_pop_totals() %>% 
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
#'@export
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
#'@return data.frame with the data aggregated per week according to disease onset date.
#'Notice that the names of the columns and the number of columns will change according to type. 
#'To recover the original function behavior, use the default type.
#'@examples
#'NOT USE: con <- dbConnect(RSQLite::SQLite(), "../../AlertaDengueAnalise/mydengue.sqlite")
#'d <- getCases(cities = 4209102, dataini = "sinpri") # dengue
#'d <- getCases(cities = 3300936, completetail = 0) # dengue
#'d <- getCases(cities = 3304557, cid10="A92.0") # chikungunya, until last day available
#'cid <- getCidades(regional = "Norte",uf = "Rio de Janeiro")
#'d <- getCases(cities = 3304557, firstday = as.Date("2023-01-01"),dataini = "sinpri") 
#'tail(d)

getCases <- function(cities, lastday = Sys.Date(), firstday = as.Date("2010-01-01"), cid10 = "A90", 
                     dataini = "notific", completetail = NA, type = "notified", datasource=con) {
      
      require(lubridate)
      assert_that(class(cities) %in% c("integer","numeric"), 
                  msg = "cities should be a vector of numeric geocodes") 
  
      assert_that(dataini %in% c("sinpri", "notific"), msg="getCases: dataini should 
                                                           be sinpri or notific. Check it!")
      cities <- sapply(cities, function(x) sevendigitgeocode(x))
      
      # dealing with synonimous cid 
      if (cid10 == "A90") {cid <- cid10} else{ # dengue, dengue hemorragica
            if (cid10 %in% c("A92", "A920","A92.0")) { # chik
                  cid <-c("A92", "A920","A92.0")
                  cid10 <- "A92.0"}  else{
                        if (cid10 %in% c("A92.8","A928")){  # zika
                              cid <- c("A92.8","A928")
                              cid10 <- "A92.8"                      
                        }                  
                  }
      }
      if (!(cid10 %in% c("A90","A92.0","A92.8")))stop(paste("Eu nao conheco esse cid10",cid10))
    
       # reading notification data form the database 
      sqlcity = paste("'", str_c(cities, collapse = "','"),"'", sep="")
      sqlcid = paste("'", str_c(cid, collapse = "','"),"'", sep="") # dealing with multiple cids for the same disease  
      
      
      if(class(datasource) == "PostgreSQLConnection"){
        comando <- paste("SELECT * from \"Municipio\".\"Notificacao\" WHERE dt_digita <= '",lastday, 
                         "' AND municipio_geocodigo IN (", sqlcity, 
                         ") AND cid10_codigo IN(", sqlcid,")", sep="")
            
        dd <- dbGetQuery(datasource,comando)
      
        if(nrow(dd)==0)stop("getCases found no data")
            
        # pegando nome da cidade e populacao 
        sql2 <- paste("SELECT nome,populacao,geocodigo from \"Dengue_global\".\"Municipio\" WHERE geocodigo IN(", sqlcity,")") 
        varglobais <- dbGetQuery(datasource,sql2)
      
      }
      if(class(datasource) == "SQLiteConnection"){
        
        comando <- paste("SELECT * from \"Notificacao\" WHERE 
                         municipio_geocodigo IN (", sqlcity, 
                         ") AND cid10_codigo IN(", sqlcid,")", sep="")
        
        dd <- dbGetQuery(datasource,comando)
        
        if(nrow(dd)==0)stop("getCases found no data")
        # fixing dates
        dd$dt_notific <- as.Date(dd$dt_notific, origin = "1970-01-01")
        dd$dt_sin_pri <- as.Date(dd$dt_sin_pri, origin = "1970-01-01")
        dd$dt_digita <- as.Date(dd$dt_digita, origin = "1970-01-01")
        
        dd <- dd %>%
          filter(dt_digita <= lastday) 
        
        # pegando nome da cidade e populacao 
        sql2 <- paste("SELECT nome,populacao,geocodigo from \"Municipio\" WHERE 
                      geocodigo IN(", sqlcity,")") 
        varglobais <- dbGetQuery(datasource,sql2)
        
      }
      
      # definindo a data para calculo da semana
      if(dataini == "notific"){
            message("cases aggregated by notification date")
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
            wrongdates <- which(dd$ininotif > 30 | dd$ininotif < 0 | is.na(dd$dt_sin_pri))  
            if(length(wrongdates) > 0) {
            message(paste(length(wrongdates), "registros com datas de inicio de sintomas invalidas"))
            dd <- dd[-wrongdates,]
            }

            # calculating  epiweek from dt_sin_pri
            dd$se_sin_pri <- epiweek(as.Date(dd$dt_sin_pri, format = "%Y-%m-%d"))
            dd <- dd %>% 
                  mutate(ano_sinpri = lubridate::year(dt_sin_pri),
                         SE = ano_sinpri*100+se_sin_pri)
            message("cases aggregated by symptom onset date")
            
            }
         
      # identificando os casos de acordo com a definicao
      dd$tipo <- "notified"  # esse grupo deveria ser os descartados
      dd$tipo[dd$classi_fin != 5] <- "probable"
      dd$tipo[dd$classi_fin != 5 & dd$criterio == 1] <- "lab_confirmed"
      
      # contando os casos de acordo com a definicao   
      casos <- dd %>% 
            group_by(municipio_geocodigo, SE) %>%
            summarise(
                  casos = length(tipo),
                  cas_prov = sum(tipo == "probable"),
                  cas_lab = sum(tipo == "lab_confirmed"))
      
      lastSE <- data2SE(lastday, format = "%Y-%m-%d")  
      # criando serie 
      sem <-  expand.grid(municipio_geocodigo = cities, 
                          SE = seqSE(from = 201001, to = lastSE[1])$SE)
      
      st <- left_join(sem, casos, by = c("municipio_geocodigo", "SE")) %>% 
            arrange(municipio_geocodigo, SE) %>%
            mutate(localidade = 0) %>%  # para uso qdo tiver divisao submunicipal
            mutate(geocodigo = municipio_geocodigo) %>%
            mutate(CID10 = cid10)%>%
            full_join(.,varglobais,"geocodigo") %>%
            select(SE, cidade = municipio_geocodigo,CID10, casos, cas_prov, 
                   cas_lab, localidade, nome, pop=populacao) 
      
      # preenchendo os NAs 
      #SElastcase <- max(st$SE[st$casos > 0], na.rm = TRUE)
      st$casos[(is.na(st$casos) & st$SE <= lastSE)] <- 0 # substitute NA for zero to indicate that no case was reported that week 
      st$cas_prov[(is.na(st$cas_prov) & st$SE <= lastSE)] <- 0 # substitute NA for zero to indicate that no case was reported that week 
      st$cas_lab[(is.na(st$cas_lab) & st$SE <= lastSE)] <- 0 # substitute NA for zero to indicate that no case was reported that week 
      
      #if(!is.na(completetail)) st$casos[st$SE > SElastcase] <- completetail
      if(any(is.na(st$pop)))warning("getCases function failed to import pop data for one or more cities", cities)
      
      # choosing what to return
      if(type == "notified") return(subset(st, select = -c(cas_prov, cas_lab)))
      if(type == "probable") return(subset(st, select = -c(casos, cas_lab)))
      if(type == "lab_confirmed") return(subset(st, select = -c(casos, cas_prov)))
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
#' @examples
#' dd <- read.cases(2019, 2020, mun_list = c(4108304, 3300936))
#' dd <- read.cases(2019, 2020, cid10 = "A92.0", mun_list = c(4108304, 3300936))

read.cases <- function(start_year, end_year, cid10 = "A90", datasource=con, mun_list){
  
  mun_list_txt <- paste0(mun_list, collapse=',')
  
  if (!(cid10 %in% c("A90","A92.0","A92.8")))stop(paste("Eu nao conheco esse cid10",cid10))
  
  
  sqlcid = paste("'", cid10,"'", sep="")
  
  
  if(class(datasource) == "PostgreSQLConnection"){
      sqlquery = paste0("SELECT dt_notific, se_notif, ano_notif, municipio_geocodigo
                        FROM  \"Municipio\".\"Notificacao\" ")
      
      sqlquery <- paste0(sqlquery, " WHERE (ano_notif >= ", start_year,
                               " AND ano_notif <= ", end_year,
                               " AND cid10_codigo = ", sqlcid,
                               " AND municipio_geocodigo IN (", mun_list_txt,"));" )
      
      df.cases.weekly <- dbGetQuery(conn = datasource, sqlquery, 
                                    stringsAsFactors=FALSE)
      
      }
  
  
  if(class(datasource) == "SQLiteConnection"){
    sqlquery = paste0("SELECT dt_notific, se_notif, ano_notif, municipio_geocodigo
                        FROM  \"Notificacao\" ")
    
    sqlquery <- paste0(sqlquery, " WHERE (ano_notif >= ", start_year,
                       " AND ano_notif <= ", end_year,
                       " AND AND cid10_codigo = ", sqlcid,
                       " AND municipio_geocodigo IN (", mun_list_txt,"));" )
    
    df.cases.weekly <- dbGetQuery(conn = datasource, sqlquery, 
                                  stringsAsFactors=FALSE) 
    df.cases.weekly$dt_notific <- as.Date(df.cases.weekly$dt_notific, origin = "1970-01-01")
    
  }
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
      effec_start_year <- min(round(df.cases.weekly$SE/100))
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


# getCasesinRio --------------------------------------------------------------
#'@description Get time series of cases per APS in Rio de Janeiro (special case) 
#'@title Get cases from an APS in Rio de Janeiro and aggregate them into weekly time series. 
#'@export
#'@param APSid 0(APS1), 1 (APS2.1), 2 (APS2.2), 3(APS3.1), 4(APS3.2), 5(APS3.3), 6(APS4),
#', 7(APS5.1), 8(APS5.2), 9(APS5.3)  
#'@param cid10 cid 10 disease code. A90 = dengue (default) , A920 = chikungunia
#'@param dataini "sinpri" or "notific"(default)
#'@param lastday end date of the time series
#'@param datasource Posgresql connection to project's database
#'@return data.frame with the data aggregated per health district and week
#'@examples
#'NOT USE: con <- dbConnect(RSQLite::SQLite(), "../../AlertaDengueAnalise/mydengue.sqlite")
#'dC = getCasesinRio(APSid = 0:9, datasource = con) # Rio de Janeiro
#'# Chikungunya:
#'dC1s = getCasesinRio(APSid = 0, cid10 = "A920") # Rio de Janeiro
# tail(dC1)

getCasesinRio <- function(APSid, lastday = Sys.Date(), cid10 = "A90", dataini="sinpri",
                          datasource = con) {
      city <- 3304557
      #dealing with synonimous cid
      if (cid10 == "A90") cid <- c("A90") # dengue, dengue hemorragica
      if (cid10 %in% c("A92", "A920","A92.0")) {cid <-c("A92", "A920","A92.0"); cid10 <- "A92.0"}  # chik
      if (cid10 %in% c("A92.8","A928")) {cid <- c("A92.8","A928"); cid10 <- "A92.8"} #zika
      if (!(cid10 %in% c("A90","A92.0","A92.8")))stop(paste("Eu nao conheco esse cid10",cid10))

      
      assert_that(all(APSid %in% 0:9), msg ="APS desconhecida ou ausente. Especificar: 0(APS1), 1 (APS2.1), 2 (APS2.2), 
                                    3(APS3.1), 4(APS3.2), 5(APS3.3), 6(APS4) 7(APS5.1), 8(APS5.2), 9(APS5.3) ")
      
      # query dados
      sqlcid <- paste("'", cid10, "'", sep = "")
      sqldate <- paste("'", lastday, "'", sep = "")
      sqlaps = paste("'", str_c(APSid, collapse = "','"),"'", sep="")
      
      if(class(datasource) == "PostgreSQLConnection"){
        sqlquery = paste("SELECT n.dt_notific, n.ano_notif, n.dt_digita,
        n.dt_sin_pri, n.se_notif, n.se_sin_pri, l.id, l.nome
        FROM  \"Municipio\".\"Notificacao\" AS n 
        INNER JOIN \"Municipio\".\"Bairro\" AS b 
        ON n.bairro_nome = b.nome 
        INNER JOIN \"Municipio\".\"Localidade\" AS l 
        ON b.\"Localidade_id\" = l.id 
        WHERE n.municipio_geocodigo = 3304557 AND l.id IN(",sqlaps, ") AND dt_digita <= ",
                         sqldate, "AND n.cid10_codigo = ", sqlcid)
      
        d <- dbGetQuery(datasource,sqlquery)
      
        #d$SEM_INI <- year(d$dt_sin_pri)*100+d$se_sin_pri 
      
        # query pop from table Municipio.localidade (only has data for Rio)
         sql2 <- paste("SELECT nome,id,populacao from \"Municipio\".\"Localidade\" 
                     WHERE id IN(", sqlaps, ")") 
         pop <- dbGetQuery(datasource,sql2)
      
      }
      
      if(class(datasource) == "SQLiteConnection"){
        sqlquery = paste("SELECT n.dt_notific, n.ano_notif, n.dt_digita,
        n.dt_sin_pri, n.se_notif, n.se_sin_pri, l.id, l.nome
        FROM  \"Notificacao\" AS n 
        INNER JOIN \"Bairro\" AS b 
        ON n.bairro_nome = b.nome 
        INNER JOIN \"Localidade\" AS l 
        ON b.\"Localidade_id\" = l.id 
        WHERE n.municipio_geocodigo = 3304557 AND l.id IN(",sqlaps, ") AND dt_digita <= ",
                         sqldate, "AND n.cid10_codigo = ", sqlcid)
        
        d <- dbGetQuery(datasource,sqlquery)
        # fixing dates
        d$dt_sin_pri <- as.Date(d$dt_sin_pri, origin = "1970-01-01")
        d$dt_notific <- as.Date(d$dt_notific, origin = "1970-01-01")
        d$dt_digita <- as.Date(d$dt_digita, origin = "1970-01-01")
        #d$SEM_INI <- year(d$dt_sin_pri)*100+d$se_sin_pri 
        
        # query pop from table Municipio.localidade (only has data for Rio)
        sql2 <- paste("SELECT nome,id,populacao from \"Localidade\" 
                    WHERE id IN(", sqlaps, ")") 
        pop <- dbGetQuery(datasource,sql2)
        
      }
      
      # agregando casos por semana por cidade 
      if(dataini == "notific"){
            message("case aggregated by notification date")
            casos <- d %>% 
                  mutate(SE = ano_notif*100+se_notif) %>%
                  group_by(id,SE) %>%
                  summarise(casos = n(),
                            localidade = unique(nome))
      }
      
      if(dataini == "sinpri"){
            message("case aggregated by symptoms date")
            casos <- d %>% 
                  mutate(ano_sinpri = lubridate::year(dt_sin_pri),
                         SE = ano_sinpri*100+se_sin_pri) %>%
              group_by(id,SE) %>%
                  summarise(casos = n(),
                            localidade = unique(nome))
      }
      
      
      # criando serie temporal
      lastSE <- data2SE(lastday, format = "%Y-%m-%d")  
      sem <-  expand.grid(id = unique(casos$id), SE = seqSE(from = 201001, 
                        to = lastSE[1])$SE)
      st <- left_join(sem,casos,by = c("id", "SE")) %>% 
            arrange(id,SE) %>%
            left_join(.,pop[,c("id","populacao")],"id") %>%
            rename(localidadeid = id) %>%
            mutate(cidade = city,
                   nome = "Rio de Janeiro",
                   casos = replace_na(casos, 0),
                   CID10 = cid10) %>% 
            mutate(localidade = case_when(is.na(localidade) & localidadeid == 0 ~ "A.P. 1.0",
                                          is.na(localidade) & localidadeid == 1 ~ "A.P. 2.1",
                                          is.na(localidade) & localidadeid == 2 ~ "A.P. 2.2",
                                          is.na(localidade) & localidadeid == 3 ~ "A.P. 3.1",
                                          is.na(localidade) & localidadeid == 4 ~ "A.P. 3.2",
                                          is.na(localidade) & localidadeid == 5 ~ "A.P. 3.3",
                                          is.na(localidade) & localidadeid == 6 ~ "A.P. 4.0",
                                          is.na(localidade) & localidadeid == 7 ~ "A.P. 5.1",
                                          is.na(localidade) & localidadeid == 8 ~ "A.P. 5.2",
                                          is.na(localidade) & localidadeid == 9 ~ "A.P. 5.3",
                                                                TRUE ~ localidade))
      
      assert_that(anyNA(st) == FALSE, msg = "getCasesinRio contains NA. This is unexpected.")       
      st  
}


# getTweet --------------------------------------------------------------
#'@description Create weekly time series from tweeter data from server. The 
#'source of this data is the Observatorio da Dengue (UFMG).
#'@title Get Tweeter Data
#'@export
#'@param cities cities's geocode. Use getCidades()
#'@param cid10 default is A90 (dengue). If not dengue, returns NA
#'@param finalday last day. Default is the last available.
#'@param datasource Use the connection to the Postgresql server for using project data.  
#'@return data.frame with weekly counts of people tweeting on dengue.
#'@examples
#'NOT USE: con <- dbConnect(RSQLite::SQLite(), "../../AlertaDengueAnalise/mydengue.sqlite")
#'tw <- getTweet(cities = c(3302205,3200300), lastday = "2014-03-01")
#'tw <- getTweet(cities = 3304557, finalday = "2016-03-01")
#'cid <- getCidades(regional = "Norte",uf = "Rio de Janeiro")
#'tw <- getTweet(cities = cid$municipio_geocodigo) 
#'tail(tw)

getTweet <- function(cities, lastday = Sys.Date(), cid10 = "A90", datasource=con) {
      
      cities <- sapply(cities, function(x) sevendigitgeocode(x))
      
      # get tweets on dengue 
      if (cid10 == "A90"){ 
            
            sqlcity = paste("'", str_c(cities, collapse = "','"),"'", sep="")
            
            if(class(datasource) == "PostgreSQLConnection"){
                  
                  comando <- paste("SELECT \"Municipio_geocodigo\", data_dia, numero
              FROM \"Municipio\".\"Tweet\" WHERE \"Municipio_geocodigo\" IN (",
                                   sqlcity,") AND data_dia <= '", lastday,"'",sep="")
                  
                  tw <- dbGetQuery(datasource,comando) 
            }
            
            if(class(datasource) == "SQLiteConnection"){
                  
                  comando <- paste("SELECT * from tweet WHERE  
                        \"Municipio_geocodigo\" IN  (", sqlcity, ")" ,sep="")
                  
                  tw <- dbGetQuery(datasource, comando)
                  
                  # fixing dates and filtering
                  tw$data_dia <- as.Date(tw$data_dia, origin = "1970-01-01")
                  tw <- tw %>%
                        filter(tw$data_dia <= as.Date(lastday))
            } 
            
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


