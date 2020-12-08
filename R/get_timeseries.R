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
#'res = getWU(stations = c('SBRJ','SBGL'), vars="temp_min", iniSE = 201201)
#'res = getWU(stations = 'SBRJ', vars=c("temp_min", "temp_med")
#'tail(res)

getWU <- function(stations, vars = "temp_min", finalday = Sys.Date(), iniSE = 201001,
                  datasource=con) {
      
      # validade climate variables
      wuvars <- c("temp_min","temp_max","temp_med","umid_min","umid_med","umid_max",
                  "pressao_min","pressao_med","pressao_max")
      if(any(!(vars %in% wuvars))) stop("wu climate variable(s) unknown or mispecified")

      # check if all stations exist
      sqlstations = paste("'", str_c(stations, collapse = "','"),"'", sep="")
      checkStationComm <- paste("SELECT estacao_id, nome FROM \"Municipio\".\"Estacao_wu\" WHERE  estacao_id IN (", sqlstations, ")",sep="")
      stanames <- dbGetQuery(datasource,checkStationComm)
      
      if(nrow(stanames)==0) stop("'stations' unknown. Check their names.")
      notfoundsta <- stations[!(stations %in% stanames$estacao_id)]
      if(length(notfoundsta) > 0) warning(paste("station",notfoundsta, "does not exist in the database."))
      
      # Geting the data from the available stations
      message(paste("are the wu stations used", cat(stanames$nome)))
      sqlstations = paste("'", str_c(stanames$estacao_id, collapse = "','"),"'", sep="")
      
      comando <- paste("SELECT * from \"Municipio\".\"Clima_wu\" WHERE 
                        \"Estacao_wu_estacao_id\" IN  (", sqlstations, ") AND 
                         data_dia <= '",finalday,"'",sep="")
      
      d <- dbGetQuery(datasource,comando) 
      
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
#'tw <- getTweet(cities = 3302205, lastday = "2014-03-01")
#'tw <- getTweet(cities = 3200300, lastday = "2016-03-01")
#'cid <- getCidades(regional = "Norte",uf = "Rio de Janeiro")
#'tw <- getTweet(cities = cid$municipio_geocodigo) 
#'tail(tw)

getTweet <- function(cities, lastday = Sys.Date(), cid10 = "A90", datasource=con) {
      
      cities <- sapply(cities, function(x) sevendigitgeocode(x))
      
      # get tweets on dengue 
      if (cid10 == "A90"){ 
            
            sqlcity = paste("'", str_c(cities, collapse = "','"),"'", sep="")

            comando <- paste("SELECT \"Municipio_geocodigo\", data_dia, numero FROM \"Municipio\".\"Tweet\" WHERE 
                \"Municipio_geocodigo\" IN (", sqlcity,") AND data_dia <= '", lastday,"'",sep="")
            
            tw <- dbGetQuery(datasource,comando) 
            
      } else {stop(paste("there is no tweet for", cid10,"in the database"))}
      
      # no tweets found for these cities 
      if(nrow(tw) == 0){
            message(paste("cidade(s)",cities,"nunca tweetou sobre dengue"))
            tw <- expand.grid(Municipio_geocodigo = cities,
                             SE = seqSE(from = 201001, to = data2SE(lastday, 
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
                          SE = seqSE(from = 201001, to = max(tw$SE))$SE)
      st <- full_join(sem,tw,by = c("Municipio_geocodigo", "SE")) %>% 
                  arrange(Municipio_geocodigo,SE) %>%
                  group_by(Municipio_geocodigo,SE)  %>%
                  summarize(tweet = sum(numero))  %>%
                  select(Municipio_geocodigo, SE, tweet)
            
      return(as.data.frame(st))      
      
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
#'@param cid10 cid 10 code. Dengue = "A90" (default), Chik = "A92.0", Zika = "A92.8", 
#'@param datasource PostgreSQLConnection to project database. 
#'@return data.frame with the data aggregated per week according to disease onset date.
#'@examples
#'d <- getCases(cities = 4314902) # dengue
#'d <- getCases(cities = 3300936, completetail = 0) # dengue
#'d <- getCases(cities = 3304557, cid10="A92.0") # chikungunya, until last day available
#'cid <- getCidades(regional = "Norte",uf = "Rio de Janeiro")
#'d <- getCases(cities = cid$municipio_geocodigo, datasource = con, dataini = "sinpri") 
#'tail(d)

getCases <- function(cities, lastday = Sys.Date(), cid10 = "A90", dataini = "notific", completetail = NA,
                     datasource=con) {
      
      assert_that(class(cities) %in% c("integer","numeric"), msg = "cities should be a vector of numeric geocodes") 
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
      
      comando <- paste("SELECT * from \"Municipio\".\"Notificacao\" WHERE dt_digita <= '",lastday, 
                         "' AND municipio_geocodigo IN (", sqlcity, 
                         ") AND cid10_codigo IN(", sqlcid,")", sep="")
            
      dd <- dbGetQuery(datasource,comando)
      
      if(nrow(dd)==0)stop("getCases found no data")
            
      # pegando nome da cidade e populacao 
      sql2 <- paste("SELECT nome,populacao,geocodigo from \"Dengue_global\".\"Municipio\" WHERE geocodigo IN(", sqlcity,")") 
      varglobais <- dbGetQuery(datasource,sql2)
      
      # agregando casos por semana por cidade 
      if(dataini == "notific"){
            message("case aggregated by notification date")
            casos = dd %>% 
                  mutate(SE = ano_notif*100+se_notif) %>%
                  group_by(municipio_geocodigo) %>%
                  count(SE)
      }
      if(dataini == "sinpri"){
            message("case aggregated by symptoms date")
            casos = dd %>% 
                  mutate(ano_sinpri = lubridate::year(dt_sin_pri),
                        SE = ano_sinpri*100+se_sin_pri) %>%
                  group_by(municipio_geocodigo) %>%
                  count(SE)
      }
      
      lastSE <- data2SE(lastday, format = "%Y-%m-%d")  
      # criando serie 
      sem <-  expand.grid(municipio_geocodigo = cities, SE = seqSE(from = 201001, to = lastSE[1])$SE)
      st <- left_join(sem,casos,by = c("municipio_geocodigo", "SE")) %>% 
            arrange(municipio_geocodigo,SE) %>%
            mutate(localidade = 0) %>%  # para uso qdo tiver divisao submunicipal
            mutate(geocodigo = municipio_geocodigo) %>%
            mutate(CID10 = cid10)%>%
            full_join(.,varglobais,"geocodigo") %>%
            select(SE, cidade = municipio_geocodigo,CID10, casos =n,localidade,nome,
                   pop=populacao) 
      
      SElastcase <- max(st$SE[st$casos > 0], na.rm = TRUE)
      st$casos[(is.na(st$casos) & st$SE <= SElastcase)] <- 0 # substitute NA for zero to indicate that no case was reported that week 
      if(!is.na(completetail)) st$casos[st$SE > SElastcase] <- completetail
      
      if(any(is.na(st$pop)))warning("getCases function failed to import pop data for one or more cities", cities)
      
      st  
}


# read.cases --------------------------------------------------------------
#' @title Function to extract case count for covered municipalities
#' @description Function \code{read.cases} extract notification data from database 
#' and aggregate by epiweek. Used by mem.
#' @param start_year first year of the time series
#' @param end_year last year of the time series
#' @param datasource Infodengue connection
#' @param mun_list vector with the municipalities' 7 digit geocodes 
#' @author Marcelo F C Gomes
#' @examples
#' dd <- read.cases(2010, 2018, mun_list = 3302403)

read.cases <- function(start_year, end_year, datasource=con, mun_list=NULL){
      sqlquery = paste0("SELECT dt_notific, se_notif, ano_notif, c.municipio_geocodigo
                        FROM  \"Municipio\".\"Notificacao\" as c
                        INNER JOIN \"Dengue_global\".regional_saude as f
                        ON c.municipio_geocodigo = f.municipio_geocodigo\n")
      if (is.null(mun_list)){
            sqlquery <- paste0(sqlquery, " WHERE (ano_notif >= ", start_year,
                               " AND ano_notif <= ", end_year, ")")    
      } else {
            mun_list_txt <- paste0(mun_list, collapse=',')
            sqlquery <- paste0(sqlquery, " WHERE (ano_notif >= ", start_year,
                               " AND ano_notif <= ", end_year,
                               " AND c.municipio_geocodigo IN (", mun_list_txt,"));" )
      }
      
      # In the database we have each single notification. So we'll have to aggregate 
      #later on We'll use an object name "weekly" from the start since we'll overwrite
      #it with the aggregate later.
      df.cases.weekly <- dbGetQuery(conn = datasource, sqlquery, stringsAsFactors=FALSE)
      
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
      df.cases.weekly$municipio_geocodigo <- as.integer((levels(df.cases.weekly$municipio_geocodigo))[df.cases.weekly$municipio_geocodigo])
      
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
#'dC = getCasesinRio(APSid = 0:9, datasource = con) # Rio de Janeiro
#'# Chikungunya:
#'dC1 = getCasesinRio(APSid = 0, cid10 = "A90", datasource = con) # Rio de Janeiro
#'dC1s = getCasesinRio(APSid = 0, cid10 = "A920", dataini = "sinpri", datasource = con) # Rio de Janeiro
#'tail(dC1)

getCasesinRio <- function(APSid, lastday = Sys.Date(), cid10 = "A90", dataini="notific",
                          datasource = con) {
      
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
      
      sqlquery = paste("SELECT n.dt_notific, n.ano_notif, n.se_sin_pri, n.dt_sin_pri, se_notif, l.id, l.nome
      FROM  \"Municipio\".\"Notificacao\" AS n 
      INNER JOIN \"Municipio\".\"Bairro\" AS b 
      ON n.bairro_nome = b.nome 
      INNER JOIN \"Municipio\".\"Localidade\" AS l 
      ON b.\"Localidade_id\" = l.id 
      WHERE n.municipio_geocodigo = 3304557 AND l.id IN(",sqlaps, ") AND dt_digita <= ",sqldate, 
                       "AND n.cid10_codigo = ", sqlcid)
      
      d <- dbGetQuery(datasource,sqlquery)
      d$SEM_INI <- year(d$dt_sin_pri)*100+d$se_sin_pri 
      
      # query pop from table Municipio.localidade (only has data for Rio)
      sql2 <- paste("SELECT nome,id,populacao from \"Municipio\".\"Localidade\" WHERE id IN(", sqlaps, ")") 
      pop <- dbGetQuery(datasource,sql2)
      if(dataini=="sinpri") for(i in 1:nsem) st$casos[i] <- sum(d$SEM_INI == st$SE[i])
      print(paste("calculating incidence using", dataini))
      
      # agregando casos por semana por cidade 
      if(dataini == "notific"){
            message("case aggregated by notification date")
            casos = d %>% 
                  mutate(SE = ano_notif*100+se_notif) %>%
                  group_by(id,SE) %>%
                  summarise(casos = n(),
                            localidade = unique(nome))
      }
      
      if(dataini == "sinpri"){
            message("case aggregated by symptoms date")
            casos = d %>% 
                  mutate(ano_sinpri = lubridate::year(dt_sin_pri),
                         SE = ano_sinpri*100+se_sin_pri) %>%
                  summarise(casos = n(),
                            localidade = unique(nome))
      }
      
      
      # criando serie temporal
      sem <-  expand.grid(id = unique(casos$id), SE = seqSE(from = 201001, 
                        to = max(casos$SE, na.rm=TRUE))$SE)
      st <- left_join(sem,casos,by = c("id", "SE")) %>% 
            arrange(id,SE) %>%
            left_join(.,pop[,c("id","populacao")],"id") %>%
            rename(localidadeid = id) %>%
            mutate(cidade = 3304557,
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



# mergedata (deprecated)  -------------------------------------------
#'@description Merge cases, tweets and climate data for the alert  
#'@title Merge cases, tweets and climate data.
#'@param cases data.frame with aggregated cases by locality (or city)
#' and epidemiological week.
#'@param tweet data.frame with tweets aggregated per week
#'@param climate data.frame with climate data aggregated per week for the
#' station of interest.
#'@return data.frame with all data available 
#'@examples
#'cas = getCases(city = 330240, datasource = con) 
#'tw = getTweet(city = 330240, datasource = con)
#'clima = getWU(stations = 'SBRJ', var=c("temp_min","umid_min"), datasource=con)
#'head(mergedata(cases = cas, tweet = tw, climate = clima))
#'head(mergedata(tweet = tw, climate = clima))
#'head(mergedata(cases = cas, climate = clima))
#'head(mergedata(tweet = tw, cases = cas))

# mergedata <- function(cases = c(), tweet =c(), climate=c(), ini=200952){
#       # checking the datasets
#       if (!is.null(cases) & !all(table(cases$SE)==1)) 
#             stop("merging require one line per SE in case dataset")
#       if (!is.null(tweet) & !all(table(tweet$SE)==1)) 
#             stop("merging require one line per SE in tweet dataset")
#       if (!is.null(climate) & !all(table(climate$SE)==1))
#             stop("merging require one line per SE in climate dataset. Mybe you have more than one station.")
#       
#       # merging
#       if (is.null(cases)) {
#             d <- merge(climate, tweet, by=c("SE"), all = TRUE)
#       } else if (is.null(tweet)){
#             d <- merge(cases, climate,  by=c("SE"), all = TRUE)     
#       } else if (is.null(climate)) {
#             d <- merge(cases, tweet[, c("SE","tweet")],  by=c("SE"), all = TRUE)
#       }
#       if (!(is.null(cases) | is.null(tweet) | is.null(climate))){
#             d <- merge(cases, tweet[, c("SE","tweet")],  by=c("SE"), all = TRUE)
#             d <- merge(d, climate,  by=c("SE"), all=TRUE)  
#       }
#       # removing beginning
#       d <- subset(d, SE > ini)
#       d
# }


