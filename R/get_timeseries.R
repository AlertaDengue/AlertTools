# PROJETO ALERTA DENGUE -------------------------------------
# FUNCOES PARA ORGANIZAR SERIES
#TEMPORAIS A PARTIR DOS DADOS BRUTOS CLAUDIA CODECO - 2015

# GetWU --------------------------------------------------------
#'@description Create weekly time series from meteorological station data in server taking the mean of the daily values.
#'@title Get Climate Data
#'@param stations station code (4 digits).
#'@param vars vector with meteorological variables. Available variables: "temp_min" (default), 
#'"temp_max","temp_med","data_dia","umid_min","umid_med","umid_max","pressao_min","pressao_med","pressao_max"
#'@param finalday last day. Default is the last available. Format = Y-m-d. 
#'@param datasource Use "data/WUdata.rda" to use test dataset. Use the connection to the Postgresql server if using project data. See also DenguedbConnect
#' to open the database connection. 
#'@return data.frame with the weekly data (cidade estacao data temp_min tmed tmax umin umed umax pressaomin pressaomed pressaomax)
#'@examples
#'res = getWU(stations = c('SBRJ','SBGL'), vars="temp_min", datasource= con)
#'res = getWU(stations = 'SBRJ', vars=c("temp_min", "temp_med"), datasource= con)
#'tail(res)

getWU <- function(stations, vars = "temp_min", finalday = Sys.Date(), datasource) {
      
      if (!all(nchar(stations) == 4)) stop("'stations' should be a vector of 4 digit station names")
      nsta = length(stations)
      
      # reading data from database
      sqlstations = paste("'", str_c(stations, collapse = "','"),"'", sep="")
      
      comando <- paste("SELECT * from \"Municipio\".\"Clima_wu\" WHERE 
                        \"Estacao_wu_estacao_id\" IN  (", sqlstations, ") AND 
                         data_dia <= '",finalday,"'",sep="")
      
      d <- dbGetQuery(datasource,comando) 
      
      # agregando vars climaticas por semana
      d1 = d %>% 
            mutate(estacao = Estacao_wu_estacao_id) %>% 
            mutate(SE = data2SE(data_dia, format = "%Y-%m-%d")) %>% # creating column SE
            group_by(estacao,SE)  %>%
            summarise_at(vars(vars),list(mean),na.rm=TRUE)
      
      # criar serie temporal-----------------------------------------
        st <-  expand.grid(estacao = stations, 
                        SE = seqSE(from = 201001, to = max(d1$SE))$SE,
                        stringsAsFactors = FALSE) %>%
                full_join(.,d1,by = c("estacao", "SE")) %>%
            arrange(estacao,SE)
    st
    
}

#bestWU -----------------------------------------------------------------
#'@description Receives a set of meteorological data and return the most complete for
#' a given city.If both are good, the first is returned.
#'@title Chooses the most updated meteorological data for a city from a set of choices
#'@param series list of the competing time series.
#'See examples.
#'@param var name of the meteorological variable 
#'@return data.frame with the series 
#'@examples
#'series1 = getWU(stations = 'SBRJ',vars="temp_min", datasource= con)
#'series2 = getWU(stations = 'SBGL',vars="temp_min", datasource= con)
#'res = bestWU(list(series1, series2),var="temp_min")
#'tail(res)

bestWU <- function(series,var){
      
      if(missing(var))stop("bestWU: please specify a valid meteorological variable")
      if(class(series)!="list") stop("bestWU: WU data must be in a list")
      # prop missing data
      propNA <- sapply(series,function(x,v=var) sum(is.na(x[,v]))/nrow(x))
      # last date with data
      lastdate <- sapply(series, function(x,v=var) x$SE[max(which(is.na(x[,v])==FALSE))])
      
      if(all(propNA ==1)) {
            message("WARNING: As duas estacoes met. não tem dados de temperatura")
            return(NULL)}
            else {return(series[[which.max(lastdate)[1]]])}
}

# GetTweet --------------------------------------------------------------
#'@description Create weekly time series from tweeter data from server. The 
#'source of this data is the Observatorio da Dengue (UFMG).
#'@title Get Tweeter Data
#'@param cities cities's geocode. Use getCidades()
#'@param cid10 default is A90 (dengue). If not dengue, returns NA
#'@param finalday last day. Default is the last available.
#'@param datasource Use the connection to the Postgresql server for using project data.  
#'@return data.frame with weekly counts of people tweeting on dengue.
#'@examples
#'res = getTweet(cities = 3302205, lastday = "2014-03-01")
#'cid <- getCidades(regional = "Norte",uf = "Rio de Janeiro")
#'res <- getTweet(cities = cid$municipio_geocodigo) 
#'tail(res)

getTweet <- function(cities, lastday = Sys.Date(), cid10 = "A90", datasource=con) {
      
      cities <- sapply(cities, function(x) sevendigitgeocode(x))
      
      # get tweets on dengue ---------------------------------------------
      if (cid10 == "A90"){ 
            
            sqlcity = paste("'", str_c(cities, collapse = "','"),"'", sep="")

            comando <- paste("SELECT \"Municipio_geocodigo\", data_dia, numero FROM \"Municipio\".\"Tweet\" WHERE 
                \"Municipio_geocodigo\" IN (", sqlcity,") AND data_dia <= '", lastday,"'",sep="")
            
            tw <- dbGetQuery(datasource,comando) 
            
      } else {stop(paste("there is no tweet for", cid10,"in the database"))}
      
      # warning: there are cities without any tweet 
      tots = tapply(tw$numero,tw$Municipio_geocodigo,sum)
      if (any(tots==0)) message(paste("cidade(s)",cities[which(tots==0)],"nunca tweetou sobre dengue"))
      
      # Counting number of tweets per SE and city -----------------
      tw <- tw %>%  # 
            mutate(SE = data2SE(data_dia, format = "%Y-%m-%d")) # creating column SE
      
      sem <-  expand.grid(Municipio_geocodigo = cities, 
                          SE = seqSE(from = 201001, to = max(tw$SE))$SE)
      st <- full_join(sem,tw,by = c("Municipio_geocodigo", "SE")) %>% 
            arrange(Municipio_geocodigo,SE) %>%
            group_by(Municipio_geocodigo,SE)  %>%
            summarize(tweet = sum(numero))  %>%
            select(cidade = Municipio_geocodigo, SE, tweet)
     
      as.data.frame(st)
}


# GetCases --------------------------------------------------------------
#'@description Create weekly time series from case data from server. The source is the SINAN. 
#'@title Get Case Data and aggregate per week and area
#'@param cities cities' geocode.
#'@param finalday last day. Default is the last available.
#'@param cid10 cid 10 code. Dengue = "A90" (default), Chik = "A92.0", Zika = "A92.8", 
#'@param datasource PostgreSQLConnection to project database . 
#'@return data.frame with the data aggregated per week according to disease onset date.
#'@examples
#'d <- getCases(cities = 3302205, lastday ="2018-03-10") # dengue
#'d <- getCases(cities = 3304557, cid10="A92.0") # chikungunya, until last day available
#'cid <- getCidades(regional = "Norte",uf = "Rio de Janeiro")
#'d <- getCases(cities = cid$municipio_geocodigo, datasource = con) 
#'tail(d)

getCases <- function(cities, lastday = Sys.Date(), cid10 = "A90", datasource=con) {
      
      city <- sapply(cities, function(x) sevendigitgeocode(x))
      
      #dealing with synonimous cid ----------------------------------------------
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
    
       # reading notification data form the database ----------------------------
      sqlcity = paste("'", str_c(cities, collapse = "','"),"'", sep="")
      sqlcid = paste("'", str_c(cid, collapse = "','"),"'", sep="") # dealing with multiple cids for the same disease  
      
      comando <- paste("SELECT * from \"Municipio\".\"Notificacao\" WHERE dt_digita <= '",lastday, 
                         "' AND municipio_geocodigo IN (", sqlcity, 
                         ") AND cid10_codigo IN(", sqlcid,")", sep="")
            
      dd <- dbGetQuery(datasource,comando)
      if(nrow(dd)==0)stop(paste("getCases did not find cid10" , cid10, "for city", city))
            
      # pegando nome da cidade e populacao -----------------------------------------
      sql2 <- paste("SELECT nome,populacao,geocodigo from \"Dengue_global\".\"Municipio\" WHERE geocodigo IN(", sqlcity,")") 
      varglobais <- dbGetQuery(datasource,sql2)
      
      # agregando casos por semana por cidade ---------------------------------------
      casos = dd %>%
            mutate(SE = ano_notif*100+se_notif) %>%
            group_by(municipio_geocodigo)%>%
            count(SE)  
      # criando serie 
      sem <-  expand.grid(municipio_geocodigo = city, SE = seqSE(from = 201001, to = max(casos$SE))$SE)
      st <- full_join(sem,casos,by = c("municipio_geocodigo", "SE")) %>% 
            arrange(municipio_geocodigo,SE) %>%
            mutate(localidade = 0) %>%  # para uso qdo tiver divisao submunicipal
            mutate(geocodigo = municipio_geocodigo) %>%
            full_join(.,varglobais,"geocodigo") %>%
            mutate(CID10 = cid10) %>%
            select(SE, cidade = municipio_geocodigo,casos =n,localidade,nome,pop=populacao) 
      
      st$casos[is.na(st$casos)] <- 0 # substitute NA for zero to indicate that no case was reported that week 
            
      if(any(is.na(st$pop)))warning("getCases function failed to import pop data for one or more cities", cities)
      
      st  
}



# read.cases --------------------------------------------------------------
#' @title Function to extract case count for covered municipalities
#' @description Function \code{read.cases} extract notification data from database and aggregate by epiweek. Used by mem.
#' @name read.cases
#' @keywords internal
#' @author Marcelo F C Gomes

read.cases <- function(start_year, end_year, con, mun_list=NULL){
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
      
      # In the database we have each single notification. So we'll have to aggregate later on
      # We'll use an object name "weekly" from the start since we'll overwrite it with the aggregate later.
      df.cases.weekly <- dbGetQuery(conn = con, sqlquery, stringsAsFactors=FALSE)
      
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
#'@param APSid 0(APS1), 1 (APS2.1), 2 (APS2.2), 3(APS3.1), 4(APS3.2), 5(APS3.3), 6(APS4),
#', 7(APS5.1), 8(APS5.2), 9(APS5.3)  
#'@param cid10 cid 10 disease code. A90 = dengue (default) , A920 = chikungunia
#'@param lastday end date of the time series
#'@return data.frame with the data aggregated per health district and week
#'@examples
#'dC = getCasesinRio(APSid = 9, datasource = con) # Rio de Janeiro
#'tail(dC)
#'dC1 = getCasesinRio(APSid = 0, cid10 = "A920", datasource = con) # Rio de Janeiro
#'tail(dC1)

getCasesinRio <- function(APSid, lastday = Sys.Date(), cid10 = "A90",
                          datasource) {
      
      sqldate <- paste("'", lastday, "'", sep = "")
      #dealing with synonimous cid
      if (cid10 == "A90") cid <- c("A90") # dengue, dengue hemorragica
      if (cid10 %in% c("A92", "A920","A92.0")) {cid <-c("A92", "A920","A92.0"); cid10 <- "A92.0"}  # chik
      if (cid10 %in% c("A92.8","A928")) {cid <- c("A92.8","A928"); cid10 <- "A92.8"} #zika
      if (!(cid10 %in% c("A90","A92.0","A92.8")))stop(paste("Eu nao conheco esse cid10",cid10))
      sqlcid <- paste("'", cid10, "'", sep = "")
      
      if(!(APSid %in% 0:9))stop("APS desconhecida ou ausente. Especificar: 0(APS1), 1 (APS2.1), 2 (APS2.2), 
                                    3(APS3.1), 4(APS3.2), 5(APS3.3), 6(APS4) 7(APS5.1), 8(APS5.2), 9(APS5.3) ")
      
      sqlquery = paste("SELECT n.dt_notific, n.ano_notif, se_notif, l.id, l.nome
      FROM  \"Municipio\".\"Notificacao\" AS n 
      INNER JOIN \"Municipio\".\"Bairro\" AS b 
      ON n.bairro_nome = b.nome 
      INNER JOIN \"Municipio\".\"Localidade\" AS l 
      ON b.\"Localidade_id\" = l.id 
      WHERE n.municipio_geocodigo = 3304557 AND l.id = ",APSid, "AND dt_digita <= ",sqldate, 
                       "AND n.cid10_codigo = ", sqlcid)
      
      d <- dbGetQuery(datasource,sqlquery)
      d$SEM_NOT <- d$ano_notif*100+d$se_notif 
      d$SEM_NOT <- data2SE(d$dt_notific, format = "%Y-%m-%d")
            
      #Cria Serie temporal de casos
      #sem <- seqSE(from = min(d$SEM_NOT), to = max(d$SEM_NOT))$SE
      sem <- seqSE(from = 201001, to = data2SE(lastday,format="%Y-%m-%d"))$SE
      nsem <- length(sem)
      st <- data.frame(SE = sem, casos = 0)
      for(i in 1:nsem) st$casos[i] <- sum(d$SEM_NOT == st$SE[i])
      st$nome <- "Rio de Janeiro"
      # agrega informacao de populacao da APS
      
      pop = NA
      sql2 <- paste("SELECT nome,id,populacao from \"Municipio\".\"Localidade\" WHERE id =", APSid) 
      varglobais <- dbGetQuery(datasource,sql2)
      st$cidade <- 3304557
      st$pop <- varglobais$populacao     
      st$CID10 <- cid10
      st$localidade <- varglobais$nome
      st$localidadeid <- varglobais$id
      st  
}



# mergedata --------------------------------------------------------------
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

mergedata <- function(cases = c(), tweet =c(), climate=c(), ini=200952){
      # checking the datasets
      if (!is.null(cases) & !all(table(cases$SE)==1)) 
            stop("merging require one line per SE in case dataset")
      if (!is.null(tweet) & !all(table(tweet$SE)==1)) 
            stop("merging require one line per SE in tweet dataset")
      if (!is.null(climate) & !all(table(climate$SE)==1))
            stop("merging require one line per SE in climate dataset. Mybe you have more than one station.")
      
      # merging
      if (is.null(cases)) {
            d <- merge(climate, tweet, by=c("SE"), all = TRUE)
      } else if (is.null(tweet)){
            d <- merge(cases, climate,  by=c("SE"), all = TRUE)     
      } else if (is.null(climate)) {
            d <- merge(cases, tweet[, c("SE","tweet")],  by=c("SE"), all = TRUE)
      }
      if (!(is.null(cases) | is.null(tweet) | is.null(climate))){
            d <- merge(cases, tweet[, c("SE","tweet")],  by=c("SE"), all = TRUE)
            d <- merge(d, climate,  by=c("SE"), all=TRUE)  
      }
      # removing beginning
      d <- subset(d, SE > ini)
      d
}


