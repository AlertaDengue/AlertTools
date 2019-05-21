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
#'res = getWU(stations = 'SBRJ', vars="temp_min", datasource= con)
#'res = getWU(stations = 'SBRJ', vars=c("temp_min", "temp_med"), datasource= con)
#'tail(res)

getWU <- function(stations, vars = "temp_min", finalday = Sys.Date(), datasource) {
      
      if (!all(nchar(stations) == 4)) stop("'stations' should be a vector of 4 digit station names")
      nsta = length(stations)
      
      # loading Test data -------------------------------------------
      if (class(datasource) == "character") {
            load(datasource)
            cities = unique(WUdata$cidade[WUdata$Estacao_wu_estacao_id%in%stations])
            message(paste("stations belong to city(es):", cities))
            d <- subset(WUdata, Estacao_wu_estacao_id %in% stations)
            d <- subset(d, as.Date(d$data, format = "%Y-%m-%d") <= finalday)
                  
      } else if (class(datasource) == "PostgreSQLConnection") {
            # creating the sql query for the stations
            sql1 = paste("'", stations[1], sep = "")
            nsta = length(stations)
            if (nsta > 1) for (i in 2:nsta) sql1 = paste(sql1, stations[i], sep = "','")
            sql1 <- paste(sql1, "'", sep = "")
            # sql query for the date
            sql2 = paste("'", finalday, "'", sep = "")
            # sql query for the variables
            sql3 = paste("data_dia, \"Estacao_wu_estacao_id\",", vars[1], sep = "")
            nv = length(vars)
            if (nv > 1) for (i in 2:nv) sql3 = paste(sql3, vars[i], sep = ",")
          
            sql <- paste("SELECT", sql3, "from \"Municipio\".\"Clima_wu\" WHERE 
                        \"Estacao_wu_estacao_id\"
                        IN  (", sql1, ") AND data_dia <= ",sql2)
            d <- dbGetQuery(datasource,sql)
      }
      
      names(d)[which(names(d)== "Estacao_wu_estacao_id")]<-"estacao"
      
      # Atribuir SE e agregar por semana-----------------------------------------
      if(nrow(d)!=0){
            d$SE <- data2SE(d$data_dia, format = "%Y-%m-%d")
            
            sem <- seqSE(from = min(d$SE), to = max(d$SE))$SE
            df <- expand.grid(SE=sem, estacao = unique(d$estacao))
            N <- length(df$SE)
            
            for (i in vars){
                  df[, i] <- NA
                  for (t in 1:N){
                        subconj <- subset(d, (SE == df$SE[t] & estacao == df$estacao[t]))
                        df[t, i] <- mean(subconj[,i])
                  }
            }
            return(df)      
      }else{message("estação(ões) ", stations, " não existem no banco de dados")
            return(NULL)}
}

# GetTweet --------------------------------------------------------------
#'@description Create weekly time series from tweeter data from server. The 
#'source of this data is the Observatorio da Dengue (UFMG).
#'@title Get Tweeter Data
#'@param city city's geocode.
#'@param cid10 default is A90 (dengue). If not dengue, returns NA
#'@param finalday last day. Default is the last available.
#'@param datasource server or "data/tw.rda" if using test dataset. 
#' Use the connection to the Postgresql server if using project data. See also DenguedbConnect
#' to open the database connection. 
#'@return data.frame with weekly counts of people tweeting on dengue.
#'@examples
#'res = getTweet(city = 330455, lastday = "2014-03-01", datasource = con)
#'tail(res)

getTweet <- function(city, lastday = Sys.Date(), cid10 = "A90", datasource) {
      
      if(nchar(city) == 6) city <- sevendigitgeocode(city)   
      
      if (cid10 == "A90"){ # get tweets on dengue
            c1 <- paste("select data_dia, numero from \"Municipio\".\"Tweet\" where 
                \"Municipio_geocodigo\" = ", city)
            tw <- dbGetQuery(datasource,c1)
            if (dim(tw)[1]>0) 
                  names(tw) <- c("data_dia","tweet")
      } else {stop("there is no tweet for cid10 in the database")}
      
      if (sum(tw$tweet)==0) message(paste("cidade",city,"nunca tweetou sobre dengue"))
      
      
      # output com data de 201001 ate lastday
      sem <- seqSE(from = 201001, to = data2SE(lastday,format="%Y-%m-%d"))$SE
      tw.agregado <- data.frame(SE = sem, tweet = NA)
      
      if (dim(tw)[1]>0){
            #      tw <- subset(tw, as.Date(data_dia, format = "%Y-%m-%d") <= lastday)
            # transformar data em SE -----------------------------------------
            tw$SE <- data2SE(tw$data_dia, format = "%Y-%m-%d")
            obsSE <- unique(tw$SE)
            for (i in obsSE) {
                  #twse <- tw$SE[i] 
                  tw.agregado$tweet[tw.agregado$SE==i] <- sum(tw$tweet[tw$SE==i])
                  #tw.agregado$tweet[tw.agregado$SE==twse] <- tw.agregado$tweet[tw.agregado$SE==twse] + sum(tw$tweet[i])
            }
            tw.agregado$cidade <- city
      }
      
      tw.agregado
}


# GetCases --------------------------------------------------------------
#'@description Create weekly time series from case data from server. The source is the SINAN. 
#'@title Get Case Data and aggregate per week and area
#'@param city city's geocode.
#'@param finalday last day. Default is the last available.
#'@param cid10 cid 10 code. Dengue = "A90" (default), Chik = "A92.0", Zika = "A92.8", 
#'@param datasource PostgreSQLConnection to project database . 
#'@return data.frame with the data aggregated per week according to disease onset date.
#'@examples
#'dC0 = getCases(city = 330455, lastday ="2018-03-10", datasource = con) # dengue
#'dC0 = getCases(city = 3302205, datasource = con) # dengue, until last day available
#'dC0 = getCases(city = 2304400, cid10= "A923", datasource = con) # zika 
#'head(dC0)

getCases <- function(city, lastday = Sys.Date(), cid10 = "A90", datasource) {
      
      if(nchar(city) == 6) city <- sevendigitgeocode(city)   
      
      #dealing with synonimous cid
      if (cid10 == "A90") cid <- c("A90") # dengue, dengue hemorragica
      if (cid10 %in% c("A92", "A920","A92.0")) {cid <-c("A92", "A920","A92.0"); cid10 <- "A92.0"}  # chik
      if (cid10 %in% c("A92.8","A928")) {cid <- c("A92.8","A928"); cid10 <- "A92.8"} #zika
      if (!(cid10 %in% c("A90","A92.0","A92.8")))stop(paste("Eu nao conheco esse cid10",cid10))
      # reading the data
      if (class(datasource) == "character") { # historical reasons
            load(datasource)
            dd <- subset(sinan, DT_DIGITA <= lastday)
            dd$SEM_NOT <- as.numeric(as.character(dd$SEM_NOT))
            
      } else if (class(datasource) == "PostgreSQLConnection"){ # current entry
            sql1 <- paste("'", lastday, "'", sep = "")
            
            # dealing with multiple cids
            lcid <- length(cid)
            cid10command <- paste("'", cid[1], sep="")
            if (lcid > 1) for (i in 2:lcid) cid10command = paste(cid10command, cid[i], sep = "','")
            cid10command <- paste(cid10command, "'", sep = "")
            
            sql <- paste("SELECT * from \"Municipio\".\"Notificacao\" WHERE dt_digita <= ",sql1, " AND municipio_geocodigo = ", city, 
                         " AND cid10_codigo IN(", cid10command,")", sep="")
            
            dd <- dbGetQuery(datasource,sql)
            if (dim(dd)[1]==0) {
                  message(paste("getCases did not find cid10" , cid10, "for city", city))
            } else {
                  dd$SEM_NOT <- dd$ano_notif * 100 + dd$se_notif
            }
            
      } else { # one or more dbf files
            nf = length(datasource)
            dd <- read.dbf(datasource[1])[,c("ID_MUNICIP","DT_NOTIFIC","SEM_NOT",
                                             "NU_ANO","DT_SIN_PRI","DT_DIGITA",
                                             "SEM_PRI","NM_BAIRRO")]
            dd <- subset(dd, ID_MUNICIP=city)
            if (nf > 1){
                  for (i in 2:nf) {
                        di <- read.dbf(datasource[i])[,c("ID_MUNICIP","DT_NOTIFIC","SEM_NOT",
                                                         "NU_ANO","DT_SIN_PRI","DT_DIGITA",
                                                         "SEM_PRI","NM_BAIRRO")]
                  dd <- rbind(dd, subset(di, ID_MUNICIP==city))
                  dd$SEM_NOT <- as.numeric(as.character(dd$SEM_NOT))
                  } 
            }
      }
      
      sem <- seqSE(from = 201001, to = data2SE(lastday,format="%Y-%m-%d"))$SE
      nsem <- length(sem)
      
      st <- data.frame(SE = sem, casos = 0)
      for(i in 1:nsem) st$casos[i] <- sum(dd$SEM_NOT == st$SE[i])

      st$localidade <- 0
      st$cidade <- city
            
      # pegando nome da cidade e populacao
      sql2 <- paste("SELECT * from \"Dengue_global\".\"Municipio\" WHERE geocodigo =", city) 
      varglobais <- dbGetQuery(datasource,sql2)
      st$nome <- varglobais$nome 
      st$pop <- varglobais$populacao
      st$CID10 <- cid10
      if(any(is.na(st$pop)))message("getCases function failed to import pop data for city", city)
      
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
#'@param dataini "notific" if use notification date to calculate incidence or "sinpri" if uses date of first symptoms 
#'@param lastday end date of the time series
#'@return data.frame with the data aggregated per health district and week
#'@examples
#'dC = getCasesinRio(APSid = 9, datasource = con) # Rio de Janeiro
#'# Chikungunya:
#'dC1 = getCasesinRio(APSid = 0, cid10 = "A920", datasource = con) # Rio de Janeiro
#'dC1s = getCasesinRio(APSid = 0, cid10 = "A920", dataini = "sinpri", datasource = con) # Rio de Janeiro
#'tail(dC1)

getCasesinRio <- function(APSid, lastday = Sys.Date(), cid10 = "A90",dataini = "notific",
                          datasource) {
      require(lubridate)
      sqldate <- paste("'", lastday, "'", sep = "")
      #dealing with synonimous cid
      if (cid10 == "A90") cid <- c("A90") # dengue, dengue hemorragica
      if (cid10 %in% c("A92", "A920","A92.0")) {cid <-c("A92", "A920","A92.0"); cid10 <- "A92.0"}  # chik
      if (cid10 %in% c("A92.8","A928")) {cid <- c("A92.8","A928"); cid10 <- "A92.8"} #zika
      if (!(cid10 %in% c("A90","A92.0","A92.8")))stop(paste("Eu nao conheco esse cid10",cid10))
      sqlcid <- paste("'", cid10, "'", sep = "")
      
      if(!(APSid %in% 0:9))stop("APS desconhecida ou ausente. Especificar: 0(APS1), 1 (APS2.1), 2 (APS2.2), 
                                    3(APS3.1), 4(APS3.2), 5(APS3.3), 6(APS4) 7(APS5.1), 8(APS5.2), 9(APS5.3) ")
      
      sqlquery = paste("SELECT n.dt_notific, n.ano_notif, n.dt_sin_pri, se_sin_pri, se_notif, l.id, l.nome
      FROM  \"Municipio\".\"Notificacao\" AS n 
      INNER JOIN \"Municipio\".\"Bairro\" AS b 
      ON n.bairro_nome = b.nome 
      INNER JOIN \"Municipio\".\"Localidade\" AS l 
      ON b.\"Localidade_id\" = l.id 
      WHERE n.municipio_geocodigo = 3304557 AND l.id = ",APSid, "AND dt_digita <= ",sqldate, 
                       "AND n.cid10_codigo = ", sqlcid)
      
      d <- dbGetQuery(datasource,sqlquery)
      d$SEM_NOT <- d$ano_notif*100+d$se_notif 
      #d$SEM_NOT <- data2SE(d$dt_notific, format = "%Y-%m-%d")
      d$SEM_INI <- year(d$dt_sin_pri)*100+d$se_sin_pri 
      
      #Cria Serie temporal de casos
      #sem <- seqSE(from = min(d$SEM_NOT), to = max(d$SEM_NOT))$SE
      sem <- seqSE(from = 201001, to = data2SE(lastday,format="%Y-%m-%d"))$SE
      nsem <- length(sem)
      st <- data.frame(SE = sem, casos = 0)
      if(dataini=="notific") for(i in 1:nsem) st$casos[i] <- sum(d$SEM_NOT == st$SE[i])
      if(dataini=="sinpri") for(i in 1:nsem) st$casos[i] <- sum(d$SEM_INI == st$SE[i])
      print(paste("calculating incidence using", dataini))
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


