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
#'@examples
#'epiYear(se = 201012)
#'epiYear(se = 201012:201522)

epiYear <- function(se, cut = 41){
  
  d <- tibble(se = se)
  d %>%
    mutate(year = round(se/100),
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
#'@return data.frame with the epidemiological weeks. 
#'@examples
#'data2SE("01-02-2020",format="%d-%m-%Y")
#'data2SE("12-02-2008",format="%d-%m-%Y")
#'data2SE(c("03-04-2013","07-01-2019"),format="%d-%m-%Y")

data2SE <- function(days, format = "%d/%m/%Y"){
  sem <- rep(NA,length(days))      
  days<-as.Date(as.character(days),format=format)
  for (i in 1:length(days)) {
    sem[i]<-episem(days[i])      
  }
  sem
}

# episem ---------------------------------------------------------------------
#' @description Find to which epidemiological week belongs a given day 
#' @author Oswaldo Cruz
#' @title Define Epidemiological Week.
#' @export
#' @param date date to be converted (class Date)
#' @param separa symbol between year and week
#' @param retorna What should be return, if epidemiological year and week ('YW'), epi. year only ('Y') or epi. week only ('W').
#'   Default: 'YW'.
#' @return epidemiological week or year. If separa = '', the output is numeric; otherwise is a character.
#' @examples
#' episem(x= as.Date("2018-12-31", format="%Y-%m-%d"))
#' episem(x= as.Date("2015-01-01", format="%Y-%m-%d"), separa='-')
#' episem(x= as.Date("2015-01-01", format="%Y-%m-%d"), retorna='Y')

episem <- function(x, format="%Y-%m-%d", separa='', retorna='YW') {
      # semana epi 1 de 2000 02/01/2000
      if (class(x)!= "Date") {
            x <- as.Date(x, format = format)
            #warning("Precisa ser do tipo Date - Convertendo de texto")
      }
      if (is.na(x) == T) {
            message("episem: Date not valid, returning NA")
            return(NA) 
      }
            ##  funcoes auxiliares - poderia usar a lubridate mas achei assim mais simples
      
      year  <- function(dt) {as.numeric(format(dt,"%Y"))}  ## retorna ano
      wday <- function(dt) {as.numeric(format(dt,"%w"))}   ## retorna dia sendo  0 = domingo a 6= sabado
      passado <- function(dt,diff=1) {as.Date(paste(as.numeric(format(dt,"%Y"))-diff,format(dt,"%m-%d"),sep="-"))} ## ano - x
      
      ## Inicio 
      
      ano <- year(x) # extrai ano
      dia1 <- as.Date(paste(ano,'01','01',sep='-'),format = "%Y-%m-%d") # primeiro do ano 
      
      diasem <- wday(dia1)  #descobre o dia da semana do dia1 
      fwd <- ifelse (diasem <=3, dia1 - diasem , dia1 + (7 - diasem) ) #se for menor ou igua a 3 (quarta) 
      fwd <- as.Date(fwd,origin = '1970-01-01') # reformata em data pois ela perde a formatacao 
      
      ## caso a data seja menor que a da 1o semana do ano (fwd)
      if (x < fwd) {
            dia1 <- passado(dia1)  # ano -1 
            diasem <- wday(dia1)  #dia da semana 
            fwd <- ifelse (diasem <=3, dia1 - diasem , dia1 + (7 - diasem) )
            fwd <- as.Date(fwd,origin = '1970-01-01')
      }
      
      diafim <- as.Date(paste(ano,'12','31',sep='-')) #Ultimo dia do ano
      diasem <- wday(diafim)                          #dia semana do ultimo dia
      
      ewd <- ifelse (diasem < 3, diafim - diasem - 1, diafim + 6 - diasem) 
      ewd <- as.Date(ewd,origin = '1970-01-01') # ultima semana epi do ano
      
      if (x > ewd) fwd <- ewd + 1 #caso a data (x) seja maior ou igual a ultiam semaan do ano
      epiweek <- floor(as.numeric(x - fwd) / 7 ) + 1 #numero de semanas e a diff da data e da primeira semana div por 7
      
      if(epiweek==0) epiweek <- 1 ## gatilho se for 0 vira semana 1
      epiyear <- year(fwd + 180) ## ano epidemiologico

      if (retorna=='YW'){
            out <- sprintf("%4d%s%02d",epiyear,separa,epiweek)  ## formata string com separador
      } else if (retorna=='Y') {
            out <- epiyear
      } else {
            out <- epiweek
      }
      
      if (separa =="") {
            return(as.numeric(out))
      } else {
            return(out)
      }
}



#' lastepiweek -----------------------------------
#' @description Calculate number of year's last epidemiological week using Brazilian standard.
#' @name lastepiweek
#' @author Marcelo F Gomes
#' @param ano Year
#' @keywords internal
#' @examples 
#' lastepiweek(2018)

lastepiweek <- function(ano){
      
      # Calcula o valor da ultima semana do ano
      
      diafim <- as.Date(paste(ano,'12','31',sep='-')) #Ultimo dia do ano
      diasem <- as.numeric(format(diafim,"%w"))       #dia semana do ultimo dia
      
      ewd <- ifelse (diasem < 3, diafim - diasem - 1, diafim + 6 - diasem) # Obtem a data do ultimo sabado
      ewd <- as.Date(ewd,origin = '1970-01-01') # ultima semana epi do ano
      
      return(episem(ewd,retorna='W'))
}

# SE2date ---------------------------------------------------------------------
#'@description Return the first day of the Epidemiological Week
#'@title Return the first day of the Epidemiological Week
#'@export
#'@param SE string vector with dates to be converted, format 201420
#'@return data.frame with SE and first day.
#'@examples
#'SE2date(se=201812)
#'SE2date(se = c(202001:202109))

SE2date <- function(se){
      if(!class(se[1]) %in% c("numeric","integer")) stop("se should be numeric or integer")

      #load("R/sysdata.rda")
      #SE$sem <- SE$Ano*100 + SE$SE
      res <- data.frame(SE = se, ini = as.Date("1970-01-01"))
      for (i in 1:length(res$SE)) res$ini[i] <- SE$Inicio[SE$SE == res$SE[i]]
      res
}

# daySEday---------------------------------------------------------------------
#'@description Return the first day of the Epidemiological Week and vice-versa
#'@title Return the first day of the Epidemiological Week and vice-versa
#'@export
#'@param x numeric vector with epidemiological weeks , format 201945, or date
#'@return data.frame with SE and first day.
#'@examples
#'daySEday(x=201812)
#'daySEday(x = c(202041:202104))
#'daySEday(x = c("2015-12-23", "2015-10-23", "2014-10-16"))

daySEday <- function(x, format = "%Y-%m-%d"){
      #load("R/sysdata.rda")
      n <- length(x)
            if(class(x[1]) %in% c("numeric","integer")) {
            assert_that(all(x > 200952 & x < 202200), msg = "day2SE: SE format = 
                        201612, btw 201001 and 202152")
                  
            res <- data.frame(SE = x, ini = as.Date("1970-01-01"))
                  
            for (i in 1:n) res$ini[i] <- SE$Inicio[SE$SE == res$SE[i]]
            return(res)
            
            }
      if(class(x[1]) == "character") x <- as.Date(x, format = format)
      assert_that(all(x <= "2021/12/31" & x >= "2010/01/01"))
      res <- data.frame(SE = NA, ini = x)
      for (i in 1:n) res$SE[i] <- SE[which(SE$Inicio<=x[i] & SE$Termino >= x[i]), "SE"]
      return(res)
      
      
      #SE$sem <- SE$Ano*100 + SE$SE
      
}


# seqSE ---------------------------------------------------------------------
#'@description Creates a sequence of epidemiological weeks and respective initial and final days
#'@title Sequence of epidemiological weeks.
#'@export
#'@param from first week in format 201401
#'@param to first week in format 201401
#'@return data.frame with the epidemiological weeks and corresponding extreme days. WARNING: only works from 2010 to 2020.
#'@examples
#'seqSE(202042, 202110)

seqSE <- function(from, to){
      #load("R/sysdata.rda")
      #SE$SE <- SE$Ano*100 + SE$SE
      N <- dim(SE)[1]
      
      if (from < SE$SE[1]){
            from <- SE$SE[1]
            #warning(paste("first SE set to", from))
      }
      
      if (to > SE$SE[N]){
            to <- SE$SE[N]
            warning(paste("This function only works from 2010 to
                          ",max(SE$Ano),". Last returned date is", to))
      }
      
      SE[which(SE$SE==from):which(SE$SE==to),]
}


# lastDBdate ---------------------------------------------------------------------
#'@description  Useful to check if the database is up-to-date. 
#'@title Returns the most recent date present in the database table. 
#'@export
#'@param tab table in the database. Either (sinan, clima_wu, tweet, historico,
#'historico_mrj). 
#'@param cities vector of geocodes.
#'@param cid10 relevant for sinan, tweeter or historico. Codes are: dengue "A90", 
#'chik "A92.0", zika "A92.8".  
#'@param stations vector with wu stations. Ex. c("SGBL", "SBRL")
#'@param datasource 
#'@return vector with two elements: data.max = most recent date in the collection of cities or stations;
#'se = corresponding epidemiological week.
#'@examples
#'cidades <- getCidades(regional = "Sete Lagoas", uf = "Minas Gerais")
#'lastDBdate(tab = "tweet", cities = cidades$municipio_geocodigo, cid10 = "A90")
#'lastDBdate(tab = "sinan", cities = cidades$municipio_geocodigo, cid10 = "A92.8")
#'lastDBdate(tab = "clima_wu", stations = "SBAF", datasource=con)  
#'lastDBdate(tab = "clima_wu", cities = cidades$municipio_geocodigo, datasource=con)  

lastDBdate <- function(tab, cities, cid10 = "A90", stations, datasource = con){
      
      # check input
      assert_that(tab %in% c("sinan", "clima_wu", "tweet", "historico"), msg =
                        "lastDBdate only works for tables sinan, clima_wu, tweet, historico and historico_mrj")
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
      
      if (tab %in% c("sinan","tweet","historico", "historico_mrj")){
      
            assert_that(cid10 %in% c("A90", "A92.0", "A92.8"), 
                        msg = "lastDBdate asking for valid cid10")
            
            sqlcity = paste("'", str_c(cities, collapse = "','"),"'", sep="")
            sqlcid = paste("'", str_c(cid, collapse = "','"),"'", sep="") # dealing with multiple cids for the same disease  
            
            
            if(tab == "sinan") {
              if(class(datasource) == "PostgreSQLConnection"){
                  sqlcom <- paste("SELECT MAX(dt_digita) from \"Municipio\".\"Notificacao\" 
                                  WHERE municipio_geocodigo IN (", sqlcity, 
                                  ") AND cid10_codigo IN(", sqlcid,")", sep="")
              }
              if(class(datasource) == "SQLiteConnection"){
                sqlcom <- paste("SELECT MAX(dt_digita) from \"Notificacao\" 
                                  WHERE municipio_geocodigo IN (", sqlcity, 
                                ") AND cid10_codigo IN(", sqlcid,")", sep="")
              }
              
            }
            
            if(tab == "tweet"){
            
                  if(cid10 != "A90"){
                        message("tweet table has no value for this cid10")
                        return(NULL)
                  } else {
                    if(class(datasource) == "PostgreSQLConnection"){
                        sqlcom <- paste0("SELECT MAX(data_dia) from \"Municipio\".\"Tweet\" WHERE
                                         \"Municipio_geocodigo\" IN (", sqlcity,")")
                    }
                    if(class(datasource) == "SQLiteConnection"){
                      sqlcom <- paste0("SELECT MAX(data_dia) from \"Tweet\" WHERE
                                         \"Municipio_geocodigo\" IN (", sqlcity,")")
                    }
                  }
            }
            
            if (tab == "historico"){
              
              if(class(datasource) == "PostgreSQLConnection"){
                      if(cid10 == "A90")    tabela <- "\"Municipio\".\"Historico_alerta\""
                      if(cid10 == "A92.0")  tabela <- "\"Municipio\".\"Historico_alerta_chik\""
                      if(cid10 == "A92.8")  tabela <- "\"Municipio\".\"Historico_alerta_zika\""
              }
              
              if(class(datasource) == "SQLiteConnection"){
                if(cid10 == "A90")    tabela <- "\"Historico_alerta\""
                if(cid10 == "A92.0")  tabela <- "\"Historico_alerta_chik\""
                if(cid10 == "A92.8")  tabela <- "\"Historico_alerta_zika\""
              }
              
              sqlcom <- paste0("SELECT MAX(\"data_iniSE\") FROM ",tabela, 
                                     " WHERE municipio_geocodigo IN (", sqlcity,")")
              }
                      
            if (tab == "historico"){
              
              if(class(datasource) == "PostgreSQLConnection"){
                        if(cid10 == "A90")    tabela <- "\"Municipio\".alerta_mrj_dengue"
                        if(cid10 == "A92.0")  tabela <- "\"Municipio\".alerta_mrj_chik"
                        if(cid10 == "A92.8")  tabela <- "\"Municipio\".alerta_mrj_zika"
              }
              
              if(class(datasource) == "SQLiteConnection"){
                if(cid10 == "A90")    tabela <- "alerta_mrj_dengue"
                if(cid10 == "A92.0")  tabela <- "alerta_mrj_chik"
                if(cid10 == "A92.8")  tabela <- "alerta_mrj_zika"
              }
                              
                              sqlcom <- paste0("SELECT MAX(data) FROM ",tabela, 
                                               " WHERE municipio_geocodigo IN (", sqlcity,")")         
            }
            
      } 
      
      if (tab == "clima_wu"){
            if(missing(stations) & !missing(cities)){
                  wu_table <- getWUstation(cities)
                  stations <- unique(wu_table$codigo_estacao_wu, wu_table$estacao_wu_sec)
            }
            
            sqlstations = paste("'", str_c(stations, collapse = "','"),"'", sep="")
            
            if(class(datasource) == "PostgreSQLConnection"){
              sqlcom <- paste("SELECT MAX(data_dia) from \"Municipio\".\"Clima_wu\" WHERE 
                               \"Estacao_wu_estacao_id\" IN ( ",sqlstations,")")
            }
            
            if(class(datasource) == "SQLiteConnection"){
              sqlcom <- paste("SELECT MAX(data_dia) from \"Clima_wu\" WHERE 
                               \"Estacao_wu_estacao_id\" IN ( ",sqlstations,")")
            }
                  
      }
      try(ult_day <- dbGetQuery(datasource,sqlcom))
      ult_se <- NA
      if(!is.na(ult_day)){
         if(is.date(ult_day)) {ult_se <- data2SE(ult_day$max, format = "%Y-%m-%d")} # SE
         else{
           ult_day <- as.Date(x = ult_day[[1]], origin = "1970-01-01")
           #ult_se <- data2SE(ult_day, format = "%Y-%m-%d")
         } 
      }
      return(c(data = ult_day, se = ult_se))
}



# DenguedbConnect ---------------------------------------------------------------------
#'@description  Opens a connection to the Project database. 
#'@title Returns the connection to the database.
#'@export 
#'@param pass password
#'@return "PostgreSQLConnection" object   
#'@examples
#'con <- DenguedbConnect(pass)
#'dbListTables(con) 
#'dbDisconnect(con)

DenguedbConnect <- function(pass){
      dbname <- "dengue"
      user <- "dengueadmin"
      password <- pass
      host <- "localhost"
      
      dbConnect(dbDriver("PostgreSQL"), user=user,
                       password=password, dbname=dbname)
      
}


# sevendigitgeocode ---------------------------------------------------------------------
#'@description  calculates the verification digit of brazilian municipalities. Required 
#'to convert 6 digits to 7 digits geocodes. 
#'@title convert 6 to 7 digits geocodes. 
#'@export
#'@return 7 digits municipality geocode.   
#'@examples
#'sevendigitgeocode(330455)
#'sevendigitgeocode(3304557)

sevendigitgeocode <- function(dig){
      
      peso <- c(1, 2, 1, 2, 1, 2, 0)
      soma <- 0
      digchar <- strsplit(as.character(dig),"")[[1]]
      ndig <- length(digchar)
      if (ndig == 7) return(dig)
      if (ndig!=6) stop("this funtion receives 6 digits geocodes only")
      
      for (i in 1:6){
            valor <- as.integer(digchar[i]) * peso[i]
            nvalor <- ifelse(valor < 10, valor, trunc(valor/10) + valor%%10)
            soma <- soma + nvalor
      }
      dv <- ifelse(soma%%10 == 0, 0, 10 - (soma%%10))
      dig*10+dv
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
#'@return vector 
#'@examples
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
#'@return vector with replaced NA.
#'@examples
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
            c.a<-auto.arima(x,max.p=5,max.q=5,max.P=5,max.Q=5)$arma
            # Modelo considerando a sazonalidade, e a parte ARIMA sugerida anteriormente:
            modelo.sarima<-arima(na.approx(v),order=c.a[c(1,6,2)],seasonal=list(order=c(c.a[3],1,c.a[4]),period=52))
            
            message(paste("temperature predicted", Nna, "steps ahead"  ))
            predito<-predict(modelo.sarima,n.ahead=Nna)$pred
            
            if (plotar == T){
                  fitado<-fitted(modelo.sarima)
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
#'@examples
#'getRegionais(uf="Rio de Janeiro")
#'head(getRegionais(uf="Rio de Janeiro",output="complete",macroreg = TRUE))
#'head(getRegionais(uf="Rio de Janeiro",output="complete"))
#'getRegionais(cities = c(3304128,3306107,3300159), uf="Rio de Janeiro")
#'getRegionais(cities = c(3304128,3306107,3300159), uf="Rio de Janeiro", macroreg = TRUE)
#'getRegionais(uf="Rio de Janeiro", sortedby = 'id')

getRegionais <- function(cities, uf, sortedby = "a", macroreg = FALSE, datasource=con, output = "names"){
      
      assert_that(!missing(uf), msg = "getRegionais: please specify uf. Ex. uf = \"Ceará\" ")
  
  if(class(datasource) == "PostgreSQLConnection"){
     sqlquery = paste("SELECT m.geocodigo, m.nome, r.nome, r.codigo, mr.nome, mr.codigo   
                  FROM \"Dengue_global\".\"Municipio\" m  
                  INNER JOIN \"Dengue_global\".regional r 
                  ON m.id_regional = r.id 
                  INNER JOIN \"Dengue_global\".macroregional mr
                  ON r.id_macroregional = mr.id 
                  where m.uf = '", uf, "'", sep="")
            
      d = dbGetQuery(datasource, sqlquery)    
  }
  
  if(class(datasource) == "SQLiteConnection"){
    sqlquery = paste("SELECT m.geocodigo, m.nome, r.nome, r.codigo, mr.nome, mr.codigo   
                  FROM \"Municipio\" m  
                  INNER JOIN regional r 
                  ON m.id_regional = r.id 
                  INNER JOIN macroregional mr
                  ON r.id_macroregional = mr.id 
                  where m.uf = '", uf, "'", sep="")
    
    d = dbGetQuery(datasource, sqlquery)    
  }   
        assert_that(nrow(d) > 0, msg = (paste("getRegionais: Database does not have the health areas for ", uf)))
      
      names(d) <- c("municipio_geocodigo","cidade","regional","codigo_regional","macroregional","codigo_macroregional")
      if(!missing(cities)) {
            d <- d %>% filter(municipio_geocodigo %in% cities)
            assert_that(nrow(d) == length(cities), msg = (paste("getRegionais: Database does not have 
                                                                the health districts for all listed cities in", uf)))
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
#'@param regional full name of the regional.
#'@param macroregional full name of the macroregional.
#'@param datasource name of the database
#'@return vector with names of the cities.
#'@examples
#'getCidades(regional = "Metropolitana I", uf="Rio de Janeiro")
#'getCidades(uf="Maranhão")
#'getCidades(uf="Maranhão", macroregional = "Norte")

getCidades <- function(regional, macroregional, uf, datasource=con){
      
      if(missing(uf)) stop("getCidades requer nome da uf por extenso")
      if(!missing(regional)){
        
        if(class(datasource) == "PostgreSQLConnection"){
          sqlquery = paste("SELECT m.geocodigo, m.nome, r.nome, r.codigo, mr.nome, mr.codigo, m.uf   
                  FROM \"Dengue_global\".\"Municipio\" m  
                  INNER JOIN \"Dengue_global\".regional r 
                  ON m.id_regional = r.id 
                  INNER JOIN \"Dengue_global\".macroregional mr
                  ON r.id_macroregional = mr.id 
                  WHERE m.uf = '", uf, "' AND r.nome = '", regional ,"'", sep="")
        }
        
        if(class(datasource) == "SQLiteConnection"){
          sqlquery = paste("SELECT m.geocodigo, m.nome, r.nome, r.codigo, mr.nome, mr.codigo, m.uf   
                  FROM \"Municipio\" m  
                  INNER JOIN regional r 
                  ON m.id_regional = r.id 
                  INNER JOIN macroregional mr
                  ON r.id_macroregional = mr.id 
                  WHERE m.uf = '", uf, "' AND r.nome = '", regional ,"'", sep="")
        }
        
        
        d <- dbGetQuery(datasource, sqlquery) 
        assert_that(nrow(d)>0, msg = "getCidades: found no city")
        names(d) <- c("municipio_geocodigo", "cidade", "regional", "regional_id", "macroregional","macroregional_id","uf")
        return(d)    
      }
  
  if(!missing(macroregional)){
    
    if(class(datasource) == "PostgreSQLConnection"){
      sqlquery = paste("SELECT m.geocodigo, m.nome, r.nome, r.codigo, mr.nome, mr.codigo, m.uf   
                  FROM \"Dengue_global\".\"Municipio\" m  
                  INNER JOIN \"Dengue_global\".regional r 
                  ON m.id_regional = r.id 
                  INNER JOIN \"Dengue_global\".macroregional mr
                  ON r.id_macroregional = mr.id 
                  WHERE m.uf = '", uf, "' AND mr.nome = '", macroregional ,"'", sep="")
    }
    
    if(class(datasource) == "SQLiteConnection"){
      sqlquery = paste("SELECT m.geocodigo, m.nome, r.nome, r.codigo, mr.nome, mr.codigo, m.uf   
                  FROM \"Municipio\" m  
                  INNER JOIN regional r 
                  ON m.id_regional = r.id 
                  INNER JOIN macroregional mr
                  ON r.id_macroregional = mr.id 
                  WHERE m.uf = '", uf, "' AND mr.nome = '", macroregional ,"'", sep="")
    }
      
      
    d <- dbGetQuery(datasource, sqlquery) 
    assert_that(nrow(d)>0, msg = "getCidades: found no city")
    names(d) <- c("municipio_geocodigo", "cidade", "regional", "regional_id", "macroregional","macroregional_id","uf")
    return(d)     
  }
  
  # retorna para todo o estado
  if(class(datasource) == "PostgreSQLConnection"){
  sqlquery = paste("SELECT m.geocodigo, m.nome, r.nome, r.codigo, mr.nome, mr.codigo, m.uf   
                  FROM \"Dengue_global\".\"Municipio\" m  
                  INNER JOIN \"Dengue_global\".regional r 
                  ON m.id_regional = r.id 
                  INNER JOIN \"Dengue_global\".macroregional mr
                  ON r.id_macroregional = mr.id 
                  WHERE m.uf = '", uf, "'", sep="")
  }
  
  if(class(datasource) == "SQLiteConnection"){
    sqlquery = paste("SELECT m.geocodigo, m.nome, r.nome, r.codigo, mr.nome, mr.codigo, m.uf   
                  FROM \"Municipio\" m  
                  INNER JOIN regional r 
                  ON m.id_regional = r.id 
                  INNER JOIN macroregional mr
                  ON r.id_macroregional = mr.id 
                  WHERE m.uf = '", uf, "'", sep="")
  }
  
  d <- dbGetQuery(datasource, sqlquery) 
  assert_that(nrow(d)>0, msg = "getCidades: found no city")
  names(d) <- c("municipio_geocodigo", "cidade", "regional", "regional_id", "macroregional","macroregional_id","uf")
  return(d)       
      
}


# write_parameters ------------------------------------
#'@description  Write the alert parameters for each city into the database, to be used in the update.alert. 
#'Currently, the parameters are: "limiar_preseason", "limiar_posseason",
#'"limiar_epidemico,"varcli", "varcli2", "clicrit", "clicrit2" , "cid10", "codmodelo". 
#' City must be already in the regionais table.
#'@title City's parameterization. 
#'@export
#'@param params vector of the names of the params to be inserted in the table. Limiar is given as incidence. 
#'It can be a subset of the default. 
#'@return the new line in the parameters table 
#'@examples
#'pars = data.frame(municipio_geocodigo = 3506003,limiar_preseason = 4.50243, limiar_posseason = 3.962566, 
#'limiar_epidemico = 67.72364, varcli = "temp_min", clicrit = 22, cid10 = "A90", codmodelo = "Af") 
#'res = write_parameters(params$municipio_geocodigo, params$cid10, params = pars)

write_parameters<-function(city, cid10, params, overwrite = FALSE, datasource = con){
      
      # checking inputs
      assert_that(class(params) == "data.frame", 
                  msg = "write_parameters: params should be a data.frame")
      
      assert_that(nrow(params) == 1 , 
                  msg = "write_parameters write one line only")
      
      assert_that(cid10 %in% c("A90","A92.0","A92.8"), 
                  msg = paste("write_parameters: not prepared for cid10 = ",params$cid10))
      
      assert_that(all(c("municipio_geocodigo","cid10") %in% names(params)), 
                  msg = paste("write.parameters: params must contain municipio_geocodigo, cid10"))
      
      assert_that(class(datasource) == "PostgreSQLConnection", 
                  msg = paste("write.parameters: datasource must be a connection to the Infodengue server"))
      
      # check if city is already in the system (Regional table) - they all are
      
      #sql1 = paste("SELECT * from \"Dengue_global\".regional_saude SET  
      #                         WHERE municipio_geocodigo = ",city, sep="")      
      #cityregtable = try(dbGetQuery(datasource, sql1))
      
      #assert_that(nrow(cityregtable)>0, 
      #            msg = paste("geocode", city, "not implemented in Infodengue. Use insertCityinAlerta()") )
      
      # Next step, check if there are any parameters for this cid10?      
      sql2 = paste("SELECT * from \"Dengue_global\".parameters SET  
                               WHERE municipio_geocodigo = ",city," AND cid10 = $$",
                                cid10,"$$", sep="")      
      
      parline = try(dbGetQuery(datasource, sql2))
      
      assert_that(nrow(parline) < 2, 
                  msg = paste("parameter table has something wrong. more than one line for", 
                                               params$cid10, "for city", city, "."))
      
      # now let's write the data
      # if line does not exist, create one with the cid and geocode:
      if(nrow(parline) == 1 & overwrite == FALSE){
            message("the following parameters were found. Rerun with overwrite = T to replace them")
            print(parline)
            return(parline)
      }
      
      if(nrow(parline) == 0){ #
       message(paste("no previous param found. Inserting new param line for city", params$municipio_geocodigo))
       linha = paste(as.character(params$municipio_geocodigo), ",\'",params$cid10, "\'",sep="")
       sql = paste("insert into \"Dengue_global\".parameters (municipio_geocodigo, cid10) values(", linha ,")")
       dbGetQuery(datasource, sql)    
       
       # check if was correctly created
       parline_now = try(dbGetQuery(datasource, sql2))
       
       assert_that(nrow(parline_now) == 1, 
                   msg = paste("parameter table has something wrong. number of lines for ", 
                               params$cid10, "for city", city, "is:", nrow(parline_now) ))
      }
      
      vars <- params %>% dplyr::select(-c("municipio_geocodigo", "cid10"))
      nvars <- length(vars)
      
      # updating parameter values
      
      for (i in 1:nvars) {
            linha = paste(names(vars)[i], " = '", vars[[i]], "'", sep = "")
            update_sql = paste("UPDATE \"Dengue_global\".parameters SET ", linha , 
                               " WHERE municipio_geocodigo = ", params$municipio_geocodigo,
                               " AND cid10 = \'", cid10, "\'", sep="")      
            try(dbGetQuery(datasource, update_sql))
      }
             
      
}

# read.parameters ------------------------------------
#'@description  Read the alert parameters for a set of cities from the database, to be used in the infodengue pipeline. 
#'Currently, the parameters are: "limiar_preseason" (pre-season incidence threshold calculated using MEM), 
#'"limiar_posseason" (pos-season incidence threshold), "limiar_epidemico"(epidemic threshold), "varcli" (name of the critical 
#'meteorological variable), "clicrit" (critical value of the meteorological variable), "cid10",
#'"codmodelo" (name of the heuristic decision model, see serCriteria()). These parameters are specified when the city is initiated 
#'in the pipeline.
#'@title Get city-level alert parameters for the infodengue pipeline.
#'@export 
#'@param cities cities' geocodes. Tip: find them using getCidades(). 
#'@param cid10 Dengue = "A90" (default), Chik = "A92.0", Zika = "A92.8"
#'@param datasource SQL connection to the database
#'@return dataframe with all parameters
#'@examples
#'read.parameters(cities = 3118601, cid10 = "A90")
#'cid <- getCidades(regional = "Norte",uf = "Rio de Janeiro")
#'read.parameters(cities = cid$municipio_geocodigo, cid10 = "A90")

read.parameters<-function(cities, cid10 = "A90", datasource=con){
      
      cities <- sapply(cities, function(x) sevendigitgeocode(x))
      if(cid10 != "A90")print("tab de parametros so tem dengue. Usando-os.")
      cid10 = "A90"
      # reading parameters from database
      
      sqlcity = paste("'", str_c(cities, collapse = "','"),"'", sep="")
      
      if(class(datasource) == "PostgreSQLConnection"){
      comando = paste("SELECT * FROM \"Dengue_global\".parameters WHERE cid10 = '", cid10 , 
                        "' AND municipio_geocodigo  IN (", sqlcity,")", sep="")
      }
      
      if(class(datasource) == "SQLiteConnection"){
        comando = paste("SELECT * FROM parameters WHERE cid10 = '", cid10 , 
                        "' AND municipio_geocodigo  IN (", sqlcity,")", sep="")
      }
      
      try(dd <- dbGetQuery(datasource,comando))
            
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
#'@examples
#'getWUstation(cities = 3304557)
#'cidades <- getCidades(regional = "Sete Lagoas", uf = "Minas Gerais")
#'getWUstation(cities = cidades$municipio_geocodigo)

getWUstation <- function(cities, datasource = con){
  sqlcity = paste("'", str_c(cities, collapse = "','"),"'", sep="")
  
  if(class(datasource) == "PostgreSQLConnection"){
  comando <- paste("SELECT municipio_geocodigo, codigo_estacao_wu, estacao_wu_sec from 
                       \"Dengue_global\".parameters WHERE municipio_geocodigo IN (", sqlcity, 
                   ")" , sep="")
  }
  if(class(datasource) == "SQLiteConnection"){
    comando <- paste("SELECT municipio_geocodigo, codigo_estacao_wu, estacao_wu_sec from 
                       parameters WHERE municipio_geocodigo IN (", sqlcity, 
                     ")" , sep="")
  }
  
  city_table <- dbGetQuery(datasource,comando)
  return(city_table)
}

# setWUstation ------------------------------------------
#'@description  Set primary and secondary meteorological stations associated 
#'with one or more cities of the same state
#'@title set meteorological stations
#'@export
#'@param st data.frame containing municipio_geocodigo, primary_station, 
#'secondary_station
#'@param UF name of the state.Ex. "Rio de Janeiro"
#'@param senha for the connection to the project database
#'@return 
#'@examples
#'NOT RUN
#'wudata = data.frame(municipio_geocodigo = 3107802, primary_station = "SBIP",
#'secondary_station = "SBGV")
#'setWUstation(wudata, UF = "Minas Gerais")
#'getWUstation(cities =wudata$municipio_geocodigo)

setWUstation <- function(st, UF, datasource = con){
      
      ncities <- nrow(st)
      
      # checking inputs
      assert_that(class(st) == "data.frame", 
                  msg = "setWUstation: st should be a data.frame")
      
      assert_that(all(names(st) %in% c("municipio_geocodigo" , "primary_station",
                                 "secondary_station")), 
                  msg = "setWUstation: st should contain columns municipio_geocodigo, 
                        primary_station, secondary_station")
      
      assert_that(class(datasource) == "PostgreSQLConnection", 
                  msg = paste("setWUstation: datasource must be a connection to the Infodengue server"))
      
      # check if city is already in the system (Regional table)
      cities_table <- getCidades(uf = UF, datasource = datasource)
      cities_in <- st$municipio_geocodigo %in% cities_table$municipio_geocodigo
       
      assert_that(sum(cities_in)==ncities, 
                  msg = paste("geocodes", st$municipio_geocodigo[cities_in == FALSE] , 
                              "not implemented in Infodengue.") )
      
      ## writing 
    
      for (i in 1:ncities) {
            el1 = paste("'", as.character(st$primary_station[i]),"'",sep="")
            el2 = paste("'", as.character(st$secondary_station[i]),"'",sep="")
            linha = paste("codigo_estacao_wu = ", el1, ",", "estacao_wu_sec = ", el2, sep = "")
            update_sql = paste("UPDATE \"Dengue_global\".parameters SET ", linha , " WHERE 
                               municipio_geocodigo = ", st$municipio_geocodigo[i], sep="")  
            cityline = try(dbGetQuery(datasource, update_sql))
      }
      
      return()
}


# insert_city_infodengue ------------------------------------
#'@description  Initial setup of a new city in the alerta system.  Can be integrated later with 
#'the delay model and write.parameters. DEPRECATED
#'@title Initial setup of a new city in the alerta system. Insert into tables Regionais. 
#'@export
#'@param city geocode of the city. Mandatory 
#'@param id_regional numerical id of the 'Regional da saude'.Mandatory
#'@param regional name of the 'Regional da saude'.Mandatory
#'@param macroreg name of the 'MacroRegional da saude'.Mandatory
#'@return 
#'@examples
#'insert_city_infodengue(geocodigo = 1111111, id_regional=1, regional = "teste", macroreg = "teste")

insert_city_infodengue<-function(geocodigo ,id_regional, regional, macroreg, datasource=con){
      
      message("this function is deprecated as all cities are in Infodengue")
      return()
      # check if city is really new
      if(nchar(geocodigo) == 6) geocodigo <- sevendigitgeocode(geocodigo)   
      consult_sql = paste("SELECT * from \"Dengue_global\".regional_saude SET  
                               WHERE municipio_geocodigo = ",geocodigo,sep="")      
      cityline = try(dbGetQuery(datasource, consult_sql))
      
      if (nrow(cityline)!=0) {
            message("city already implemented. Nothing done.")
            if(any(is.na(cityline))) message("Use write.parameters() to set the alert parameters.")
            return(cityline)
      }
      else{
            el1 = as.character(geocodigo)
            el2 = as.character(id_regional)
            el3 = paste("'",regional,"'",sep="")
            el4 = paste("'",macroreg,"'",sep="")
            linha = paste(el1,el2,el3,el4,sep=",")
            sqlinsert1 = paste("insert into \"Dengue_global\".\"regional_saude\" (municipio_geocodigo, id_regional, 
                      nome_regional, nome_macroreg) values(", linha ,")")
            
            try(dbGetQuery(datasource, sqlinsert1))
            }
      
       
      cityline
}
      
# berra - tryCatch fun
berra <- function(expr){
  tryCatch(expr,
           error = function(e){
             message("An error occurred:\n", e)
           },
           warning = function(w){
             message("A warning occured:\n", w)
           },
           finally = {
             message("Finally done!")
           })
}

