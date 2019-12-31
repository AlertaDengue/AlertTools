# PROJETO ALERTA DENGUE -------------------------------------
# Funcoes auxiliadoras para formatacao dados de clima do Alerta dengue
# Claudia Codeco 2015
# -----------------------------------------------------------

# data2SE ---------------------------------------------------------------------
#'@description Find to which epidemiological week belongs a given day. Uses episem function 
#'(formula generated data).
#'@title Define Epidemiological Week
#'@param date string vector with dates to be converted
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
#' @title Define Epidemiological Week
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
#'@param SE string vector with dates to be converted, format 201420
#'@return data.frame with SE and first day.
#'@examples
#'SE2date(se=201812)
#'SE2date(se = c(201401:201409))

SE2date <- function(se){
      if(!class(se[1]) %in% c("numeric","integer")) stop("se should be numeric or integer")

      #load("R/sysdata.rda")
      #SE$sem <- SE$Ano*100 + SE$SE
      res <- data.frame(SE = se, ini = as.Date("1970-01-01"))
      for (i in 1:length(res$SE)) res$ini[i] <- SE$Inicio[SE$SE == res$SE[i]]
      res
}


# seqSE ---------------------------------------------------------------------
#'@description Creates a sequence of epidemiological weeks and respective initial and final days
#'@title Sequence of epidemiological weeks
#'@param from first week in format 201401
#'@param to first week in format 201401
#'@return data.frame with the epidemiological weeks and corresponding extreme days. WARNING: only works from 2010 to 2020.
#'@examples
#'seqSE(201802, 202110)


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
            warning(paste("This function only works from 2010 to", max(SE$Ano),". Last SE returned is", to))
      }
      
      SE[which(SE$SE==from):which(SE$SE==to),]
}


# lastDBdate ---------------------------------------------------------------------
#'@description  Useful to check if the database is up-to-date. 
#'@title Returns the most recent date present in the database table. 
#'@param tab table in the database. Either (sinan, clima_wu, tweet ou historico). 
#'@param city city geocode, if empty, whole dataset is considered. Not implemented yet.
#'@param station wu station.
#'@param datasource 
#'@return most recent date 
#'@examples
#'lastDBdate(tab="tweet",datasource=con)
#'lastDBdate(tab="tweet", city=330240,datasource=con)
#'lastDBdate(tab="sinan", city=330240,datasource=con)
#'lastDBdate(tab="clima_wu", station="SBAF",datasource=con)  

lastDBdate <- function(tab, city = NULL, station = NULL, datasource){
      if (tab == "sinan"){
            if (is.null(city)) {
                  sql <- "SELECT dt_notific from \"Municipio\".\"Notificacao\""
                  print("Nenhuma cidade indicada, data refere-se ao banco todo")
                  } else {
                        if(nchar(city) == 6) city <- sevendigitgeocode(city)
                        sql <- paste("SELECT dt_notific from \"Municipio\".\"Notificacao\" WHERE municipio_geocodigo = ", city)
                        }
            dd <- dbGetQuery(datasource,sql)
            date <- max(dd$dt_notific)
      }
      
      if (tab == "tweet"){
            if (is.null(city)) {
                  sql <- paste("SELECT data_dia from \"Municipio\".\"Tweet\"")
                  print("Nenhuma cidade indicada, data refere-se ao banco todo")
            } else {
                  if(nchar(city) == 6) city <- sevendigitgeocode(city)
                  sql <- paste("SELECT data_dia from \"Municipio\".\"Tweet\" WHERE \"Municipio_geocodigo\" = ", city)
                  }
            dd <- dbGetQuery(datasource,sql)
            date <- max(dd$data_dia)
      }
      
      
      if (tab == "clima_wu"){
            if (!is.null(city)) stop("indique a estacao desejada")
            if (is.null(station)) {
                  sql <- paste("SELECT data_dia from \"Municipio\".\"Clima_wu\" WHERE 
                               \"Estacao_wu_estacao_id\"")
                  print("Nenhuma estacao indicada, data e' a mais recente no banco todo")
            } else {
                  sql1 <- paste("'", station, "'",sep = "")
                  sql <- paste("SELECT * from \"Municipio\".\"Clima_wu\" WHERE \"Estacao_wu_estacao_id\" = ",sql1)
            }
            dd <- dbGetQuery(datasource,sql)
            datacomdados <- which(is.na(dd$temp.min)==FALSE) 
            date <- dd$data_dia[max(datacomdados)]
      }
      
      if (tab == "historico"){
            if (is.null(city)) {
                  sql <- paste("SELECT \"data_iniSE\" from \"Municipio\".\"Historico_alerta\"")
                  print("Nenhuma cidade indicada, data refere-se ao banco todo")
            } else {
                  sql <- paste("SELECT \"data_iniSE\" from \"Municipio\".\"Historico_alerta\" WHERE municipio_geocodigo = ", city)
            }
            dd <- dbGetQuery(datasource,sql)
            date <- max(dd$data_iniSE)
      }
      date
}



# DenguedbConnect ---------------------------------------------------------------------
#'@description  Opens a connection to the Project database. 
#'@title Returns the connection to the database. 
#'@return "PostgreSQLConnection" object   
#'@examples
#'con <- DenguedbConnect()
#'dbListTables(con) 
#'dbDisconnect(con)

DenguedbConnect <- function(){
      dbname <- "dengue"
      user <- "dengueadmin"
      password <- "aldengue"
      host <- "localhost"
      
      dbConnect(dbDriver("PostgreSQL"), user=user,
                       password=password, dbname=dbname)
      
}


# sevendigitgeocode ---------------------------------------------------------------------
#'@description  calculates the verification digit of brazilian municipalities. Required 
#'to convert 6 digits to 7 digits geocodes. 
#'@title convert 6 to 7 digits geocodes. 
#'@return 7 digits municipality geocode.   
#'@examples
#'sevendigitgeocode(330455)

sevendigitgeocode <- function(dig){
      peso <- c(1, 2, 1, 2, 1, 2, 0)
      soma <- 0
      digchar <- strsplit(as.character(dig),"")[[1]]
      ndig <- length(digchar)

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
#'@title methods to substitute NAs.Use the function na.approx from package zoo. 
#'@param v vector with missing elements.
#'@param rule rule for filling the missing cells. "zero" just fills them with 0; "linear"
#' interpolate using tzoo::na.approx. In this case, the tails are not filled. If "arima", then it interpolates using
#' linear and extrapolates using arima (calling AlertTools::temp.predict) 
#'@param maxgap maximum number of consecutive NAs to fill. Longer gaps will be left i=unchanged. Only works for rule = "zero"
#' or "linear"
#'@return vector with replaced NA.
#'@examples
#'# Interpolation:
#'v <- c(1,2,3,NA,5,6,NA,NA,9,10,NA,NA)
#'nafill(v, rule = "zero")
#'nafill(v, rule = "linear")
#'# Inter using linear and Extrapolation using arima
#'head(cliSBCB)
#'nafill(cliSBCB[,3], rule= "arima")

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
            
            # Para saber os coeficientes da parte ARIMA atraves de criterios de selecao 
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
#'@param uf full name of the state.
#'@param sortedby the options are: 'a' alphabetically, 'id' regional id number, if available 
#'@param database name of the database
#'@return vector with names of the regionais.
#'@examples
#'getRegionais(uf="Rio de Janeiro")
#'getRegionais(uf="Rio de Janeiro", sortedby = 'id')

getRegionais <- function(uf, sortedby = "a", datasource=con){
      
      sqlquery = paste("SELECT nome_regional, id_regional, uf 
                  FROM \"Dengue_global\".\"Municipio\" 
                  INNER JOIN \"Dengue_global\".regional_saude
                  ON municipio_geocodigo = geocodigo
                  where uf = '", uf, "'", sep="")
      
      d = dbGetQuery(datasource, sqlquery)    
      if (nrow(d) == 0) stop(paste("Banco de dados nao tem regionais para o estado ", uf))
      
      if(sortedby == 'a') out <- sort(unique(d$nome_regional)) 
      if(sortedby == 'id') {
            out <- unique(d[order(d$id_regional),"nome_regional"])
            if (any(is.na(d$id_regional))) {
                  warning("getRegionais: codigo das regionais nao encontrado, trocando argummento para sortedby = 'a'")
                  out <- sort(unique(d$nome_regional))
            }
            }
      out
}


# getCidades ------------------------------------
#'@description  consult database to get list of cities 
#'@title get list of cities. 
#'@param uf full name of the state.
#'@param regional full name of the regional.
#'@param datasource name of the database
#'@return vector with names of the cities.
#'@examples
#'getCidades(regional = "Metropolitana I", uf="Rio de Janeiro",datasource=con)

getCidades <- function(regional, uf, datasource=con){
      
      if(missing(uf)) stop("getCidades requer nome da uf por extenso")
      if(!missing(regional)){
            sqlquery = paste("SELECT municipio_geocodigo, nome, nome_regional, uf 
                  FROM \"Dengue_global\".\"Municipio\" 
                  INNER JOIN \"Dengue_global\".regional_saude
                  ON municipio_geocodigo = geocodigo
                  where uf = '", uf, "' AND nome_regional = '",regional ,"'", sep="")      
      }
      
      if(missing(regional)){
            sqlquery = paste("SELECT municipio_geocodigo, nome, nome_regional, uf 
                  FROM \"Dengue_global\".\"Municipio\" 
                  INNER JOIN \"Dengue_global\".regional_saude
                  ON municipio_geocodigo = geocodigo
                  where uf = '", uf, "'", sep="")      
      }
      
      d = dbGetQuery(datasource, sqlquery)    
      d 
}


# write.parameters ------------------------------------
#'@description  Write the alert parameters for each city into the database, to be used in the update.alert. 
#'Currently, the parameters are:"codigo_estacao_wu", "limiar_preseason", "limiar_posseason",
#'"limiar_epidemico, "estacao_wu_sec".  City must be already in the regionais table.
#'@title City's parameterization. 
#'@param params vector of the names of the params to be inserted in the table. Default: params = c("codigo_estacao_wu", 
#'"limiar_preseason", "limiar_posseason","limiar_epidemico, "estacao_wu_sec"). It can be a subset of the default. 
#'@param tab data.frame with the values for each city. It must have a column named "municipio_geocodigo",
#'plus other columns named as in newpars. 
#'@return nothing.
#'@examples
#'newpars = c("limiar_preseason", "limiar_posseason","estacao_wu_sec")
#'res = write.parameters(newpars,tab)

write.parameters<-function(params, tab, senha){
      
      out = senha#readline("Essa funcao ira mudar o funcionamento do alerta e requer senha do banco de dados:")
      
      conn <- dbConnect(dbDriver("PostgreSQL"), user="dengueadmin",
                       password=out, dbname="dengue")
      
      vars = names(tab)
      nrows = dim(tab)[1]
      
      # check if tab columns are correct
      stopifnot("municipio_geocodigo"%in%vars)
      for(i in params) stopifnot(i%in%vars)
      
      if(nchar(tab$municipio_geocodigo[1]) == 6) {for (i in 1:nrows) 
            tab$municipio_geocodigo[i] <- sevendigitgeocode(tab$municipio_geocodigo[i])}

      # string com vetor de nomes das variaveis
      varnames <- paste("(", params[1], sep="")
      for (j in params[-1]) varnames <- paste(varnames, j, sep=",")
      varnames <- paste(varnames, ")", sep="")
      
      # identificando as variaveis string 
      p1 <- ifelse(any(params == "codigo_estacao_wu"), which(params == "codigo_estacao_wu"), NA)
      p2 <- ifelse(any(params == "estacao_wu_sec"), which(params == "estacao_wu_sec"), NA)
      stringvars = as.vector(na.exclude(c(p1,p2)))           
      
      for (li in 1:nrows){
            linha = ""
            cid = tab$municipio_geocodigo[li]
            
            # check if city is new
            update_sql = paste("SELECT * from \"Dengue_global\".regional_saude SET  
                               WHERE municipio_geocodigo = ",cid,sep="")      
            cityline = try(dbGetQuery(conn, update_sql))
            if (nrow(cityline)==0) {# city not implemented
                  message(paste("geocode", cid, "not implemented in Infodengue. Use insertCityinAlerta()") )
                  next
            }
            
            for (i in 1:length(params)) {
                  if (i %in% stringvars & !is.na(as.character(tab[li,params[i]]))) 
                        value = paste(params[i],"='", as.character(tab[li,params[i]]), "'", sep="")
                        
                  else value = paste(params[i],"=", as.character(tab[li,params[i]]), sep="")
                  
                  linha = ifelse (i>1, paste(linha, value, sep=","), paste(linha, value, sep=""))
            }
            linha = gsub("NA","NULL",linha)
            
            update_sql = paste("UPDATE \"Dengue_global\".regional_saude SET ", linha, 
                               " WHERE municipio_geocodigo = ",cid,sep="")      
            
            try(dbGetQuery(conn, update_sql))
      }
      dbReadTable(conn, c("Dengue_global","regional_saude"))
      dbDisconnect(conn)
}

# read.parameters ------------------------------------
#'@description  Read the alert parameters for a set of  cities from the database, to be used in the update.alert. 
#'Currently, the parameters are:"codigo_estacao_wu", "limiar_preseason", "limiar_posseason",
#'"limiar_epidemico, "estacao_wu_sec".  
#'@title Get city alert parameters. 
#'@param city geocode
#'@param region regional's name
#'@param state state's name
#'@param datasource SQL connection to the database
#'@return dataframe with all parameters
#'@examples
#'read.parameters(city = 3118601, datasource = con)
#'read.parameters(region="Norte", state = "Rio de Janeiro", datasource = con)
#'read.parameters(state = "Rio de Janeiro", datasource = con)
#'read.parameters(datasource = con) #set no filter to get all cities

read.parameters<-function(city, region, state, datasource){
      
      vars  = " geocodigo, nome, nome_regional, id_regional, uf, populacao, codigo_estacao_wu, 
                estacao_wu_sec,limiar_preseason, limiar_posseason, limiar_epidemico,varcli,tcrit,ucrit "

            # Getting metadata from table regional_saude
      if(!missing (city)) { # if updating a single city
            if(nchar(city) == 6) city <- sevendigitgeocode(city) 
            
            
            sql = paste("SELECT ",vars, " FROM \"Dengue_global\".\"Municipio\" 
                        INNER JOIN \"Dengue_global\".regional_saude
                        ON municipio_geocodigo = geocodigo
                        where geocodigo = '", city, "'", sep=" ")
            dd <- dbGetQuery(datasource,sql)
            
      }
      
      if (!missing(region)){ # if one or more regionais
            regionais = region
            sql1 = paste("'", regionais[1], sep = "")
            ns = length(regionais)
            if (ns > 1) for (i in 2:ns) sql1 = paste(sql1, regionais[i], sep = "','")
            sql1 <- paste(sql1, "'", sep = "")
            
            sql = paste("SELECT ", vars, " FROM \"Dengue_global\".\"Municipio\" 
                        INNER JOIN \"Dengue_global\".regional_saude
                        ON municipio_geocodigo = geocodigo
                        where nome_regional IN (",sql1,")",sep="")
            dd <- dbGetQuery(con,sql)
            if (dim(dd)[1]==0) stop(paste("A regional",region, "nao foi achada. 
                                          Verifique se escreveu certo (por extenso)"))
            
            if(length(unique(dd$uf)) > 1){
                  if (missing(state)) stop(cat("Existe mais de uma regional com esse nome. 
                                          Especifique o estado (por extenso):", unique(dd$uf)))
                  dd <- subset(dd, uf == state)
            }
      }
      
      if((missing(city) & missing(region) & !missing(state))){ # state level
            sql = paste("SELECT ",vars, " FROM \"Dengue_global\".\"Municipio\" 
                        INNER JOIN \"Dengue_global\".regional_saude
                        ON municipio_geocodigo = geocodigo
                        where uf = '", state, "'", sep="")
            dd <- dbGetQuery(datasource,sql)
      }
      
      if((missing(city) & missing(region) & missing(state))){
            sql = paste("SELECT ",vars, " FROM \"Dengue_global\".\"Municipio\" 
                        INNER JOIN \"Dengue_global\".regional_saude
                        ON municipio_geocodigo = geocodigo", sep="")
            dd <- dbGetQuery(datasource,sql)
      }
      dd
}
      


# insertCityinAlerta ------------------------------------
#'@description  Initial setup of a new city in the alerta system.  Can be integrated later with 
#'the delay model and write.parameters
#'@title Initial setup of a new city in the alerta system.
#'@param city geocode of the city. Mandatory 
#'@param id_regional numerical id of the 'Regional da saude'
#'@param regional name of the 'Regional da saude'
#'@param estacao_wu_sec main weather station
#'@return to be defined
#'@examples
#'insertCityinAlerta(city=3200300, id_regional=0, regional = "ES-MN-AlfredoChaves")

insertCityinAlerta<-function(city,id_regional,regional,senha){
      
      out = senha #readline("Digite a senha do banco de dados:")
      
      conn <- dbConnect(dbDriver("PostgreSQL"), user="dengueadmin",
                        password=out, dbname="dengue")
      
      # check if city is really new
      if(nchar(city) == 6) city <- sevendigitgeocode(city)   
      consult_sql = paste("SELECT * from \"Dengue_global\".regional_saude SET  
                               WHERE municipio_geocodigo = ",city,sep="")      
      cityline = try(dbGetQuery(conn, consult_sql))
      
      if (nrow(cityline)!=0) {
            message("city already implemented. Nothing done.")
      }
      else{
            el1 = as.character(city)
            el2 = as.character(id_regional)
            el3 = paste("'",regional,"'",sep="")
            linha = paste(el1,el2,el3,sep=",")
            sqlinsert = paste("insert into \"Dengue_global\".\"regional_saude\" (municipio_geocodigo, id_regional, 
                      nome_regional) values(", linha ,")")
            try(dbGetQuery(conn, sqlinsert))   
            cityline = try(dbGetQuery(conn, consult_sql))
      }
      dbDisconnect(conn)
      cityline
}
      
      
     
      


#'lastDBdate(tab="tweet", city=330240)
#'lastDBdate(tab="sinan", city=330240)
#'lastDBdate(tab="clima_wu", station="SBAF")  

