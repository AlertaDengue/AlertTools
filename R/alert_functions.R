# PROJETO ALERTA DENGUE -------------------------------------
# Funcoes auxiliares para o pipeline do Infodengue (em pipe_infodengue.R)
# Claudia Codeco versao 2022
# 

# is_receptive -----------------------------------------------------------------
#'@title Define conditions to issue Yellow alert.
#'@description Yellow is raised when environmental and epidemiological conditions
#' required for transmission are detected.  
#'@export
#'@param d dataset with data to feed the alert, containing the variables specified in crit.
#'only one place at a time.
#'@param crit criteria for receptivity. 
#'Ex: "(inc_3 > 4 & temp_min_1 > 20) | (inc_3 > 0 & temp_min_2 > 30)". Valid variables 
#'are: inc, temp_min, temp_max, umid_min, umid_max, with 0 to 3 time lags.
#'@param values vector with the required variables
#'@param miss how missing data is treated. "last" if last value is repeated. 
#'It is currently the only option
#'@return returns the data, and the receptivity (1 = receptive, 0 = not receptive).  
#'@examples
#' # Parameters of the alert model
#' rule <- "(inc_3 > 4 & temp_min_1 > 20) | (inc_3 > 0 & temp_min_2 > 30)"
#' dat <- data.frame(SE = 201001:201052, inc = rnorm(52, 50, 20), temp_min = rnorm(52, 20, 5),
#' temp_max = rnorm(52, 25, 5), umid_min = rnorm(52, 60, 10), umid_max = rnorm(52, 80, 5))
#' dat <- is_receptive(dat, rule)
#' # Using Alertools data example:
#' data <- AlertTools::dd %>% filter(codigo_regional == 12002)
#' data <- lag_variables(data)
#' crit <- AlertTools::regras
#' data <- data %>% is_receptive(crit = crit$regra[1])
#' table(data$receptivo)

is_receptive <- function(d, crit = rule){
      require("sjmisc")
      require("imputeTS")
      require("quantmod") #lag
      
      #checking input
      vars <- c( "inc_3", 
               "temp_min_1", "temp_min_2", "temp_min_3",
               "temp_max_1", "temp_max_2", "temp_max_3", 
               "umid_min_1", "umid_min_2", "umid_min_3",
               "umid_max_1", "umid_max_2", "umid_max_3")

      assert_that(all(vars %in% names(d)), 
                  msg = "is.receptive says that crit contains unknown variables")      
      
      assert_that(all(c("inc","temp_min","temp_max","umid_min","umid_max") %in%
                            names(d)), 
                  msg = "is.receptive says that d should contain:
                  inc,temp_min,temp_max,umid_min,umid_max")      
      
      # compute receptivity
      d$regra <- crit
      d$receptivo <- NA
      for(i in 1:nrow(d)) {
            d$regra[i] = str_replace_all(d$regra[i], 
                                                c("temp_min_1" = as.character(d$temp_min_1[i]),
                                                   "temp_min_2" = as.character(d$temp_min_2[i]),
                                                   "temp_min_3" = as.character(d$temp_min_3[i]),
                                                   "umid_min_1" = as.character(d$umid_min_1[i]),
                                                   "umid_min_2" = as.character(d$umid_min_2[i]),
                                                   "umid_min_3" = as.character(d$umid_min_3[i]),
                                                   "temp_max_1" = as.character(d$temp_max_1[i]),
                                                   "temp_max_2" = as.character(d$temp_max_2[i]),
                                                   "temp_max_3" = as.character(d$temp_max_3[i]),
                                                   "umid_max_1" = as.character(d$umid_max_1[i]),
                                                   "umid_max_2" = as.character(d$umid_max_2[i]),
                                                   "umid_max_3" = as.character(d$umid_max_3[i]),
                                                   "inc_3" = as.character(d$inc_3[i])))
      d$receptivo[i] = as.numeric(eval(parse(text = d$regra[i])))
      }
      
      # check output
      assert_that(class(d$receptivo) == "numeric", msg = "check is.receptive. output not correct")
      d
}

# count_transmission_weeks -----------------------------------------------------------------
#'@title Counts the number of successive weeks with Rt > 1
#'@description Indicator used for raising the orange alert  
#'@export
#'@param d dataset with data to feed the alert. It requires the previous 
#'computation of Rt.
#'@param alpha significance level for considering Rt > 1 (default = 0.1)
#'@param maxw maximum value for the indicator (default = 30) 
#'@return returns the data, and the number of weeks with transmission.  
#'@examples
#' # Parameters of the alert model (requires connection)
#' casos <- getCases(c(3302205, 3304557), cid10 = "A90", type = "all", 
#' completetail = 0, dataini = "sinpri") 
#' casos <- casos %>% Rt(count = "casos", group = "cidade", 
#' gtdist="normal", meangt=3, sdgt = 1) 
#' casos <- casos %>% count_transmission_weeks()

count_transmission_weeks <- function(d, alpha = 0.1, maxw = 52){
      
      #checking input
      assert_that(all(c("p1") %in% names(d)), 
                  msg = "weeks_w_transmission says that d should contain:
                  p1. Check if Rt was computed")      
    
      cidades <- unique(d$cidade) 
      ncities <- length(cidades)      
      
      res <- list() 
      for(i in 1:ncities){
            # time lag matrix
            dc <- d[d$cidade == cidades[i], ]
            m <- matrix(data = NA, nrow = nrow(dc), ncol = maxw)
            m[,1] <- dc$p1
            for(j in 2:maxw) m[,j] = lag(dc$p1, j)     
            
            dc$weeks_transmission <- rowSums(m > (1 - alpha))
            res[[i]] <- dc
      }
     d <- res %>% bind_rows()
      
      # check output
      assert_that(class(d$weeks_transmission) == "numeric", 
                  msg = "check count_transmission_weeks. output not correct")
      d
}

# weeks_above_threshold -----------------------------------------------------------------
#'@title Counts the number of successive weeks with incidence above a given threshold
#'@description Indicator used for raising the red alert  
#'@export
#'@param d dataset with data to feed the alert. It is recommended the adjustment
#' of incidence first
#'@param threshold incidence threshold. Either a number to be applied to all , or 
#'a data.frame with two columns: geocode, threshold. 
#'@param var variable used for computation (default = inc)
#'@param maxw max number of weeks (default = 52)
#'@param varname name of the output variable. Default : wat
#'@return returns the data, and the number of weeks with transmission.  
#'@examples
#' # Parameters of the alert model (requires connection)
#' mun <- getCidades(uf = "Acre")
#' casos <- getCases(mun$municipio_geocodigo, cid10 = "A90", 
#' dataini = "sinpri", lastday = "2018-05-01") %>% mutate(inc = casos/pop*100000)
#' pars <- read.parameters(mun$municipio_geocodigo)
#' thre <- pars[, c("municipio_geocodigo","limiar_epidemico")]
#' casos <- casos %>% weeks_above_threshold(var = "inc", threshold = thre, varname = "z")
#' boxplot(casos$z ~ casos$cidade, type = "s", main = "weeks_above_threshold")
#' # for the pipeline, we use maxw = 2
#' casos <- casos %>% weeks_above_threshold(var = "casos", maxw = 2)

weeks_above_threshold <- function(d, var = "inc", varname = "wat", 
                                  threshold, maxw = 52){
      require(quantmod)
      #checking input
      assert_that(all(var %in% names(d)), 
                  msg = "weeks_above_threshold says that the incidence variable is missing.")      
      
      n <- ncol(threshold) 
      cidades <- unique(d$cidade) 
      ncities <- length(cidades)      
      
      # check if the input is one threshold per city
      if (n > 1) assert_that(nrow(threshold) == ncities, msg = paste("check threshold argument,
                        size different from the number of cities: thresholds = ", nrow(threshold), 
                        "number of cities = ", ncities, "."))
      # or a single value for all cities
      if(n == 1) threshold <- data.frame(municipio_geocodigo = cidades, 
                                         threshold = threshold)
      res <- list() 
      for(i in 1:ncities){
            dc <- d[d$cidade == cidades[i], ]
            # time lag matrix
            m <- matrix(data = NA, nrow = nrow(dc), ncol = maxw)
            m[,1] <- dc[[var]]
            for(j in 2:maxw) m[,j] = lag(dc[[var]], j)
      
            # compute weeks above threshold
            th <- threshold[threshold$municipio_geocodigo == cidades[i] , 2]
            dc[[varname]] <- rowSums(m > th)
            res[[i]] <- dc
      }
      d <- res %>% bind_rows()
      
       
      # check output
      assert_that(class(d[[varname]]) == "numeric", 
                  msg = "check weeks_above_threshold. output not correct")
      d
}


#tabela_historico --------------------------------------------------------------------
#'@title Convert the alert object into a data.frame and calculate indicators 
#'@description Function to organize the alert results for easy reading and inserting 
#'in the database. Also computes receptivity, transmission and incidence levels.
#'@export
#'@param obj object created by the pipeline.
#'@param ini_se first week of the table. Default is the first date in obj.
#'@param last_se last week of the table. Default is the last date in obj. To do.
#'@param type "notified", if it should return total counts, disregarding the 
#'final classification. For compatibility reasons. 
#'@param versao Default is current's date
#'@return data.frame with the data to be written. 
#'@examples
#'# Several cities at once:
#'cidades <- getCidades(uf = "Mato Grosso", datasource = con)
#'res <- pipe_infodengue(cities = cidades$municipio_geocodigo[1:3], cid10 = "A90", 
#'finalday= "2018-01-10")
#'restab <- tabela_historico(res, iniSE = 201701,type = "all") 
#'tail(restab)
#'# One city:
#'res <- pipe_infodengue(cities = 3304557, cid10 = "A90", 
#'finalday= "2015-01-10")
#'restab <- tabela_historico(res) 
#'tail(restab)

tabela_historico <- function(obj, iniSE, lastSE, type = "notified", versao = Sys.Date()){
      
      # --------- create single data.frame ------------------#
      # if object created by pipe_infodengue():
      if(class(obj)=="list" & class(obj[[1]])=="alerta"){
            data <- transpose(obj)[[1]] %>% bind_rows()   # unlist data
            indices <- transpose(obj)[[2]] %>% bind_rows()  #unlist indices
            
      } else if (class(obj)=="alerta"){ #if object created directly by fouralert()
            data <- obj$data
            indices <- obj$indices
      }
      d <- cbind(data, indices)
      
      # defining the id (SE+julian(versaomodelo)+geocodigo+localidade)
      gera_id <- function(x) paste(data$cidade[x], data$Localidade_id[x], data$SE[x], 
                                   as.character(julian(versao)), sep="")
      d$id <- sapply(1:nrow(data), gera_id) 
      
      
      # ------------removing umid_min ----------------------#
      # just because it is not implemented yet in the dataset
      # if("umid_min" %in% names(d)) d <- subset(d, select = -umid_min)
      
      # ---------- filtering dates -------------------------#
      if(missing(iniSE)) iniSE <- 0
      if(missing(lastSE)) lastSE <- 300000
      
      d <- d %>%
            filter(SE >= iniSE & SE <= lastSE) %>% 
            rename(municipio_geocodigo = cidade,
                   municipio_nome = nome,
                   casos_est = tcasesmed,
                   casos_est_min = tcasesICmin,
                   casos_est_max = tcasesICmax,
                   nivel = level,
                   temp_min = temp_min,
                   umid_max = umid_max) %>%
            mutate(p_rt1 = ifelse(is.na(p1),0,p1),
                   p_inc100k =casos_est/pop*1e5,
                   Localidade_id  = ifelse(is.na(localidade),0,localidade),
                   data_iniSE = SE2date(SE)$ini,
                   versao_modelo = as.character(versao))
      d$Rt[is.na(d$Rt)] <- 0
      
      pars <- read.parameters(d$municipio_geocodigo, cid10 = d$CID10[1])
      
      d <- d %>%   # new stuff
            rename(
                  receptivo = cytrue,  # weeks with receptive conditions
                  transmissao = cotrue)   # weeks with sustained transm
      
      d1 <- d %>%
            left_join(pars[2:5]) %>%
            mutate(  # compating estimated incidence with thresholds
                  nivel_inc = case_when(
                        p_inc100k < limiar_preseason ~ 0,
                        p_inc100k >= limiar_preseason & p_inc100k < limiar_epidemico ~ 1,
                        p_inc100k >= limiar_epidemico ~ 2
                  )
            )
      # --------- checking all required variables ------------#
      varnames <-c("data_iniSE", "SE", "CID10","casos", "casos_est", 
                   "casos_est_min", "casos_est_max", "municipio_geocodigo", 
                   "p_rt1", "p_inc100k", "Localidade_id", "nivel", "id", "versao_modelo", 
                   "municipio_nome", "tweet", "Rt", "pop", "temp_min","temp_med",
                   "temp_max","umid_min","umid_med","umid_max", "receptivo", 
                   "transmissao", "nivel_inc") 
      
      if(type != "notified" & all(c("cas_prov","cas_lab","inc_prov") %in% names(d1))) {
            varnames <-c("data_iniSE", "SE", "CID10","casos", "cas_prov", "cas_lab",
                         "casos_est", "casos_est_min", "casos_est_max", "municipio_geocodigo", 
                         "p_rt1", "p_inc100k", "inc", "Localidade_id", "nivel", "id", "versao_modelo", 
                         "municipio_nome", "tweet", "Rt", "pop", "temp_min","temp_med",
                         "temp_max","umid_min","umid_med","umid_max", "receptivo", 
                         "transmissao", "nivel_inc")
      } else {
             message("probable cases not returned, returning all cases")
      }
       
      if(all(varnames %in% names(d1))) {
         dfinal <- d1[,varnames]
         return(dfinal)
         } else {
         message(paste("historico_alerta is not returning the required variables", 
                       varnames[!varnames %in% names(d1)]))
            return(NULL)
         }
}


#tabela_historico_intra --------------------------------------------------------------------
#'@title Convert the alert object into a data.frame and calculate indicators for cities with subdivisions
#'@description Function to organize the alert results for easy reading and inserting 
#'in the database. Specific for municipalities with subdivisions. 
#'@export
#'@param obj object created by the pipeline.
#'@param ini_se first week of the table. Default is the first date in obj.
#'@param last_se last week of the table. Default is the last date in obj. To do.
#'@param versao Default is current's date
#'@return data.frame with the data to be written. 
#'@examples
#'NOT RUN without connection
#'# Rio de Janeiro
#'alerio <- pipe_infodengue_intra(city = 3304557, datarelatorio=202105, 
#'delaymethod="bayesian", cid10 = "A90", dataini = "sinpri")
#'restab <- tabela_historico_intra(alerio, iniSE = 201801) 
#'tail(restab)

tabela_historico_intra <- function(obj, iniSE, lastSE, versao = Sys.Date()){
      
      assert_that(class(obj) == "alerta_intra", msg = "tabela_historico_mun: obj must be of class alerta_intra.")
      
      # --------- create single data.frame ------------------#
      data <- transpose(obj)[[1]] %>% bind_rows()   # unlist data
      indices <- transpose(obj)[[2]] %>% bind_rows()  #unlist indices
      
      d <- cbind(data, indices)
      
      # defining the id (SE+julian(versaomodelo)+geocodigo+localidade)
      gera_id <- function(x) paste(data$cidade[x], data$Localidade_id[x], data$SE[x], 
                                   as.character(julian(versao)), sep="")
      d$id <- sapply(1:nrow(data), gera_id) 
      
      # ---------- filtering dates -------------------------#
      if(missing(iniSE)) iniSE <- 0
      if(missing(lastSE)) lastSE <- 300000
      
      d <- d %>%
            filter(SE >= iniSE & SE <= lastSE) %>% 
            rename(municipio_geocodigo = cidade,
                   municipio_nome = nome,
                   casos_est = tcasesmed,
                   casos_est_min = tcasesICmin,
                   casos_est_max = tcasesICmax,
                   nivel = level) %>%
            mutate(p_rt1 = ifelse(is.na(p1),0,p1),
                   p_inc100k =casos_est/populacao*1e5,
                   Localidade_id  = ifelse(is.na(localidade),0,localidade),
                   data_iniSE = SE2date(SE)$ini,
                   versao_modelo = as.character(versao))
      d$Rt[is.na(d$Rt)] <- 0
      
      pars <- read.parameters(d$municipio_geocodigo, cid10 = d$CID10[1])
      
      d <- d %>%   # new stuff
            rename(
                  receptivo = cytrue,  # weeks with receptive conditions
                  transmissao = cotrue)   # weeks with sustained transm
      
      d1 <- d %>%
            left_join(pars[2:5]) %>%
            mutate(  # compating estimated incidence with thresholds
                  nivel_inc = case_when(
                        p_inc100k < limiar_preseason ~ 0,
                        p_inc100k >= limiar_preseason & p_inc100k < limiar_epidemico ~ 1,
                        p_inc100k > limiar_epidemico ~ 2
                  )
            )
      
      d1
}


#write_alerta --------------------------------------------------------------------
#'@title Write historico_alerta into the database.
#'@description Function to write the pipeline results into the database. 
#'Receives the object created by the function historico.alerta. If writetofile == TRUE,
#'it saves the sql command in a text file. If FALSE, it will write directly in the database
#'using the connection. 
#'@export
#'@param d object created by tabela_historico()
#'@param writetofile TRUE if an sql object will be the output; FALSE if not.
#'@param arq file name to store sql object 
#'@param datasource posgreSQL conn to project's database
#'@return the same data.frame from the input
#'@examples
#'# Parameters for the model 
#'cidades <- getCidades(regional = "Norte",uf = "Rio de Janeiro",datasource = con)
#'res <- pipe_infodengue(cities = cidades$municipio_geocodigo[1], cid10 = "A90", 
#'finalday= "2016-07-12",nowcasting="none")
#'restab <- tabela_historico(res)
#'# NOT RUN 
#'t1 <- Sys.time()
#'write_alerta(restab[1:10,])
#'t2 <- Sys.time() - t1

write_alerta<-function(d, writetofile = FALSE, datasource = con, arq = "output.sql"){
   
   # check input
   assert_that(class(d) == "data.frame", msg = "write_alerta: d is not a data.frame. d should
                  be an output from tabela_historico.")
   
   if(writetofile == FALSE) assert_that(class(datasource) == "PostgreSQLConnection", msg = "write_alerta: 
                 works only for writing into Infodengue's server")
   
   cid10 = unique(d$CID10)
   assert_that(length(cid10) == 1, msg = "write_alerta: d must contain only one cid10")
   
   dcolumns <- c("SE", "data_iniSE", "casos_est", "casos_est_min", "casos_est_max",
                 "casos","municipio_geocodigo","p_rt1","p_inc100k","Localidade_id",
                 "nivel","id","versao_modelo","municipio_nome","Rt", "pop", "tweet",
                 "receptivo","transmissao","nivel_inc","temp_min","temp_med","temp_max",
                 "umid_min","umid_med","umid_max")
   
   if(!("temp_min" %in% names(d))) d$temp_min <- NA
   if(!("umid_max" %in% names(d))) d$umid_max <- NA
   
   assert_that(all(dcolumns %in% names(d)), msg = "write_alerta: check if d contains required
                                                           columns")
   
   # nomes das tabelas para salvar os historicos:
   if(cid10=="A90") {tabela <-  "Historico_alerta"; constr.unico = "alertas_unicos"}
   if(cid10=="A92.0") {tabela <-  "Historico_alerta_chik"; constr.unico = "alertas_unicos_chik"}
   if(cid10=="A92.8") {tabela <-  "Historico_alerta_zika"; constr.unico = "alertas_unicos_zika"}
   if(!(cid10 %in% c("A90", "A92.0", "A92.8"))) stop(paste("não sei onde salvar histórico para o agravo", cid10))
   
   # ------ vars to write 
   
   dados <- d %>%
      select(all_of(dcolumns))
   
   
   # ------ sql command
   varnamesforsql <- c("\"SE\"", "\"data_iniSE\"", "casos_est", "casos_est_min", "casos_est_max",
                       "casos","municipio_geocodigo","p_rt1","p_inc100k","\"Localidade_id\"",
                       "nivel","id","versao_modelo","municipio_nome", "tweet", "\"Rt\"", "pop",
                       "tempmin","tempmed","tempmax","umidmin","umidmed","umidmax",
                       "receptivo", "transmissao","nivel_inc")
   
   varnames.sql <- str_c(varnamesforsql, collapse = ",")
   updates = str_c(paste(varnamesforsql,"=excluded.",varnamesforsql,sep=""),collapse=",") # excluidos, se duplicado
   
   escreve_linha <- function(li){  # para escrever no sql
      vetor <- dados[li,]
      vetor$municipio_nome = gsub(vetor$municipio_nome, pattern = "'", replacement = "''")
      linha = paste0(vetor$SE,",'",
                     as.character(vetor$data_iniSE), "',", 
                     str_c(vetor[1,c("casos_est","casos_est_min","casos_est_max",
                                     "casos","municipio_geocodigo","p_rt1","p_inc100k","Localidade_id","nivel","id")], collapse=","),",'",
                     as.character(vetor$versao_modelo),"','",
                     as.character(vetor$municipio_nome),"',",
                     str_c(vetor[1,c("tweet","Rt","pop","temp_min","temp_med","temp_max","umid_min","umid_med","umid_max")], collapse = ","), ",",
                     str_c(vetor[1,c("receptivo","transmissao","nivel_inc")], collapse = ",")
      )
      
      #if("temp_min" %in% names(vetor)) linha = paste0(linha,",", vetor$temp_min, ",","NA")
      #if("umid_max" %in% names(vetor)) linha = paste0(linha,",", "NA", ",", vetor$umid_max)
      linha = gsub("NA","NULL",linha)
      linha = gsub("NaN","NULL",linha)
      
      
      insert_sql = paste("INSERT INTO \"Municipio\".\"",tabela,"\" (" ,varnames.sql,") VALUES (", linha, ") 
                                    ON CONFLICT ON CONSTRAINT ",constr.unico,"  
                                     DO UPDATE SET ",updates, ";",sep="")
      
      if(writetofile == FALSE) {
         try(dbGetQuery(datasource, insert_sql))    
      }
      
      insert_sql
   }
   
   # escrevendo no sql
   
   
   if(writetofile){
      f <- file(arq,open="w",encoding="utf8")
      for(i in 1:nrow(d)) writeLines(escreve_linha(i), f)
      message("writing alerta into file ", arq)
      close(f)
      
   } else{
      print(paste("writing alerta into table", tabela))
      try(dbGetQuery(datasource, "BEGIN TRANSACTION;"))  ##  start a transaction 
      
      1:nrow(d) %>% map(escreve_linha)  ## the  sql inserts will only be processed after the end of the transaction 
      
      try(dbGetQuery(datasource, "COMMIT TRANSACTION;")) ## finish the transaction and insert the lines 
      ## in case of failure is possible to roll back (undo) 
      ## ROLLBACK TRANSACTION;
   } 
   
}




#write_alerta_local --------------------------------------------------------------------
#'@title Write historico_alerta into the local SQLite database.
#'@description Function to write the output of the pipeline into the local SQLite database. 
#'The input must be the object created by the function historico.alerta.
#'@export
#'@param d object created by tabela_historico()
#'@param datasource SQLite conn 
#'@return the same data.frame from the input
#'@examples
#'NOT USE: con <- dbConnect(RSQLite::SQLite(), "../../AlertaDengueAnalise/mydengue.sqlite")
#'# Parameters for the model 
#'cidades <- getCidades(regional = "Norte",uf = "Rio de Janeiro",datasource = con)
#'res <- pipe_infodengue(cities = cidades$municipio_geocodigo, cid10 = "A90", 
#'finalday= "2021-07-12",nowcasting="none", dataini = "sinpri")
#'restab <- tabela_historico(res)
#'# NOT RUN 
#'t1 <- Sys.time()
#'write_alerta_local(restab)
#'t2 <- Sys.time() - t1
write_alerta_local <- function(d, datasource = con){
   
   assert_that(class(datasource) == "SQLiteConnection", msg = "write_alerta_local: 
                 works only for writing into local SQLite server")
   
   # check input
   assert_that(class(d) == "data.frame", msg = "write_alerta_local: d is not a data.frame. d should
                  be an output from tabela_historico.")
   
   
   dcolumns <- c("SE", "data_iniSE", "casos_est", "casos_est_min", "casos_est_max",
                 "casos","municipio_geocodigo","p_rt1","p_inc100k","Localidade_id",
                 "nivel","id","versao_modelo","municipio_nome","Rt", "pop", "tweet",
                 "receptivo","transmissao","nivel_inc","temp_min","umid_max")
   
   if(!("temp_min" %in% names(d))) d$temp_min <- NA
   if(!("umid_max" %in% names(d))) d$umid_max <- NA
   
   
   assert_that(all(dcolumns %in% names(d)), msg = "write_alerta: check if d contains required
                                                           columns")
   
   # ------ vars to write 
   
   dados <- d %>%
      select(all_of(dcolumns)) %>%
      rename(temp_min = temp_min,
             umid_max = umid_max)
   
   # ---------- which table? 
   cid10 <- d$CID10[1]
   muns <- unique(dados$municipio_geocodigo)
   
   # deleting all recent records
   minSE <- min(dados$SE)

   if(cid10 == "A90") {
      rs <- dbSendStatement(datasource, 
                                            'DELETE FROM \"Historico_alerta\" 
                                            WHERE SE >= :s AND
                                            [municipio_geocodigo] == $m')
   
      dbBind(rs, params = list(s = rep(minSE, length(muns)), m = muns))
      dbGetRowsAffected(rs)
      dbClearResult(rs)
   
      # writing new data
      dbWriteTable(datasource, name = "Historico_alerta", dados, append = TRUE)
      message("tabela historico_alerta atualizada localmente para dengue")
   }
   
   if(cid10 == "A92.0") {
      rs <- dbSendStatement(datasource, 
                            'DELETE FROM \"Historico_alerta_chik\" 
                                            WHERE SE >= :s AND
                                            [municipio_geocodigo] == $m')
      
      dbBind(rs, params = list(s = rep(minSE, length(muns)), m = muns))
      dbGetRowsAffected(rs)
      dbClearResult(rs)
      
      # writing new data
      dbWriteTable(datasource, name = "Historico_alerta_chik", dados, append = TRUE)
      message("tabela historico_alerta atualizada localmente para chik")
   }
   
   if(cid10 == "A92.8") {
      rs <- dbSendStatement(datasource, 
                            'DELETE FROM \"Historico_alerta_zika\" 
                                            WHERE SE >= :s AND
                                            [municipio_geocodigo] == $m')
      
      dbBind(rs, params = list(s = rep(minSE, length(muns)), m = muns))
      dbGetRowsAffected(rs)
      dbClearResult(rs)
      
      # writing new data
      dbWriteTable(datasource, name = "Historico_alerta_zika", dados, append = TRUE)
      message("tabela historico_alerta atualizada localmente para zika")
   }
   
  d
}


#write_alertaRio --------------------------------------------------------------------
#'@title Write the Rio de janeiro alert into the database.
#'@description Function to write the alert results into the database. 
#'@export
#'@param obj object created by the tabela_historico_intra function and contains 
#' alerts for each locality.
#'@param datasource connection to infodengue database
#'@return data.frame with the data to be written. 
#'@examples
#'alerio <- pipe_infodengue_intra(city = 3304557, datarelatorio=202105, 
#'delaymethod="bayesian", cid10 = "A90", dataini = "sinpri")
#'restab <- tabela_historico_intra(alerio, iniSE = 201801) 
#'write_alertaRio(restab[1:10,])

write_alertaRio<-function(obj, datasource = con){
      
      # -------  check input
      assert_that(class(obj) == "data.frame", msg = "write_alertaRio: obj should
                  be an output from tabela_historico_intra. ")
      
      
      listaAPS <- c("APS 1", "APS 2.1", "APS 2.2", "APS 3.1", "APS 3.2", "APS 3.3"
                    , "APS 4", "APS 5.1", "APS 5.2", "APS 5.3")
      APSlabel <- c("1.0", "2.1", "2.2", "3.1", "3.2", "3.3","4.0","5.1","5.2","5.3")
      cid10 <- obj$CID10[1]
      
      # ------- columns to write 
      dcolumns <- c("se","aps","data","tweets","casos","casos_est","casos_estmin","casos_estmax",
                    "tmin","rt","prt1","inc","nivel")
      
      # ------- preparing data.frame
      d <- obj %>%
            rename(se = SE,
                   casos_estmin = casos_est_min,
                   casos_estmax = casos_est_max,
                   tmin = temp_min,
                   tweets = tweet,
                   rt = Rt,
                   prt1 = p_rt1) %>%
            mutate(data = SE2date(se)$ini,
                   aps = APSlabel[(localidadeid+1)],
                   prt1 = replace_na(prt1, 0)) %>%
            select(all_of(dcolumns)) %>%
            arrange(se) 
      
      # ----- sql command        
      # setting table and constraint  
      if (cid10 == "A90") {tabela <- "alerta_mrj"; sqlconstr = "unique_aps_se"}
      if (cid10 == "A92.0") {tabela <- "alerta_mrj_chik"; sqlconstr = "unique_chik_aps_se"}
      
      varnames <- "(se,aps,data,tweets,casos,casos_est,casos_estmin,casos_estmax,tmin,rt,prt1,
                        inc,nivel)"
      varnames.sql <- str_c(dcolumns, collapse = ",")
      updates = str_c(paste(dcolumns,"=excluded.",dcolumns,sep=""),collapse=",") # excluidos, se duplicado
      
      escreve_linha <- function(li){
            vetor <- d[li,]
            linha = paste(vetor[1,1],",'",as.character(vetor[1,2]),"','", 
                          as.character(vetor[1,3]),"',", 
                          str_c(vetor[1,4:13], collapse=","), sep="")
            linha = gsub("NA","NULL",linha)
            linha = gsub("NaN","NULL",linha)
            insert_sql2 = paste("INSERT INTO \"Municipio\".", tabela, " ", varnames, 
                                " VALUES (", linha, ") ON CONFLICT ON CONSTRAINT ", sqlconstr, 
                                " DO UPDATE SET ",updates, sep="")
            try(dbGetQuery(datasource, insert_sql2))    
            
      }
      # escrevendo
      
      try(dbGetQuery(datasource, "BEGIN TRANSACTION;"))  ##  start a transaction 
      
      1:nrow(d) %>% map(escreve_linha)
      
      try(dbGetQuery(datasource, "COMMIT TRANSACTION;")) 
      
      #refresh_sql = "REFRESH MATERIALIZED VIEW uf_total_view;"
      #try(dbGetQuery(datasource, refresh_sql))
      
      message(paste("dados escritos na tabela", tabela))
}


