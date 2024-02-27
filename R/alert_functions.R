# PROJETO ALERTA DENGUE -------------------------------------
# Funcoes de calculo do alerta 
# Claudia Codeco 2015
# 


#setCriteria -------------------------------------------------------------------
#'@title Define rules to issue a four level alert Green-Yellow-Orange-Red.
#'@description The criteria for transition between colors (alert levels) can be 
#'chosen from existing rules or can be specified by the user. The built in rules are: 
#'Af (minimum temperature defines yellow) and Aw (humidity does), AsAw (temp min and max humidity together).  
#'@export 
#'@param rule either a built-in rule ("Af", "Aw", "AsAw","Awi","AsAwi") or a list with three elements defining criteria for 
#'transition to yellow (level 2), orange (level 3) and red (level 4). See description.
#'@param delays list with three elements, each one is a vector: c(delay to turn on, delay to turn off)
#'@param values named vector of values for the critical parameters. Use character.   
#'@return list with rules. To be useful, this list must contain variables that match those in the data.  
#'@examples
#'Defining values manually
#'val <- c("varcli" ="temp_min", "limiar_preseason"="10","limiar_epidemico"="100", "clicrit"="22"
#', "clicrit2" = "80", varcli2 = "umid_max")
#'setCriteria(rule="Af",values=val)
#'setCriteria(rule="AsAw",values=val)
#'Using infodengue parameters:
#'val <- read.parameters(1200401)
#'setCriteria(rule=val$codmodelo, values=val)

setCriteria <- function(rule=NULL, values=NULL, 
                        delays = list(delayy = c(0,0), delayo = c(0,0), delayr = c(0,0))){
      
      # checking input
      if(!is.null(rule)) assert_that(rule %in% c("Af","Aw","AsAw","Awi","AsAwi"),
                                     msg = "setcriteria: rule unknown. Try Af, Aw, Awi, AsAw or AsAwi")
      if(is.null(rule)) {
            assert_that(!is.null(values), msg = "setcriteria: if rule is null, values must be provided.")
            assert_that(any(names(values) %in% c("varcli", "clicrit", "limiar_preseason",
                                                 "limiar_epidemico")), msg = "setcriteria: elements missing from arg values")
      }
      
      
      # pre-defined rules
      if(!is.null(rule)){
            
            if(rule[1] == "Af"){
                  criteria <- list(
                        crity = c("temp_min > temp_crit & inc > 0", 3, 0), #3,2
                        crito = c("p1 > 0.95 & inc > limiar_preseason", 2, 1), #3,2
                        critr = c("inc > limiar_epidemico & casos > 10", 2, 1) #2,2
                  )} 
            if (rule[1] == "Aw"){
                  criteria = list(
                        crity = c("umid_max > umid_crit & inc > 0", 3, 0), #3,2
                        crito = c("p1 > 0.95 & inc > limiar_preseason", 2, 0), #3,2
                        critr = c("inc > limiar_epidemico & casos > 10", 2, 0) #2,2
                  )}
         if (rule[1] == "Awi"){
            criteria = list(
               crity = c("umid_min > umid_crit & inc > 0", 3, 0), #3,2
               crito = c("p1 > 0.95 & inc > limiar_preseason", 2, 0), #3,2
               critr = c("inc > limiar_epidemico & casos > 10", 2, 0) #2,2
            )}
         if(rule[1] == "AsAw"){
                  criteria = list(
                        crity = c("temp_min > temp_crit & umid_max > umid_crit & inc > 0", 3, 0), #3,2
                        crito = c("p1 > 0.95 & inc > limiar_preseason", 3, 0), #3,2
                        critr = c("inc > limiar_epidemico & casos > 10", 2, 0) #2,2
                  )}
         if(rule[1] == "AsAwi"){
            criteria = list(
               crity = c("temp_min > temp_crit & umid_min > umid_crit & inc > 0", 3, 0), #3,2
               crito = c("p1 > 0.95 & inc > limiar_preseason", 3, 0), #3,2
               critr = c("inc > limiar_epidemico & casos > 10", 2, 0) #2,2
            )}
            # user defined rules      
      } else {  
         message("setcriteria function using user defined criteria")
            criteria<-lapply(1:3, function(x) c(rule[[x]], delays[[x]]))
            names(criteria) <- c("crity","crito","critr")
      }
      
      # substituting values (very bad coding, should be improved)
      if(!is.null(values)) {  #used in the pipeline
            if (!("varcli2" %in% names(values))) values[["varcli2"]] <- "xx"
            
            if (rule[1] %in% c("Af", "AsAw","AsAwi")){ # reading temp
                  assert_that(values[["varcli"]] == "temp_min" | values[["varcli2"]] == "temp_min",
                              msg = "setcriteria: Af, AsAw and AsAwi require temp_min")
                  tm <- names(values)[which(values == "temp_min")]
                  if(tm == "varcli")  values <- c(values, "temp_crit" = values[["clicrit"]])
                  if(tm == "varcli2") values[["temp_crit"]] <- c(values, "temp_crit" = values[["clicrit2"]])
            }
            
            if (rule[1] %in% c("Aw", "AsAw")){ # reading umidmax
                  assert_that(values[["varcli"]] == "umid_max" | values[["varcli2"]] == "umid_max",
                              msg = "setcriteria: Aw and AsAw require umid_max")
                  um <- names(values)[which(values == "umid_max")]
                  if(um == "varcli")   values <- c(values, "umid_crit" = values[["clicrit"]])
                  if(um == "varcli2")  values <- c(values, "umid_crit" = values[["clicrit2"]])
            }
            
            if (rule[1] %in% c("Awi", "AsAwi")){ # reading umidmin
               assert_that(values[["varcli"]] == "umid_min" | values[["varcli2"]] == "umid_min",
                           msg = "setcriteria: Awi and AsAwi require umid_min")
               um <- names(values)[which(values == "umid_min")]
               if(um == "varcli")   values <- c(values, "umid_crit" = values[["clicrit"]])
               if(um == "varcli2")  values <- c(values, "umid_crit" = values[["clicrit2"]])
            }
            
            #if(class(values) == "data.frame") 
            #      { # handling values from read.parameters
            values <- unlist(sapply(names(values),function(x) values[[x]])) 
            #      }
            
            
            criteria <- lapply(criteria, function(x) c(str_replace_all(x[1], values), x[c(2,3)]))
      }
      
      criteria
}


#fouralert ---------------------------------------------------------------------
#'@title Define conditions to issue a four level alert Green-Yellow-Orange-Red.
#'@description Yellow is raised when environmental conditions required for
#'positive mosquito population growth are detected, green otherwise.Orange 
#'indicates evidence of sustained transmission, red indicates evidence of 
#'an epidemic scenario.  
#'@export
#'@param obj dataset with data to feed the alert, containing the variables specified in crit.
#'@param crit criteria for the alert colors. See setCriteria()
#'@param dy if inc > 0, and rt was orange or red at least once in the past 
#'dy weeks -> level yellow. Default: dy=4 
#'@param miss how missing data is treated. "last" if last value is repeated. 
#'It is currently the only option
#'@return returns an object of class "alerta" containing four elements: the data, 
#'the alert indices, and the rules used to define the indices.  
#'@examples
#' # Parameters of the alert model
#'val = c(varcli ="temp_min", "clicrit"="22","limiar_preseason"="10","limiar_epidemico"="100")
#'criteria = setCriteria(rule="Af",values=val)
#'# Get, organize data 
#'cas = getCases(cities = 3200300, cid10 = "A90") %>% 
#'      Rt(count = "casos",gtdist="normal", meangt=3, sdgt = 1) %>%
#'      mutate(inc = casos/pop*100000)
#'cli = getWU(stations = 'SBGL', vars="temp_min") %>%
#'      mutate(temp_min = nafill(temp_min, rule = "arima"))
#'tw = getTweet(cities = 3200300)
#'# Calculate alert      
#'ale <- plyr::join_all(list(cas,cli,tw), by="SE") 
#'resf <- fouralert(ale, crit = criteria)


fouralert <- function(obj, crit, miss="last",dy=4){
      
      # checking input
      assert_that(all(names(crit) %in% c("crity", "crito", "critr")) &
                        all(sapply(crit,length) %in% 3),
                  msg = "fouralert: argument crit is mispecified")
      
      
      # criteria
      #cyellow = crit[[1]]; corange = crit[[2]]; cred = crit[[3]]
      parsed_rules <- lapply(crit, function(x) parse(text=x[1]))
      delay_turnon <- lapply(crit, function(x) as.numeric(x[[2]]))
      delay_turnoff <- lapply(crit, function(x) as.numeric(x[[3]]))
      
      # checking delays 
      #assert_that(all(sapply(c(delay_turnon, delay_turnoff), is.count)),
      #            msg = "fouralert: delays are mispecified")
      
      # fun to accumulate conditions 
      accumcond <- function(vec,lag){
            if (lag ==1 )return(vec)
            zoo::rollapply(vec, lag, sum, align = "right", fill = NA)
      }
      
      # fun assert condition (week and accumulated) 
      assertcondition <- function(dd, nivel){
            condtrue <- with(dd, as.numeric(eval(parsed_rules[[nivel]])))
            mi <- which(is.na(condtrue)) # missing conditions
            if (miss == "last"){  
                  #if(le %in% mi) message("missing condition, repeating last value")
                  for (i in mi[mi!=1]) condtrue[i] <- condtrue[i-1]
            }
            # counting accumulated conditions
            ncondtrue <- accumcond(condtrue, delay_turnon[[nivel]])
            cbind(condtrue, ncondtrue)
      }
      # 
      le <- nrow(obj)
      
      indices <- data.frame(cbind(assertcondition(obj, 1),
                                  assertcondition(obj, 2),
                                  assertcondition(obj, 3)))
      names(indices) <- c("cytrue", "nytrue","cotrue", "notrue","crtrue", "nrtrue")
      
      # setting the alert level when delay_on is reached(1 = green, 2 = yellow, 3 = orange, 4 = red)
      indices$level <- 1
      indices$level[indices$nytrue == delay_turnon[1]] <-2
      indices$level[indices$notrue == delay_turnon[2]] <-3
      indices$level[indices$nrtrue == delay_turnon[3]] <-4
      
      
      
      # delayed turnoff
      delayturnoff <- function(level){
            delay_level = delay_turnoff[[(level-1)]]# as.numeric(as.character(cond[3])) 
            
            ifelse (delay_level == 0, return(indices),
                    {pos <- which(indices$level==level) %>% # weeks with alert at level delay_level
                          lapply(.,function(x)x+seq(0,delay_level)) %>% # current and subsequent weeks 
                          unlist() %>% 
                          unique()
                    pos <- pos[pos<=le] # remove inexisting rows
                    indices$level[pos] <- level #unlist(lapply(indices$level[pos], function(x) max(x,2)))
                    return(indices)
                    })
      }
      
      indices <- delayturnoff(level=4)
      indices <- delayturnoff(level=3)
      indices <- delayturnoff(level=2)
      
      # from orange-red to yellow:
      # if rt was orange or red at least once in the past dy weeks -> level yellow
      #contains_34 <- which(zoo::rollapply(indices$level,list(c(-dy:0)),
      #                                    function(x) any(x>=3), fill=NA))
      
      # to visualize how it works, descomment the following lines
      #indices$dy <- NA
      #indices$dy[contains_34]<-pmax(indices$level[contains_34], rep(2, length(contains_34)))
      
      #indices$level[contains_34]<-pmax(indices$level[contains_34], 
      #                                 rep(2, length(contains_34)))
      
      ale <- list(data=obj, indices=indices, crit = crit, n=4)
      class(ale)<-"alerta" 
      return(ale)      
}

#pipe_infodengue ---------------------------------------------------------------------
#'@title pipeline used by infodengue 
#'@description wrap of functions used by Infodengue.
#'@export
#'@param cities In general, a vector of 7-digit geocodes. If it is a data.frame containing geocodes
#' and all parameters, these will replace the database's parameters. 
#'@param cid10 default is A90 (dengue). Chik = A92.0, Zika = A92.8
#'@param narule how to treat missing climate data. Do nothing (default), "zero" fills 
#'with zeros, "linear" for linear interpolation, "arima" for inter and extrapolation.
#'@param finalday if provided, uses only disease data reported up to that day
#'@param iniSE first date of the disease data. Default = 201501. Minimum = 201001. 
#'@param datarelatorio epidemiological week
#'@param nowcasting  "fixedprob" for static model, "bayesian" for the dynamic model.
#'"none" for not doing nowcast (default) 
#'@param completetail if sinan data is older than final_day, fill in the tail with NA (default) or 0.  
#'@param dataini "notif" (default) or "sinpri" 
#'@param writedb TRUE if it should write into the database, default is FALSE.
#'@param datasource posgreSQL connection to project's database
#'@return data.frame with the week condition and the number of weeks within the 
#'last lag weeks with conditions = TRUE.
#'@examples
#'cidades <- getCidades(uf = "Amapá",datasource = con)
#'res <- pipe_infodengue(cities = cidades$municipio_geocodigo[13:15] , cid10 = "A90",
#'nowcasting="none", dataini= "sinpri", completetail = 0, datarelatorio = 202124)
#'tail(tabela_historico(res))
#'res <- pipe_infodengue(cities = 4209102 , cid10 = "A90",
                       #'nowcasting="bayesian", dataini= "sinpri", completetail = 0,
                       #' datarelatorio = 202345)
#'# User's parameters (not working)
#'dd <- read.parameters(cities = c(3200300)) %>% mutate(limiar_epidemico = 100)
#'res <- pipe_infodengue(cities = dd, cid10 = "A90", 
#'finalday= "2018-08-12",nowcasting="none")
#'restab <- tabela_historico(res)

pipe_infodengue <- function(cities, cid10="A90", datarelatorio, finalday = Sys.Date(), 
                            iniSE = 201001, nowcasting="none", narule=NULL,
                            writedb = FALSE, datasource = con, userinput =FALSE,
                            completetail = NA, dataini = "notific"){
      
      
      if(missing(datarelatorio)) {
            datarelatorio <- data2SE(finalday, format = "%Y-%m-%d") 
      } else { # if datarelatorio & finalday are given, priority is datarelatorio
            finalday <- SE2date(datarelatorio)$ini+6
      }
      
      # check dates
      #last_sinan_date <- lastDBdate(tab = "sinan", cid10 = cid10, cities = cities)
      #print(paste("last sinan date for",cid10 ,"is", last_sinan_date$se))
      
      #assert_that(!is.na(last_sinan_date$se), msg = paste("no sinan data for cid10", cid10)) 
      
      #if(last_sinan_date$se < datarelatorio) {
      #      
      #      if (userinput){
      #            message(paste("last date in database is",last_sinan_date$se,
      #                          ". Should I continue with SE =", datarelatorio,
      #                          "? tecle Y if YES, or change to new date"))
      #            x <- scan("stdin", character(), n = 1)
      #             if(x!="Y") { 
      #                   datarelatorio <- as.numeric(x)   
      #                   } else {completetail <- 0} # complete the tail with zeros
      #      } 
      #}
      
      # If cities is a vector of geocodes, the pipeline reads the parameters from the dataframe
      if (class(cities) %in% c("integer","numeric")) {
            pars_table <- read.parameters(cities = cities, cid10 = cid10)
            message("reading parameters from database")
      } else { # if city contains data already
            if(all(c("municipio_geocodigo","limiar_preseason",
                     "limiar_posseason","limiar_epidemico",
                     "varcli","clicrit","varcli2","clicrit2","cid10","codmodelo") %in% names(cities))) {
                  message("using user's provided parameters")
                  
                  pars_table <- cities
            }
            else({message("don't know how to run the pipeline for these inputs")
                  return(NULL)}
            )
      }
      
      # number of cities 
      nlugares <- nrow(pars_table)
      cidades <- pars_table$municipio_geocodigo
      print(paste("sera'feita analise de",nlugares,"cidade(s):"))
      print(cidades)      
      
      
      estacoes_cidades <- getWUstation(cidades)
      estacoes <- na.omit(unique(c(estacoes_cidades$estacao_wu_sec, estacoes_cidades$codigo_estacao_wu)))
      print("usando dados de clima das estacoes:")
      print(estacoes)
      
      # Reading the meteorological data
      #print('Obtendo os dados de clima...')
      #varscli <- na.omit(unique(c(pars_table$varcli, pars_table$varcli2)))
      varscli <- c("umid_max", "temp_min", "umid_min","umid_med","temp_med","temp_max")
      cliwu <- getWU(stations = estacoes, vars = varscli, finalday = finalday)
      
      # Reading Cases
      print("Obtendo dados de notificacao ...")
      print(cities)
      casos <- getCases(cities, lastday = finalday, cid10 = cid10, type = "all", # novo
                        dataini = dataini, completetail = completetail) 
      message("getCases done")
      casos$inc <- casos$casos/casos$pop*100000
      casos$inc_prov <- casos$cas_prov/casos$pop*100000 # novo
      
      # Reading tweets 
      if(cid10 == "A90"){
            print("Reading tweets...")
            dT = getTweet(cities, lastday = finalday, cid10 = "A90")
      }
      
      
      # para cada cidade ...
      
      ## FUN  calc.alerta (internal)
      calc.alerta <- function(x){  #x = cities[i]
         message(x)
            # params
            parcli.x <- estacoes_cidades[estacoes_cidades$municipio_geocodigo == x, c("codigo_estacao_wu", "estacao_wu_sec")]      
            varcli.x <- na.omit(c(pars_table$varcli[pars_table$municipio_geocodigo == x],
                                  pars_table$varcli2[pars_table$municipio_geocodigo == x]))
            if("NA" %in% varcli.x) varcli.x <- varcli.x[varcli.x!="NA"]
            # escolhe a melhor serie meteorologica para a cidade, usando apenas a primeira var 
            cli.x <- bestWU(series = list(cliwu[cliwu$estacao == parcli.x[[1]],],
                                          cliwu[cliwu$estacao == parcli.x[[2]],]), var = varcli.x[1])
            
            # se nenhuma estacao tiver dados (cli.x = NULL), 
            propNA <- sum(is.na(cli.x[,varcli.x[1]]))/nrow(cli.x[1])
            
            lastdatewu <- ifelse(propNA < 1, cli.x$SE[max(which(is.na(cli.x[,varcli.x[1]])==FALSE))],
                                 NA)
            print(paste("Rodando alerta para ", x, "usando estacao", cli.x$estacao[1],
                        "(ultima leitura:", lastdatewu,")"))
            
            # climate data interpolation using arima (if there is any data) 
            if(!is.null(narule) & !is.na(lastdatewu)){
                  cli.x[,varcli.x[1]] <- nafill(cli.x[,varcli.x[1]], rule = narule) 
                  if(length(varcli.x) == 2) cli.x[,varcli.x[2]] <- nafill(cli.x[,varcli.x[2]], rule = narule) 
            } 
            
            # casos + nowcasting + Rt + incidencia 
            
            cas.x <- casos %>% 
                  filter(cidade == x) %>%
                  adjustIncidence(method = "none",  
                                  nowSE = datarelatorio, 
                                  nyears = 1) 
             
            if(nowcasting != "none"){  # handling errors in bayesian nowcast
               try(cas.x <- casos %>% 
                      filter(cidade == x) %>%
                      adjustIncidence(method = nowcasting,  
                                      nowSE = datarelatorio, 
                                      nyears = 1)) 
            }
                        
            cas.x <- cas.x %>%
               Rt(count = "tcasesmed",gtdist="normal", meangt=3, sdgt = 1) %>%
               mutate(inc = tcasesmed/pop*100000)  %>%
               arrange(SE)
            
            # Joining all data
            ale <- cas.x %>% 
                  left_join(cli.x, by = c("SE"))
            
            if(exists("dT")) {
                  dt.x <- dT[dT$Municipio_geocodigo == x, c("SE", "tweet")]
                  if(nrow(dt.x) > 0){
                        ale <- merge(ale, dt.x, by = "SE",all.x = T)      
                  } else ale$tweet <- 0
            } else{
                  ale$tweet <- 0
            }
            
            assert_that(nrow(ale)>0, msg = "check alertapipeline. error makes nrow(ale) = 0")
            
            # build rules
            crit.x <- pars_table[pars_table$municipio_geocodigo==x,] # parameters
            crit.x.vector <- structure(as.character(crit.x), names = as.character(names(crit.x))) # dataframe -> vector
            criteriaU <- setCriteria(rule = crit.x$codmodelo, values = crit.x.vector) # valued criteria
            
            # Apply alert rules
            y <- fouralert(ale, crit = criteriaU)  # apply alert 
            y     
      }      
      
      res <- lapply(cidades, calc.alerta) %>% setNames(cidades) # antes o nome era character, agora e o geocodigo
      #       nick <- gsub(" ", "", nome, fixed = TRUE)
      #       #names(alerta) <- nick
      
      if (writedb == TRUE) write_alerta(alerta)
      
      res
}



#pipe_infodengue_intra ---------------------------------------------------------------------
#'@title 4 level alert Green-Yellow-Orange-Red for partitions within municipalities.
#'@description Yellow is raised when environmental conditions required for
#'positive mosquito population growth are detected, green otherwise.Orange 
#'indicates evidence of sustained transmission, red indicates evidence of 
#'an epidemic scenario.  
#'@export
#'@param city geocode (one city at a time). Currently only works for Rio.
#'@param locs subset of localities. Currently not used.  
#'@param datarelatorio last epidemiological week (format = 201401) 
#'@param iniSE first epidemiological week
#'@param cid10 default is A90 (dengue). Chik = A920.
#'@param narule how to fill climate missing data (arima is the only option)
#'@param delaymethod "none" (Default) or "bayesian"
#'@param dataini "sinpri" or "notif". Default = "sinpri" 
#'@param datasource name of the sql connection.
#'@return list with an alert object for each APS.
#'@examples
#'alerio <- pipe_infodengue_intra(city = 3304557, datarelatorio=202113, 
#'delaymethod="none", cid10 = "A90", dataini = "sinpri")
#'ale.chik <- pipe_infodengue_intra(city = 3304557, datarelatorio = 202108, 
#'iniSE = 201001, cid10 = "A920", dataini = "sinpri", delaymethod = "bayesian")

pipe_infodengue_intra <- function(city, locs, datarelatorio, cid10 = "A90", 
                                  iniSE = 201001, delaymethod = "none", 
                                  narule="arima", finalday = Sys.Date(),
                                  dataini = "sinpri", datasource=con){
      
      assert_that(narule == "arima", msg = "alerta_intra: arima is the only na fill method available")
      assert_that(cid10 %in% c("A90","A920","A92.8"), msg = "alerta_intra: check cid10" )
      assert_that(length(city) == 1, msg = "alerta_intra: works for one city at a time.")
      
      if(missing(datarelatorio)) {
            datarelatorio <- data2SE(finalday, format = "%Y-%m-%d") 
      } else { # if datarelatorio & finalday are given, priority is datarelatorio
            finalday <- SE2date(datarelatorio)$ini+6
      }
      
      # getting parameters and criteria
      crit.x <- read.parameters(city, cid10 = cid10)
      crit.x.vector <- structure(as.character(crit.x), names = as.character(names(crit.x))) # dataframe -> vector
      crit <- setCriteria(rule = "Af", values = crit.x.vector)
      
      # reading data
      message("lendo dados ...")
      
      cas = getCasesinRio(APSid = 0:9, cid10 = cid10,dataini = dataini,lastday = finalday)  ## need to change to include other cities
      cli = getWU(stations = c('SBRJ',"SBJR","SBGL"), vars = "temp_min",finalday = finalday) # too
      message("cli")
      if(cid10 == "A90") {
            tw <- getTweet(cities = city, cid10="A90",lastday = finalday)[,c("SE","tweet")]
      }else {
            tw <- data.frame(SE = seqSE(from = min(cas$SE), to = max(cas$SE))$SE,
                             tweet = NA)
      }
      message("tw")
      # nowcasting and Rt parameters 
      if(cid10=="A90") meangt = 3 # 3 weeks
      if(cid10=="A920") meangt = 2 # 2 weeks
      
      # output object # too
      APS <- c("APS 1", "APS 2.1", "APS 2.2", "APS 3.1", "APS 3.2", "APS 3.3"
               , "APS 4", "APS 5.1", "APS 5.2", "APS 5.3")
      res <- vector("list", length(APS))
      names(res) <- APS
      
      # Function to calculate alert per aps
      message("calculando...")
      calc.alertaintra <- function(aps){
            print(aps)
            cli.aps <- cli %>%
                  filter(estacao == 
                               case_when(
                                     aps %in% 0:2 ~ "SBRJ",
                                     aps %in% 3:5 ~ "SBGL",
                                     TRUE ~  "SBJR"
                               )
                  )
            
            d.aps <- cas %>%
                  filter(localidadeid == aps) %>%
                  filter(SE >= iniSE) %>%
                  arrange(SE) %>%
                  adjustIncidence(method = delaymethod, nowSE = datarelatorio, nyears = 1, pdig = p) %>%
                  Rt(count = "tcasesmed",gtdist="normal", meangt= meangt, sdgt = 1) %>%
                  mutate(inc = tcasesmed/populacao*100000) %>%
                  full_join(cli.aps, by = "SE") %>% 
                  full_join(tw, by = "SE")
            
            y <- fouralert(obj = d.aps[d.aps$SE <= datarelatorio,],crit = crit)
            print(paste("nivel do alerta de ",d.aps$localidade[1],":", 
                        tail(y$indices$level,1)))
            y
      }      
      naps <- 0:9
      res <- lapply(naps, calc.alertaintra) %>% setNames(APS) # antes o nome era character, agora e o geocodigo
      #       nick <- gsub(" ", "", nome, fixed = TRUE)
      #       #names(alerta) <- nick
      
      
      #if (writedb == TRUE) write_alerta(alerta, write = "db")  
      class(res) <- "alerta_intra"
      res
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
            left_join(pars[c("municipio_geocodigo", "limiar_preseason", 
                             "limiar_epidemico")]) %>%
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


