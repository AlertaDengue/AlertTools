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
#'@param rule either a built-in rule ("Af", "Aw", "AsAw") or a list with three elements defining criteria for 
#'transition to yellow (level 2), orange (level 3) and red (level 4). See description.
#'@param delays list with three elements, each one is a vector: c(delay to turn on, delay to turn off)
#'@param values named vector of values for the critical parameters. Use character.   
#'@return list with rules. To be useful, this list must contain variables that match those in the data.  
#'@examples
#'NOT USE: setCriteria(rule="Af")
#'Defining a rule
#'myrule = list(crity = "temp_min > 25 & casos > 0", crito = "p1 > 0.9 & inc > 1", 
#'critr = "inc > 100 & casos > 10")
#'mydelay = list(delayy = c(3,1), delayo = c(3,1), delayr = c(2,2))
#'setCriteria(rule = myrule, delays = mydelay)
#'Defining values manually
#'val <- c("varcli" ="temp_min", "limiar_preseason"="10","limiar_epidemico"="100", "clicrit"="22"
#', "clicrit2" = "80", varcli2 = "umid_max")
#'setCriteria(rule="Af",values=val)
#'setCriteria(rule="AsAw",values=val)
#'Using infodengue parameters:
#'val <- read.parameters(3304557)
#'setCriteria(rule="Af", values=val)

setCriteria <- function(rule=NULL, values=NULL, 
                        delays = list(delayy = c(0,0), delayo = c(0,0), delayr = c(0,0))){
      
      # checking input
      if(!is.null(rule)) assert_that(rule %in% c("Af","Aw","AsAw"),msg = "setcriteria: rule unknown. Try Af, Aw or AsAw")
      if(is.null(rule)) {
            assert_that(!is.null(values), msg = "setcriteria: if rule is null, values must be provided.")
            assert_that(any(names(values) %in% c("varcli", "clicrit", "limiar_preseason",
                                            "limiar_epidemico")), msg = "setcriteria: elements missing from arg values")
            }
      
      
      # pre-defined rules
      if(!is.null(rule)){
      
            if(rule[1] == "Af"){
                  criteria <- list(
                  crity = c("temp_min > temp_crit & inc > 0", 3, 2), #3,2
                  crito = c("p1 > 0.95 & inc > limiar_preseason", 2, 2), #3,2
                  critr = c("inc > limiar_epidemico & casos > 5", 2, 2) #2,2
            )} 
            if (rule[1] == "Aw"){
                   criteria = list(
                         crity = c("umid_max > umid_crit & inc > 0", 3, 2), #3,2
                   crito = c("p1 > 0.95 & inc > limiar_preseason", 2, 2), #3,2
                   critr = c("inc > limiar_epidemico & casos > 5", 2, 2) #2,2
                   )}
            if(rule[1] == "AsAw"){
                  criteria = list(
                        crity = c("temp_min > temp_crit & umid_max > umid_crit & inc > 0", 3, 2), #3,2
                        crito = c("p1 > 0.95 & inc > limiar_preseason", 3, 2), #3,2
                        critr = c("inc > limiar_epidemico & casos > 5", 2, 2) #2,2
                  )}
       # user defined rules      
      } else {  
            criteria<-lapply(1:3, function(x) c(rule[[x]], delays[[x]]))
            names(criteria) <- c("crity","crito","critr")
      }
      
      # substituting values (very bad coding, should be improved)
      if(!is.null(values)) {
            if (!("varcli2" %in% names(values))) values[["varcli2"]] <- "xx"
            
            if (rule[1] %in% c("Af", "AsAw")){
                  assert_that(values[["varcli"]] == "temp_min" | values[["varcli2"]] == "temp_min",
            msg = "setcriteria: Af and AsAw require temp_min")
                  tm <- names(values)[which(values == "temp_min")]
                  if(tm == "varcli")  values <- c(values, "temp_crit" = values[["clicrit"]])
                  if(tm == "varcli2") values[["temp_crit"]] <- c(values, "temp_crit" = values[["clicrit2"]])
      }
      
      if (rule[1] %in% c("Aw", "AsAw")){
            assert_that(values[["varcli"]] == "umid_max" | values[["varcli2"]] == "umid_max",
                        msg = "setcriteria: Aw and AsAw require umid_max")
            um <- names(values)[which(values == "umid_max")]
            if(um == "varcli")   values <- c(values, "umid_crit" = values[["clicrit"]])
            if(um == "varcli2")  values <- c(values, "umid_crit" = values[["clicrit2"]])
      
            if(class(values) == "data.frame") 
                  { # handling values from read.parameters
            values <- unlist(sapply(names(values),function(x) values[[x]])) 
            }
      }
            
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
#'# Better visualization
#'tail(tabela_historico(resf),n=3)


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
      assert_that(all(sapply(c(delay_turnon, delay_turnoff), is.count)),
                  msg = "fouralert: delays are mispecified")

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
      contains_34 <- which(zoo::rollapply(indices$level,list(c(-dy:0)),
                                    function(x) any(x>=3), fill=NA))
      
      # to visualize how it works, descomment the following lines
      #indices$dy <- NA
      #indices$dy[contains_34]<-pmax(indices$level[contains_34], rep(2, length(contains_34)))
      
      indices$level[contains_34]<-pmax(indices$level[contains_34], 
                                        rep(2, length(contains_34)))
      
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
#'cidades <- getCidades(regional = "Norte",uf = "Rio de Janeiro",datasource = con)
#'res <- pipe_infodengue(cities = cidades$municipio_geocodigo, cid10 = "A90", 
#'finalday= "2020-01-23",nowcasting="none")
#'head(tabela_historico(res))
#'# User's parameters
#'dd <- read.parameters(cities = c(3200300)) %>% mutate(limiar_epidemico = 100)
#'res <- pipe_infodengue(cities = dd, cid10 = "A90", 
#'finalday= "2018-08-12",nowcasting="none")
#'restab <- tabela_historico(res)
#'
#'res <- pipe_infodengue(cities = 3141009, cid10 = "A90", 
#'finalday= "2020-01-23",nowcasting="none")
#'tail(tabela_historico(res))
pipe_infodengue <- function(cities, cid10="A90", datarelatorio, finalday = Sys.Date(), iniSE = 201001, nowcasting="none", 
                            narule=NULL, writedb = FALSE, datasource = con, userinput =FALSE, completetail = NA, dataini = "notif"){
      
     
      
      if(missing(datarelatorio)) {
            datarelatorio <- data2SE(finalday, format = "%Y-%m-%d") 
      } else { # if datarelatorio & finalday are given, priority is datarelatorio
            finalday <- SE2date(datarelatorio)$ini+6
      }
      
      # check dates
      last_sinan_date <- lastDBdate(tab = "sinan", cid10 = cid10, cities = cities)
      print(paste("last sinan date for",cid10 ,"is", last_sinan_date$se))
      
      assert_that(!is.na(last_sinan_date$se), msg = paste("no sinan data for cid10", cid10)) 
      
      if(last_sinan_date$se < datarelatorio) {
            
            if (userinput){
                  message(paste("last date in database is",last_sinan_date$se,
                                ". Should I continue with SE =", datarelatorio,
                                "? tecle Y if YES, or change to new date"))
                  x <- scan("stdin", character(), n = 1)
                   if(x!="Y") { 
                         datarelatorio <- as.numeric(x)   
                         } else {completetail <- 0} # complete the tail with zeros
            } 
      }
      
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
      
      # Reading the names of the meterological stations for each city
      sqlcity = paste("'", str_c(cidades, collapse = "','"),"'", sep="")
      comando <- paste("SELECT id, nome_regional, nome_macroreg, municipio_geocodigo, codigo_estacao_wu, estacao_wu_sec from 
                       \"Dengue_global\".regional_saude WHERE municipio_geocodigo IN (", sqlcity, 
                       ")" , sep="")
      city_table <- dbGetQuery(datasource,comando)
      
      estacoes <- na.omit(unique(c(city_table$estacao_wu_sec, city_table$codigo_estacao_wu)))
      print("usando dados de clima das estacoes:")
      print(estacoes)
      
      # Reading the meteorological data
      #print('Obtendo os dados de clima...')
      varscli <- na.omit(unique(c(pars_table$varcli, pars_table$varcli2)))
      
      cliwu <- getWU(stations = estacoes, vars = varscli, finalday = finalday)
      
      # Reading Cases
      print("Obtendo dados de notificacao ...")
      
      casos <- getCases(cidades, lastday = finalday, cid10 = cid10, completetail = completetail) %>%
            mutate(inc = casos/pop*100000)
      
      # Reading tweets 
      if(cid10 == "A90"){
            print("Reading tweets...")
            dT = getTweet(cidades, lastday = finalday, cid10 = "A90")
      }
      
      
      # para cada cidade ...
      
      ## FUN  calc.alerta (internal)
      calc.alerta <- function(x){  #x = cities[i]
            # params
            parcli.x <- city_table[city_table$municipio_geocodigo == x, c("codigo_estacao_wu", "estacao_wu_sec")]      
            varcli.x <- na.omit(c(pars_table$varcli[city_table$municipio_geocodigo == x],
                                pars_table$varcli2[city_table$municipio_geocodigo == x]))
            
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
                  adjustIncidence(method = nowcasting,  nyears = 1) %>%
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



#alertaRio ---------------------------------------------------------------------
#'@title 4 level alert Green-Yellow-Orange-Red for Rio de Janeiro.
#'@description Yellow is raised when environmental conditions required for
#'positive mosquito population growth are detected, green otherwise.Orange 
#'indicates evidence of sustained transmission, red indicates evidence of 
#'an epidemic scenario.  
#'@export
#'@param naps subset of vector 0:9 corresponding to the id of the APS. Default is all of them.
#'@param se last epidemiological week (format = 201401) 
#'@param iniSE first epidemiological week
#'@param cid10 default is A90 (dengue). Chik = A920.
#'@param narule how to fill climate missing data (arima is the only option)
#'@param delaymethod atribbute of adjuntincidence. "fixedprob" or "bayesian"
#'@param pdig parameters for adjustIncidence = "fixedprob"
#'@param datasource name of the sql connection.
#'@return list with an alert object for each APS.
#'@examples
#'alerio1 <- alertaRio(naps = 0:2, se=201952, delaymethod="fixedprob", cid10 = "A90")
#'alerio2 <- alertaRio(naps = 0:2, se=201952, delaymethod="fixedprob", cid10 = "A920")
#'delaymethod="fixedprob",verbose=FALSE,dataini = "sinpri")
alertaRio <- function(naps = 0:9, se, cid10 = "A90", iniSE = 201001,
                      delaymethod = "fixedprob", narule="arima", 
                      pdig = c(2.5016,1.1013), datasource=con){
      
      assert_that(narule == "arima", msg = "alertaRio: arima is the only na fill method available")
      assert_that(cid10 %in% c("A90","A920"), msg = "alertaRio: cid10 = A90 for dengue or cid10 = A920 for chik" )
      
      if(missing(se)) se = data2SE(days = Sys.Date(), format = "%Y-%m-%d")
      
      # getting parameters and criteria
      crit.x <- read.parameters(3304557, cid10 = cid10)
      crit.x.vector <- structure(as.character(crit.x), names = as.character(names(crit.x))) # dataframe -> vector
      crit <- setCriteria(rule = "Af", values = crit.x.vector)
      
      # reading data
      message("lendo dados ...")
      print(paste("Ultimos registros de",cid10,":",
                        lastDBdate("sinan", cities=3304557,cid10 = cid10)[["se"]]))
      print(paste("Ultimos registros de tweets:",
                        lastDBdate("tweet", cid10 = cid10, cities=3304557)$se))
      
      
      cas = getCasesinRio(APSid = naps, cid10 = cid10)
      cli = getWU(stations = c('SBRJ',"SBJR","SBGL"), vars = "temp_min")
      if(cid10 == "A90") {
            tw <- getTweet(cities = 3304557, cid10="A90")[,c("SE","tweet")]
      }else {
            tw <- data.frame(SE = seqSE(from = min(cas$SE), to = max(cas$SE))$SE,
                                   tweet = NA)
      }
      
                  
      
      # nowcasting and Rt parameters 
      if(cid10=="A90") meangt = 3 # 3 weeks
      if(cid10=="A920") meangt = 2 # 2 weeks
      if(delaymethod == "fixedprob") p <- plnorm(seq(7,20,by=7), pdig[1], pdig[2])
      
      # output object 
      APS <- c("APS 1", "APS 2.1", "APS 2.2", "APS 3.1", "APS 3.2", "APS 3.3"
               , "APS 4", "APS 5.1", "APS 5.2", "APS 5.3")[(naps + 1)]    
      res <- vector("list", length(APS))
      names(res) <- APS
      
      # Function to calculate alert per aps
      calc.alertaRio <- function(aps){
            
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
                  adjustIncidence(method = delaymethod, pdig = p) %>%
                  Rt(count = "tcasesmed",gtdist="normal", meangt= meangt, sdgt = 1) %>%
                  mutate(inc = tcasesmed/populacao*100000) %>%
                  full_join(cli.aps, by = "SE") %>% 
                  full_join(tw, by = "SE")
            
            y <- fouralert(obj = d.aps[d.aps$SE <= se,],crit = crit)
            print(paste("nivel do alerta de ",d.aps$localidade[1],":", 
                        tail(y$indices$level,1)))
            y
      }      
      
      res <- lapply(naps, calc.alertaRio) %>% setNames(APS) # antes o nome era character, agora e o geocodigo
      #       nick <- gsub(" ", "", nome, fixed = TRUE)
      #       #names(alerta) <- nick
      
      
      #if (writedb == TRUE) write_alerta(alerta, write = "db")  
      class(res) <- "alertario"
      res
}


#plot_alerta --------------------------------------------------------------------
#'@title Plot the time series of infodengue's alert levels for one or more cities.
#'@description Function to plot the output of tabela_historico. 
#'@export
#'@param obj object created by tabela_historico() or from the pipeline.
#'@param geocodigo city's geocode. If missing, plots for all cities will be created.
#'@param var variable to be plotted, usually "cases", or "inc". 
#'@param cores colors corresponding to the levels 1, 2, 3, 4.
#'@param ini first epidemiological week. If not stated, use the first one available
#'@param fim last epidemiological week. If not stated, use the last one available 
#'@param ylab y axis label
#'@param yrange y axis range
#'@param salvar TRUE to save the figure (default is FALSE) 
#'@param nome.fig figure name (default is "grafico")
#'@return a plot for each city
#'@examples
#' # Parameters for the model
#'cidades <- getCidades(regional = "Norte",uf = "Rio de Janeiro", datasource = con)
#'res <- pipe_infodengue(cities = cidades$municipio_geocodigo, cid10 = "A90", 
#'finalday= "2018-08-12",nowcasting="none")
#'restab <- tabela_historico(res)
#'plot_alerta(restab, geocodigo = cidades$municipio_geocodigo, var="casos")

plot_alerta<-function(obj, geocodigo, var = "casos", cores = c("#0D6B0D","#C8D20F","orange","red"), 
                      ini=201001, fim=202001, ylab=var, yrange, salvar = FALSE, 
                      nome.fig = "grafico", datasource=con){
      
      # type of object to plot
      if(class(obj) == "alerta") obj <- tabela_historico(obj)
      if(class(obj[[1]]) == "alerta") obj <- tabela_historico(obj)

      assert_that(var %in% names(obj), msg = paste("plot_alerta: I don't find the 
                                                   variable", var, "to plot"))
      
      assert_that(class(obj) != "alertario)", msg = paste("use plot_alertaRio() to plot alerta rio"))
      
      if(missing(geocodigo)) geocodigo <- unique(obj$municipio_geocodigo)
      if(missing(ini))ini <- min(obj$SE)
      if(missing(ini))fim <- max(obj$SE)
      
      d <- obj %>% filter(SE >= ini & SE <= fim)
      
      # get the thresholds
      pars_table <- read.parameters(cities = geocodigo, 
                                    cid10 = d$CID10[1]) 
      #print(pars_table)
      # para uma cidade, dados e parametros para o grafico
      
      plota <- function(geo, yrange){
            
            dd <- d %>% filter(municipio_geocodigo == geo)            
            pars <- pars_table %>% filter(municipio_geocodigo == geo)
            
            x <- seq_along(dd$SE)
            ticks <- seq(1, length(dd$SE), length.out = 8)
            if(missing(yrange)) yrange = range(dd[,var], na.rm=TRUE)
            
            if(salvar == TRUE) png(paste(nome.fig,geo,"_",max(dd$SE),".png", sep = ""))
            # grafico
            par(mai=c(0,0,0,0),mar=c(4,4,0,4))
            plot(x, dd[,var], xlab = "", ylab = var, type="l", ylim= yrange, axes=FALSE)
            axis(1, at = ticks, labels = dd$SE[ticks], las=3, cex=0.8)
            axis(2, las=2, cex=0.8)
            
            for (i in 1:4) {
                  onde <- which(dd$nivel==i) 
                  if (length(onde))
                        segments(x[onde],0,x[onde],(dd[onde,var]),col=cores[i],lwd=3)
            }
            if(var == "casos") {
                  abline(h=pars$limiar_preseason*dd$pop[1]/1e5, lty=2, col="darkgreen")
                  abline(h=pars$limiar_epidemico*dd$pop[1]/1e5, lty=2, col="red")
            }
            if (var == "p_inc100k") {
                  abline(h=pars$limiar_preseason, lty=2, col="darkgreen")
                  abline(h=pars$limiar_epidemico, lty=2, col="red")
            }
            if(var == "temp_min" & pars$clicrit == "temp_min") abline(h = pars$clicrit, lty=2, col = "yellow")
            if(var == "umid_max" & pars$clicrit == "umid_max") abline(h = pars$clicrit, lty=2, col = "yellow")
            if(salvar == TRUE) dev.off()
      }
      
      geocodigo %>% map(plota)
      return()
      #par(new=T)
      #plot(dd[,var], col="white", type="l",axes=FALSE , xlab="", ylab="" ) #*coefs[2] + coefs[1]
      #        axis(1, pos=0, lty=0, lab=FALSE)
      #        axis(4, las=2, cex=0.6 ,col.axis="darkgrey")
      #        mtext("casos",4, line=3,col="darkgrey", cex=0.7)
      #       }
}

#plot_alertaRio --------------------------------------------------------------------
#'@title Plot the time series of alerts for each APS. Specific for the Rio de Janeiro alert model.
#'@description Function to plot the output of alertaRio(). 
#'@export
#'@param ale object created by alertaRio()
#'@param aps aps to be plotted. If missing, figures will be created for all aps (health districts) 
#'@param var variable to be plotted, usually "cases", or "inc". 
#'@param cores colors corresponding to the levels 1, 2, 3, 4.
#'@param ini first epidemiological week. If not stated, use the first one available
#'@param fim last epidemiological week. If not stated, use the last one available 
#'@param ylab y axis label
#'@param yrange y axis range
#'@param salvar TRUE to save the figure (default is FALSE) 
#'@param nome.fig figure name (default is "grafico")
#'@return a plot
#'@examples
#' # Parameters for the model
#'alerio2 <- alertaRio(naps = 0:9, se=201804, delaymethod="fixedprob", cid10 = "A90")
#'plot_alertaRio(alerio2, var = "inc")

plot_alertaRio<-function(ale, aps, var = "casos", cores = c("#0D6B0D","#C8D20F","orange","red"), 
                      ini=201001, fim=202001, ylab=var, yrange, salvar = FALSE, 
                      nome.fig = "grafico", datasource=con){
      
      # type of object to plot
      assert_that(class(ale) == "alertario", msg = "plot_alertaRio can only be used with object created by alertaRio()") 
      
      obj <- write_alertaRio(ale, write = FALSE)
      #if(class(obj) == "alertario") obj <- write_alertaRio(obj, write = "no")
      assert_that(var %in% names(obj), msg = paste("plot_alertaRio: I don't find the 
                                                   variable", var, "in", ale))
      
      
      if(missing(aps)) aps <- unique(obj$aps)
      if(missing(ini))ini <- min(obj$se)
      if(missing(ini))fim <- max(obj$se)
      
      d <- obj %>% filter(se >= ini & se <= fim)
      
      # get the thresholds (the same for all aps)
      pars <- read.parameters(cities = 3304557, cid10 = unique(na.omit(d$CID10))) 
      #print(pars_table)
      # para uma cidade, dados e parametros para o grafico
      
      plota <- function(ap, yrange){
            
            dd <- d %>% filter(aps == ap)            
            x <- seq_along(dd$se)
            ticks <- seq(1, length(dd$se), length.out = 8)
            if(missing(yrange)) yrange = range(dd[,var], na.rm=TRUE)
            
            if(salvar == TRUE) png(paste(nome.fig,aps,"_",max(dd$se),".png", sep = ""))
            # grafico
            par(mai=c(0,0,0,0),mar=c(4,4,0,4))
            plot(x, dd[,var], xlab = "", ylab = var, type="l", ylim= yrange, axes=FALSE)
            axis(1, at = ticks, labels = dd$se[ticks], las=3, cex=0.8)
            axis(2, las=2, cex=0.8)
            
            for (i in 1:4) {
                  onde <- which(dd$nivel==i) 
                  if (length(onde))
                        segments(x[onde],0,x[onde],(dd[onde,var]),col=cores[i],lwd=3)
            }
            if(var == "casos") {
                  abline(h=pars$limiar_preseason, lty=2, col="darkgreen")
                  abline(h=pars$limiar_epidemico, lty=2, col="red")
            }
            if (var == "inc") {
                  abline(h=pars$limiar_preseason/dd$pop[1]*100000, lty=2, col="darkgreen")
                  abline(h=pars$limiar_epidemico/dd$pop[1]*100000, lty=2, col="red")
            }
            if(var == "temp_min" & pars$clicrit == "temp_min") abline(h = pars$clicrit, lty=2, col = "yellow")
            
            if(salvar == TRUE) dev.off()
      }
      
      aps %>% map(plota)
      return()
      
}

#map_Rio --------------------------------------------------------------------
#'@title Plot the alert map for Rio de Janeiro city.
#'@description Function to plot a map of the alert. 
#'@export
#'@param obj object created by the twoalert and fouralert functions.
#'@param data Date
#'@param cores colors corresponding to the levels 1, 2, 3, 4.
#'@param filename if present, the map is saved.
#'@param dir directory where map will be saved. 
#'@param resol dpi png resolution, default is 200
#'@return a map
#'@examples
#'params <- c(varcli ="temp_min", clicrit=22, limiar_epidemico=100, limiar_preseason = 14.15)
#'criter <- setCriteria(rule="Af", values = params)
#'alerio2 <- alertaRio(naps = 0:2, crit = criter, se=201804, delaymethod="fixedprob")
#'map_Rio(alerio2)

map_Rio<-function(obj, cores = c("green","yellow","orange","red"), data, datasource=con,
                  shapefile = "../report/Rio_de_Janeiro/shape/CAPS_SMS.shp", filename="", dir, resol=200){
      
      #stopifnot(names(obj[[1]]) == c("data", "indices", "rules","n"))
      
      require(maptools,quietly = TRUE,warn.conflicts = FALSE)
      mapa <- readShapeSpatial(shapefile,ID="COD_AP_SMS")
      d2 <- obj[[1]]$data
      
      # definindo a data do mapa (se nao for dada na funcao, usar a ultima)
      if(missing(data)) {
            ultima_se <- sort(d2$SE,decreasing =TRUE)[1]
      } else {
            ultima_se <- data
            stopifnot (which(d2$SE==ultima_se)>0)
            }
      
      nomesAPS = names(obj)
      lastab <- data.frame(APS = nomesAPS, cor = "grey")
      lastab$cor <- as.character(lastab$cor)
      for (i in 1:10){
            i2 <- obj[[i]]$indices
            lastab$cor[i] <- cores[i2[which(d2$SE==ultima_se),c("level")]]      
      }
      
      lastab$APS2 <-  as.numeric(gsub("APS ","",lastab$APS))
      mapa@data$COD_AP_SMS <- as.numeric(as.character(mapa@data$COD_AP_SMS))
      mapa@data <- merge(mapa@data,lastab,by.x="COD_AP_SMS",by.y="APS2",all=TRUE)
      
      if(!missing(filename)){#salvar
            figname = paste(dir,filename, sep="") 
            png(figname, width = 16, height = 15, units="cm", res=resol)
            message("mapa salvo em como  ", figname)
      }
      par(mfrow=c(1,1),mai=c(0,0,0,0),mar=c(4,1,1,1))
      plot(mapa,col=mapa@data$cor)
      coords <- coordinates(mapa)
      coords[1,1] <- -43.19
      coords[2,2] <- -22.945
      #text(coords,label=mapa@data$COD_AP_SMS,cex=0.6)
      legend("bottom",fill=cores,c("atividade baixa","condicoes favoraveis transmissao",
                                   "transmissao sustentada","atividade alta"),bty="n",cex=0.6)
                                   
      par(cex.main=0.7)
      title(paste0( "Mapa MRJ por APs \n"," Semana ",substr(ultima_se,5,6)," de ",
                    substr(ultima_se,1,4)),line=-1)
      
    
      if(!missing(filename)) {dev.off()} #salvar
      
      
}

#geraMapa --------------------------------------------------------------------
#'@title Plot the alert map for any state.
#'@description Function to plot a map of the alert.
#'@export 
#'@param alerta object created by update.alerta.
#'@param subset nomes das cidades que serão mostradas no mapa. 
#'@param se epidemiological week (format = 201610).  
#'@param cores colors corresponding to the levels 1, 2, 3, 4.
#'@param legpos legend position. Default is bottomright. Check the other options in the function legend.
#'@param titulo title of the map
#'@param filename if present, the map is saved.
#'@param dir directory where map will be saved 
#'@param shapefile shapefile containing polygons for the municipalities
#'@param varid shapefile variable indicating the geocode of the municipalities  
#'@param varname name of the variable to be plotted
#'@param caption Default is TRUE 
#'@param resol dpi png resolution, default is 200
#'@return a map
#'@examples
#' # Parameters for the model
#'cidades <- getCidades(regional = "Norte",uf = "Rio de Janeiro",datasource = con)
#'res <- pipe_infodengue(cities = cidades$municipio_geocodigo, cid10 = "A90", 
#'finalday= "2018-08-12",nowcasting="none")
#'geraMapa(alerta=res, subset = c(3300936, 3302403), se=201804, 
#'shapefile="shape/33MUE250GC_SIR.shp", varid="CD_GEOCMU", titulo="RJ-Norte")

geraMapa<-function(alerta, subset, se, cores = c("green","yellow","orange","red"), legpos="bottomright",
                   titulo="", filename, dir="", shapefile, varid, varname, caption=TRUE, 
                   resol = 200){
      
      require(maptools,quietly = TRUE,warn.conflicts = FALSE)
      
      assert_that(class(alerta[[1]]) == "alerta", msg = "geraMapa: alerta argument must have class alerta. 
                  Try using fouralert or infodengue_pipeline.")
     
       N = length(alerta) # numero de cidades presentes no objeto alerta.
      
      # subset de cidades para o mapa
      if (!missing(subset)){
            assert_that(class(subset) %in% c("integer","numeric"), msg = "gerMapa: subset must be a vector of geocodes" ) 
            ale <- alerta[which(as.numeric(names(alerta)) %in% subset)]
      } else {ale <- alerta}
      
      # table com as cidades e cores
      lastab <- data.frame(cidade = names(ale), geocodigo = NA, cor = "grey"
                           , nome=NA, short = NA)
      lastab$cor <- as.character(lastab$cor)
      n = length(ale)
      for (i in 1:n){ # por cidade
            ciddata <- ale[[i]]$data
            inddata <- ale[[i]]$indices
            lastab[i,2:5] <- c(ciddata$cidade[1],
                               cores[inddata[which(ciddata$SE==se),c("level")]],
                               ciddata$nome[1], substring(ciddata$nome[1],1,3))
      }
      
      mapa <- readShapeSpatial(shapefile,ID=varid)
      meumapa <- mapa[as.character(mapa@data$CD_GEOCMU) %in% lastab$geocodigo,]
      meumapa@data <- merge(meumapa@data,lastab,by.x=varid,by.y="geocodigo")
      
      if(!missing(filename)){#salvar
            png(paste(dir,filename,sep="")
                , width = 16, height = 15, units="cm", res=resol)
      }
      
      par(mfrow=c(1,1),mai=c(0,0,0,0),mar=c(4,1,1,1))
      plot(meumapa,col=meumapa@data$cor)
      coords <- coordinates(meumapa)
      if (caption == TRUE) text(coords,label=meumapa@data$short,cex=0.6)
      legend(legpos,fill=cores,c("Atividade baixa","Alerta de transmissão","Transmissão sustentada",
                                        "Atividade alta"),bty="n",cex=0.8)
      par(cex.main=0.7)
      title(paste0(titulo, "Semana ",substr(se,5,6)," de ",substr(se,1,4)),line=-1)
      
      if(!missing(filename)) {dev.off()} #salvar
}


#tabela_historico --------------------------------------------------------------------
#'@title Write the alert object into the database.
#'@description Function to write the alert results into the database. 
#'@export
#'@param obj object created by the pipeline.
#'@param ini_se first week of the table. Default is the first date in obj.
#'@param last_se last week of the table. Default is the last date in obj. To do.
#'@param versao Default is current's date
#'@return data.frame with the data to be written. 
#'@examples
#'cidades <- getCidades(regional = "Norte",uf = "Rio de Janeiro",datasource = con)
#'res <- pipe_infodengue(cities = cidades$municipio_geocodigo, cid10 = "A90", 
#'finalday= "2013-01-10")
#'restab <- tabela_historico(res) 
#'tail(restab)

tabela_historico <- function(obj, iniSE, lastSE, versao = Sys.Date()){
      
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
      id <- sapply(1:nrow(data), gera_id) 
      
      # ---------- filtering dates -------------------------#
      if(missing(iniSE)) iniSE <- min(d$SE)
      if(missing(lastSE)) lastSE <- max(d$SE)
      
      d <- d %>%
            filter(SE >= iniSE & SE <= lastSE) %>% 
            rename(municipio_geocodigo = cidade,
                   municipio_nome = nome,
                   p_inc100k =inc,
                   casos_est = tcasesmed,
                   casos_est_min = tcasesICmin,
                   casos_est_max = tcasesICmax) %>%
            mutate(p_rt1 = ifelse(is.na(p1),0,p1),
                   Localidade_id  = ifelse(is.na(localidade),0,localidade),
                   data_iniSE = SE2date(SE)$ini,
                   nivel = indices$level,
                   versao_modelo = as.character(versao),
                   id = id)
      d$Rt[is.na(d$Rt)] <- 0
      
      d
}

#write_alerta --------------------------------------------------------------------
#'@title Write historico_alerta into the database.
#'@description Function to write the pipeline results into the database. 
#'Receives the object created by the function historico.alerta.
#'@export
#'@param d object created by tabela_historico()
#'@param datasource posgreSQL conn to project's database
#'@return the same data.frame from the input
#'@examples
#'# Parameters for the model
#'cidades <- getCidades(regional = "Norte",uf = "Rio de Janeiro",datasource = con)
#'res <- pipe_infodengue(cities = cidades$municipio_geocodigo[1], cid10 = "A90", 
#'finalday= "2016-08-12",nowcasting="none")
#'restab <- tabela_historico(res) 
#'write_alerta(restab)

write_alerta<-function(d, datasource = con){
            
      
      # check input
      assert_that(class(d) == "data.frame", msg = "write_alerta: d is not a data.frame. d should
                  be an output from tabela_historico.")
      
      cid10 = unique(d$CID10)
      assert_that(length(cid10) == 1, msg = "write_alerta: d must contain only one cid10")
      
      dcolumns <- c("SE", "data_iniSE", "casos_est", "casos_est_min", "casos_est_max",
                    "casos","municipio_geocodigo","p_rt1","p_inc100k","Localidade_id",
                    "nivel","id","versao_modelo","municipio_nome","Rt", "pop", "tweet")
      dcolumns1 <-c("temp_min", "umid_max")
      
      assert_that(all(dcolumns %in% names(d)), msg = paste("write_alerta: check if d contains
                                                           columns", dcolumns))
      assert_that(any(dcolumns1 %in% names(d)), msg = paste("write_alerta: check if d contains
                                                           climate variable in", dcolumns1))
      
             sepvarnamesR <- c("SE", "data_iniSE", "casos_est", "casos_est_min", "casos_est_max",
                             "casos","tweet","tempmin","umidmax","municipio_geocodigo","Rt", "p_rt1","pop",
                             "p_inc100k","Localidade_id","nivel","versao_modelo","id")
      # nomes das tabelas para salvar os historicos:
      if(cid10=="A90") {tabela <-  "Historico_alerta"; constr.unico = "alertas_unicos"}
      if(cid10=="A92.0") {tabela <-  "Historico_alerta_chik"; constr.unico = "alertas_unicos_chik"}
      if(cid10=="A92.8") {tabela <-  "Historico_alerta_zika"; constr.unico = "alertas_unicos_zika"}
      if(!(cid10 %in% c("A90", "A92.0", "A92.8"))) stop(paste("não sei onde salvar histórico para o agravo", cid10))
      
      print(paste("writing alerta into table", tabela))
      
      # ------ vars to write 
      
      if("temp_min" %in% names(d)) dcolumns = c(dcolumns, "temp_min")
      if("umid_max" %in% names(d)) dcolumns = c(dcolumns, "umid_max")
            
      dados <- d %>%
            select(all_of(dcolumns))
      
       
      # ------ sql command
      varnamesforsql <- c("\"SE\"", "\"data_iniSE\"", "casos_est", "casos_est_min", "casos_est_max",
                          "casos","municipio_geocodigo","p_rt1","p_inc100k","\"Localidade_id\"",
                          "nivel","id","versao_modelo","municipio_nome","\"Rt\"", "pop", "tweet", "tempmin", "umidmax")
      
      varnames.sql <- str_c(varnamesforsql, collapse = ",")
      updates = str_c(paste(varnamesforsql,"=excluded.",varnamesforsql,sep=""),collapse=",") # excluidos, se duplicado
      
      
      escreve_linha <- function(li){
            vetor <- dados[li,]
            vetor$municipio_nome = gsub(vetor$municipio_nome, pattern = "'", replacement = "''")
            linha = paste0(vetor$SE,",'",as.character(vetor$data_iniSE),"',", str_c(vetor[1,c("casos_est","casos_est_min","casos_est_max",
                  "casos","municipio_geocodigo","p_rt1","p_inc100k","Localidade_id","nivel","id")], collapse=","), ",'", 
                  as.character(vetor$versao_modelo),"','",as.character(vetor$municipio_nome),"',", 
                          str_c(vetor[1,c("Rt","pop","tweet")], collapse = ","))
            if("temp_min" %in% names(vetor)) linha = paste0(linha,",", vetor$temp_min, ",","NA")
            if("umid_max" %in% names(vetor)) linha = paste0(linha,",", "NA", ",", vetor$umid_max)
            linha = gsub("NA","NULL",linha)
            linha = gsub("NaN","NULL",linha)
            
            insert_sql = paste("INSERT INTO \"Municipio\".\"",tabela,"\" (" ,varnames.sql,") VALUES (", linha, ") ON CONFLICT ON CONSTRAINT ",constr.unico,"  
                                     DO UPDATE SET ",updates, sep="")
            try(dbGetQuery(datasource, insert_sql))    
            insert_sql
      }
      
      # escrevendo
      1:nrow(d) %>% map(escreve_linha)
      print("done")
}



#write_alertaRio --------------------------------------------------------------------
#'@title Write the Rio de janeiro alert into the database.
#'@description Function to write the alert results into the database. 
#'@export
#'@param obj object created by the alertRio function and contains alerts for each APS.
#'@param write use "db" if data.frame should be inserted into the project database,
#' or "no" (default) if nothing is saved. 
#'@param version default is the current date
#'@return data.frame with the data to be written. 
#'@examples
#'params <- c(varcli ="temp_min", clicrit=22, limiar_epidemico=100, limiar_preseason = 14.15)
#'criter <- setCriteria(rule="Af", values = params)
#'alerio2 <- alertaRio(naps = c(1,2), crit = criter, datasource=con, se = 201201)
#'res <- write_alertaRio(alerio2, write="no")
#'tail(res)  

write_alertaRio<-function(obj, write = "no", datasource = con, version = Sys.Date()){
      
      # check input
      assert_that(class(obj) == "alertario", msg = "write_alertaRio: obj is an alertaRio object" )
      
      
      listaAPS <- c("APS 1", "APS 2.1", "APS 2.2", "APS 3.1", "APS 3.2", "APS 3.3"
                    , "APS 4", "APS 5.1", "APS 5.2", "APS 5.3")
      APSlabel <- c("1.0", "2.1", "2.2", "3.1", "3.2", "3.3","4.0","5.1","5.2","5.3")
      cid10 <- obj[[1]]$data$CID10[1]
      
      dados <- data.frame()
      n <- length(obj)
      for (i in 1:n){
            data <- obj[[i]]$data
            indices <- obj[[i]]$indices   
            
            # creating the data.frame with the required columns
            d <- data %>%
                  rename(se = SE,
                         casos_est = tcasesmed,
                         casos_estmin = tcasesICmin,
                         casos_estmax = tcasesICmax,
                         tmin = temp_min,
                         tweets = tweet,
                         rt = Rt,
                         prt1 = p1) %>%
                  mutate(data = SE2date(se)$ini,
                         aps = APSlabel[(localidadeid[1]+1)],
                         nivel = indices$level,
                         prt1 = replace_na(prt1, 0)) %>%
                  arrange(se) %>% 
                  select(-pdig)
            dados <- rbind(dados,d)
      }
           
      if(write == "db"){
            
            # nome da tabela no banco de dados e do respectivo constraint 
            if (cid10 == "A90") {tabela <- "alerta_mrj"; sqlconstr = "unique_aps_se"}
            if (cid10 == "A92.0") {tabela <- "alerta_mrj_chik"; sqlconstr = "unique_chik_aps_se"}
            
            # se tiver ja algum registro com mesmo aps e SE, esse sera substituido pelo atualizado.
            # ------ vars to write            
            varnames <- "(se,aps,data,tweets,casos,casos_est,casos_estmin,casos_estmax,tmin,rt,prt1,
                        inc,nivel)"
            sepvarnames <- c("se","aps","data","tweets","casos","casos_est","casos_estmin","casos_estmax",
                                   "tmin","rt","prt1","inc","nivel")
            
            # ------ sql command
            varnames.sql <- str_c(sepvarnames, collapse = ",")
            updates = str_c(paste(sepvarnames,"=excluded.",sepvarnames,sep=""),collapse=",") # excluidos, se duplicado
            
            escreve_linha <- function(li){
                  vetor <- dados[li,sepvarnames]
                  linha = paste(vetor[1,1],",'",as.character(vetor[1,2]),"','", 
                                as.character(vetor[1,3]),"',", 
                                str_c(vetor[1,4:13], collapse=","), sep="")
                  linha = gsub("NA","NULL",linha)
                  linha = gsub("NaN","NULL",linha)
                  insert_sql2 = paste("INSERT INTO \"Municipio\".", tabela, " ", varnames, 
                                      " VALUES (", linha, ") ON CONFLICT ON CONSTRAINT ", sqlconstr, " DO
                               UPDATE SET ",updates, sep="")
                  try(dbGetQuery(datasource, insert_sql2))    
                  
            }
            # escrevendo
            1:nrow(dados) %>% map(escreve_linha)
            refresh_sql = "REFRESH MATERIALIZED VIEW uf_total_view;"
            try(dbGetQuery(datasource, refresh_sql))
            message(paste("dados escritos na tabela", tabela))
            
      }
      dados
}
      

#update.alerta (deprecated) ----------------------------------------
#'@title Define conditions to issue a 4 level alert Green-Yellow-Orange-Red for any city or region.
#'@description Yellow is raised when environmental conditions required for
#'positive mosquito population growth are detected, green otherwise.Orange 
#'indicates evidence of sustained transmission, red indicates evidence of 
#'an epidemic scenario. For most places weather stations are provided in the database and choice
#'is authomatic according to data quality. But it can also be provided manually (not implemented).
#'@param city city's geocode (6 or 7 digits).
#'@param region full name of 'regional' or state (same name present in the database).
#'@param state full name of state (same name present in the database). Required if there are more than one region with the same name.
#'@param temp_station code of the meteorological station for temperature. 
#'If not provided, use default from database. To be implemented.  
#'@param pars list of parameters for the alerta, defined in config.R
#'@param crit criteria for the alert colors, defined in configglobal.R
#'@param GT list with the generation time distribution . Default is dengue
#'@param cid10 default is A90 (dengue). Chik = A92.0, Zika = A92.8
#'@param adjustdelay Default is TRUE, if F, there is no delay adjustment and estimated = observed.
#'@param delaymethod Defaut is "fixedprob", alternative is "bayesian". Only used if adjustdelay=T
#'@param writedb TRUE if it should write into the database, default is FALSE.
#'@param sefinal if given, it stops at that week
#'@return data.frame with the week condition and the number of weeks within the 
#'last lag weeks with conditions = TRUE.
#'@examples
#' # Parameters for the model
#'criteriaU = setCriteria(rule = "Af", val = c("tcrit"="22","preseas"="10","inccrit"="100"))
#'# Running the model:
#'res <- update.alerta(city = c(3549805,3302205), crit = criteriaU, datasource = con,sefinal=201850)
#'res <- update.alerta(region = "Norte", state = "Rio de Janeiro", pars = pars.RJ, crit = criteriaU, adjustdelay=T, datasource = con,
#'sefinal=201704, delaymethod="fixedprob")
#'tail(res$data)
# 
# update.alerta <- function(city, region, state, pars, crit, GT = list(gtdist = "normal", meangt=3, sdgt=1.2), cid10 = "A90", writedb = FALSE,
#                           datasource, sefinal,adjustdelay=T, delaymethod="fixedprob"){
#       
#       # Getting metadata from table regional_saude
#       if(!missing (city)) { # if updating a single city
#             if(nchar(city) == 6) city <- sevendigitgeocode(city) 
#             dd <- read.parameters(city = city, datasource = datasource)
#       }
#       
#       if (!missing(region)){ # if one or more regionais
#             dd <- read.parameters(region = region, state = state, datasource=datasource)     
#       } 
#       
#       if ((missing(region) & missing(city) &!missing(state)))  {
#             dd <- read.parameters(state = state, datasource=datasource)    
#       }
#       #
#       nlugares = nrow(dd)[1]
#       if (nlugares == 0) stop("A cidade ou regiao ou estado nao foi encontrada(o)")
#       
#       print(paste("sera'feita analise de",nlugares,"cidade(s):"))
#       print(dd$geocodigo)      
#       
#       # 
#       message("obtendo dados de clima ...")
#       estacoes <- unique(c(dd$estacao_wu_sec, dd$codigo_estacao_wu))
#       cli <- list()
#       allvars.cli <- c("temp_min","temp_med","temp_max","umid_min","umid_med","umid_max",
#                        "pressao_min","pressao_med","pressao_max")
#       
#       for (k in 1:length(estacoes)) {
#             cliwu <- getWU(stations = estacoes[k],var=allvars.cli
#                            ,datasource = datasource)
#             #message("estacao", k, "tem dimensao",nrow(cliwu))
#             if (!is.null(cliwu)){
#                   if (!missing(sefinal)) cliwu =  subset(cliwu,SE<=sefinal)
#             } 
#             if (nrow(cliwu)>0){
#                   cli[[k]] <- cliwu
#                   names(cli)[k]<-as.character(unique(cli[[k]]$estacao))      
#             }
#       }
#       
#       #names(cli) <-estacoes
#       estacoes.validas <- names(cli)
#       print(estacoes.validas)
#       alertas <- list()
#       for (i in 1:nlugares){ # para cada cidade ...
#             
#             geocidade = dd$geocodigo[i]
#             lastdatewu = NA
#             
#             # escolhendo a melhor estacao meteorologica com base na temperatura:
#             estacao_sec = dd$estacao_wu_sec[i] # nome da estacao prioritaria
#             na_sec = 1; na_pri = 1 
#             if (estacao_sec %in% estacoes.validas){
#                   dadoscli_sec <- cli[[estacao_sec]] # temperatura
#                   
#                   na_sec = sum(is.na(dadoscli_sec$temp_min))/dim(dadoscli_sec)[1] # prop dados faltantes
#                   if (na_sec < 1)lastdate_sec <- dadoscli_sec$SE[max(which(is.na(dadoscli_sec$temp_min)==FALSE))]  # ultima data 
#                   estacao = estacao_sec
#             }
#             
#             estacao_pri = dd$codigo_estacao_wu[i] # nome da estacao substituta
#             if(estacao_pri %in% estacoes.validas){
#                   dadoscli_pri <- cli[[estacao_pri]] # temp na estacao substituta
#                   na_pri = sum(is.na(dadoscli_pri$temp_min))/dim(dadoscli_pri)[1] # prop dados faltantes
#                   if (na_pri < 1)lastdate_pri <- dadoscli_pri$SE[max(which(is.na(dadoscli_pri$temp_min)==FALSE))]  # ultima data        
#             }
#             
#             if(na_sec==1 & na_pri==1) message("WARNING: As duas estacoes met. da ", geocidade, " não tem dados de temperatura")
#             if(na_sec==1 & na_pri!=1) {estacao = estacao_pri; lastdatewu = lastdate_pri}
#             if(na_sec!=1 & na_pri==1) {estacao = estacao_sec; lastdatewu = lastdate_sec}      
#             if(na_sec!=1 & na_pri!=1){
#                   lastdatewu = ifelse(lastdate_sec>=lastdate_pri , lastdate_sec, lastdate_pri)
#                   estacao = ifelse(lastdate_sec>=lastdate_pri, estacao_sec, estacao_pri)
#             }
#             ##
#             print(paste("(Cidade ",i,"de",nlugares,")","Rodando alerta para ", geocidade, "usando estacao", estacao,"(ultima leitura:", lastdatewu,")"))
#             
#             # --------------- consulta dados do sinan
#             dC0 = getCases(city = geocidade, cid10 = cid10, datasource=datasource) 
#             
#             # --------------- consulta dados do tweet apenas se for dengue 
#             if(cid10 == "A90") dT = getTweet(city = geocidade, lastday = Sys.Date(),datasource=datasource) 
#             dW = cli[[estacao]]
#             
#             # cortando os dados para a janela temporal solicitada
#             if (!missing(sefinal)){
#                   dC0 <-subset(dC0, SE<=sefinal)
#                   if(cid10 == "A90") dT <- subset(dT, SE<=sefinal)
#             }
#             
#             # junta os dados
#             if(cid10 == "A90") {d <- mergedata(cases = dC0, climate = dW, tweet = dT)}
#             else{
#                   d <- mergedata(cases = dC0, climate = dW)
#                   d$tweet <- NA
#             }
#             
#             # ----------- interpolacao e extrapolação das variaveis climaticas
#             
#             vars.cli <-which(names(d)%in%allvars.cli) # posicao das variaveis climaticas em d
#             
#             for (j in vars.cli) {
#                   if (is.na(tail(d[,j])[1])) try(d[,j] <-nafill(d[,j], rule="arima"))  
#             }
#             
#             # parsi e' pars de uma unica cidade. Atualmente os limiares sao lidos do banco de dados
#             # E'preciso extrair no caso de region 
#             if (nlugares > 1) {
#                   d$nome_regional <- dd$nome_regional[dd$geocodigo==geocidade]
#                   parsi <- pars[[d$nome_regional[1]]]
#             } else {
#                   parsi <- pars
#             }
#             
#             # Limiares
#             parsi$preseas <- dd$limiar_preseason[i]
#             parsi$posseas <- dd$limiar_posseason[i]
#             parsi$inccrit <- dd$limiar_epidemico[i]
#             
#             if (!missing(sefinal)) d <- subset(d,SE<=sefinal)
#             # preenchendo potenciais missings
#             d$cidade[is.na(d$cidade)==TRUE] <- geocidade
#             d$nome[is.na(d$nome)==TRUE] <- na.omit(unique(d$nome))[1]
#             d$pop[is.na(d$pop)==TRUE] <- na.omit(unique(d$pop))[1]
#             
#             # se tiver ajuste de atraso pelo metodo tradicional, usar plnorm, senao pdig = 1 
#             if(adjustdelay == T){
#                   if(delaymethod=="fixedprob"){
#                         pdig <- rep(1, 20*7)[2:20]
#                         if(cid10=="A90") pdig <- plnorm((1:20)*7, parsi$pdig[1], parsi$pdig[2])[2:20]
#                         if(cid10=="A92.0") pdig <- plnorm(seq(7,20,by=7), parsi$pdigChik[1], parsi$pdigChik[2])
#                         #p <- plnorm(seq(7,20,by=7), pars$pdig[1], pars$pdig[2])
#                         dC2 <- adjustIncidence(d, pdig = pdig, method = "fixedprob") # ajusta a incidencia
#                   }
#                   if(delaymethod=="bayesian") {
#                         dC2 <- adjustIncidence(d, method = "bayesian")
#                   }
#             }else{
#                   dC2 <- d
#                   dC2$tcasesmed <- dC2$casos
#                   dC2$tcasesICmin <- dC2$casos
#                   dC2$tcasesICmax <- dC2$casos
#             }
#             
#             
#             dC3 <- Rt(dC2, count = "tcasesmed", gtdist=GT$gtdist, meangt=GT$meangt, sdgt = GT$sdgt) # calcula Rt
#             
#             alerta <- fouralert(dC3, pars = parsi, crit = crit, pop=dd$pop[i], miss="last") # calcula alerta
#             nome = dd$nome[i]
#             nick <- gsub(" ", "", nome, fixed = TRUE)
#             #names(alerta) <- nick
#             N = dim(alerta$indices)[1]
#             print(paste("nivel do alerta de ",nome,":", alerta$indices$level)[N])
#             
#             if (nlugares > 1) {
#                   alertas[[i]]<-alerta
#                   names(alertas)[i]<-nick
#             } 
#             if (writedb == TRUE) {
#                   res <- write_alerta(alerta)
#                   #write.csv(alerta,file=paste("memoria/", nick,hoje,".csv",sep="")) 
#             }
#       }
#       
#       res = alerta
#       if(nlugares > 1) res = alertas
#       res
# }
