# PROJETO ALERTA DENGUE -------------------------------------
# Funcoes de calculo do alerta 
# Claudia Codeco 2015
# -----------------------------------------------------------

#setCriteria -------------------------------------------------------------------
#'@title Define rules to issue a four level alert Green-Yellow-Orange-Red.
#'@description The criteria for transition between colors (alert levels) can be 
#'chosen from existing rules or can be specified by the user. The built in rules are: 
#'Af (minimum temperature defines yellow) and Aw (humidity does).   
#'@param rule either a built-in rule ("Af" or "Aw") or a list with three elements defining criteria for transition to yellow (level 2),
#' orange (level 3) and red (level 4). See description.
#'@param delays list with three elements, each one is a vector: c(delay to turn on, delay to turn off)
#'@param values named vector of values for the critical parameters. Use character.   
#'@return list with rules. To be useful, this list must contain variables that match
#' those in the data.  
#'@examples
#'Af is originally created for dengue in Southeast Brazil
#'setCriteria(rule="Af")
#'Defining a rule
#'myrule = list(crity = "temp_min > 25 & casos > 0", crito = "p1 > 0.9 & inc > 1", 
#'critr = "inc > 100 & casos > 10")
#'mydelay = list(delayy = c(3,1), delayo = c(3,1), delayr = c(2,2))
#'setCriteria(rule = myrule, delays = mydelay)
#'Defining values
#'val = c(varcli ="temp_min", "clicrit"="22","limiar_preseason"="10","limiar_epidemico"="100")
#'setCriteria(rule="Af",values=val)

setCriteria <- function(rule=NULL, values=NULL, 
                        delays = list(delayy = c(0,0), delayo = c(0,0), delayr = c(0,0))){
      
      ##
      if(rule[1] == "Af"){
                  criteria <- list(
                  crity = c("temp_min > clicrit & casos > 0", 3, 3), #3,2
                  crito = c("p1 > 0.95 & inc > limiar_preseason", 3, 2), #3,2
                  critr = c("inc > limiar_epidemico & casos > 5", 2, 2) #2,2
            )
             
       } else if (rule[1] == "Aw"){
                   criteria = list(
                   crity = c("umid_max > clicrit & casos > 0", 3, 2), #3,2
                   crito = c("p1 > 0.95 & inc > limiar_preseason", 3, 2), #3,2
                   critr = c("inc > limiar_epidemico & casos > 5", 2, 2) #2,2
             )
       } else {
            criteria<-lapply(1:3, function(x) c(rule[[x]], delays[[x]]))
            names(criteria) <- c("crity","crito","critr")
      }
      # substituting values
      if(!is.null(values)) {
            stopifnot(rule[1] == "Af" & values[["varcli"]] == "temp_min" | 
                      rule[1] == "Aw" & values[["varcli"]] == "umid_max")
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
#'@param obj dataset with data to feed the alert, containing the variables specified in crit.
#'@param crit criteria for the alert colors. See setCriteria()
#'@param miss how missing data is treated. "last" if last value is repeated. 
#'It is currently the only option
#'@return returns an object of class "alerta" containing four elements: the data, 
#'the alert indices, and the rules used to define the indices.  
#'@examples
#' # Parameters of the alert model
#'val = c(varcli ="temp_min", "clicrit"="22","limiar_preseason"="10","limiar_epidemico"="100")
#'criteria = setCriteria(rule="Af",values=val)
#'# Get, organize data 
#'cas = getCases(cities = 3304557, cid10 = "A90", datasource=con) %>% 
#'      Rt(count = "casos",gtdist="normal", meangt=3, sdgt = 1) %>%
#'      mutate(inc = casos/pop*100000)
#'cli = getWU(stations = 'SBGL', vars=c("temp_min"), datasource=con) %>%
#'      mutate(temp_min = nafill(temp_min, rule = "arima")) 
#'# Calculate alert      
#'ale <- plyr::join_all(list(cas,cli),by="SE") 
#'resf <- fouralert(ale, crit = criteria)
#'# Better visualization
#'tail(tabela.historico(resf))


fouralert <- function(obj, crit, miss="last"){
      le <- nrow(obj)
      
      # criteria
      #cyellow = crit[[1]]; corange = crit[[2]]; cred = crit[[3]]
      parsed_rules <- lapply(crit, function(x) parse(text=x[1]))
      delay_turnon <- lapply(crit, function(x) as.numeric(x[[2]]))
      delay_turnoff <- lapply(crit, function(x) as.numeric(x[[3]]))
      
      # accumulating condition function
      accumcond <- function(vec, lag) {
            if (lag == 1) return(vec)
            le <- length(vec)
            ac <- vec[lag:le]
            for(j in 1:(lag-1)) ac <- rowSums(cbind(ac, vec[(lag-j):(le-j)]), na.rm = TRUE)
            c(rep(NA,(lag-1)), ac)
      }
      
      # calculating each condition (week and accumulated)  
      assertcondition <- function(dd, nivel){
            condtrue <- with(dd, as.numeric(eval(parsed_rules[[nivel]])))
            mi <- which(is.na(condtrue)) # missing conditions
            if (miss == "last"){  
                  if(le %in% mi) message("missing condition, repeating last value")
                  for (i in mi[mi!=1]) condtrue[i] <- condtrue[i-1]
            }
            # counting accumulated conditions
            ncondtrue <- accumcond(condtrue, delay_turnon[[nivel]])
            cbind(condtrue, ncondtrue)
      }
      indices <- data.frame(cbind(assertcondition(obj, 1),
                                  assertcondition(obj, 2),
                                  assertcondition(obj, 3)))
      names(indices) <- c("cytrue", "nytrue","cotrue", "notrue","crtrue", "nrtrue")
      
      # setting the alert level when delay_on is reached(1 = green, 2 = yellow, 3 = orange, 4 = red)
      indices$level <- 1
      indices$level[indices$nytrue == delay_turnon[1]] <-2
      indices$level[indices$notrue == delay_turnon[2]] <-3
      indices$level[indices$nrtrue == delay_turnon[3]] <-4
      
    
      # making it orange if now is pRt>crit and in the past 3 weeks, alert was orange at least once 
      #for (k in 5:dim(indices)[1]){
      #      if (indices$level[k] != 4 & indices$cotrue[k] == 1 & 
      #          any(indices$level[(k-2):k]==3)) indices$level[k]<-3
      #}
      
      # from orange-red to yellow:
      # if inc > 0, and rt was orange or red at least once in the past 8 weeks -> level yellow
      indices$level[intersect(x = which(obj$inc>0), 
                              y = which(zoo::rollapply(indices$level,8,function(x) any(x>=3))))]<-2
      
      # delayed turnoff
      delayturnoff <- function(level){
            delay_level = delay_turnoff[[(level-1)]]# as.numeric(as.character(cond[3])) 
            
            ifelse (delay_level == 0, return(indices),
                    {pos <- which(indices$level==delay_level) %>% # weeks with alert at level delay_level
                    lapply(.,function(x)x+seq(0,delay_level)) %>% # current and subsequent weeks 
                          unlist() %>% 
                          unique()
                    pos <- pos[pos<=le] # remove inexisting rows
                    indices$level[pos] <- unlist(lapply(indices$level[pos], function(x) max(x,2)))
                    return(indices)
            })
      }
            
      indices <- delayturnoff(level=4)
      indices <- delayturnoff(level=3)
      indices <- delayturnoff(level=2)
      ale <- list(data=obj, indices=indices, crit = crit, n=4)
      class(ale)<-"alerta" 
      return(ale)      
}

#pipe.infodengue ---------------------------------------------------------------------
#'@title pipeline used by infodengue 
#'@description wrap of functions used by Infodengue.
#'@param cities In general, a vector of 7-digit geocodes. If it is a data.frame containing geocodes
#' and all parameters, these will replace the database's parameters. 
#'@param cid10 default is A90 (dengue). Chik = A92.0, Zika = A92.8
#'@param narule how to treat missing climate data. Do nothing (default), "zero" fills 
#'with zeros, "linear" for linear interpolation, "arima" for inter and extrapolation.
#'@param finalday if provided, uses only disease data reported up to that day
#'@param nowcasting  "fixedprob" for static model, "bayesian" for the dynamic model.
#'"none" for not doing nowcast (default) 
#'@param writedb TRUE if it should write into the database, default is FALSE.
#'@return data.frame with the week condition and the number of weeks within the 
#'last lag weeks with conditions = TRUE.
#'@examples
#'cidades <- getCidades(regional = "Norte",uf = "Rio de Janeiro",datasource = con)
#'res <- pipe.infodengue(cities = cidades$municipio_geocodigo, cid10 = "A90", 
#'finalday= "2018-08-12",nowcasting="none")
#'class(res)
#'class(res[[1]])
#'head(tabela.historico(res))
#'# User's parameters
#'dd <- read.parameters(cities = c(3300159,3302403)) %>% mutate(limiar_epidemico = 100)
#'res <- pipe.infodengue(cities = dd, cid10 = "A90", 
#'finalday= "2018-08-12",nowcasting="none")
#'

pipe.infodengue <- function(cities, cid10="A90", finalday = Sys.Date(), nowcasting="none", 
                            narule=NULL, writedb = FALSE, datasource = con){
      
      # If cities is a vector of geocodes, the pipeline reads the parameters from the dataframe
      if (class(cities) %in% c("integer","numeric")) {
            pars_table <- read.parameters(cities = cities, cid10 = cid10, datasource)
            message("reading parameters from database")
      } else {
            if(all(c("municipio_geocodigo","limiar_preseason",
                     "limiar_posseason","limiar_epidemico",
                     "varcli","clicrit","cid10","codmodelo") %in% names(cities))) {
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
      comando <- paste("SELECT id, nome_regional, municipio_geocodigo, codigo_estacao_wu, estacao_wu_sec from 
                       \"Dengue_global\".regional_saude WHERE municipio_geocodigo IN (", sqlcity, 
                       ")" , sep="")
      city_table <- dbGetQuery(datasource,comando)
      
      estacoes <- unique(c(city_table$estacao_wu_sec, city_table$codigo_estacao_wu))
      print("usando dados de clima das estacoes:")
      print(estacoes)
      
      # Reading the meteorological data
      #print('Obtendo os dados de clima...')
      varscli <- unique(pars_table$varcli)
      cliwu <- getWU(stations = estacoes, vars = varscli, finalday = finalday,datasource)
      
      # Reading Cases
      #print("Obtendo dados de notificacao ...")
      
      casos <- getCases(cidades, lastday = finalday, cid10 = cid10) %>%
            mutate(inc = casos/pop*100000)
      
      # Reading tweets 
      if(cid10 == "A90"){
            print("Reading tweets...")
            dT = getTweet(cidades, lastday = finalday, cid10 = "A90", datasource)
      }
      
      res <- list()
       # para cada cidade ...
      
      calc.alerta <- function(x){  #x = cities[i]
            # params
            parcli.x <- city_table[city_table$municipio_geocodigo == x, c("codigo_estacao_wu", "estacao_wu_sec")]      
            varcli.x <- pars_table$varcli[city_table$municipio_geocodigo == x]
            
            # escolhe a melhor serie meteorologica para a cidade, 
            cli.x <- bestWU(series = list(cliwu[cliwu$estacao == parcli.x[[1]],],
                                          cliwu[cliwu$estacao == parcli.x[[2]],]), var = varcli.x)
            
            lastdatewu <- cli.x$SE[max(which(is.na(cli.x[,varcli.x])==FALSE))]
            print(paste("Rodando alerta para ", x, "usando estacao", cli.x$estacao[1],"(ultima leitura:", lastdatewu,")"))
            
            # climate data interpolation using arima 
            if(!is.null(narule)) cli.x[,varcli.x] <- nafill(cli.x[,varcli.x], rule = narule) 
            
            # casos + nowcasting + Rt + incidencia 
            
            cas.x <- casos %>% 
                  filter(cidade == x) %>%
                  adjustIncidence(method = nowcasting) %>%
                  Rt(count = "tcasesmed",gtdist="normal", meangt=3, sdgt = 1) %>%
                  mutate(inc = tcasesmed/pop*100000)  
            
            # Joining all data
            if(exists("dT")) {
                  ale <- plyr::join_all(list(cas.x,cli.x,dT))
            } else{
                  ale <- plyr::join_all(list(cas.x,cli.x))
                  ale$tweets <- 0
            }
            
            # build rules
            crit.x <- pars_table[pars_table$municipio_geocodigo==x,] # parameters
            crit.x.vector <- structure(as.character(crit.x), names = as.character(names(crit.x))) # dataframe -> vector
            criteriaU <- setCriteria(rule = crit.x$codmodelo, values = crit.x.vector) # valued criteria
            
            # Apply alert rules
            y <- fouralert(ale, crit = criteriaU)  # apply alert 
            
            print(paste("nivel do alerta de ",cas.x$nome[1],":", tail(y$indices$level,1)))
            y
      }      
      
      res <- lapply(cidades, calc.alerta) %>% setNames(cidades) # antes o nome era character, agora e o geocodigo
      #       nick <- gsub(" ", "", nome, fixed = TRUE)
      #       #names(alerta) <- nick
      
      if (writedb == TRUE) write.alerta(alerta, write = "db")
      
      res
}



#alertaRio ---------------------------------------------------------------------
#'@title 4 level alert Green-Yellow-Orange-Red for Rio de Janeiro.
#'@description Yellow is raised when environmental conditions required for
#'positive mosquito population growth are detected, green otherwise.Orange 
#'indicates evidence of sustained transmission, red indicates evidence of 
#'an epidemic scenario.  
#'@param pars parameters of the alert.
#'@param naps subset of vector 0:9 corresponding to the id of the APS. Default is all of them.
#'@param datasource it is the name of the sql connection.
#'@param se last epidemiological week (format = 201401) 
#'@param cid10 default is A90 (dengue). Chik = A920.
#'@param delaymethod atribbute of adjuntincidence. "fixedprob" or "bayesian"
#'@param verbose FALSE
#'@return list with an alert object for each APS.
#'@examples
#'params <- list(pdigChik = c(2.5016,1.1013), tcrit=22, inccrit=100, preseas = 14.15, posseas = 18)
#'criter <- list(
#'crity = c("temp_min > tcrit | (temp_min < tcrit & inc > preseas)", 3, 0),
#'crito = c("p1 > 0.9 & inc > preseas", 2, 2),
#'critr = c("inc > inccrit", 1, 2)
#')
#'ini = Sys.time()
#'alerio2 <- alertaRio(naps = 0, pars=params, cid = "A920", crit = criter, datasource=con, se=201804,
#'delaymethod="fixedprob",verbose=FALSE)
#'Sys.time()-ini

#'names(alerio2)

alertaRio <- function(naps = 0:9, pars, crit, datasource, se, cid10 = "A90", verbose = TRUE, delaymethod = "fixedprob"){
      
      message("obtendo dados de clima e tweets ...")
      if(cid10 == "A90") tw = getTweet(city = 3304557, cid10="A90", datasource = datasource) 
      cli.SBRJ = getWU(stations = 'SBRJ', datasource=datasource)
      cli.SBJR = getWU(stations = 'SBJR', datasource=datasource)
      cli.SBGL = getWU(stations = 'SBGL', datasource=datasource)
      
      if (verbose){
            message("As ultimas datas no banco sao:")
            print(paste("Ultimos registros de dengue:",lastDBdate("sinan", city=330455,datasource=datasource)))
            #print(paste("Ultimos registros de tweets:",lastDBdate("tweet", city=330455,datasource=datasource)))
            
            out = readline("deseja continuar (y/n)?")
            if(out == "n") stop("alerta interrompido pelo usuario")
      }
      
      APS <- c("APS 1", "APS 2.1", "APS 2.2", "APS 3.1", "APS 3.2", "APS 3.3"
               , "APS 4", "APS 5.1", "APS 5.2", "APS 5.3")[(naps + 1)]
      
      # parametros do modelo de ajuste de atraso (caso fixedprob)
      if(cid10=="A90") p <- plnorm(seq(7,20,by=7), pars$pdig[1], pars$pdig[2])
      if(cid10=="A920") p <- plnorm(seq(7,20,by=7), pars$pdigChik[1], pars$pdigChik[2])
      
      res <- vector("list", length(APS))
      names(res) <- APS
      for (i in 1:length(APS)){
            message(paste("rodando", APS[i],"..."))
            cas = getCasesinRio(APSid = naps[i], cid10 = cid10, datasource=datasource)
            d <- merge(cas, cli.SBRJ, by.x = "SE", by.y = "SE")
            
            # dados de tweet so existem para dengue
            if (cid10=="A90") d <- merge(d, tw, by.x = "SE", by.y = "SE")
            else d$tweet <- NA
            # interpolacao e extrapolacao do clima
            if (is.na(tail(d$temp_min)[1])) try(d$temp_min <-nafill(d$temp_min, rule="arima"))   
            # delay model
            casfit<-adjustIncidence(obj=d, pdig = p,method = delaymethod)
            if(cid10=="A90") casr<-Rt(obj = casfit, count = "tcasesmed", gtdist="normal", meangt=3, sdgt = 1)   
            if(cid10=="A920") casr<-Rt(obj = casfit, count = "tcasesmed", gtdist="normal", meangt=2, sdgt = 1)   
              
            res[[i]] <- fouralert(obj = casr[casr$SE <= se,], pars = pars, crit = crit, pop=cas$pop[1])
      }
            
      res
}


#plot.alerta --------------------------------------------------------------------
#'@title Plot the time series of warnings.
#'@description Function to plot the output of tabela.historico. 
#'@param obj object created by tabela.historico()
#'@param var variable to be plotted, usually "cases", or "p_inc100k". 
#'@param cores colors corresponding to the levels 1, 2, 3, 4.
#'@param ini first epidemiological week. If not stated, use the first one available
#'@param fim last epidemiological week. If not stated, use the last one available 
#'@return a plot
#'@examples
#' # Parameters for the model
#'cidades <- getCidades(regional = "Norte",uf = "Rio de Janeiro",datasource = con)
#'res <- pipe.infodengue(cities = cidades$municipio_geocodigo, cid10 = "A90", 
#'finalday= "2018-08-12",nowcasting="none")
#'restab <- tabela.historico(res)
#'plot.alerta(restab, geocodigo = cidades$municipio_geocodigo, var="casos")

plot.alerta<-function(obj, geocodigo, var = "casos", cores = c("#0D6B0D","#C8D20F","orange","red"), 
                      ini=201001, fim=202001, ylab=var, yrange, salvar = FALSE, 
                      nome.fig = "grafico", datasource=con){
      
      stopifnot(var %in% names(obj))
      if(missing(geocodigo)) geocodigo <- unique(restab$municipio_geocodigo)
      if(missing(ini))ini <- min(restab$SE)
      if(missing(ini))fim <- max(restab$SE)
      
      d <- obj %>% filter(SE >= ini & SE <= fim)
      
      # get the thresholds
      pars_table <- read.parameters(cities = geocodigo, 
                                    cid10 = d$CID10[1], datasource = datasource) 
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
                  abline(h=pars$limiar_preseason, lty=2, col="darkgreen")
                  abline(h=pars$limiar_epidemico, lty=2, col="red")
            }
            if (var == "p_inc100k") {
                  abline(h=pars$limiar_preseason/dd$pop[1]*100000, lty=2, col="darkgreen")
                  abline(h=pars$limiar_epidemico/dd$pop[1]*100000, lty=2, col="red")
            }
            if(var == "temp_min" & pars$clicrit == "temp_min") abline(h = pars$clicrit, lty=2, col = "yellow")
            if(var == "umid_max" & pars$clicrit == "umid_max") abline(h = pars$clicrit, lty=2, col = "yellow")
            if(salvar == TRUE) dev.off()
      }
      
      geocodigo %>% map(plota)
      
      #par(new=T)
      #plot(dd[,var], col="white", type="l",axes=FALSE , xlab="", ylab="" ) #*coefs[2] + coefs[1]
      #        axis(1, pos=0, lty=0, lab=FALSE)
      #        axis(4, las=2, cex=0.6 ,col.axis="darkgrey")
      #        mtext("casos",4, line=3,col="darkgrey", cex=0.7)
      #       }
}
      
#map.Rio --------------------------------------------------------------------
#'@title Plot the alert map for Rio de Janeiro city.
#'@description Function to plot a map of the alert 
#'@param obj object created by the twoalert and fouralert functions.
#'@param var to be ploted in the graph, usually cases when available.  
#'@param cores colors corresponding to the levels 1, 2, 3, 4.
#'@param filename if present, the map is saved.
#'@param dir directory where map will be saved. 
#'@param resol dpi png resolution, default is 200
#'@return a map
#'@examples
#'map.Rio(alerio)

map.Rio<-function(obj, cores = c("green","yellow","orange","red"), data, datasource=con,
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
      legend("bottom",fill=cores,c("atividade baixa","condicoes favoraveis transmissao","transmissao sustentada","atividade alta"),bty="n",cex=0.6)
      par(cex.main=0.7)
      title(paste0( "Mapa MRJ por APs \n"," Semana ",substr(ultima_se,5,6)," de ",
                    substr(ultima_se,1,4)),line=-1)
      
    
      if(!missing(filename)) {dev.off()} #salvar
      
      
}

#geraMapa --------------------------------------------------------------------
#'@title Plot the alert map for any state.
#'@description Function to plot a map of the alert 
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
#'@param resol dpi png resolution, default is 200
#'@return a map
#'@examples
#' # Parameters for the model
#'criteriaU = list(crity = c("temp_min > tcrit", 3, 1),
#'crito = c("p1 > 0.95 & inc > preseas", 3, 1),
#'critr = c("inc > inccrit", 2, 2))
#'gtdist="normal"; meangt=3; sdgt = 1.2
#'pars.RJ <- NULL
#'pars.RJ[["Norte"]] <- list(pdig = c(2.997765,0.7859499),tcrit=22, ucrit = NA, inccrit = 100, preseas=8.283, posseas = 7.67878514885295, legpos="bottomright")
#'# Running the model:
#'res <- update.alerta(region = "Norte", state = "Rio de Janeiro", pars = pars.RJ, crit = criteriaU, adjustdelay=F, datasource = con,
#'sefinal=201704, delaymethod="fixedprob")
#'cidades = getCidades(regional = "Norte", uf = "Rio de Janeiro", datasource = con)["nome"]
#'geraMapa(alerta=res, subset = cidades, se=201704, datasource=con, shapefile="shape/33MUE250GC_SIR.shp",
#'varid="CD_GEOCMU", titulo="RJ-Norte", legpos="topright")

geraMapa<-function(alerta, subset, cores = c("green","yellow","orange","red"), legpos="bottomright", se, 
                   datasource, shapefile, varid, varname, titulo="", filename, dir="",caption=TRUE, resol = 200){
      
      require(maptools,quietly = TRUE,warn.conflicts = FALSE)
      
      N = length(alerta) # numero de cidades presentes no objeto alerta.
      
      # subset de cidades para o mapa
      if (!missing(subset)){
            ale <- alerta[which(names(alerta) %in% gsub(" ","", subset$nome))]
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


#tabela.historico --------------------------------------------------------------------
#'@title Write the alert object into the database.
#'@description Function to write the alert results into the database. 
#'@param obj object created by the pipeline.
#'@return data.frame with the data to be written. 
#'@examples
#'cidades <- getCidades(regional = "Norte",uf = "Rio de Janeiro",datasource = con)
#'res <- pipe.infodengue(cities = cidades$municipio_geocodigo, cid10 = "A90", 
#'finalday= "2013-01-10")
#'restab <- tabela.historico(res)

tabela.historico <- function(obj, versao = Sys.Date()){
      
      # --------- create single data.frame ------------------#
      # if object created by pipe.infodengue():
      if(class(obj)=="list" & class(obj[[1]])=="alerta"){
            data <- transpose(obj)[[1]] %>% bind_rows()   # unlist data
            indices <- transpose(obj)[[2]] %>% bind_rows()  #unlist indices      
      } else if (class(obj)=="alerta"){ #if object created directly by fouralert()
            data <- obj$data
            indices <- obj$indices
      }
      
      # defining the id (SE+julian(versaomodelo)+geocodigo+localidade)
      gera_id <- function(x) paste(data$cidade[x], data$Localidade_id[x], data$SE[x], 
                                   as.character(julian(versao)), sep="")
      id <- sapply(1:nrow(data), gera_id) 
      
      # modifying column names and adding some new cols for writing
      d<- data %>%
            rename(municipio_geocodigo = cidade,
                   municipio_nome = nome,
                   p_inc100k =inc) %>%
            mutate(casos_est = ifelse(has_name(.,"tcasesmed"),tcasesmed,NA),
                   casos_est_min = ifelse(has_name(.,"tcasesICmin"),tcasesICmin,NA),
                   casos_est_max = ifelse(has_name(.,"tcasesICmax"),tcasesICmax,NA),
                   p_rt1 = ifelse(is.na(p1),0,p1),
                   Localidade_id  = ifelse(is.na(localidade),0,localidade),
                   data_iniSE = SE2date(SE)$ini,
                   nivel = indices$level,
                   versao_modelo = as.character(versao),
                   id = id)
                   
      # ---------if writing into historico_alerta table ---------# 
      # se tiver ja algum registro com mesmo geocodigo e SE, esse sera substituido pelo atualizado.
      d
}

#write.alerta --------------------------------------------------------------------
#'@title Write historico.alerta into the database.
#'@description Function to write the pipeline results into the database. Receives the object created by the function historico.alerta
#'@param obj object created by historico.alerta()
#'@return the same data.frame from the input
#'@examples
#' # Parameters for the model
#'cidades <- getCidades(regional = "Norte",uf = "Rio de Janeiro",datasource = con)
#'res <- pipe.infodengue(cities = cidades$municipio_geocodigo[1], cid10 = "A90", 
#'finalday= "2018-08-12",nowcasting="none")
#'restab <- tabela.historico(res[1]) 
#'write.alerta(restab[1:5,])

write.alerta<-function(d, datasource = con){
      
      cid10 = unique(d$CID10)
      stopifnot(length(cid10)==1)
      
      # nomes das tabelas para salvar os historicos:
      if(cid10=="A90") {tabela <-  "Historico_alerta"; constr.unico = "alertas_unicos"}
      if(cid10=="A92.0") {tabela <-  "Historico_alerta_chik"; constr.unico = "alertas_unicos_chik"}
      if(cid10=="A92.8") {tabela <-  "Historico_alerta_zika"; constr.unico = "alertas_unicos_zika"}
      if(!(cid10 %in% c("A90", "A92.0", "A92.8"))) stop(paste("não sei onde salvar histórico para o agravo", cid10))
      
      print(paste("writing alerta into table", tabela))
      
      # ------ vars to write --------
      dcolumns <- c("SE", "data_iniSE", "casos_est", "casos_est_min", "casos_est_max",
                    "casos","municipio_geocodigo","p_rt1","p_inc100k","Localidade_id",
                    "nivel","id","versao_modelo","municipio_nome")
      
      dados <- d %>% select(dcolumns)
      
      # ------ sql command -----
      varnamesforsql <- c("\"SE\"", "\"data_iniSE\"", "casos_est", "casos_est_min", "casos_est_max",
                          "casos","municipio_geocodigo","p_rt1","p_inc100k","\"Localidade_id\"",
                          "nivel","id","versao_modelo","municipio_nome")
      
      varnames.sql <- str_c(varnamesforsql, collapse = ",")
      updates = str_c(paste(varnamesforsql,"=excluded.",varnamesforsql,sep=""),collapse=",") # excluidos, se duplicado
      
      
      escreve_linha <- function(li){
            vetor <- dados[li,]
            linha = paste(vetor[1,1],",'",as.character(vetor[1,2]),"',", str_c(vetor[1,3:11], collapse=","),
                          ",",vetor[1,12],",'", as.character(vetor[1,13]),"','",as.character(vetor[1,14]),"'", sep="")
            linha = gsub("NA","NULL",linha)
            insert_sql = paste("INSERT INTO \"Municipio\".\"",tabela,"\" (" ,varnames.sql,") VALUES (", linha, ")ON CONFLICT ON CONSTRAINT ",constr.unico,"  
                                     DO UPDATE SET ",updates, sep="")
            try(dbGetQuery(datasource, insert_sql))    
            insert_sql
      }
      
      # escrevendo
      1:nrow(d) %>% map(escreve_linha)
      print("done")
}





#write.alertaRio --------------------------------------------------------------------
#'@title Write the Rio de janeiro alert into the database.
#'@description Function to write the alert results into the database. 
#'@param obj object created by the alertRio function and contains alerts for each APS.
#'@param write use "db" if data.frame should be inserted into the project database,
#' or "no" (default) if nothing is saved. 
#'@return data.frame with the data to be written. 
#'@examples
#'alerio2 <- alertaRio(naps = c(1,2), datasource=con)
#'res <- write.alertaRio(alerio2, write="db")
#'tail(res)

write.alertaRio<-function(obj, write = "no", version = Sys.Date()){
      
      listaAPS <- c("APS 1", "APS 2.1", "APS 2.2", "APS 3.1", "APS 3.2", "APS 3.3"
                    , "APS 4", "APS 5.1", "APS 5.2", "APS 5.3")
      APSlabel <- c("1.0", "2.1", "2.2", "3.1", "3.2", "3.3","4.0","5.1","5.2","5.3")
      stopifnot(names(obj) %in% listaAPS)
      
      n <- length(obj)
      dados <- data.frame()
      cid10 <- obj[[1]]$data$CID10[1]
      #dealing with synonimous cid
      if (cid10 == "A90") cid <- c("A90") # dengue, dengue hemorragica
      if (cid10 %in% c("A92", "A920","A92.0")) {cid <-c("A92", "A920","A92.0"); cid10 <- "A92.0"}  # chik
      if (cid10 %in% c("A92.8","A928")) {cid <- c("A92.8","A928"); cid10 <- "A92.8"} #zika
      if (!(cid10 %in% c("A90","A92.0","A92.8")))stop(paste("Eu nao conheco esse cid10",cid10))
      
      # nome da tabela no banco de dados e do respectivo constraint 
      if (cid10 == "A90") {tabela <- "alerta_mrj"; sqlconstr = "unique_aps_se"}
      if (cid10 == "A92.0") {tabela <- "alerta_mrj_chik"; sqlconstr = "unique_chik_aps_se"}
      
      for (i in 1:n){
            data <- obj[[i]]$data
            indices <- obj[[i]]$indices   
            cidade <- data$nome[1]
            # creating the data.frame with the required columns
            d <- data.frame(se = data$SE)
            d$aps <- APSlabel[(data$localidadeid[1]+1)]
            d$data <- SE2date(d$se)$ini
            d$tweet <- data$tweet
            d$casos <- data$casos
            d$casos_est <- data$tcasesmed
            d$casos_est_min <- data$tcasesICmin
            d$casos_est_max <- data$tcasesICmax
            d$tmin <- data$temp_min
            d$rt <- data$Rt
            d$p_rt1 <- data$p1
            d$p_rt1[is.na(d$p_rt1)] <- 0
            d$inc <- data$inc
            d$nivel <- indices$level

            if(write == "db"){
                  
                  # se tiver ja algum registro com mesmo aps e SE, esse sera substituido pelo atualizado.
                  
                  varnames <- "(se,aps,data,tweets,casos,casos_est,casos_estmin,casos_estmax,tmin,rt,prt1,
                  inc,nivel)"
                  
                  sepvarnames <- c("se","aps","data","tweets","casos","casos_est","casos_estmin","casos_estmax",
                                   "tmin","rt","prt1","inc","nivel")
                  
                  updates <- paste(sepvarnames[1],"=excluded.",sepvarnames[1],sep="")
                  for(i in 2:length(sepvarnames)) updates <- paste(updates, paste(sepvarnames[i],"=excluded.",
                                                                                  sepvarnames[i],sep=""),sep=",") 
                  
                  
                  stringvars = c(2,3)            
                  for (li in 1:dim(d)[1]){
                        linha = as.character(d[li,1])
                        for (i in 2:length(sepvarnames)) {
                              if (i %in% stringvars & !is.na(as.character(d[li,i]))) {
                                    value = paste("'", as.character(d[li,i]), "'", sep="")
                                    linha = paste(linha, value, sep=",")
                              }
                              else {linha = paste(linha, as.character(d[li,i]),sep=",")}
                        }
                        linha = gsub("NA","NULL",linha)
                        linha = gsub("NaN","NULL",linha)
                        #insert_sql2 = paste("INSERT INTO \"Municipio\".alerta_mrj " ,varnames, 
                        #                    " VALUES (", linha, ") ON CONFLICT ON CONSTRAINT unique_aps_se DO
                        #       UPDATE SET ",updates, sep="")
                        
                        insert_sql2 = paste("INSERT INTO \"Municipio\".", tabela, " ", varnames, 
                                            " VALUES (", linha, ") ON CONFLICT ON CONSTRAINT ", sqlconstr, " DO
                               UPDATE SET ",updates, sep="")
                        
                        try(dbGetQuery(con, insert_sql2))
                  }
            }
            
            refresh_sql = "REFRESH MATERIALIZED VIEW uf_total_view;"
            try(dbGetQuery(con, refresh_sql))
            message(paste("dados escritos na tabela", tabela))
            dados <- rbind(dados,d)
      }
      dados
}
      



#update.alerta ---------------------------------------------------------------------
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

update.alerta <- function(city, region, state, pars, crit, GT = list(gtdist = "normal", meangt=3, sdgt=1.2), cid10 = "A90", writedb = FALSE,
                          datasource, sefinal,adjustdelay=T, delaymethod="fixedprob"){
      
      # Getting metadata from table regional_saude
      if(!missing (city)) { # if updating a single city
            if(nchar(city) == 6) city <- sevendigitgeocode(city) 
            dd <- read.parameters(city = city, datasource = datasource)
      }
      
      if (!missing(region)){ # if one or more regionais
            dd <- read.parameters(region = region, state = state, datasource=datasource)     
      } 
      
      if ((missing(region) & missing(city) &!missing(state)))  {
            dd <- read.parameters(state = state, datasource=datasource)    
      }
      #
      nlugares = nrow(dd)[1]
      if (nlugares == 0) stop("A cidade ou regiao ou estado nao foi encontrada(o)")
      
      print(paste("sera'feita analise de",nlugares,"cidade(s):"))
      print(dd$geocodigo)      
      
      # -------------------------------------
      message("obtendo dados de clima ...")
      estacoes <- unique(c(dd$estacao_wu_sec, dd$codigo_estacao_wu))
      cli <- list()
      allvars.cli <- c("temp_min","temp_med","temp_max","umid_min","umid_med","umid_max",
                       "pressao_min","pressao_med","pressao_max")
      
      for (k in 1:length(estacoes)) {
            cliwu <- getWU(stations = estacoes[k],var=allvars.cli
                           ,datasource = datasource)
            #message("estacao", k, "tem dimensao",nrow(cliwu))
            if (!is.null(cliwu)){
                  if (!missing(sefinal)) cliwu =  subset(cliwu,SE<=sefinal)
            } 
            if (nrow(cliwu)>0){
                  cli[[k]] <- cliwu
                  names(cli)[k]<-as.character(unique(cli[[k]]$estacao))      
            }
      }
      
      #names(cli) <-estacoes
      estacoes.validas <- names(cli)
      print(estacoes.validas)
      alertas <- list()
      for (i in 1:nlugares){ # para cada cidade ...
            
            geocidade = dd$geocodigo[i]
            lastdatewu = NA
            
            # escolhendo a melhor estacao meteorologica com base na temperatura:
            estacao_sec = dd$estacao_wu_sec[i] # nome da estacao prioritaria
            na_sec = 1; na_pri = 1 
            if (estacao_sec %in% estacoes.validas){
                  dadoscli_sec <- cli[[estacao_sec]] # temperatura
                  
                  na_sec = sum(is.na(dadoscli_sec$temp_min))/dim(dadoscli_sec)[1] # prop dados faltantes
                  if (na_sec < 1)lastdate_sec <- dadoscli_sec$SE[max(which(is.na(dadoscli_sec$temp_min)==FALSE))]  # ultima data 
                  estacao = estacao_sec
            }
            
            estacao_pri = dd$codigo_estacao_wu[i] # nome da estacao substituta
            if(estacao_pri %in% estacoes.validas){
                  dadoscli_pri <- cli[[estacao_pri]] # temp na estacao substituta
                  na_pri = sum(is.na(dadoscli_pri$temp_min))/dim(dadoscli_pri)[1] # prop dados faltantes
                  if (na_pri < 1)lastdate_pri <- dadoscli_pri$SE[max(which(is.na(dadoscli_pri$temp_min)==FALSE))]  # ultima data        
            }
            
            if(na_sec==1 & na_pri==1) message("WARNING: As duas estacoes met. da ", geocidade, " não tem dados de temperatura")
            if(na_sec==1 & na_pri!=1) {estacao = estacao_pri; lastdatewu = lastdate_pri}
            if(na_sec!=1 & na_pri==1) {estacao = estacao_sec; lastdatewu = lastdate_sec}      
            if(na_sec!=1 & na_pri!=1){
                  lastdatewu = ifelse(lastdate_sec>=lastdate_pri , lastdate_sec, lastdate_pri)
                  estacao = ifelse(lastdate_sec>=lastdate_pri, estacao_sec, estacao_pri)
            }
            ##
            print(paste("(Cidade ",i,"de",nlugares,")","Rodando alerta para ", geocidade, "usando estacao", estacao,"(ultima leitura:", lastdatewu,")"))
            
            # --------------- consulta dados do sinan
            dC0 = getCases(city = geocidade, cid10 = cid10, datasource=datasource) 
            
            # --------------- consulta dados do tweet apenas se for dengue 
            if(cid10 == "A90") dT = getTweet(city = geocidade, lastday = Sys.Date(),datasource=datasource) 
            dW = cli[[estacao]]
            
            # cortando os dados para a janela temporal solicitada
            if (!missing(sefinal)){
                  dC0 <-subset(dC0, SE<=sefinal)
                  if(cid10 == "A90") dT <- subset(dT, SE<=sefinal)
            }
            
            # junta os dados
            if(cid10 == "A90") {d <- mergedata(cases = dC0, climate = dW, tweet = dT)}
            else{
                  d <- mergedata(cases = dC0, climate = dW)
                  d$tweet <- NA
            }
            
            # ----------- interpolacao e extrapolação das variaveis climaticas
            
            vars.cli <-which(names(d)%in%allvars.cli) # posicao das variaveis climaticas em d
            
            for (j in vars.cli) {
                  if (is.na(tail(d[,j])[1])) try(d[,j] <-nafill(d[,j], rule="arima"))  
            }
            
            # parsi e' pars de uma unica cidade. Atualmente os limiares sao lidos do banco de dados
            # E'preciso extrair no caso de region 
            if (nlugares > 1) {
                  d$nome_regional <- dd$nome_regional[dd$geocodigo==geocidade]
                  parsi <- pars[[d$nome_regional[1]]]
            } else {
                  parsi <- pars
            }
            
            # Limiares
            parsi$preseas <- dd$limiar_preseason[i]
            parsi$posseas <- dd$limiar_posseason[i]
            parsi$inccrit <- dd$limiar_epidemico[i]
            
            if (!missing(sefinal)) d <- subset(d,SE<=sefinal)
            # preenchendo potenciais missings
            d$cidade[is.na(d$cidade)==TRUE] <- geocidade
            d$nome[is.na(d$nome)==TRUE] <- na.omit(unique(d$nome))[1]
            d$pop[is.na(d$pop)==TRUE] <- na.omit(unique(d$pop))[1]
            
            # se tiver ajuste de atraso pelo metodo tradicional, usar plnorm, senao pdig = 1 
            if(adjustdelay == T){
                  if(delaymethod=="fixedprob"){
                        pdig <- rep(1, 20*7)[2:20]
                        if(cid10=="A90") pdig <- plnorm((1:20)*7, parsi$pdig[1], parsi$pdig[2])[2:20]
                        if(cid10=="A92.0") pdig <- plnorm(seq(7,20,by=7), parsi$pdigChik[1], parsi$pdigChik[2])
                        #p <- plnorm(seq(7,20,by=7), pars$pdig[1], pars$pdig[2])
                        dC2 <- adjustIncidence(d, pdig = pdig, method = "fixedprob") # ajusta a incidencia
                  }
                  if(delaymethod=="bayesian") {
                        dC2 <- adjustIncidence(d, method = "bayesian")
                  }
            }else{
                  dC2 <- d
                  dC2$tcasesmed <- dC2$casos
                  dC2$tcasesICmin <- dC2$casos
                  dC2$tcasesICmax <- dC2$casos
            }
            
            
            dC3 <- Rt(dC2, count = "tcasesmed", gtdist=GT$gtdist, meangt=GT$meangt, sdgt = GT$sdgt) # calcula Rt
            
            alerta <- fouralert(dC3, pars = parsi, crit = crit, pop=dd$pop[i], miss="last") # calcula alerta
            nome = dd$nome[i]
            nick <- gsub(" ", "", nome, fixed = TRUE)
            #names(alerta) <- nick
            N = dim(alerta$indices)[1]
            print(paste("nivel do alerta de ",nome,":", alerta$indices$level)[N])
            
            if (nlugares > 1) {
                  alertas[[i]]<-alerta
                  names(alertas)[i]<-nick
            } 
            if (writedb == TRUE) {
                  res <- write.alerta(alerta, write = "db")
                  #write.csv(alerta,file=paste("memoria/", nick,hoje,".csv",sep="")) 
            }
      }
      
      res = alerta
      if(nlugares > 1) res = alertas
      res
}