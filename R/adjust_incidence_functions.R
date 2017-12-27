# PROJETO ALERTA DENGUE -------------------------------------
# Funcoes de correcao dos dados de notificacao
# Claudia Codeco 2015
# -----------------------------------------------------------


# adjustIncidence ---------------------------------------------------------------------
#'@description Often, there is a delay between symptom onset and notification. This function 
#'adjust the time series of reported cases by adding the cases that will be reported in the future.
#'It requires knowing the probability of notification per week passed. This function assumes a stationary
#'notification process, there is, no influence of covariates or any temporal inhomogeneity.   
#'@title Adjust incidence data correcting for the notification delay.
#'@param obj data.frame with crude weekly cases (not adjusted). This data.frame comes from the getCases
#' function (if withdivision = FALSE), of getCases followed by casesinlocality (if dataframe is available
#' per bairro)  
#'@param method "fixedprob" for fixed delay prob per week; "bayesian" for the dynamic model 
#'@param pdig for the "fixedprob" method. It is a vector of probability of been typed in the database up to 1, 2, 3, n, weeks after symptoms onset.
#'The length of the vector corresponds to the maximum delay. After day, it is assumed that p = 1. The default
#'was obtained from Rio de Janeiro. 
#'@param Dmax for the "bayesian" method. Maximum number of weeks that is modeled
#'@param nyears for the "bayesian" method. Number of years of data used for fitting the model  
#'@return data.frame with pdig (proportion reported), median and 95percent confidence interval for the 
#'predicted cases-to-be-notified)
#'@examples
#'# fixedprob
#'res = getCases(city = 330240, datasource=con) 
#'head(res)
#'resfit<-adjustIncidence(obj=res)
#'tail(resfit)
#' # bayesian
#'resfit<-adjustIncidence(obj=res,method="bayesian",datasource=con)
#'tail(resfit)

adjustIncidence<-function(obj, method = "fixedprob", pdig = plnorm((1:20)*7, 2.5016, 1.1013), Dmax=12, nyears = 3, datasource=NA){
      
  # checking if only one city in obj
  ncities <- length(unique(obj$cidade)) 
  if (ncities > 1) stop("Function adjustIncidence: only runs for a city at a time")
  le = length(obj$casos) 
  lse = length(obj$SE) 
  
  obj$pdig <- NA
  obj$tcasesICmin <- NA
  obj$tcasesmed <- NA
  obj$tcasesICmax <- NA
  
  
  if (method == "fixedprob"){
        # creating the proportion vector
        lp <- length(pdig)
        
        if(le > lp) {obj$pdig <- c(rep(1, times = (le - lp)), rev(pdig))
        } else if (le == lp) {obj$pdig <- rev(pdig)
        } else obj$pdig <- rev(pdig)[1:le]
        
        lambda <- (obj$casos/obj$pdig) - obj$casos   
        corr <- function(lamb,n=500) sort(rpois(n,lambda=lamb))[c(02,50,97)] # calcula 95% IC e mediana estimada da parte estocastica 
        
        for(i in 1:length(obj$casos)) obj[i,c("tcasesICmin","tcasesmed","tcasesICmax")] <- corr(lamb = lambda[i]) + obj$casos[i]
  }
  
 if (method == "bayesian"){
       thisyear <- floor(obj$SE[lse]/100)
       # Leo's functions
       dados <- getdelaydata(cities=unique(obj$cidade), years = (thisyear-nyears):thisyear, cid10 = obj$CID10[1], datasource=con)
       lastdate <- max(dados$SE_notif)
       
       res <- delaycalc(dados)
       outp <- fitDelay.inla(res, Dmax = Dmax)
       delay <- prob.inc(outp, plotar = FALSE)
       
       # adding to the alert data obj
       nweeks <- dim(delay)[2]
       obj$tcasesICmin <- obj$casos; obj$tcasesICmax <- obj$casos; obj$tcasesmed <- obj$casos
       obj$tcasesICmin[(le-nweeks+1):le]<-delay[3,]
       obj$tcasesmed[(le-nweeks+1):le]<-delay[1,]
       obj$tcasesICmax[(le-nweeks+1):le]<-delay[4,]
 }   
 
  obj
}




# fitDelayModel ---------------------------------------------------------------------
#'@description Fit lognormal model to the notification delay and return the parameters. See
#'details in doi: http://dx.doi.org/10.1101/046193     
#'@title Fit lognormal model to the notification delay
#'@param cities geocode of one of more cities. If more than one city, a single model is fitted to the whole dataset.  
#'@param period range of dates for the analysis. Format: c("2010-01-01","2015-12-31"). 
#'Default is the whole period available. 
#'@param datasource sql connection
#'@param plotar if TRUE, show plot of the fitted model
#'@return object with a summary of the analysis and suggestion of best model
#'@examples
#'con <- DenguedbConnect()
#'res1<-fitDelayModel(cities=330240, datasource=con)
#'res<-fitDelayModel(cities=330240, period=c("2013-01-01","2016-01-01"), datasource=con)
#'# Parameters are
#'list(meanlog=res$icoef[1], sdlog=exp(res$icoef[2]))

#fitDelayModel<-function(cities, period, plotar = TRUE, datasource=con, verbose=TRUE){
#      
#      ncities = length(cities)
#      if(nchar(cities)[1] == 6) for (i in 1:ncities) cities[i] <- sevendigitgeocode(cities[i])
#      
#      if (class(datasource) == "PostgreSQLConnection"){
#            
#            sql1 = paste("'", cities[1], sep = "")
#            if (ncities > 1) for (i in 2:ncities) sql1 = paste(sql1, cities[i], sep = "','")
#            sql1 <- paste(sql1, "'", sep = "")
#            
#            sqlselect <- paste("SELECT municipio_geocodigo, ano_notif, dt_notific, dt_digita from \"Municipio\".\"Notificacao\" WHERE municipio_geocodigo IN(", sql1, ")")
#            dd <- dbGetQuery(datasource,sqlselect)
#            
#      }
      
#      if (dim(dd)[1]==0) {
#            if(verbose==TRUE) message(paste("No notification in this(these) cities"))
#            return(NULL)
#            
#      } else {
            
#            # seleciona periodo
#            if (!missing(period)) dd<-subset(dd, (dd$dt_notific > as.Date(period[1]) & (dd$dt_notific > as.Date(period[2]))))
            
            # calcula o tempo de atraso
#            dd$diasdigit<-as.numeric(dd$dt_digita-dd$dt_notific)
            
#            nrow.before <- dim(dd)[1] 
            # check if there is na 
#            nas <- sum(is.na(dd$diasdigit))
#            if(nas>0) dd <-dd[-which(is.na(dd$diasdigit)==TRUE),]
            
            # check if there are negative times!!
#            negs <- sum(dd$diasdigit<0)
#            if(negs>0) dd <-dd[-which(dd$diasdigit<0),]
            
            # further remove records with more than 6 mo delay
#            dd <-dd[-which(dd$diasdigit>180 | dd$diasdigit == 0),] # 
#            nrow.after <- dim(dd)[1]
#            loss <- nrow.before - nrow.after
            
#            if(verbose==TRUE) message(paste(loss, "cases removed for lack of information or delay > 6 months. Number of cases is ",nrow.after))
            
#            if (dim(dd)[1]==0) {
#                  if(verbose==TRUE)  message(paste("No cases left."))
#                  return(NULL)
#            }
            # Models
#            dd$status<-TRUE
#            y <- Surv(time=dd$diasdigit, event=dd$status==TRUE)
#            km <- survfit(y~1, data = dd)
#            mlognorm<-survreg(y~1,dist="lognormal",x=TRUE,y=TRUE,model=TRUE)
            
#            if(plotar == TRUE){
#                  par(mar=c(1,1,1,1))
#                  plot(km,xlim=c(0,60),xlab="", ylab="")
#                  meanlog=mlognorm$icoef[1]; sdlog=exp(mlognorm$icoef[2])
#                  lines(0:60,(1-plnorm(0:60,meanlog,sdlog)), lwd=3,col=3)
                  
#            }
            
#            return(mlognorm)                  
#      }
#}

# fitDelayModel ---------------------------------------------------------------------
#'@description Fit lognormal model to the notification delay and return the parameters. 
#'@title Fit lognormal model to the notification delay
#'@param cities geocode of one of more cities. If more than one city, a single model is fitted to the whole dataset.  
#'@param cid10 CID 10 code of the disease. Dengue = "A90" is default, Chik = "A920" 
#'@param period range of dates for the analysis. Format: c("2010-01-01","2015-12-31"). 
#'Default is the whole period available. 
#'@param dateini either dt_notific(default) or dt_sin_pri
#'@param datasource sql connection
#'@param plotar if TRUE, show plot of the fitted model
#'@return object with a summary of the analysis and suggestion of best model
#'@examples
#'con <- DenguedbConnect()
#'res1<-fitDelayModel(cities=330240, datasource=con)
#'res<-fitDelayModel(cities=330240, period=c("2013-01-01","2016-01-01"), datasource=con)
#'res1<-fitDelayModel(cities=3304557, cid10="A920",datasource=con)

#'# Parameters are
#'list(meanlog=res$icoef[1], sdlog=exp(res$icoef[2]))

fitDelayModel<-function(cities, period, plotar = TRUE, cid10 = "A90", datasource, verbose=TRUE, inidate="dt_notific"){
      
      ncities = length(cities)
      if(nchar(cities)[1] == 6) for (i in 1:ncities) cities[i] <- sevendigitgeocode(cities[i])
      
      if (class(datasource) == "PostgreSQLConnection"){
            
            sql1 = paste("'", cities[1], sep = "")
            if (ncities > 1) for (i in 2:ncities) sql1 = paste(sql1, cities[i], sep = "','")
            sql1 <- paste(sql1, "'", sep = "")
            cid10command <- paste("'", cid10,"'", sep="")             
            
            sqlselect <- paste("SELECT municipio_geocodigo, ano_notif, dt_notific, dt_sin_pri, dt_digita from \"Municipio\".\"Notificacao\" WHERE
                               municipio_geocodigo IN(", sql1, ") AND cid10_codigo = ", cid10command)
            dd <- dbGetQuery(datasource,sqlselect)
            
      }
      
      if (dim(dd)[1]==0) {
            if(verbose==TRUE) message(paste("No notification in this(these) cities"))
            return(NULL)
            
      } else {
            
            # seleciona periodo e calcula tempo de atraso
            if (!missing(period)){
                  if (inidate=="dt_notific") {
                        dd<-subset(dd, (dd$dt_notific > as.Date(period[1]) & (dd$dt_notific > as.Date(period[2]))))
                        dd$diasdigit<-as.numeric(dd$dt_digita-dd$dt_notific)
                  }
                  if (inidate == "dt_sin_pri") {
                        dd<-subset(dd, (dd$dt_sin_pri > as.Date(period[1]) & (dd$sin_pri > as.Date(period[2]))))
                        dd$diasdigit<-as.numeric(dd$dt_digita-dd$dt_sin_pri)
                  }
            } 
            else{
                  if (inidate=="dt_notific")  dd$diasdigit<-as.numeric(dd$dt_digita-dd$dt_notific)
                  if (inidate == "dt_sin_pri") dd$diasdigit<-as.numeric(dd$dt_digita-dd$dt_sin_pri)
            }
            
            
            nrow.before <- dim(dd)[1] 
            nzero = sum(dd$diasdigit ==0 ); n180 = sum(dd$diasdigit>180)
            # check if there is na or time 0 (this model does not fit to delay = 0)
            if(sum(is.na(dd$diasdigit))>0) dd <-dd[-which(is.na(dd$diasdigit)==TRUE),]
            dd <-dd[-which(dd$diasdigit>180 | dd$diasdigit == 0),] # remove records with more than 6 mo dela
            nrow.after <- dim(dd)[1]
            loss <- nrow.before - nrow.after
            
            if(verbose==TRUE) {
                  message(paste(loss, "cases removed for lack of information, delay = 0 or delay > 6 months. Number of cases for the analysis is ",nrow.after))
                  message(paste(nzero, "removed because delay=0 and", n180 , "removed because delay > 180 days" ))
                  
            }
            
            if (dim(dd)[1]==0) {
                  if(verbose==TRUE)  message(paste("No cases left."))
                  return(NULL)
            }
            # Models
            dd$status<-TRUE
            y <- Surv(time=dd$diasdigit, event=dd$status==TRUE)
            km <- survfit(y~1, data = dd)
            mlognorm<-survreg(y~1,dist="lognormal",x=TRUE,y=TRUE,model=TRUE)
            
            if(plotar == TRUE){
                  par(mar=c(4,3,1,1))
                  plot(km,xlim=c(0,60),ylab="",xlab="days")
                  meanlog=mlognorm$icoef[1]; sdlog=exp(mlognorm$icoef[2])
                  lines(0:60,(1-plnorm(0:60,meanlog,sdlog)), lwd=3,col=3)
                  
            }
            
            return(mlognorm)                  
      }
}

# updateDelayModel ---------------------------------------------------------------------
#'@description Apply the fitDelayModel to all or a subset of cities.
#'@title Update notification delay model.
#'@param cities geocode of one of more cities.
#'@param ufs list of ufs (full name as in the database). Required even if only a subset of cities is the target  
#'@param regional TRUE, if model should be fitted at the regional level too
#'@param period range of dates for the analysis. Format: c("2010-01-01","2015-12-31"). 
#'Default is the whole period available. 
#'@param datasource sql connection.
#'@param plotar if TRUE, show plot of the fitted model.
#'@param write TRUE if result.
#'@return object with a summary of the analysis and suggestion of best model. 
#'@examples
#'con <- DenguedbConnect()
#'par(mfrow=c(2,1))
#'res<-updateDelayModel(cities=c(330240, 330045), period=c("2013-01-01","2016-01-01"), plotar=TRUE, ufs = "Rio de Janeiro", datasource=con)
#'res<-updateDelayModel(ufs="Rio de Janeiro", period=c("2013-01-01","2016-01-01"), datasource=con)
#'res<-updateDelayModel(ufs="Rio de Janeiro", period=c("2013-01-01","2016-01-01"), regional=TRUE, datasource=con)

updateDelayModel <- function(cities, ufs, period, datasource, plotar=FALSE, write, verbose=FALSE, regional=FALSE){
     
     if (!(class(datasource) == "PostgreSQLConnection")) stop("please provide a sql connection")
     
      # all cities of the requested states       
     nufs <- length(ufs)
     dd <- getCidades(uf = ufs[1],datasource = datasource)
     if(nufs>1) for (i in 2:nufs) dd <- rbind(dd, getCidades(uf = ufs[i], datasource=datasource))
     
     #set of cities, if requested
     if(!missing(cities)) {
           ncities = length(cities)
           if(nchar(cities)[1] == 6) for (i in 1:ncities) cities[i] <- sevendigitgeocode(cities[i])
           
           # check if all cities are within the defined states
           if(!all(cities %in% dd$municipio_geocodigo)) stop("Check your specification. Mismatch btw citiesand states")
    
           dd <- subset(dd, dd$municipio_geocodigo %in% cities)
     }
     
     dd$casos <- NA
     dd$meanlog <- NA
     dd$sdlog <- NA
     
     for (i in 1:dim(dd)[1]){
             mod <- fitDelayModel(cities=dd$municipio_geocodigo[i], plotar=plotar, verbose=verbose, datasource=datasource)
             if (!is.null(mod)){
                   dd$meanlog[i] <- mod$icoef[1]
                   dd$sdlog[i] <- mod$icoef[2]
                   dd$casos[i] <- summary(mod)$n
             }
             if (plotar==TRUE) legend("topright",legend = dd$nome[i],bty="n",cex=0.7)
     }
     
     if(regional == TRUE){
           dd$meanlogR <- NA
           dd$sdlogR <- NA
           
           listaregs <- unique(dd$nome_regional)
           for(j in listaregs){
                 modreg <- fitDelayModel(cities=dd$municipio_geocodigo[dd$nome_regional == j], plotar=plotar, verbose=verbose, datasource=datasource)
                 dd$meanlogR[dd$nome_regional == j] <- modreg$icoef[1]
                 dd$sdlogR[dd$nome_regional == j] <- modreg$icoef[2]
           }
      
           
     }
dd
}


###########################################
## Leo's delay model

# getdelaydata ------------------------------------------------------------------
#'@description Gets delay data for one or more cities. Internal function used in the delay fitting using inla. 
#'@title Get delay data for one or more cities for delay analysis
#'@param cities vector with geocodes
#'@param cid10 disease code, Default is dengue. "A92.0" for chik, "A92.8" for zika
#'@param years vector with set of years for analysis. Default (NULL) is to get all years of data available.
#'@param datasource valid connection to database
#'@return list with d = data.frame with data.
#'@author Claudia Codeco
#'@examples
#'dados <- getdelaydata(cities=3304557, years=c(2016, 2017), cid10="A92", datasource=con)  # Not run without connection

getdelaydata <- function(cities, years = NULL, cid10 = "A90", datasource){
      
      ncities = length(cities)
      nyears = length(years)
      
      if(nchar(cities)[1] == 6) for (i in 1:ncities) cities[i] <- sevendigitgeocode(cities[i])
      
      #dealing with synonimous cid
      if (cid10 == "A90") cid <- c("A90") # dengue, dengue hemorragica
      if (cid10 %in% c("A92", "A920","A92.0")) {cid <-c("A92", "A920","A92.0"); cid10 <- "A92.0"} # chik
      if (cid10 %in% c("A92.8","A928")) {cid <- c("A92.8","A928"); cid10 <- c("A92.8")} #zika
      
      if (class(datasource) == "PostgreSQLConnection"){
            
            sql1 = paste("'", cities[1], sep = "")
            if (ncities > 1) for (i in 2:ncities) sql1 = paste(sql1, cities[i], sep = "','")
            sql1 <- paste(sql1, "'", sep = "")
            
            lcid <- length(cid)
            cid10command <- paste("'", cid[1], sep="")
            if (lcid > 1) for (i in 2:lcid) cid10command = paste(cid10command, cid[i], sep = "','")
            cid10command <- paste(cid10command, "'", sep = "")
            
            if (nyears == 0){# means that all years will be included in the analysis
                  sqlselect <- paste("SELECT municipio_geocodigo, ano_notif, dt_notific, se_notif, dt_digita from \"Municipio\".\"Notificacao\" WHERE
                               municipio_geocodigo IN(", sql1, ") AND cid10_codigo IN(", cid10command,")")
            } else { # filter some years
                  sql2 = paste("'", years[1], sep = "")
                  if(nyears > 1) for (i in 2:nyears) sql2 = paste(sql2, years[i], sep = "','")
                  sql2 <- paste(sql2, "'", sep = "")
                  
                  sqlselect <- paste("SELECT municipio_geocodigo, ano_notif, dt_notific, se_notif, dt_digita from \"Municipio\".\"Notificacao\" WHERE
                               municipio_geocodigo IN(", sql1, ") AND cid10_codigo IN(", cid10command, ") AND ano_notif IN (",sql2,")")      
            }
            
            dd <- dbGetQuery(datasource,sqlselect)
            
      } else {stop("getdelaydata: requires a valid PostgreSQLConnection")}
      dd$SE_notif <- dd$ano_notif * 100 + dd$se_notif
      dd$cid10 <- cid10
      #dd[,-dd$se_notif]      
      dd
}


# delaycalc ---------------------------------------------------------------------
#'@description The second function to be used in the delay fitting process using inla. Calculates the number
#' of cases reported per week with a given delay. Also removes data with notification delay greater than truncdays. 
#'@title Organize delay data for analysis and produce a nice plot.
#'@param d dataset with case data containing at least three variables: the initial and final dates and a variable
#' identifying the epidemiological week. Only one city at a time.
#'@param tini variable indicating the initial date
#'@param tfim variable indicating the end date
#'@param SE variable indicating the epidemiological week
#'@param date.format date format. Default is "%d-%m-%Y"
#'@param truncdays Default is 183 days
#'@param plotar Default is TRUE
#'@return list with d = data.frame with the epidemiological weeks; delay.tbl and delay.week 
#'are internal objects used for plotting.
#'@author Leo Bastos
#'@examples
#'dados <- getdelaydata(cities=3302205, datasource=con)
#'res = delaycalc(dados)  
#'head(res$d)  # data
#'head(res$delay.tbl)  # running matrix

delaycalc <- function(d, tini = "dt_notific", tfim = "dt_digita", SE = "SE_notif", date.format = "%Y-%m-%d", 
                      truncdays = 183, verbose = TRUE){
      
      # Checking if there is more than one city
      ncities <- length(unique(d$municipio_geocodigo))
      if(ncities != 1)stop("delaycalc error: delay function can only be applied to one city at a time.") 
      
      dd <- d[,c(tini,tfim,SE)]
      names(dd)<-c("tini","tfim","SE")
      
      # getting the time data  (# acrescentar testes de existencia e de formato)
      tini = as.Date(as.character(d[,tini]),format=date.format )
      tfim = as.Date(as.character(d[,tfim]),format=date.format )
      rm(d)
      
      # Calculating delay time
      dd$DelayDays <- difftime(tfim, tini, units = "days")
      
      # Number of notifications greater than 6 months (>= 182 days)
      if (verbose==TRUE){
      
            message(paste("number of notifications with delay greater than",truncdays,"days =",
                    sum(dd$DelayDays >= truncdays, na.rm = T),"in",length(dd$DelayDays),". They will be excluded.")) 
      }
      
      dd <- na.exclude(dd[dd$DelayDays < truncdays, ])
      
      # Delay in weeks
      dd$DelayWeeks <- floor( dd$DelayDays / 7 )
      
      aux <- tapply(dd$DelayWeeks >= 0 , INDEX = dd$SE, FUN = sum, na.rm = T)
      delay.tbl <- data.frame(Notifications = aux[order(rownames(aux))])
      
      for(k in 0:26){  
            aux <- tapply(dd$DelayWeeks == k, INDEX = dd$SE, FUN = sum, na.rm = T)
            delay.tbl[paste("d",k, sep="")] <- aux[order(rownames(aux))]
      }
      
      delay.week <- paste("d",0:26, sep="")
      cores <- heat.colors(n = 27, alpha = 0.8)
      
      yyy <- t(as.matrix(delay.tbl[delay.week] ))
      
      
      list(d = dd, delay.tbl = delay.tbl, delay.week = delay.week)
}



# fitDelay.inla ---------------------------------------------------------------------
#'@description Fit model  Y ~ 1 + f(Time, model = "rw1") + f(Delay, model = "rw1") to object created by delaycalc.
#'@title Fit INLA model to notification delay.
#'@param obj object with observed delays, produced by delaycalc().
#'@param Tactual today's date used for estimation. 
#'@param Dmax maximum delay allowed (in weeks).
#'@return list containing the fitted model (out), the data, arguments and some objects to be used
#'by other auxiliary functions. 
#'@examples
#'res = delaycalc(dados)
#'outp<-fitDelay.inla(res)


fitDelay.inla <- function(obj, Tactual = nrow(obj$delay.tbl), Dmax = 12, plotar = FALSE){
      
      require(INLA)
      message("fitting..")
      # creating a continuous sequence of weeks within the study period (#aqui da para otimizar)
      d <- obj$d
      d$ano <- floor(d$SE/100) # years in the dataset
      allweeks = expand.grid(ano=min(d$ano):max(d$ano), semanas = 1:52)
      allweeks = allweeks[order(allweeks$ano),]
      allweeks$se <- allweeks$ano*100+allweeks$semanas  # all weeks
      
      aux <- tapply(d$DelayWeeks >= 0 , INDEX = d$SE, FUN = sum, na.rm = T)
      
      #delay.tbl <- data.frame(Notifications = aux[order(rownames(aux))])
      delay.tbl <- obj$delay.tbl
      
      #delay.week <- paste("d",0:26, sep="")
      delay.week <- obj$delay.week
      
      for(k in 0:26){  
            aux <- tapply(d$DelayWeeks == k, INDEX = d$SE, FUN = sum, na.rm = T)
            delay.tbl[paste("d",k, sep="")] <- aux[order(rownames(aux))]
      }
      delay.data <- delay.tbl[delay.week]
      
      # tempo maximo do banco
      Tmax <- nrow(delay.data)
      
      delay.data.obs <- delay.data[1:Tactual,(0:Dmax)+1]
      
      # Time index of the unknown counts (Dmax+1,...,Tactual) 
      index.time <- (Tactual-Dmax+1):Tactual
      
      delay.data.obs.trian <- delay.data.obs
      
      # Creating the run-off triangle data frame
      delay.data.obs.trian[outer(1:Tactual, 0:Dmax, FUN = "+") > Tactual] <- NA
      
      # This function creates a data frame from the run-off triangle matrix to be used in INLA
      make.df.trian <- function(M){
            Time <- nrow(M)
            Delay <- ncol(M)
            aux.df <- data.frame(Y = as.vector(as.matrix(M)), 
                                 Time = rep(x = 1:Time, times = Delay),
                                 Delay = rep(x = 1:Delay, each=Time)
            )
            aux.df
      }
      
      # Creating a data frame for INLA
      delay.inla.trian <- make.df.trian(delay.data.obs.trian)
      
      # Find the missing values
      index.missing <- which(is.na(delay.inla.trian$Y))      
      
      model <- Y ~ 1 + f(Time, model = "rw1") + f(Delay, model = "rw1")
      
      output <- inla(model, family = "nbinomial", data = delay.inla.trian,
                     control.predictor = list(link = 1, compute = T),
                     control.compute = list( config = T, waic=TRUE, dic=TRUE))
      
      c(WAIC = output$waic$waic, DIC = output$dic$dic)
      
      message("sampling...")
      
      delay.samples.list <- inla.posterior.sample(n = 10, output)
      
      
      # Sampling the missing triangule from inla output in vector format
      aaa <- lapply(X = delay.samples.list, FUN = function(x, idx = index.missing) rnbinom(n = idx, mu = exp(x$latent[idx]), size = x$hyperpar[1])
      ) 
      
      
      # Creating a vectorized version of the triangle matrix
      delay.vec.trian <- inla.matrix2vector(as.matrix(delay.data.obs.trian[index.time,]))
      
      # Transforming back from the vector form to the matrix form
      bbb <- lapply(aaa, FUN = function(xxx, data = delay.vec.trian){
            data[which(is.na(data))] <- xxx
            inla.vector2matrix(data, ncol = Dmax+1) } )
      
      
      # Samples of {N_t : t=Tactual-Dmax+1,...Tactual}
      ccc <- sapply(bbb, FUN = function(x) rowSums(x) )
      
      
      Nt.true <- rowSums(delay.data.obs[index.time,])
      Nt.obs <- rowSums(delay.data.obs.trian[index.time,], na.rm=T)
      # Nt.forecast <- rowSums(matrix(output$summary.fitted.values$mean, ncol=Dmax+1)[index.time,])
      
      if (plotar == TRUE){
            par(mfrow=c(1,1))
            plot(index.time, Nt.true, ylim=range(Nt.true, Nt.obs), ylab="", xlab="",pch=16)
            points(index.time, Nt.obs, pch=3)
            #lines(index.time, Nt.forecast, col=2)
            lines(index.time, rowMeans(ccc), col=2)
            lines(index.time, apply(ccc,1,quantile,probs = 0.025), col=2, lty=2)
            lines(index.time, apply(ccc,1,quantile,probs = 0.975), col=2, lty=2)
            legend("topleft", c("Observed counts", "Real counts", "Posterior prediction",
                                "95% CI limits"), pch=c(3,16,NA,NA), lty=c(NA,NA,1,2), col=c(1,1,2,2))
            
      }
            
      
      list(out=output,post=ccc,Tactual=Tactual, Dmax=Dmax,delay.data.obs=delay.data.obs,
           delay.data.obs.trian=delay.data.obs.trian,Tmax=Tmax)
}


# plot.inla.re ---------------------------------------------------------------------
#'@description Plot the random effects of the delay model fitted using fitDelay.inla()
#'@title plot delay and time random effects
#'@param outputRE random effect components of the object created by the fitDelay.inla function
#'@return graphs 
#'@examples
#'res = delaycalc(dados)
#'outp<-fitDelay.inla(res)
#'par(mfrow=c(2,1),mar=c(4,4,2,2))
#'plot.inla.re(outp$out$summary.random$Time, xlab="semana epidemiologica")
#'plot.inla.re(outp$out$summary.random$Delay, xlab="semana de atraso")


plot.inla.re = function(outputRE, xlab){
      plot( outputRE$mean, type = "n", ylim = range(outputRE[,c(4,6)]), ylab="", xlab=xlab )
      polygon(x = c(outputRE$ID, rev(outputRE$ID)),
              y = c(outputRE$'0.025quant', rev(outputRE$'0.975quant')),
              border = "black", col = "gray")
      lines(outputRE$mean, lty=1, lwd=2)
      lines(x = range(outputRE$ID), y = rep(0,2), lty=2)
      return(NULL)  
}


# prob.inc ---------------------------------------------------------------------
#'@description predicted incidence using fitDelay.inla(). 
#'@title posterior distribution of the incidence 
#'@param obj created by the fitDelay.inla function
#'@return table with mean, median, 2.5% and 97.5% incidence.
#'@author Leo Bastos
#'@examples
#'dados <- getdelaydata(cities=3302205, datasource=con)
#'res = delaycalc(dados)
#'outp<-fitDelay.inla(res)
#'delay <- prob.inc(outp)

prob.inc<-function(obj, plotar=T){
      
      ccc <- obj$post
      Dmax <- obj$Dmax
      delay.data.obs <-obj$delay.data.obs
      Tactual <-obj$Tactual
      delay.data.obs.trian <- obj$delay.data.obs.trian
      delay.data.obs <- obj$delay.data.obs
      
      # quantiles
      post.sum = function(x,probs = c(0.5, 0.025,0.975)) c(mean = mean(x), quantile(x,probs))
      
      apply(ccc,1,FUN = post.sum)[,Dmax]
      post.sum(ccc[Dmax,])
      
      post.prob = function(x, prob = c(100, 200)) c(Ps100 = mean(x < prob[1]), Pg200 = mean(x > prob[2]) ) 
      
      apply(ccc,1,FUN = post.prob, prob=c(500,1000))[,Dmax]
      
      teste <- apply(ccc, MARGIN = 1, post.sum)
      
      if(plotar == TRUE){
            par(mfrow=c(1,1))
            max.y <- max(teste[4,])  
            plot(rowSums(delay.data.obs[1:Tactual ,] ), xlab  =  "", 
                 ylab = "Casos", type = "n", axes=F, xlim = c(Tactual-24,Tactual), 
                 ylim = c(0,max.y) )
            polygon(x = c((Tactual-Dmax+1):Tactual,Tactual:(Tactual-Dmax+1)),
                    y = c(teste[3,], rev(teste[4,])),
                    border = 3, col = "lightgray", lty=3)
            lines((Tactual-Dmax+1):Tactual, teste[1,], col=3, lty=3, lwd=2)
            #lines(rowSums(delay.data.obs.trian[1:Tactual,], na.rm=T), col=2, lwd=2, lty=2)
            lines(rowSums(delay.data.obs[1:Tactual,], na.rm=T), col=1, lwd=2)
            axis(2)
            xlabels <- rownames(outp$delay.data.obs)[Tactual-24:Tactual]
            axis(1, at=seq(Tactual-24,Tactual,length.out = 6), 
                 labels = xlabels[seq(1,24,length.out = 6)])
            legend("topleft", c("Notificados", "Estimados"),
                   lty=1:2, lwd=2, col=c(1,3))
            
      }
      
      teste
}

