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
#'@param pdig vector of probability of been typed in the database up to 1, 2, 3, n, weeks after symptoms onset.
#'The length of the vector corresponds to the maximum delay. After day, it is assumed that p = 1. The default
#'was obtained from Rio de Janeiro. 
#'@return data.frame with pdig (proportion reported), median and 95percent confidence interval for the 
#'predicted cases-to-be-notified)
#'@examples
#'res = getCases(city = c(330455), withdivision = FALSE) # Rio de Janeiro
#'head(res)
#'resfit<-adjustIncidence(obj=res)
#'tail(resfit)
#'plot(tail(resfit$casos,n=30),type="l",ylab="cases",xlab="weeks")
#'lines(tail(resfit$tcasesmed,n=30),col=2)
#'lines(tail(resfit$tcasesmin,n=30),col=2,lty=3)
#'lines(tail(resfit$tcasesmax,n=30),col=2,lty=3)
#'legend(12,20,c("notified cases","+ to be notified cases"),lty=1, col=c(1,2),cex=0.7)

adjustIncidence<-function(obj, pdig = plnorm((1:20)*7, 2.5016, 1.1013)){
  le = length(obj$casos) 
  lse = length(obj$SE) 
  
  # creating the proportion vector
  lp <- length(pdig)
  
  if(le > lp) {obj$pdig <- c(rep(1, times = (le - lp)), rev(pdig))
  } else if (le == lp) {obj$pdig <- rev(pdig)
  } else obj$pdig <- rev(pdig)[1:le]
  
  lambda <- (obj$casos/obj$pdig) - obj$casos   
  corr <- function(lamb,n=500) sort(rpois(n,lambda=lamb))[c(02,50,97)] # calcula 95% IC e mediana estimada da parte estocastica 
  
  obj$tcasesICmin <- NA
  obj$tcasesmed <- NA
  obj$tcasesICmax <- NA
  
  for(i in 1:length(obj$casos)) obj[i,c("tcasesICmin","tcasesmed","tcasesICmax")] <- corr(lamb = lambda[i]) + obj$casos[i]
  
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
#'@description Fit lognormal model to the notification delay and return the parameters. See
#'details in doi: http://dx.doi.org/10.1101/046193     
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
#'@title Get delay data for analysis
#'@param d dataset with case data containing at least three variables: the initial and final dates and a variable
#' identifying the epidemiological week (SEM_NOT).
#'@param tini variable indicating the initial date
#'@param tfim variable indicating the end date
#'@param SE variable indicating the epidemiological week
#'@param date.format date format. Default is "%d-%m-%Y"
#'@param truncdays Default is 183 days
#'@param plotar Default is TRUE
#'@return list with d = data.frame with the epidemiological weeks; delay.tbl and delay.week 
#'are internal objects used for plotting.
#'@author Claudia Codeco
#'@examples
#'dados <- getdelaydata(cities=c(3302205, 3304557), datasource=con)  # Not run without connection

getdelaydata <- function(cities, years, cid10 = "A90", datasource){
      
      ncities = length(cities)
      if(nchar(cities)[1] == 6) for (i in 1:ncities) cities[i] <- sevendigitgeocode(cities[i])
      
      if (class(datasource) == "PostgreSQLConnection"){
            
            sql1 = paste("'", cities[1], sep = "")
            if (ncities > 1) for (i in 2:ncities) sql1 = paste(sql1, cities[i], sep = "','")
            sql1 <- paste(sql1, "'", sep = "")
            cid10command <- paste("'", cid10,"'", sep="")    
            
            sqlselect <- paste("SELECT municipio_geocodigo, ano_notif, dt_notific, se_notif, dt_digita from \"Municipio\".\"Notificacao\" WHERE
                               municipio_geocodigo IN(", sql1, ") AND cid10_codigo = ", cid10command)
            dd <- dbGetQuery(datasource,sqlselect)
            
      }
      dd$SE_notif <- dd$ano_notif * 100 + dd$se_notif
      dd[,-dd$se_notif]      
      dd
}


# delaycalc ---------------------------------------------------------------------
#'@description The second function to be used in the delay fitting process using inla. Calculates the number of cases reported
#'per week with a given delay. Also removes data with notification delay greater than truncdays. Produces a 
#'nive plot.  
#'@title Organize delay data for analysis and produce a nice plot.
#'@param d dataset with case data containing at least three variables: the initial and final dates and a variable
#' identifying the epidemiological week (SEM_NOT). Only one city at a time.
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


