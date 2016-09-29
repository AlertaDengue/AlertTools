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

fitDelayModel<-function(cities, period, plotar = TRUE, datasource, verbose=TRUE){
      
      ncities = length(cities)
      if(nchar(cities)[1] == 6) for (i in 1:ncities) cities[i] <- sevendigitgeocode(cities[i])
      
      if (class(datasource) == "PostgreSQLConnection"){
            
            sql1 = paste("'", cities[1], sep = "")
            if (ncities > 1) for (i in 2:ncities) sql1 = paste(sql1, cities[i], sep = "','")
            sql1 <- paste(sql1, "'", sep = "")
            
            sqlselect <- paste("SELECT municipio_geocodigo, ano_notif, dt_notific, dt_digita from \"Municipio\".\"Notificacao\" WHERE municipio_geocodigo IN(", sql1, ")")
            dd <- dbGetQuery(datasource,sqlselect)
            
      }
      
      if (dim(dd)[1]==0) {
            if(verbose==TRUE) message(paste("No notification in this(these) cities"))
            return(NULL)
            
      } else {
            
            # seleciona periodo
            if (!missing(period)) dd<-subset(dd, (dd$dt_notific > as.Date(period[1]) & (dd$dt_notific > as.Date(period[2]))))
            
            # calcula o tempo de atraso
            dd$diasdigit<-as.numeric(dd$dt_digita-dd$dt_notific)
            
            nrow.before <- dim(dd)[1] 
            # check if there is na 
            nas <- sum(is.na(dd$diasdigit))
            if(nas>0) dd <-dd[-which(is.na(dd$diasdigit)==TRUE),]
            
            # check if there are negative times!!
            negs <- sum(dd$diasdigit<0)
            if(negs>0) dd <-dd[-which(dd$diasdigit<0),]
            
            # further remove records with more than 6 mo delay
            dd <-dd[-which(dd$diasdigit>180 | dd$diasdigit == 0),] # 
            nrow.after <- dim(dd)[1]
            loss <- nrow.before - nrow.after
            
            if(verbose==TRUE) message(paste(loss, "cases removed for lack of information or delay > 6 months. Number of cases is ",nrow.after))
            
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
                  par(mar=c(1,1,1,1))
                  plot(km,xlim=c(0,60),xlab="", ylab="")
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


