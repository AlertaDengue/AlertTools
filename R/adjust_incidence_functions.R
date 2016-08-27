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
#'res<-fitDelayModel(cities=330240, datasource=con)
#'res<-fitDelayModel(cities=330240, period=c("2013-01-01","2016-01-01"), datasource=con)
#'# Parameters are
#'list(meanlog=res$icoef[1], sdlog=exp(res$icoef[2]))

fitDelayModel<-function(cities, period, plotar = TRUE, datasource){
      
      ncities = length(cities)
      if(nchar(cities)[1] == 6) for (i in ncities) cities[i] <- sevendigitgeocode(cities[i])
      
      result <- c()
      
      if (class(datasource) == "PostgreSQLConnection"){
            
            sql1 = paste("'", cities[1], sep = "")
            if (ncities > 1) for (i in 2:ncities) sql1 = paste(sql1, cities[i], sep = "','")
            sql1 <- paste(sql1, "'", sep = "")
            
            sqlselect <- paste("SELECT municipio_geocodigo, ano_notif, dt_notific, dt_digita from \"Municipio\".\"Notificacao\" WHERE municipio_geocodigo IN(", sql1, ")")
            dd <- dbGetQuery(datasource,sqlselect)
            
      }
      
      if (dim(dd)[1]==0) {
            message(paste("No notification in this(these) cities"))
            return()
            
      } else {
            
            # seleciona periodo
            if (!missing(period)) dd<-subset(dd, (dd$dt_notific > as.Date(period[1]) & (dd$dt_notific > as.Date(period[2]))))
            
            dd$diasdigit<-as.numeric(dd$dt_digita-dd$dt_notific)
            nrow.before <- dim(dd)[1] 
            dd <-dd[-which(is.na(dd$diasdigit)==TRUE),]
            dd <-dd[-which(dd$diasdigit>180 | dd$diasdigit == 0),] # remove records with more than 6 mo delay
            nrow.after <- dim(dd)[1]
            loss <- nrow.before - nrow.after
            
            message(paste(loss, "cases removed for lack of information or delay > 6 months. Number of cases is ",nrow.after))
            
            if (dim(dd)[1]==0) {
                  message(paste("No cases left."))
                  return()
            }
            # Models
            dd$status<-TRUE
            y <- Surv(time=dd$diasdigit, event=dd$status==TRUE)
            km <- survfit(y~1, data = dd)
            mlognorm<-survreg(y~1,dist="lognormal",x=TRUE,y=TRUE,model=TRUE)
            
            if(plotar == TRUE){
                  plot(km,xlim=c(0,60),main="delay model",xlab="day", ylab="fraction not reported")
                  meanlog=mlognorm$icoef[1]; sdlog=exp(mlognorm$icoef[2])
                  lines(0:60,(1-plnorm(0:60,meanlog,sdlog)), lwd=3,col=3)
            }
            
            return(mlognorm)                  
      }
}