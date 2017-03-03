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
                  plot(km,xlim=c(0,60),xlab="", ylab="",xlab="days")
                  meanlog=mlognorm$icoef[1]; sdlog=exp(mlognorm$icoef[2])
                  lines(0:60,(1-plnorm(0:60,meanlog,sdlog)), lwd=3,col=3)
                  
            }
            
            return(mlognorm)                  
      }
}
