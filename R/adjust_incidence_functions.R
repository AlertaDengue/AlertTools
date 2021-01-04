# PROJETO ALERTA DENGUE -------------------------------------
# Funcoes de correcao dos dados de notificacao
# Claudia Codeco 2015
# -----------------------------------------------------------


# adjustIncidence ---------------------------------------------------------------------
#'@description This function estimate the time series of reported cases by adding the cases that will be reported in the future.
#'Two methods were implemented, the "fixedprob" requires knowing the probability of notification per week passed. This function assumes a stationary
#'notification process, there is, no influence of covariates or any temporal inhomogeneity.  The "bayesian"
#'is dynamic. 
#'@title Correct incidence data with notification delay (nowcasting).
#'@export
#'@param obj data.frame with crude weekly cases (not adjusted). This data.frame comes from the getCases
#' function (if withdivision = FALSE), of getCases followed by casesinlocality (if dataframe is available
#' per bairro)  
#'@param method "fixedprob" for fixed delay prob per week; "bayesian" for the 
#'dynamic model . If "none" just repeats case values
#'@param pdig for the "fixedprob" method. It is a vector of probability of been typed in the database up to 1, 2, 3, n, weeks after symptoms onset.
#'The length of the vector corresponds to the maximum delay. After day, it is assumed that p = 1. The default
#'was obtained from Rio de Janeiro. 
#'@param Dmax for the "bayesian" method. Maximum number of weeks that is modeled
#'@param nyears for the "bayesian" method. Number of years of data used for fitting the model  
#'@param lastSE for the "bayesian" method. Last epidemiological week to be considered. If NA, will use last digitalization date.
#'@return data.frame with pdig (proportion reported), median and 95percent confidence interval for the 
#'predicted cases-to-be-notified)
#'@examples
#'# fixedprob
#'d <- getCases(cities = 2103307) 
#'tail(d)
#'resfit<-adjustIncidence(obj = d)
#'tail(resfit)
#' # bayesian
#'resfit<-adjustIncidence(obj=d, method = "bayesian", datasource = con)
#'tail(resfit)

adjustIncidence<-function(obj, method = "fixedprob", pdig = plnorm((1:20)*7, 2.5016, 1.1013), 
                          Dmax=10, nyears = 2, datasource = con, lastSE=NA){
  
  city <- unique(obj$cidade)  
  cid <- obj$CID10[1]
  # checking if only one city in obj
  assert_that(length(city) == 1, msg = "adjustIncidence only works for one city at a time.")
  
  le = nrow(obj) 
  
  obj$tcasesICmin <- obj$casos
  obj$tcasesmed <- obj$casos
  obj$tcasesICmax <- obj$casos
  
  
  if (method == "fixedprob"){
        # creating the proportion vector
        obj$pdig <- NA
        lp <- length(pdig)
        
        if(le > lp) {obj$pdig <- c(rep(1, times = (le - lp)), rev(pdig))
        } else if (le == lp) {obj$pdig <- rev(pdig)
        } else obj$pdig <- rev(pdig)[1:le]
        
        lambda <- (obj$casos/obj$pdig) - obj$casos   
        corr <- function(lamb,n=500) sort(rpois(n,lambda=lamb))[c(02,50,97)] # calcula 95% IC e mediana estimada da parte estocastica 
        
        for(i in 1:length(obj$casos)) obj[i,c("tcasesICmin","tcasesmed","tcasesICmax")] <- corr(lamb = lambda[i]) + obj$casos[i]
        obj %>% mutate(pdig=NULL)
         }
  
 if (method == "bayesian"){
       message("computing nowcasting...")
       dados <- getdelaydata(cities=city, nyears = nyears, cid10 = cid, datasource = datasource)
       message("bayesnowcasting...")
       
       resfit<-try(bayesnowcasting(dados, Dmax))
       
       if(!is.null(resfit)){
         message("bayesnowcasting done")
         # adding to the alert data obj
         obj$tcasesICmin[(le-Dmax):le]<-resfit$LI
         obj$tcasesmed[(le-Dmax):le]<-resfit$Mean
         obj$tcasesICmax[(le-Dmax):le]<-resfit$LS
       } else {message("nowcasting failed, returning the original count")}
 }   
 
  if(method=="none") message("nowcasting not done, returning the original counts")
  
  obj
}


# bayesnowcasting ---------------------------------------------------------------------
#'@description This function estimate the Bayesian nowcast (new version)
#'@title Correct incidence data with notification delay (nowcasting).
#'@export
#'@param obj data.frame with individual cases, containing columns municipio_geocodigo, dt_notific, dt_sin_pri, dt_digita 
#'@param Dmax for the "bayesian" method. Maximum number of weeks that is modeled
#'@return data.frame with median and 95percent confidence interval for the 
#'predicted cases-to-be-notified)
#'@examples
#' # bayesian
#'dd <- getdelaydata(cities=2103307, nyears=2, cid10="A90", datasource=con)
#'resfit<-bayesnowcasting(dd)
#'tail(resfit)

bayesnowcasting <- function(d, Dmax = 10){
  
  # check input 
  if(is.null(names(d))) {
    message("bayesnowcasting: no data, returning NULL")
    return(NULL)}    
  
   #d contains columns
  assert_that(all(c("municipio_geocodigo", "dt_notific", "dt_sin_pri", "dt_digita")
              %in% names(d)), msg = "bayesnowcasting requires data with columns
              municipio_geocodigo, dt_notific, dt_sin_pri, dt_digita")
  
  d <- d %>% mutate(
    dt_notific_epiweek = epiweek(dt_notific), 
    dt_notific_aux =  as.numeric(format(as.Date(dt_notific), "%w")),
    dt_notific_week = dt_notific + 6 - dt_notific_aux,
    dt_notific_epiyear = epiyear(dt_notific), 
    dt_digita_epiweek = epiweek(dt_digita),
    dt_digita_epiyear = epiyear(dt_digita),
    delay_epiweek = ifelse( dt_digita_epiyear == dt_notific_epiyear,
                            as.numeric(dt_digita_epiweek - dt_notific_epiweek),
                            as.numeric(dt_digita_epiweek - dt_notific_epiweek) + 52
    )
  ) 
  
  # casos observados por semama
  obs <- d %>%
    group_by(dt_notific_week) %>%
    summarize(casos = n())
  
  Inicio <- min(d$dt_notific_week)
  # Ultimo dia com notificacao ou digitacao
  #Fim <- max(d$dt_digita, d$dt_notific, na.rm = T)
  Fim <- max(d$dt_digita, d$dt_notific, na.rm = T)
  Fim <- Fim + 6 - as.numeric(format(as.Date(Fim), "%w")) # why?

    # contruindo a matriz de atraso - running triang
  tibble(Date = c(Inicio,Fim) ) %>% 
    mutate(Weekday = weekdays(Date) )

  tbl.dates <- tibble(dt_notific_week = seq(Inicio, Fim, by = 7)) %>% 
    rowid_to_column(var = "Time")
  Today = max(tbl.dates$Time)
  
  dados.ag <- d %>% 
    filter( dt_digita <= Fim) %>% 
    mutate(
      delay_epiweek = ifelse(delay_epiweek > Dmax, NA, delay_epiweek)
    ) %>% 
    drop_na(delay_epiweek) %>% 
    group_by(dt_notific_week, delay_epiweek) %>% 
    dplyr::summarise(
      Casos = n()
    ) %>% # View()
    # Passando para o formato wide
    spread(key = delay_epiweek, value = Casos) %>%  # View()
    # Adicianoando todas as data, algumas semanas nao houveram casos
    full_join( 
      y = tbl.dates, 
      by = "dt_notific_week" ) %>% # View() 
    # Voltando para o formato longo
    gather(key = "Delay", value = Casos, -dt_notific_week, -Time) %>% # View()
    mutate(
      Delay = as.numeric(Delay),
      # Preparing the run-off triangle
      Casos = ifelse( 
        test = (Time + Delay) <= Today, 
        yes = replace_na(Casos, 0), 
        no = NA)
    ) %>% #View()
    dplyr::rename( Date = dt_notific_week) %>%  #ungroup() %>% 
    # Sorting by date
    dplyr::arrange(Date) 
  
  
  dados.ag.full <- d %>% 
    group_by(dt_notific_week) %>% 
    dplyr::summarise(
      Total = n()
    ) %>% # View()
    # Adicianoando todas as data, algumas semanas nao houveram casos
    right_join( 
      y = tbl.dates, 
      by = "dt_notific_week" ) %>% # View() 
    mutate(
      Total = replace_na(Total, 0)
    ) %>% # View()
    dplyr::rename( Date = dt_notific_week) %>%  #ungroup() %>% 
    # Sorting by date
    dplyr::arrange(Date) 
  
# Model equation
model.dengue <- Casos ~ 1 + 
  f(Time, model = "rw2", constr = T
    #hyper = list("prec" = list(prior = "loggamma", param = c(0.001, 0.001) ))
    #hyper = list("prec" = list(prior = half_normal_sd(.1) ))
  ) +
  f(Delay, model = "rw1", constr = T
    # hyper = list("prec" = list(prior = "loggamma", param = c(0.001, 0.001) ))
    #hyper = list("prec" = list(prior = half_normal_sd(.1) ))
  )  + 
  # Efeito tempo-atraso
  f(TimeDelay, model = "iid", constr = T
    #   hyper = list("prec" = list(prior = "loggamma", param = c(0.001, 0.001) ))
  )


output.dengue <- nowcast.INLA(
  model.day = model.dengue,
  dados.ag = dados.ag %>%
    mutate(TimeDelay = paste(Time, Delay))
)

pred.dengue <- nowcasting(output.dengue, dados.ag, 
                          Dm = Dmax, Fim = max(dados.ag$Date))


pred.dengue.summy <- pred.dengue %>% group_by(Date) %>% 
  dplyr::summarise( Mean = mean(Casos),
                    Median = median(Casos), 
                    LI = quantile(Casos, probs = 0.025),
                    LS = quantile(Casos, probs = 0.975)
  ) %>%
  left_join(obs, by = c("Date" = "dt_notific_week"))

pred.dengue.summy$SE <- daySEday(pred.dengue.summy$Date)$SE

pred.dengue.summy
}


# getdelaydata ------------------------------------------------------------------
#'@description Gets delay data for one or more cities. Internal function used in the delay fitting using inla. 
#'@title Get delay data for one or more cities for delay analysis
#'@export
#'@param cities vector with geocodes
#'@param cid10 disease code, Default is dengue. "A92.0" for chik, "A92.8" for zika
#'@param lastday last digitation day
#'@param nyears number of years of data used for fitting: Default = 2 years
#'@param datasource valid connection to database
#'@return list with d = data.frame with data.
#'@examples
#'dados <- getdelaydata(cities=4100905, nyears=2, cid10="A90", datasource=con)  # Not run without connection

getdelaydata <- function(cities, nyears= 2, cid10 = "A90", lastday = Sys.Date(), 
                         datasource = con){
  
  ncities = length(cities)
  #cities <- sapply(cities, function(x) sevendigitgeocode(x))
  
  #dealing with synonimous cid
  if (cid10 == "A90") cid <- "A90" # dengue, dengue hemorragica
  if (cid10 %in% c("A92", "A920","A92.0")) {cid <-c("A92", "A920","A92.0"); cid10 <- "A92.0"} # chik
  if (cid10 %in% c("A92.8","A928")) {cid <- c("A92.8","A928"); cid10 <- c("A92.8")} #zika
  
  # reading notification data form the database ----------------------------
  sqlcity = paste("'", str_c(cities, collapse = "','"),"'", sep="")
  sqlcid = paste("'", str_c(cid, collapse = "','"),"'", sep="") # dealing with multiple cids for the same disease  
  
  firstday <- lastday - nyears * 365
  
  comando <- paste("SELECT municipio_geocodigo, dt_notific, dt_sin_pri, dt_digita
                       from \"Municipio\".\"Notificacao\" WHERE dt_digita <= '",lastday, 
                   "' AND dt_digita > '",firstday, "' AND municipio_geocodigo IN (", sqlcity, 
                   ") AND cid10_codigo IN(", sqlcid,")", sep="")
  
  dd <- dbGetQuery(datasource,comando)
  
  
  if(nrow(dd) == 0){
    message(paste("getdelaydata: found no", cid, "data for", sqlcity ))
    return(NULL)
  } else {
    #dd$SE_notif <- dd$ano_notif * 100 + dd$se_notif
    #dd$cid10 <- cid10
    
    
    # Removing records with missing dt_digita
    nb <- nrow(dd)
    dd <- dd[!is.na(dd$dt_digita),]
    na.dtdigita <- nb - nrow(dd)
    if (na.dtdigita > 0) message(paste("getdelaydata: number of records with missing dates: ",na.dtdigita,
                                       " out of ",nb, "notifications" ))
    
    # Create SE_digit
    #dd$se_digit <- mapply(function(x) episem(x, retorna='W'), dd[, 'dt_digita'])
    #dd$ano_digit <- mapply(function(x) episem(x, retorna='Y'), dd[, 'dt_digita'])
    #dd$SE_digit <- daySEday(dd$dt_digita)$SE
    #dd$ano_digit <- floor(dd$SE_digit/100)
    #dd$se_digit <- dd$SE_digit - dd$ano*100
    dd
  }
}



## OLD STUFF ----

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

#fitDelayModel<-function(cities, period, plotar = TRUE, cid10 = "A90", datasource, verbose=TRUE, inidate="dt_notific"){
#      
#      ncities = length(cities)
#      if(nchar(cities)[1] == 6) for (i in 1:ncities) cities[i] <- sevendigitgeocode(cities[i])
#      
#      if (class(datasource) == "PostgreSQLConnection"){
#            
#            sql1 = paste("'", cities[1], sep = "")
#            if (ncities > 1) for (i in 2:ncities) sql1 = paste(sql1, cities[i], sep = "','")
#            sql1 <- paste(sql1, "'", sep = "")
#             cid10command <- paste("'", cid10,"'", sep="")             
#             
#             sqlselect <- paste("SELECT municipio_geocodigo, ano_notif, dt_notific, dt_sin_pri, dt_digita from \"Municipio\".\"Notificacao\" WHERE
#                                municipio_geocodigo IN(", sql1, ") AND cid10_codigo = ", cid10command)
#             dd <- dbGetQuery(datasource,sqlselect)
#             
#       }
#       
#       if (dim(dd)[1]==0) {
#             if(verbose==TRUE) message(paste("No notification in this(these) cities"))
#             return(NULL)
#             
#       } else {
#             
#             # seleciona periodo e calcula tempo de atraso
#             if (!missing(period)){
#                   if (inidate=="dt_notific") {
#                         dd<-subset(dd, (dd$dt_notific > as.Date(period[1]) & (dd$dt_notific > as.Date(period[2]))))
#                         dd$diasdigit<-as.numeric(dd$dt_digita-dd$dt_notific)
#                   }
#                   if (inidate == "dt_sin_pri") {
#                         dd<-subset(dd, (dd$dt_sin_pri > as.Date(period[1]) & (dd$sin_pri > as.Date(period[2]))))
#                         dd$diasdigit<-as.numeric(dd$dt_digita-dd$dt_sin_pri)
#                   }
#             } 
#             else{
#                   if (inidate=="dt_notific")  dd$diasdigit<-as.numeric(dd$dt_digita-dd$dt_notific)
#                   if (inidate == "dt_sin_pri") dd$diasdigit<-as.numeric(dd$dt_digita-dd$dt_sin_pri)
#             }
#             
#             
#             nrow.before <- dim(dd)[1] 
#             nzero = sum(dd$diasdigit ==0 ); n180 = sum(dd$diasdigit>180)
#             # check if there is na or time 0 (this model does not fit to delay = 0)
#             if(sum(is.na(dd$diasdigit))>0) dd <-dd[-which(is.na(dd$diasdigit)==TRUE),]
#             dd <-dd[-which(dd$diasdigit>180 | dd$diasdigit == 0),] # remove records with more than 6 mo dela
#             nrow.after <- dim(dd)[1]
#             loss <- nrow.before - nrow.after
#             
#             if(verbose==TRUE) {
#                   message(paste(loss, "cases removed for lack of information, delay = 0 or delay > 6 months. Number of cases for the analysis is ",nrow.after))
#                   message(paste(nzero, "removed because delay=0 and", n180 , "removed because delay > 180 days" ))
#                   
#             }
#             
#             if (dim(dd)[1]==0) {
#                   if(verbose==TRUE)  message(paste("No cases left."))
#                   return(NULL)
#             }
#             # Models
#             dd$status<-TRUE
#             y <- Surv(time=dd$diasdigit, event=dd$status==TRUE)
#             km <- survfit(y~1, data = dd)
#             mlognorm<-survreg(y~1,dist="lognormal",x=TRUE,y=TRUE,model=TRUE)
#             
#             if(plotar == TRUE){
#                   par(mar=c(4,3,1,1))
#                   plot(km,xlim=c(0,60),ylab="",xlab="days")
#                   meanlog=mlognorm$icoef[1]; sdlog=exp(mlognorm$icoef[2])
#                   lines(0:60,(1-plnorm(0:60,meanlog,sdlog)), lwd=3,col=3)
#                   
#             }
#             
#             return(mlognorm)                  
#       }
# }

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

# updateDelayModel <- function(cities, ufs, period, datasource, plotar=FALSE, write, verbose=FALSE, regional=FALSE){
#      
#      if (!(class(datasource) == "PostgreSQLConnection")) stop("please provide a sql connection")
#      
#       # all cities of the requested states       
#      nufs <- length(ufs)
#      dd <- getCidades(uf = ufs[1],datasource = datasource)
#      if(nufs>1) for (i in 2:nufs) dd <- rbind(dd, getCidades(uf = ufs[i], datasource=datasource))
#      
#      #set of cities, if requested
#      if(!missing(cities)) {
#            ncities = length(cities)
#            if(nchar(cities)[1] == 6) for (i in 1:ncities) cities[i] <- sevendigitgeocode(cities[i])
#            
#            # check if all cities are within the defined states
#            if(!all(cities %in% dd$municipio_geocodigo)) stop("Check your specification. Mismatch btw citiesand states")
#     
#            dd <- subset(dd, dd$municipio_geocodigo %in% cities)
#      }
#      
#      dd$casos <- NA
#      dd$meanlog <- NA
#      dd$sdlog <- NA
#      
#      for (i in 1:dim(dd)[1]){
#              mod <- fitDelayModel(cities=dd$municipio_geocodigo[i], plotar=plotar, verbose=verbose, datasource=datasource)
#              if (!is.null(mod)){
#                    dd$meanlog[i] <- mod$icoef[1]
#                    dd$sdlog[i] <- mod$icoef[2]
#                    dd$casos[i] <- summary(mod)$n
#              }
#              if (plotar==TRUE) legend("topright",legend = dd$nome[i],bty="n",cex=0.7)
#      }
#      
#      if(regional == TRUE){
#            dd$meanlogR <- NA
#            dd$sdlogR <- NA
#            
#            listaregs <- unique(dd$nome_regional)
#            for(j in listaregs){
#                  modreg <- fitDelayModel(cities=dd$municipio_geocodigo[dd$nome_regional == j], plotar=plotar, verbose=verbose, datasource=datasource)
#                  dd$meanlogR[dd$nome_regional == j] <- modreg$icoef[1]
#                  dd$sdlogR[dd$nome_regional == j] <- modreg$icoef[2]
#            }
#       
#            
#      }
# dd
# }
# 

###########################################
## Leo's delay model


# delaycalc ---------------------------------------------------------------------
#'@description The second function to be used in the delay fitting process using inla. Calculates the number
#' of cases reported per week with a given delay. Also removes data with notification delay greater than truncdays. 
#'@title Organize delay data for analysis and produce a nice plot.
#'@param d dataset with case data containing at least three variables: the event week  (tini) and event registry (tfim). Only one city at a time.
#'@param nt_year variable indicating the event year
#'@param nt_week variable indicating the event week
#'@param dg_year variable indicating the event registry year
#'@param dg_week variable indicating the event registry week
#'@param SE variable indicating epidemiological week of reference
#'@param lastSE variable with final notification week to be considered. Format YYYYWW. If NA, uses 
#' max(100*d$dg_year+d$dg_week). Default=NA.
#'@param truncweeks Default is 25 weeks
#'@param plotar Default is TRUE
#'@return list with d = data.frame with the epidemiological weeks; delay.tbl and delay.week 
#'are internal objects used for plotting. Author Leo Bastos
#'@examples
#'dados <- getdelaydata(cities=3304557, datasource=con)
#'res = delaycalc(dados)  
#'head(res$d)  # data
#'head(res$delay.tbl)  # running matrix

# delaycalc <- function(d, nt_year = "ano_notif", nt_week = "se_notif",
#                       dg_year = "ano_digit", dg_week = "se_digit",
#                       SE = "SE_notif", lastSE=NA,
#                       truncweeks = 25, verbose = TRUE){
#       
#       # Checking if there is more than one city
#       ncities <- length(unique(d$municipio_geocodigo))
#       if(ncities != 1)stop("delaycalc error: delay function can only be applied to one city at a time.") 
#       
#       dd <- d[,c(SE, nt_year, nt_week, dg_year, dg_week)]
#       names(dd)<-c("SE", "nt_year", "nt_week", "dg_year", "dg_week")
#       
#       rm(d)
# 
#       # Remove data uploaded later than lastSE, if any:
#       if (is.na(lastSE)){
#             lastdate <- max(dd$SE)      
#       } else {
#             lastdate <- lastSE
#       }
#       lastdate.year <- as.integer(lastdate/100)
#       lastdate.week <- as.integer(lastdate-100*lastdate.year)
#       dd <- dd[100*dd$dg_year + dd$dg_week <= lastdate, ]
#       
#       # Calculating delay time in epiweeks
#       dd$DelayWeeks <- dd$dg_week - dd$nt_week +
#             (dd$dg_year - dd$nt_year)*as.integer(sapply(dd$nt_year,lastepiweek))
#       
# 
#       # Number of notifications greater than truncweeks
#       if (verbose==TRUE){
#       
#             message(paste("number of notifications with delay greater than",truncweeks,"weeks =",
#                     sum(dd$DelayWeeks >= truncweeks, na.rm = T),"in",length(dd$DelayWeeks),". They will be excluded.")) 
#       }
#       
#       dd <- na.exclude(dd[dd$DelayWeeks <= truncweeks, ])
#       
#       # Prepare filled epiweeks data frame:
#       # # Fill all epiweeks:
#       fyear <- min(dd$nt_year)
#       fweek <- min(dd$nt_week[dd$nt_year == fyear])
#       years.list <- c(fyear:lastdate.year)
#       df.epiweeks <- data.frame(SE=character())
#       for (y in years.list){
#             epiweeks <- c()
#             fweek <- ifelse(y > fyear, 1, fweek)
#             lweek <- ifelse(y < lastdate.year, as.integer(lastepiweek(y)), lastdate.week)
#             for (w in c(fweek:lweek)){
#                   epiweeks <- c(epiweeks, paste0(y,sprintf('%02d', w)))
#             }
#             df.epiweeks <- rbind(df.epiweeks, data.frame(list(SE=epiweeks)))
#       }
#       rownames(df.epiweeks) <- df.epiweeks$SE
#       
#       aux <- tapply(dd$DelayWeeks >= 0 , INDEX = dd$SE, FUN = sum, na.rm = T)
#       delay.tbl <- data.frame(Notifications = aux[order(rownames(aux))])
#       
#       for(k in 0:truncweeks){  
#             aux <- tapply(dd$DelayWeeks == k, INDEX = dd$SE, FUN = sum, na.rm = T)
#             delay.tbl[paste("d",k, sep="")] <- aux[order(rownames(aux))]
#       }
#       
#       delay.week <- paste("d",0:truncweeks, sep="")
#       
#       # Fill missing epiweeks (the ones wiht zero notifications):
#       delay.tbl <- merge(df.epiweeks, delay.tbl, by=0, all.x=T)
#       delay.tbl[is.na(delay.tbl)] <- 0
#       rownames(delay.tbl) <- delay.tbl$Row.names
#       
#       list(d = dd, delay.tbl = delay.tbl[, c('SE', 'Notifications', delay.week)], delay.week = delay.week)
# }



