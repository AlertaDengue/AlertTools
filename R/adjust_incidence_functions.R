# PROJETO ALERTA DENGUE -------------------------------------
# Funcoes de correcao dos dados de notificacao
# Claudia Codeco 2015
# -----------------------------------------------------------


# adjustIncidence ---------------------------------------------------------------------
#'@description This function estimate the time series of reported cases by adding
#' the cases that will be reported in the future.
#'@title Correct incidence data with notification delay (nowcasting).
#'@export
#'@param obj data.frame with crude weekly cases (not adjusted). This data.frame 
#'comes from the getCases function (if withdivision = FALSE), of getCases followed
#' by casesinlocality (if dataframe is available per bairro)  
#' @param datas data.frame with notification data. Requires: municipio_geocodigo,
#' dt_sin_pro, dt_digita, dt_notific. 
#'@param method "bayesian" or "none". The later just repeats case values
#'@param pdig for the "fixedprob" method. It is a vector of probability of been 
#'typed in the database up to 1, 2, 3, n, weeks after symptoms onset. DEPRECATED.
#'@param Dmax for the "bayesian" method. Maximum number of weeks that is modeled
#'@param nyears for the "bayesian" method. Number of years of data used for fitting the model  
#'@param safelimit if median estimate is larger than 'safelimit' times 
#''sum(tail(cases, n=5))', nowcasting fails. 
#'@param nowSE for the "bayesian" method. Epidemiological week to be considered 
#'for the nowcast. If NA, the maximum SE in obj is used.
#'@return data.frame with median and 95percent 
#'confidence interval for the predicted cases-to-be-notified
#'@examples
#'muns <- getCidades(uf = "Acre", regional = "Baixo Acre e Purus")
#'t1 <- Sys.time()
#'d <- getCases(cities = muns$municipio_geocodigo, dataini = "sinpri")
#'dd <- d[d$cidade == 1200401,]
#'t1 <- Sys.time()
#'resfit<-adjustIncidence(obj=dd, method = "bayesian", nowSE = 202412, datasource = con)
#'t2 <- Sys.time()
#'message(paste("total time was", t2-t1))

#'# Using argument 'datas' is good to save time with dataset access
#'t1 <- Sys.time()
#'load("caselist.RData")  # output of getCases
#'resfit<-adjustIncidence(obj=dd, datas = caselist, method = "bayesian", 
#'nowSE = 202412, datasource = con)
#'t2 <- Sys.time()

adjustIncidence<-function(obj, datas, method = "none", pdig = plnorm((1:20)*7, 2.5016, 1.1013), 
                          Dmax=10, nyears = 2, datasource = con, nowSE, safelimit = 5){
  city <- unique(obj$cidade)  
  cid <- obj$CID10[1]
  # checking if only one city in obj
  assert_that(length(city) == 1, 
              msg = "adjustIncidence only works for one city at a time.")

  le = nrow(obj) 
  
  # checking date
  if(missing(nowSE)) {
    stop("adjustIncidence: nowSE must be provided")
    }   # last date in the input object
  else{
    assert_that(nowSE <= max(obj$SE), 
                msg = "adjustIncidence: lastSE larger than max(obj$SE). Check input.")
    obj <- subset(obj, SE <= nowSE)  # assigned input
  }
  
  obj$tcasesICmin <- obj$casos
  obj$tcasesmed <- obj$casos
  obj$tcasesICmax <- obj$casos
  
  if(sum(tail(obj$casos, n = 52), na.rm = TRUE) <= 50 | 
         sum(tail(obj$casos, n = 5), na.rm = TRUE) <= 5){
    message("less than 50 cases in the last 12 months or less than 5 cases 
            in the last month. Nowcasting not done")
    return(obj)
  } 
  
 if (method == "bayesian"){
       message(paste("computing nowcasting for city ",city," date ",nowSE, "..."))
       nowday <- SE2date(nowSE)$ini + 6
       if(missing(datas)){
             dados <- getdelaydata(cities=city, nyears = nyears, cid10 = cid, 
                                   lastday = nowday, datasource = datasource)     
       }else{
             dados <- datas %>% filter(municipio_geocodigo == city)
             assert_that(dim(dados)[1] > 0, msg="adjustIncidence: city not in dataset") 
       }
       resfit<-bayesnowcasting(dados, Dmax = Dmax, nowSE = nowSE)
       
       # checking estimates
       if(!is.null(resfit)){
         if(tail(resfit$Median, n = 1) > 
            safelimit * sum(tail(obj$casos, n = 5),na.rm = TRUE)){
           message("nowcast estimate is too large, returning the original count.")
         } else {
           if(tail(resfit$LS, n = 1) > 
              10 * safelimit * sum(tail(obj$casos, n = 5),na.rm = TRUE)){
             message("upper limit nowcasting too large, returning t_cases_max = NA")
             resfit$LS <- NA
           expl <- which(resfit$LS > 10000)  # check if any LS was too big
           resfit$LS[expl] <- NA
           message("upper limit nowcarting too large in some dates, 
                   returning t_cases_max = NA")
           }
           message("bayesnowcasting done")
           # adding to the alert data obj
           
           for(se in resfit$SE) {
             rse <- which(resfit$SE == se)
             obj[obj$SE == se, c("tcasesICmin", "tcasesmed", "tcasesICmax")] <- 
               c(resfit$LI[rse], resfit$Median[rse], resfit$LS[rse])  
           }
         }
        
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
#'@param Dmax for the "bayesian" method. Maximum number of weeks that is nowcasted
#'@param nweeks number of weeks used for model calibration, default is 50.
#'@param nowSE week of the nowcasting (ex. 202110). 
#'@param interacao TRUE (default) to include in the model the delay-time interaction term
#'@param tweet FALSE (default). TRUE to include tweet in the model
#'@return data.frame with median and 95percent confidence interval for the 
#'predicted cases-to-be-notified)
#'@examples
#'dados <- getdelaydata(cities=3304557, nyears=1, cid10="A90", 
#'lastday = as.Date("2024-03-23"), datasource=con)  # Not run without connection
#'resfitcIsT<-bayesnowcasting(dados, nowSE = 202412)

bayesnowcasting <- function(d, Dmax = 10, nowSE, nweeks = 50, interacao = FALSE, tweet = F){
  
  # check input 
  if(is.null(names(d))) {
    message("bayesnowcasting: no data, returning NULL")
    return(NULL)}    
  
  if(nrow(d) < 50) {
    message("bayesnowcasting: few data, returning NULL")
    return(NULL)}    
  
  if(missing(nowSE)) stop("bayesnowcasting requires definition of nowcast week ")
      
  #check if d contains required columns
  assert_that(all(c("municipio_geocodigo", "dt_notific", "dt_sin_pri", "dt_digita")
                  %in% names(d)), msg = "bayesnowcasting requires data with columns
              municipio_geocodigo, dt_notific, dt_sin_pri, dt_digita")

   # remove cases with wrong dt_sin_pri  
  # this condition must be equal in the getCases function
  
  d$ininotif <- d$dt_notific - d$dt_sin_pri
  wrongdates <- which(d$ininotif > 365 | d$ininotif < 0 | is.na(d$dt_sin_pri))  
  if(length(wrongdates) > 0) {
    message(paste(length(wrongdates), "registros com datas de inicio de sintomas invalidas"))
    d <- d[-wrongdates,]
  }      
  
  # defining time period 
  # termino da semana nowcasted
  Fim <- SE2date(nowSE)$ini + 6 # soma 6 para obter sabado (fim da semana) 
  Inicio <- Fim - nweeks * 7  
    
  # removing those cases with onset prior to Inicio
  d <- d[-(which(d$dt_sin_pri < Inicio)),]
  
  
  # checking again
  if(is.null(names(d))) {
    message("bayesnowcasting: no valid data, returning NULL")
    return(NULL)}    
  
  if(nrow(d) < 50) {
    message("bayesnowcasting: few valid data, returning NULL")
    return(NULL)}    
  
  message(paste("nowcast will be calibrated with ", nrow(d), "cases"))
  
  ## getting tweet data 
  # if(tweet == TRUE){
  #   tw <- getTweet(cities = unique(d$municipio_geocodigo), 
  #                  lastday = SE2date(nowSE)$ini)
  #   tw <- tw %>%
  #     mutate(dt_iniweek = SE2date(SE)$ini + 6)  %>%
  #     filter(dt_iniweek > min(d$dt_sinpri_week))
  # }
  #   
  # nowcasting
  d <- d %>% mutate(
    dt_sinpri_epiweek = epiweek(dt_sin_pri),  # se da data sinpri
    dt_sinpri_aux =  as.numeric(format(as.Date(dt_sin_pri), "%w")), # dia da semana (0 = sunday)
    dt_sinpri_week =  dt_sin_pri + 3 - dt_sinpri_aux, # colocar centrado na quarta da SE (formula y = -aux+3)
    dt_sinpri_epiyear = epiyear(dt_sin_pri), 
    dt_sinpri_SE = dt_sinpri_epiyear*100 + dt_sinpri_epiweek,
    dt_digita_epiweek = epiweek(dt_digita),
    dt_digita_epiyear = epiyear(dt_digita),
    dt_digita_SE = dt_digita_epiyear*100 + dt_digita_epiweek,
    delay_epiweek = ifelse( dt_digita_epiyear == dt_sinpri_epiyear,
                            as.numeric(dt_digita_epiweek - dt_sinpri_epiweek),
                            as.numeric(dt_digita_epiweek - dt_sinpri_epiweek) + 
                                  52 * (dt_digita_epiyear - dt_sinpri_epiyear)
    )
  ) 
  
  # casos observados por semama
  obs <- d %>%
    group_by(dt_sinpri_week) %>%
    summarize(casos = n())
 
                
  
  # contruindo a matriz de atraso - running triang - tem que ser quarta.
  Fim_now <- SE2date(nowSE)$ini + 3 # soma 3 para obter quarta (meio da semana) 
  Ini_now <- Fim_now - nweeks * 7  
  tibble(Date = c(Ini_now,Fim_now) ) %>% 
    mutate(Weekday = weekdays(Date) )
  
  tbl.dates <- tibble(dt_sinpri_week = seq(Ini_now, Fim_now, by = 7)) %>% 
    rowid_to_column(var = "Time")
  Today <- max(tbl.dates$Time)  # running time
  
  dados.ag <- d %>% 
    filter( dt_digita <= Fim & dt_digita >= Ini_now) %>% 
    mutate(
      delay_epiweek = ifelse(delay_epiweek > Dmax, NA, delay_epiweek)
    ) %>% 
    drop_na(delay_epiweek) %>% # discard cases with delay > 10 weeks
    group_by(dt_sinpri_week, delay_epiweek) %>% 
    dplyr::summarise(
      Casos = n()
    ) %>% # View()
    # Passando para o formato wide
    spread(key = delay_epiweek, value = Casos) %>%  # View()
    # Adicianoando todas as data, algumas semanas nao houveram casos
    full_join( 
      y = tbl.dates, 
      by = "dt_sinpri_week" ) %>% # View() 
    # Voltando para o formato longo
    gather(key = "Delay", value = Casos, -dt_sinpri_week, -Time) %>% # View()
    mutate(
      Delay = as.numeric(Delay),
      # Preparing the run-off triangle
      Casos = ifelse( 
        test = (Time + Delay) <= Today, 
        yes = replace_na(Casos, 0), 
        no = NA)
    ) %>% #View()
    dplyr::rename( Date = dt_sinpri_week) %>%  #ungroup() %>% 
    # Sorting by date
    dplyr::arrange(Date) 
  
  
  ## Juntando tweet (se tweet == TRUE)
  # if (tweet == TRUE){
  #   dados.ag <- dados.ag %>% 
  #     left_join(tw[,c(3,4)], by = c("Date" = "dt_iniweek" )) %>%
  #     mutate(tweet =  replace_na(tweet, 0))
  # }
  
  # nao vi esse obj sendo usado
  # dados.ag.full <- d %>% 
  #   group_by(dt_sinpri_week) %>% 
  #   dplyr::summarise(
  #     Total = n()
  #   ) %>% # View()
  #   # Adicianoando todas as data, algumas semanas nao houveram casos
  #   right_join( 
  #     y = tbl.dates, 
  #     by = "dt_sinpri_week" ) %>% # View() 
  #   mutate(
  #     Total = replace_na(Total, 0)
  #   ) %>% # View()
  #   dplyr::rename( Date = dt_sinpri_week) %>%  #ungroup() %>% 
  #   # Sorting by date
  #   dplyr::arrange(Date) 
  # 
  
  # Model equations (4 possibilities)
  if(interacao & !tweet) {
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
  } 
  
  if(!interacao & !tweet) {  #default
    model.dengue <- Casos ~ 1 + 
      f(Time, model = "rw2", constr = T
        #hyper = list("prec" = list(prior = "loggamma", param = c(0.001, 0.001) ))
        #hyper = list("prec" = list(prior = half_normal_sd(.1) ))
      ) +
      f(Delay, model = "rw1", constr = T
        # hyper = list("prec" = list(prior = "loggamma", param = c(0.001, 0.001) ))
        #hyper = list("prec" = list(prior = half_normal_sd(.1) ))
      )
    # message("modelo de nowcasting sem interacao")
  }
  
  if(interacao & tweet) {
    model.dengue <- Casos ~ 1 + tweet +
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
  } 
  
  if(!interacao & tweet) {
    model.dengue <- Casos ~ 1 + tweet + 
      f(Time, model = "rw2", constr = T
        #hyper = list("prec" = list(prior = "loggamma", param = c(0.001, 0.001) ))
        #hyper = list("prec" = list(prior = half_normal_sd(.1) ))
      ) +
      f(Delay, model = "rw1", constr = T
        # hyper = list("prec" = list(prior = "loggamma", param = c(0.001, 0.001) ))
        #hyper = list("prec" = list(prior = half_normal_sd(.1) ))
      )
    # message("modelo de nowcasting sem interacao")
  }
  
  output.dengue <- nowcast.INLA(
    model.day = model.dengue, 
    dados.ag = dados.ag %>%
      mutate(TimeDelay = paste(Time, Delay))
  )
  
  # we could save other features, like prob greater threshold
  pred.dengue <- nowcasting(output.dengue, dados.ag, 
                            Dm = Dmax, Fim = Fim) # max(dados.ag$Date))
  
  
  pred.dengue.summy <- pred.dengue %>% group_by(Date) %>% 
    dplyr::summarise( Mean = mean(Casos, na.rm = TRUE),
                      Median = median(Casos, na.rm = TRUE), 
                      LI = quantile(Casos, probs = 0.025, na.rm = TRUE),
                      LS = quantile(Casos, probs = 0.975, na.rm = TRUE)
    ) %>%
    left_join(obs, by = c("Date" = "dt_sinpri_week")) 
  
  pred.dengue.summy$SE <- daySEday(pred.dengue.summy$Date)$SE
  
  pred.dengue.summy
}



#' # bayesnowcasting ---------------------------------------------------------------------
#' #'@description This function estimate the Bayesian nowcast (new version)
#' #'@title Correct incidence data with notification delay (nowcasting).
#' #'@export
#' #'@param obj data.frame with individual cases, containing columns municipio_geocodigo, dt_notific, dt_sin_pri, dt_digita 
#' #'@param Dmax for the "bayesian" method. Maximum number of weeks that is modeled
#' #'@param Fim date for the nowcasting (date). Default is today.
#' #'@return data.frame with median and 95percent confidence interval for the 
#' #'predicted cases-to-be-notified)
#' #'@examples
#' #' # bayesian
#' #'dd <- getdelaydata(cities=3167202, nyears=1, cid10="A90", datasource=con)
#' #'resfit<-bayesnowcasting(dd)
#' #'resfit
#' 
#' bayesnowcasting <- function(d, Dmax = 10, Fim = Sys.Date()){
#'   
#'   # check input 
#'   if(is.null(names(d))) {
#'     message("bayesnowcasting: no data, returning NULL")
#'     return(NULL)}    
#'   
#'   if(nrow(d) < 50) {
#'     message("bayesnowcasting: few data, returning NULL")
#'     return(NULL)}    
#'   
#'   
#'    #d contains columns
#'   assert_that(all(c("municipio_geocodigo", "dt_notific", "dt_sin_pri", "dt_digita")
#'               %in% names(d)), msg = "bayesnowcasting requires data with columns
#'               municipio_geocodigo, dt_notific, dt_sin_pri, dt_digita")
#'   
#'   d <- d %>% mutate(
#'     dt_notific_epiweek = epiweek(dt_notific), 
#'     dt_notific_aux =  as.numeric(format(as.Date(dt_notific), "%w")),
#'     dt_notific_week = dt_notific + 6 - dt_notific_aux,
#'     dt_notific_epiyear = epiyear(dt_notific), 
#'     dt_digita_epiweek = epiweek(dt_digita),
#'     dt_digita_epiyear = epiyear(dt_digita),
#'     delay_epiweek = ifelse( dt_digita_epiyear == dt_notific_epiyear,
#'                             as.numeric(dt_digita_epiweek - dt_notific_epiweek),
#'                             as.numeric(dt_digita_epiweek - dt_notific_epiweek) + 52
#'     )
#'   ) 
#'   
#'   # casos observados por semama
#'   obs <- d %>%
#'     group_by(dt_notific_week) %>%
#'     summarize(casos = n())
#'   
#'   Inicio <- min(d$dt_notific_week)
#'   # Ultimo dia com notificacao ou digitacao
#'   #Fim <- max(d$dt_digita, d$dt_notific, na.rm = T)
#'   #Fim <- max(d$dt_digita, d$dt_notific, na.rm = T)
#'   #Fim <- Fim + 6 - as.numeric(format(as.Date(Fim), "%w")) # why?
#' 
#'     # contruindo a matriz de atraso - running triang
#'   tibble(Date = c(Inicio,Fim) ) %>% 
#'     mutate(Weekday = weekdays(Date) )
#' 
#'   tbl.dates <- tibble(dt_notific_week = seq(Inicio, Fim, by = 7)) %>% 
#'     rowid_to_column(var = "Time")
#'   Today = max(tbl.dates$Time)
#'   
#'   dados.ag <- d %>% 
#'     filter( dt_digita <= Fim) %>% 
#'     mutate(
#'       delay_epiweek = ifelse(delay_epiweek > Dmax, NA, delay_epiweek)
#'     ) %>% 
#'     drop_na(delay_epiweek) %>% 
#'     group_by(dt_notific_week, delay_epiweek) %>% 
#'     dplyr::summarise(
#'       Casos = n()
#'     ) %>% # View()
#'     # Passando para o formato wide
#'     spread(key = delay_epiweek, value = Casos) %>%  # View()
#'     # Adicianoando todas as data, algumas semanas nao houveram casos
#'     full_join( 
#'       y = tbl.dates, 
#'       by = "dt_notific_week" ) %>% # View() 
#'     # Voltando para o formato longo
#'     gather(key = "Delay", value = Casos, -dt_notific_week, -Time) %>% # View()
#'     mutate(
#'       Delay = as.numeric(Delay),
#'       # Preparing the run-off triangle
#'       Casos = ifelse( 
#'         test = (Time + Delay) <= Today, 
#'         yes = replace_na(Casos, 0), 
#'         no = NA)
#'     ) %>% #View()
#'     dplyr::rename( Date = dt_notific_week) %>%  #ungroup() %>% 
#'     # Sorting by date
#'     dplyr::arrange(Date) 
#'   
#'   
#'   dados.ag.full <- d %>% 
#'     group_by(dt_notific_week) %>% 
#'     dplyr::summarise(
#'       Total = n()
#'     ) %>% # View()
#'     # Adicianoando todas as data, algumas semanas nao houveram casos
#'     right_join( 
#'       y = tbl.dates, 
#'       by = "dt_notific_week" ) %>% # View() 
#'     mutate(
#'       Total = replace_na(Total, 0)
#'     ) %>% # View()
#'     dplyr::rename( Date = dt_notific_week) %>%  #ungroup() %>% 
#'     # Sorting by date
#'     dplyr::arrange(Date) 
#'   
#' # Model equation
#' model.dengue <- Casos ~ 1 + 
#'   f(Time, model = "rw2", constr = T
#'     #hyper = list("prec" = list(prior = "loggamma", param = c(0.001, 0.001) ))
#'     #hyper = list("prec" = list(prior = half_normal_sd(.1) ))
#'   ) +
#'   f(Delay, model = "rw1", constr = T
#'     # hyper = list("prec" = list(prior = "loggamma", param = c(0.001, 0.001) ))
#'     #hyper = list("prec" = list(prior = half_normal_sd(.1) ))
#'   )  + 
#'   # Efeito tempo-atraso
#'   f(TimeDelay, model = "iid", constr = T
#'     #   hyper = list("prec" = list(prior = "loggamma", param = c(0.001, 0.001) ))
#'   )
#' 
#' 
#' output.dengue <- nowcast.INLA(
#'   model.day = model.dengue,
#'   dados.ag = dados.ag %>%
#'     mutate(TimeDelay = paste(Time, Delay))
#' )
#' 
#' pred.dengue <- nowcasting(output.dengue, dados.ag, 
#'                           Dm = Dmax, Fim = max(dados.ag$Date))
#' 
#' 
#' pred.dengue.summy <- pred.dengue %>% group_by(Date) %>% 
#'   dplyr::summarise( Mean = mean(Casos, na.rm = TRUE),
#'                     Median = median(Casos, na.rm = TRUE), 
#'                     LI = quantile(Casos, probs = 0.025, na.rm = TRUE),
#'                     LS = quantile(Casos, probs = 0.975, na.rm = TRUE)
#'   ) %>%
#'   left_join(obs, by = c("Date" = "dt_notific_week")) 
#' 
#' pred.dengue.summy$SE <- daySEday(pred.dengue.summy$Date)$SE
#' 
#' pred.dengue.summy
#' }
#' 

# getdelaydata ------------------------------------------------------------------
#'@description Gets delay data for one or more cities. Internal function used in
#' the delay fitting using inla. 
#'@title Get delay data for one or more cities for delay analysis
#'@export
#'@param cities vector with geocodes
#'@param cid10 disease code, Default is dengue. "A92.0" for chik, "A92.8" for zika
#'@param lastday last digitation day
#'@param nyears number of years of data used for fitting: Default = 2 years
#'@param datasource valid connection to database
#'@return list with d = data.frame with data.
#'@examples
#'dados <- getdelaydata(cities=3304557, nyears=1, cid10="A90", 
#'lastday = as.Date("2023-12-31"))  # Not run without connection

getdelaydata <- function(cities, nyears= 2, cid10 = "A90", lastday = Sys.Date(), 
                         datasource = con){
  
  ncities = length(cities)
  #cities <- sapply(cities, function(x) sevendigitgeocode(x))
  
  #dealing with synonimous cid
  if (cid10 == "A90") cid <- "A90" # dengue, dengue hemorragica
  if (cid10 %in% c("A92", "A920","A92.0")) {cid <-c("A92", "A920","A92.0"); cid10 <- "A92.0"} # chik
  if (cid10 %in% c("A92.8","A928")) {cid <- c("A92.8","A928"); cid10 <- c("A92.8")} #zika
  
  # reading notification data form the database 
  sqlcity = paste("'", str_c(cities, collapse = "','"),"'", sep="")
  sqlcid = paste("'", str_c(cid, collapse = "','"),"'", sep="") # dealing with multiple cids for the same disease  
  
  firstday <- lastday - nyears * 365
  
  if(class(datasource) == "PostgreSQLConnection"){
    comando <- paste("SELECT municipio_geocodigo, dt_notific, dt_sin_pri, dt_digita
                       from \"Municipio\".\"Notificacao\" WHERE dt_digita <= '",lastday, 
                   "' AND dt_digita >= '",firstday, "' AND municipio_geocodigo IN (", sqlcity, 
                   ") AND cid10_codigo IN(", sqlcid,")", sep="")
  
    dd <- dbGetQuery(datasource,comando)
  }
  
  if(class(datasource) == "SQLiteConnection"){
    comando <- paste("SELECT municipio_geocodigo, dt_notific, dt_sin_pri, dt_digita
                       from \"Notificacao\" WHERE dt_digita <= '",as.numeric(lastday), 
                     "' AND dt_digita > '",as.numeric(firstday), "' AND municipio_geocodigo IN 
                     (", sqlcity, ") AND cid10_codigo IN(", sqlcid,")", sep="")
    
    dd <- dbGetQuery(datasource,comando)
    
    if(nrow(dd)==0)stop("getCases found no data")
    # fixing dates
    dd$dt_notific <- as.Date(dd$dt_notific, origin = "1970-01-01")
    dd$dt_sin_pri <- as.Date(dd$dt_sin_pri, origin = "1970-01-01")
    dd$dt_digita <- as.Date(dd$dt_digita, origin = "1970-01-01")
    dd <- dd %>%
      filter(dt_digita <= lastday & dt_digita > firstday)
    
  }
  
  
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
    if (na.dtdigita > 0) message(paste("getdelaydata: number of records with 
                                       missing dates: ",na.dtdigita,
                                       " out of ",nb, "notifications. 
                                       This may cause consistency problems 
                                       between obs and pred cases" ))
    
     dd
  }
}



