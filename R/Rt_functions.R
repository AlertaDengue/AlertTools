# PROJETO ALERTA DENGUE -------------------------------------
# Funcoes de calculo de Rt
# Claudia Codeco 2015
# -----------------------------------------------------------


#Rtoriginal -----------------------------------------------------------------------
#'@description Calculates the effective reproductive number from the growth rate 
#'of cases. The The original formulation is simply the ratio between 3-weeks accumulated
#'cases at week (t+1) and the 3-weeks accumulated cases at week t. 
#'@title Computes the effective reproductive number using a Alerta's original formulation.
#'@param obj object with the format of getcases. A data.frame with variables "casos" and "SE".
#'@param count case counts (adjusted or nonadjusted incidence).   
#'@param meangt the exact period between primary and secondary infections
#'@param CI Model used to compute the confidence interval. "beta" is the only choice.
#'@return data.frame with estimated Rt and confidence intervals
#'@examples
#'res = getCases(city = c(330455), withdivision = FALSE) # Rio de Janeiro
#' # Rt original
#'rt<-Rtoriginal(obj = res, count = "casos", meangt=3)
#'plot(rt$Rt, type="l", xlab = "weeks", ylab = "Rt")
#'lines(rt$lwr,lty=3)
#'lines(rt$upr,lty=3)
#'abline(h = 1, col = 2)


Rtoriginal<-function(obj, count = "casos", meangt, CI = "beta", alpha = .95, a0 = 2 , b0 = 3){  
  
  if(!any(c(count,"SE") %in% names(obj))) stop("obj must be a data.frame with variables 
                                              SE and var, at least. Consider using getCases")
  y <- obj[,count]
  le <- length(y)
  if (le < 2*meangt) warning("you need a time series                           
                             with size at least 2 generation intervals to estimate Rt")
  
  message("Rtoriginal is deprecated. Consider using Rt.")
  ac <- y[meangt:le]
  
  for(i in 1:(meangt-1)) ac <- ac+y[(meangt-i):(le-i)]    
  mle<-length(ac)
  # CI (using Luis Max funtion)
  K <- length(ac)
  jk1 <- ac[2:K] # J[k+1] 
  jk  <- ac[1:(K-1)]# J[k]  
  obj$Rt[(meangt + 1):le] <- jk1/jk
  obj$upr <- NA
  obj$lwr <- NA
  obj$p1 <- NA
  ## p1 = Pr(R>1) 
  if (CI == "beta"){
    for( k in 1: (le-meangt)){
      obj$p1[k+meangt] <- 1 - pbeta(.5, shape1 = jk1[k], shape2 = jk[k])
      obj[k+meangt, c("upr","lwr")] <- ll(betaconf(alpha = alpha, x = jk1[k], 
                                    n = jk1[k] + jk[k], a = a0, b = b0 ))
    }
  }
    return(obj)
}


#Rt -----------------------
#'@description Calculates the effective reproductive number from the growth rate 
#'of cases. Uses formula 4.2 in Wallinga and Lispitch (2007). Confidence interval assume
#'ratio between two Poissons (see Luis Max documentation). 
#'@title Computes the effective reproductive number using alternative 
#'distributions for the generation interval.
#'@param object a data.frame with variables "casos" and "SE", ideally from getCases().
#'@param meangt if gtdist = "delta" it is the exact period between primary and 
#'secondary infections). If gtdist = "normal", it is the mean generation time.
#'@param sdgt if gtdist = "normal", it is the standard deviation of the generation time 
#'distribution.
#'@param CI Model used to compute the confidence interval. Possible choice: "beta". 
#'  
#'@return data.frame with estimated Rt and confidence intervals. 
#'@examples
#'d <- getCases(cities = 3302205, lastday ="2018-03-10") # dengue
#' # Rt original
#'rt<-Rtoriginal(obj = d, count = "casos", meangt=3)
#'plot(rt$Rt, type="l", xlab = "weeks", ylab = "Rt")
#'lines(rt$lwr,lty=3); lines(rt$upr,lty=3)
#'abline(h = 1, col = 2)
#' # Rt delta and normal
#'rtdelta<-Rt(obj = d, count = "casos", gtdist="delta", meangt=3)
#'rtnorm<-Rt(obj = d, count = "casos", gtdist="normal", meangt=3, sdgt = 1)
#'lines(rtdelta$Rt, col = 3)
#'lines(rtdelta$lwr,lty = 3, col = 3)
#'lines(rtdelta$upr,lty = 3, col = 3)
#'lines(rtnorm$Rt, col = 4)
#'lines(rtnorm$lwr,lty = 3, col = 4)
#'lines(rtnorm$upr,lty = 3, col = 4)
#'legend(30,3,c("original","delta","normal"),lty=1, col = c(1,3,4), cex = 0.7)

Rt<-function(obj, count = "casos", gtdist, meangt, sdgt, CI = "beta", alpha = .95, a0 = 2 , b0 = 3){
  
  
  if(!any(c(count,"SE") %in% names(obj))) stop("obj must be a data.frame with variables 
                                              SE and var, at least. Consider using getCases")
  y <- obj[,count]
  le <- length(y)
  if (le < 2*meangt) warning("you need a time series                           
                             with size at least 2 generation intervals to estimate Rt")
  
  if (gtdist == "normal") ga <- rev(dnorm(x = 1:le, mean = meangt, sd = sdgt))
  if (gtdist == "delta")  {
    ga <- rep(0, le)
    ga[le - meangt] <- 1
  }
  
  obj$Rt <- NA
  obj$lwr <- NA
  obj$upr <- NA
  obj$p1 <- NA
  
    for (t in ceiling(2*meangt):le){
    num = y[t]
    deno = sum(y[1:t] * ga[(le-t+1):le]) # equation 4.1 in Wallinga and Lipsitch 2007
    obj$Rt[t]<-num/deno
    if (CI == "beta"){
       obj$p1[t] <- 1 - pbeta(.5, shape1 = num, shape2 = deno)
       obj[t, c("lwr","upr")] <- ll(betaconf(alpha = alpha, x = num, 
                               n = num + deno, a = a0, b = b0 ))
    }
  }
  
  obj
}


## obtain 100\lapha confidence/credibility intervals for the success probability \theta
betaconf <- function(alpha = .95, x, n, a = 1, b = 1, CP = "FALSE"){
  if(CP=="TRUE"){  
    lower <- 1 - qbeta((1-alpha)/2, n + x - 1, x)
    upper <- 1 - qbeta((1+alpha)/2, n - x, x + 1)
  }else{
    lower <- qbeta( (1-alpha)/2, a + x, b + n - x)
    upper <- qbeta(( 1+alpha)/2, a + x, b + n - x)  
  } 
  return(c(lower, upper))
  #CP stands for Clopper-Pearson
  #Default is 'Bayesian' with an uniform prior over p (a=b=1)
}


# R = theta/(1-theta)
ll <- function(x) {x/(1-x)}


################# NEW
# Function for parameter in Extrinsic Incubation Period
lambdaEIP<-function(T,v=4.3,beta0=7.9,betat=-0.21,Tbar=0) v/exp(beta0+betat*(T-Tbar))  

# Function for parameter in Intrinsic Incubation Period
lambdaIIP<-function(v=16,beta0=1.78) v/exp(beta0) 


#GenTimeDist -----------------------------------------------------------------------
#' @title Calculates temperature dependent generation time distribution
#' @description  Function to produce matrix of generation time distribution that depends on temperature.
#' For details, see the reference Codeco et al (2018).
#' @param serTemp : Temperature series
#' @param GT.max : maximum number of weeks to consider for generation time
#' @param cid10 "A90" for dengue (default), "A92": chikungunya, "A92.8": zika. 
#' @param smooth "sinusoidal" (default) or "raw"  
#' @return matrix with generation time distributions per week, one row per week, one column per SE. 
#' @references Codeco et al (2018) https://doi.org/10.1016/j.epidem.2018.05.011
#' @examples 
#'cli = getWU(stations = 'SBGL', vars=c("temp_med"), datasource=con) %>%
#'      mutate(temp_min = nafill(temp_med, rule = "arima")) 
#'gt <- GenTimeDist(cli$temp_med[1:50], smooth = "loess")
#'contour(gt, ylab="time", xlab="generation time (week)")

GenTimeDist <- function(serTemp, cid10 = "A90", GT.max = 10, smooth = "sinusoidal", nc = 1){
      
      # disease-specific parameters --------------------
      if (cid10 == "A90"){ # dengue
            par_a=c(16, 4.3, 1, 1)
            par_b=c(1/2.69821, 1/0.4623722, 1, 1)
            } else if (cid10 == "A92") {
                  message("no generation time parameters for chikungunya, using dengue's")
                  par_a=c(16, 4.3, 1, 1)
                  par_b=c(1/2.69821, 1/0.4623722, 1, 1)
            } else if (cid10 == "A92.8"){
                  message("no generation time parameters for zika, using dengue's")
                  par_a=c(16, 4.3, 1, 1)
                  par_b=c(1/2.69821, 1/0.4623722, 1, 1)
            }
      
      Tmax = length(serTemp)
      tt = 1:Tmax
      
      # raw temperature data or smoothed data?
      if(smooth == "sinusoidal"){
            mod <- lm(serTemp ~ sin(2*pi/52*tt)+cos(2*pi/52*tt))
            serie <- predict(mod)
      } else {
            serie <- serTemp
      }
      
      # function calculates the distribution in a given week x
      evalGenTimeDist <- function(x, serT=serie, a=par_a, b=par_b) {
            mxx <- int_sum_gamma_T(1, a, b, Temp=serT[x:(Tmax+GT.max+1)], t=tt[x:(Tmax+GT.max+1)], max=GT.max, unitscale=7)
            mxx$pdf
      }
      # for all weeks...
      gt <- parallel::mcmapply(evalGenTimeDist, tt, mc.cores=nc)
      class(gt) <- "generationtime"
      gt
}



#evalGenTimeDist --Not good yet-------------------------------------------------------------
#' @description  Function to produce matrix of generation time distribution
#' @param x : 
#' @param a : vector of 4 parameters for 4 gamma distributions
#' @param  b : vector of 4 parameters for 4 gamma distributions
#' @param serT : Temperature series
#' @param tt : time series
#' @param GT.max : maximum number of weeks to consider for generation time
#' @param Tmax 
#' @return matrix with generation time distributions per week, one row per week, one column per SE. 
#' @example 
#'cli = getWU(stations = 'SBGL', vars=c("temp_min"), datasource=con) %>%
#'      mutate(temp_min = nafill(temp_min, rule = "arima")) 
#'maxcores <- detectCores()  # paralelizacao
#'system.time(gt <- mcmapply(evalGenTimeDist, 1:nrow(cli), MoreArgs=list(serT=cli$temp_min, tt=1:nrow(cli)), mc.cores=1))   

# calcGenTimeDist <- function(cities, cid10 = "A90", datasource = con, x=1, a=c(16, 4.3, 1, 1), b=c(1/2.69821, 1/0.4623722, 1, 1),
#                               GT.max = 10, Tmax, ...){
#       pars_table <- read.parameters(cities = cities, cid10 = cid10, datasource)
#       
#       # Reading the names of the meterological stations for each city
#       sqlcity = paste("'", str_c(cities, collapse = "','"),"'", sep="")
#       comando <- paste("SELECT id, nome_regional, municipio_geocodigo, codigo_estacao_wu, estacao_wu_sec from 
#                        \"Dengue_global\".regional_saude WHERE municipio_geocodigo IN (", sqlcity, 
#                        ")" , sep="")
#       city_table <- dbGetQuery(datasource,comando)
#       
#       estacoes <- unique(c(city_table$estacao_wu_sec, city_table$codigo_estacao_wu))
#       print("usando dados de clima das estacoes:")
#       print(estacoes)
#       
#       # Reading the meteorological data
#       #print('Obtendo os dados de clima...')
#       cliwu <- getWU(stations = estacoes, vars = temp_med, finalday = finalday,datasource)
#       
#       
#       
#       map(mcmapply(evalGenTimeDist, MoreArgs=list(serT=cli$temp_min, tt=1:nrow(cli)), mc.cores=8))
# }


