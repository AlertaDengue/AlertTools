"
Reads historical incidence data and extract epidemic thresholds per AP,
using MEM package https://cran.r-project.org/web/packages/mem/

:apsids: list with APs name.
:epithresholds: list with full epimem report for each ap, in the same order as apsids
:dfthresholds: dataframe with epidemic threhsold per AP, with pre, pos, mid, high and very high
               intensity thresholds
"

require(mem)
require(plyr)
#require(logging)

# Initialize logger
#basicConfig()
#addHandler(writeToFile, file="./dengue-mem.log", level='INFO')
bindseason <- function(df1=data.frame(), df2=data.frame(), baseyear=integer()){
  "
  Function to bind season incidences from df1 onto df2, placing each season in a new colum
  Returns:
  :df3: data.frame df2 with 2 new colums from df1$SE and df1$inc based on df1$SE within season
        of base year, which is from winter of baseyear to winter of baseyear+1 (south hemisphere).
  "
  
  if (missing(df1) | missing(df2) | missing(baseyear)){
    #logerror('missing argument on function call', logger='dengue-mem.bindseason')
    return(NULL)
  }

  ti <- (baseyear+1)*100
  tf <- (baseyear+1)*100 + 41
  df3 <- cbind(df2, df1[(df1$SE > max(df1$SE[df1$SE<ti])-12 & df1$SE < tf),
                      c('SE', 'inc')])
  suff <- paste(as.character(baseyear),as.character(baseyear+1), sep='-')
  newse <- paste0('SE',suff)
  newinc <- suff
  df3 <- rename(df3, c('SE'=newse, 'inc'=newinc))
  
  #loginfo('Function executed and exited with status 0', logger='dengue-mem.bindseason')
  return(df3)
}


# applymem ------------------------------------
#'@description  Function to apply epimem algorithm on df.data and generate full reports as well as a data frame with summary of relevant thresholds (pre, pos, mid, high, veryhigh) 
#'@title Get epidemic thresholds  for a list of cities
#'@param df.data data frame with municipio_geocodigo in a column named municipio_geocodigo, and incidence seasons in each column each row gives the incidence in each season, for each municipio_geocodigo, for each week.
#'@param l.seasons vector with incidence columns to be used. Additional arguments accepted by memmodel function
#' @param i.n.max Number of pre-epidemic values used to calculate the threshold.
#' @param i.type.curve Type of confidence interval to calculate the modelled curve.
#' @param i.level.threshold Level of confidence interval to calculate the threshold.
#' @param i.level.curve Level of confidence interval to calculate the modelled curve.
#' @param i.data Data frame of input data.
#' @param i.seasons Maximum number of seasons to use.
#' @param i.type.threshold Type of confidence interval to calculate the threshold.
#' @param i.tails.threshold Tails for the confidence interval to calculate the threshold.
#' @param i.type.intensity Type of confidence interval to calculate the intensity thresholds.
#' @param i.level.intensity Levels of confidence interval to calculate the intensity thresholds.
#' @param i.tails.intensity Tails for the confidence interval to calculate the threshold.
#' @param i.type.other Type of confidence interval to calculate length, start and percentages.
#' @param i.level.other Level of confidence interval to calculate length, start and percentages.
#' @param i.method Method to calculate the optimal timing of the epidemic.
#' @param i.param Parameter to calculate the optimal timing of the epidemic.
#' @param i.type.boot Type of bootstrap technique.
#' @param i.iter.boot Number of bootstrap iterations.
#'@author Marcelo F Gomes
#'@details internal function, used by info.dengue.apply.mem.   Defatul arguments passed to memmodel function:
#'@return epithresholds: list with full epimem report for each municipio_geocodigo, keyed by AP's name. dfthresholds: data frame with thresholds for each AP.

applymem <- function(df.data, l.seasons, ...){
  if (missing(df.data)){
    #logerror('missing argument on function call', logger='dengue-mem.applymem')
    message('missing argument on function call: applymem')
        return(NULL)
  }
  
  # List of municipio_geocodigos
  municipio_geocodigoids <- unique(df.data$municipio_geocodigo)
  
  # Prepare output objects
  epithresholds <- list()
  df.typ.real.curve <-data.frame() # Typical seasonal curve
  dfthresholds <- data.frame(municipio_geocodigoids)
  dfthresholds <- rename(dfthresholds, c('municipio_geocodigoids'='municipio_geocodigo'))
  
  dfthresholds['pre'] <- NULL # Pre-epidemic threshold (at .95 confidence interval by default)
  dfthresholds['pos'] <- NULL # Post-epidemic threshold
  dfthresholds['mid'] <- NULL # Mid activity threshold (corresponding to 0.4 quantile by default)
  dfthresholds['high'] <- NULL # High activity threshold (corresponding to 0.9 quantile by default)
  dfthresholds['veryhigh'] <- NULL # Very high activity threshold (corresponding to 0.95 quantile by default)
  dfthresholds['inicio'] <- NULL # Typical begining of epidemic activity
  dfthresholds['duracao'] <- NULL # Typical duration
  
  for (geocodid in municipio_geocodigoids){
    f <- function(geocod, l.seasons, ...){
      # Firstly, use all seasons
      df.geocodid <- df.data[df.data$municipio_geocodigo==geocod, l.seasons]
      non.null.seasons <- df.geocodid[, colSums(df.geocodid)>0]
      l.seasons.geocodid <- names(non.null.seasons)
      epitmp <- memmodel(i.data=non.null.seasons, ...)
      
      # Discard seasons that are below threshold and rerun.
      # This is useful for properly defining activity levels during an epidemic
      discard <- NULL
      prethreshold <- epitmp$pre.post.intervals[1,3]
      postthreshold <- epitmp$pre.post.intervals[2,3]
      epitmp$typ.real.curve <- epitmp$typ.curve 
      typ.real.curve <- rename(data.frame(epitmp$typ.real.curve), c('X1'='baixo', 'X2'='mediano' ,'X3'='alto'))
      # Clean typical curve:
      typ.real.curve$mediano[is.na(typ.real.curve$mediano)] <- 0
      typ.real.curve$baixo[typ.real.curve$baixo < 0] <- 0
      typ.real.curve$baixo[is.na(typ.real.curve$baixo)] <- typ.real.curve$mediano[is.na(typ.real.curve$baixo)]
      typ.real.curve$alto[is.na(typ.real.curve$alto)] <- typ.real.curve$mediano[is.na(typ.real.curve$alto)]
      
      episeasons <- sapply(non.null.seasons, max, na.rm=TRUE) > prethreshold
      epitmp <- memmodel(i.data=non.null.seasons[, episeasons], ...)
      epitmp$typ.real.curve <- epitmp$typ.curve 
      
      # Store full report in epithresholds:
      epithresholds[[geocod]] <- epitmp
      
      # Store typical curves from full set of seasons
      epithresholds[[geocod]]$typ.real.curve <- typ.real.curve
      epithresholds[[geocod]]$typ.real.curve['SE'] <- c(seq(41,52), seq(1,40))
      
      # Store epidemic thresholds
      dfthresholds$pre[dfthresholds$municipio_geocodigo==geocod] <- epitmp$pre.post.intervals[1,3]
      dfthresholds$pos[dfthresholds$municipio_geocodigo==geocod] <- epitmp$pre.post.intervals[2,3]
      dfthresholds$veryhigh[dfthresholds$municipio_geocodigo==geocod] <- epitmp$epi.intervals[1,4]
      dfthresholds$inicio[dfthresholds$municipio_geocodigo==geocod] <- (epitmp$mean.start - 1 + 41) %% 52
      if (dfthresholds$inicio[dfthresholds$municipio_geocodigo==geocod]==0){
        dfthresholds$inicio[dfthresholds$municipio_geocodigo==geocod] = 52
      }
      ci.start.i <- (epitmp$ci.start[1,1] - 1 + 41) %% 52
      if (ci.start.i==0){
        ci.start.i = 52
      }
      ci.start.f <- (epitmp$ci.start[1,3] - 1 + 41) %% 52
      if (ci.start.f==0){
        ci.start.f = 52
      }
      dfthresholds$inicio.ic[dfthresholds$municipio_geocodigo==geocod] <- paste0('[', ci.start.i, '-',
                                                                                 ci.start.f, ']')
      dfthresholds$duracao[dfthresholds$municipio_geocodigo==geocod] <- epitmp$mean.length
      dfthresholds$duracao.ic[dfthresholds$municipio_geocodigo==geocod] <- paste0('[', epitmp$ci.length[1,1], '-',
                                                                                  epitmp$ci.length[1,3], ']')
      return(list("epithresholds.tmp"=epithresholds, "dfthresholds.tmp"=dfthresholds))
    }
    
    res <- try(f(geocod=geocodid, l.seasons=l.seasons, ...), TRUE)
    if (!inherits(res, "try-error")){
      epithresholds <- res$epithresholds.tmp
      dfthresholds <- res$dfthresholds
    }
    
  }
  #loginfo('Function executed and exited with status 0!', logger='dengue-mem.applymem')
  return(list("epimemthresholds"=epithresholds, "dfthresholds"=dfthresholds))
}

# "
# Example of usage, based on ./alertaAPS_201539.csv input file.
# Applies method and generate full report and plots for each AP.
# "
# # Read historical data
# dfcomplete <- read.csv('./alertaAPS_201539.csv')
# 
# # Store only necessary data, separating seasons by columns
# dfsimple <- dfcomplete[dfcomplete$SE > max(dfcomplete$SE[dfcomplete$SE<201100])-12 &
#                          dfcomplete$SE < 201141,
#                        c('APS', 'SE', 'inc')]
# dfsimple <- rename(dfsimple, c('SE'='SE2010-2011', 'inc'='inc2010-2011'))
# seasons <- c('inc2010-2011')
# for (i in 2011:2014){
#   if (max(dfcomplete$SE) >= (i+1)*100 + 41){
#     dfsimple <- bindseason(dfcomplete, dfsimple, i)
#     seasons <- cbind(seasons, paste0('inc',i,'-',i+1))
#   }
# }
# 
# thresholds <- applymem(dfsimple)
# # Plotting structure:
# 
# for (aps in unique(dfsimple$APS)){
#   sprintf("APS: %s\n", aps)
#   print(thresholds$epimemthresholds[[aps]])
#   plot(thresholds$epimemthresholds[[aps]])
#   title(main=aps)
# }