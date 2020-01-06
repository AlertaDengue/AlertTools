# Function to apply MEM algorithm to generate pre-epidemic, post-epidemic, and high activity thresholds
# to municipalities covered by InfoDengue project
#' 
#' Function \code{infodengue_apply_mem} uses MEM package to generate activity thresholds.
#' Apply MEM algorithm discarding seasons below local pre-epidemic threshold.
#' For each municipality, pre-epidemic (limiar_preseason) threshold has a minimum set at 5 cases.
#' If calculated value falls below that, it is set to 5, with high activity (limiar_epidemico) set 
#' to either the 90% quantile or 10, whichever is the greatest. The thresholds are given both in number of cases or
#' as incidence. The alert model uses incidence. 
#' @name infodengue_apply_mem
#' @param start_year Historical data starting year to consider. Default: 0 (i.e., as old as possible)
#' @param end_year Historical data final year to consider. Default: previous year from Sys.Date()
#' @param mun_list Vector of municipalities geocode. Default: NULL (i.e., all municipalites)
#' @param write Whether results should be saved into project's database. Default: 'no'
#'    'no' return data.table without writing in database.
#'    'db' writes data.table to InfoDengue's database using \code{write.parameters} from AlerttTools package.
#' @param con Connection to PostGreSQL database, using \code{dbConnect} from RPostgreSQL package
#' @param passwd database password for writing output to db if write='db'.
#' @param i.n.max Number of points by seasons to be used for pre-epidemic and epidemic regions to calculate each threshold.
#'    If 0 (default), uses all points in those regions. Else, uses n max values in each region per season.
#'    This value is passed to i.n.max parameter in \code{memmodel}, from MEM package.
#' @param limiar.preseason Level of confidence interval to calculate pre/post-season thresholds. Default: 0.9.
#'    This value is passed to i.level.threshold parameter in \code{memmodel}, from MEM package.
#' @param limiar.epidemico Level of confidence interval to calculate high activity during epidemic region. Default: 0.95.
#'    This value is passed to i.level.intensity parameter in \code{memmodel}, from MEM package.
#' @param mincases.pre minimum number of cases to launch a preseason alert. Default: 5
#' @param mincases.epi minimum number of cases to launch an epidemic alert. Default: 10
#' @param ... Optional arguments passed to \code{memmodel}, from MEM package.
#' @return Function \code{info.dengue.apply.mem} will return a list with thresholds calculated by mem, by simple percentile and the
#' choice used in the infodengue model, by municipality: 
#'  \describe{
#'  \item{mem: threshold values calculated by mem}{municipio_geocodigo = geocode, inc_preseason = preseason incidence threshold, inc_posseason = 
#'  pos-season incidence threshold, inc_epidemico = high incidence threshold, inicio and inicio.ic = estimated begining of the season
#'  duracao, duracao.ic = season duration. ano_inicio and ano_fim used for calculation.} 
#'  \item{percentiles: threshold values calculated as percentiles (incidence)}{quant_pre = preseason incidence threshold,
#'  quant_pos = posseason incidence threshold, quant_epidemico = epidemic incidence threshold.} 
#'  \item{min_threshold_inc:}{pre-defined minimum threshold.} 
#'  \item{threshold: values used by Infodengue}{MEM's values if cases > min.cases, percentiles otherwise: limiar_preseason, limiar_posseason, 
#'  limiar_epidemico.}    
#'    }
#' @examples
#' Generate thresholds for Rio de Janeiro, Curitiba and Vitoria, using the whole history. 
#' Return object instead of writing to data base:
#' mun_list <- c(3304557, 4106902, 3205309)
#' mun_list <- getCidades(uf = "Ceará", datasource=con)$municipio_geocodigo
#' thresCE <- infodengue_apply_mem(mun_list=mun_list,database=con)
#' 
#' A nice way to visualize the calculated thresholds
#' plot(thresCE) 
#' 
#' Write to database instead of returning object requires password:
#' thres <- infodengue_apply_mem(con=cond, passwd=password, mun_list=mun_list[1:10], write='db')


infodengue_apply_mem <- function(mun_list, start_year=2010, end_year=as.integer(format(Sys.Date(), '%Y'))-1,
                                  write='no', database, passwd=NULL, i.n.max=0,
                                  limiar.preseason=0.95, limiar.epidemico=0.95, i.type.curve=2,
                                  i.type.threshold=2, i.type.intensity=2, mincases.pre = 5, mincases.pos = 5,
                                  mincases.epi=10, ...){
  
  require(mem, quietly=TRUE, warn.conflicts=FALSE)
  #require(plyr, quietly=TRUE, warn.conflicts=FALSE)
  require(data.table, quietly=TRUE, warn.conflicts=FALSE)
  
  #stopifnot(is.numeric(mun_list),"MEM: mun_list should be a numeric vector")       
  # Read population table
  sqlquery = paste("SELECT  geocodigo as municipio_geocodigo, populacao
  FROM  \"Dengue_global\".\"Municipio\" AS m 
  INNER JOIN \"Dengue_global\".regional_saude as f
  ON m.geocodigo = f.municipio_geocodigo")
            
  df.pop <- dbGetQuery(conn=con, sqlquery)
  
  # Process data in chuncks for 300 municipalities at a time:
  if (is.null(mun_list)){
    mun_list <- unique(df.pop$municipio_geocodigo)
  } else {
    df.pop <- df.pop[df.pop$municipio_geocodigo %in% mun_list,]
  }
  mun_list <- split(mun_list, ceiling(seq_along(mun_list)/300))
  
  # Prepare output data table
  #thresholds.table <-   data.table('municipio_geocodigo'=integer(), 'ano_inicio'=integer(), 'ano_fim'=integer(),
  #                                 'pre'=double(), 'pos'=double(), 'muitoalta'=double())
  thresholds.table <-   data.table(NULL)
 
   # Run by chuncks:
  for (mun_chunck in mun_list){
        
    # Read historical cases table
    df.inc <- read.cases(start_year, end_year, mun_list=mun_chunck)
    effec_start_year <- min(round(df.inc$SE/100))
    # Build incidence
    df.inc <- merge.data.frame(df.inc, df.pop, by='municipio_geocodigo')
    df.inc['inc'] <- df.inc$casos * 100000.0 / df.inc$populacao
    
    # Store only necessary data, separating seasons by columns
    dfsimple <- df.inc[df.inc$SE > max(df.inc$SE[df.inc$SE<(effec_start_year+1)*100])-12 &
                         df.inc$SE < (effec_start_year+1)*100+41, c('municipio_geocodigo', 'SE', 'inc')]
    effec_start_year_lbl <- paste0(effec_start_year,'-',effec_start_year+1)
    dfsimple <- plyr::rename(dfsimple, c('SE'=paste0('SE',effec_start_year_lbl), 'inc'=effec_start_year_lbl))
    seasons <- c(effec_start_year_lbl)
    for (i in (effec_start_year+1):end_year){
      if (max(df.inc$SE) >= (i+1)*100 + 41){
        dfsimple <- bindseason(df.inc, dfsimple, i)
        seasons <- cbind(seasons, paste0(i,'-',i+1))
      }
    }
    
    # Apply quantile method (new)
    quantile.tab <- df.inc %>% 
          group_by(municipio_geocodigo) %>%
          summarise(quant_pre = max(mincases.pre, quantile(casos, probs = 0.10))/mean(populacao)*1e5,
                    quant_pos = max(mincases.pre,quantile(casos, probs = 0.10))/mean(populacao)*1e5,
                    quant_epidemico = max(mincases.epi,quantile(casos, probs = limiar.epidemico))/mean(populacao)*1e5)
    
    # Apply mem method
    thresholds.tab <- data.table(municipio_geocodigo=mun_chunck)
    base.cols <- c('municipio_geocodigo', 'pre', 'pos', 'veryhigh')
    thresholds <- applymem(dfsimple, seasons, i.n.max=i.n.max, i.level.threshold=limiar.preseason,
                           i.level.intensity=limiar.epidemico,
                           i.type.curve=i.type.curve, i.type.threshold=i.type.threshold,
                           i.type.intensity=i.type.intensity, ...)$dfthresholds#[base.cols]
    thresholds.tab <- merge(thresholds.tab, thresholds, by='municipio_geocodigo', all=TRUE) # mem calcula limiar em incidencia
    thresholds.tab <- merge(thresholds.tab, df.pop, by='municipio_geocodigo', all.x = TRUE) # agrega pop para calcular casos
    thresholds.tab <- merge(thresholds.tab, quantile.tab, by='municipio_geocodigo', all.x = TRUE) # agrega pop para calcular casos
    thresholds.tab <- thresholds.tab %>%
          mutate(mininc_pre = mincases.pre/populacao*1e5,
                 mininc_pos = mincases.pos/populacao*1e5,
                 mininc_epi = mincases.epi/populacao*1e5,
                limiar_preseason = case_when(pre > mininc_pre ~ pre, TRUE ~ quant_pre),
                 limiar_posseason = case_when(pos > mininc_pos ~ pos, TRUE ~ quant_pos),
                 limiar_epidemico = case_when(veryhigh > mininc_epi ~ veryhigh, TRUE ~ quant_epidemico)) %>%
          rename(inc_preseason = pre,
                 inc_posseason = pos,
                 inc_epidemico = veryhigh)
    
    
    thresholds.tab$ano_inicio <- start_year
    thresholds.tab$ano_fim <- end_year
    #thresholds.tab <- thresholds.tab[, c('municipio_geocodigo', 'ano_inicio', 'ano_fim','pre', 'pos', 'muitoalta')]
    thresholds.table <- rbindlist(list(thresholds.table, thresholds.tab))
    
  }
#thresholds.table <- plyr::rename(thresholds.table, replace=c('pre'='limiar_preseason', 'pos'='limiar_posseason',
#                                                         'muitoalta'='limiar_epidemico'))
        
  if (write=='db'){
    tgt.cols <- c('municipio_geocodigo', 'limiar_preseason', 'limiar_posseason', 'limiar_epidemico')
    write.parameters(params=tgt.cols, tab=thresholds.table[, tgt.cols], senha=passwd)
  } else {
        obj <- list(mem = thresholds.table[,c('municipio_geocodigo','inc_preseason','inc_posseason','inc_epidemico','inicio',
                                           'inicio.ic','duracao','duracao.ic')],
                    percentiles = thresholds.table[,c('municipio_geocodigo','quant_pre','quant_pos','quant_epidemico')],
                    min_threshold_inc = thresholds.table[,c('municipio_geocodigo','populacao','mininc_pre','mininc_pos','mininc_epi')],
                    thresholds = thresholds.table[,c('municipio_geocodigo','limiar_preseason','limiar_posseason',
                                                  'limiar_epidemico','ano_inicio','ano_fim')])
        class(obj) <- "infomem"
        return(obj)
  }
  
}

# USE: png("threshold_PR.png",width = 30,height = 10,units = "cm",res=100)
#  plot(thres1)
# title("PR")
# dev.off()
plot.infomem <- function(obj,...){
      ordem <- order(obj$min_threshold_inc$populacao)
      plot(obj$mem$inc_epidemico[ordem], axes=FALSE,xlab="",ylab="red thresholds",pch=16, ylim=c(0,max(obj$mem$inc_epidemico,
                                                                                                obj$percentiles$quant_epidemico,
                                                                                                obj$min_threshold_inc$mininc_epi,
                                                                                                obj$thresholds$limiar_epidemico,na.rm=TRUE)))
      axis(2)
      axis(1,at = 1:nrow(obj$mem), labels = obj$mem$municipio_geocodigo[ordem],las=2,cex.axis=0.6)
      points(obj$percentiles$quant_epidemico[ordem], pch="*")
      points(obj$min_threshold_inc$mininc_epi[ordem], pch="+")
      lines(obj$thresholds$limiar_epidemico[ordem], type="h")
      legend("topright",legend = c("mem","percentile","minimum","infodengue"), pch = c('o','*','+','|'),cex=0.8)
      #abline(h=10, col =2)
}
