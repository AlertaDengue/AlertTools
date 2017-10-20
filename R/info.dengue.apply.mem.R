#' Function to apply MEM algorithm to generate pre-epidemic, post-epidemic, and high activity thresholds
#' to municipalities covered by InfoDengue project
#' 
#' Function \code{info.dengue.apply.mem} uses MEM package to generate activity thresholds.
#' Apply MEM algorithm discarding seasons below local pre-epidemic threshold.
#' For each Municipality, pre-epidemic (limiar_preseason) threshold has a minimum set at 5 cases.
#' If calculated value falls below that, it is set to 5, with high activity (limiar_epidemico) set to 10 cases.
#' @name info.dengue.apply.mem
#'
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
#' @param ... Optional arguments passed to \code{memmodel}, from MEM package.
#'
#' @return Function \code{info.dengue.apply.mem} will return a data.table object with results by municipality:
#' \item{thresholds.table}{A data.table object with results for each municipality geocode, with columns
#' municipio_geocodigo, ano_inicio, ano_fim, limiar_preseason, limiar_posseason, limiar_epidemico}
#' @export
#'
#' @examples
#' Generate thresholds for Rio de Janeiro, Curitiba and Vitoria, using the whole history.
#' Return object instead of writing to data base:
#' mun_list <- c(3304557, 4106902, 3205309)
#' mun_list <- c(3205309)
#' 
#' thresholds.table <- info.dengue.apply.mem(con=con, passwd=password, start_year=0, mun_list=mun_list)
#' 
#' Write to database instead of returning object:
#' info.dengue.apply.mem(con=con, passwd=password, start_year=0, mun_list=mun_list, output='db')

info.dengue.apply.mem <- function(mun_list=mun_list, start_year=0, end_year=as.integer(format(Sys.Date(), '%Y'))-1,
                                  write='no', con, passwd=NULL, i.n.max=0,
                                  limiar.preseason=0.90, limiar.epidemico=0.95, ...){
  
  require(mem, quietly=TRUE, warn.conflicts=FALSE)
  require(plyr, quietly=TRUE, warn.conflicts=FALSE)
  require(data.table, quietly=TRUE, warn.conflicts=FALSE)
  
  # Read population table
  sqlquery = paste("SELECT  geocodigo as municipio_geocodigo, populacao
  FROM  \"Dengue_global\".\"Municipio\" AS m 
  INNER JOIN \"Dengue_global\".regional_saude as f
  ON m.geocodigo = f.municipio_geocodigo")
            
  df.pop <- dbGetQuery(con, sqlquery)
  
  # Process data in chuncks for 300 municipalities at a time:
  if (is.null(mun_list)){
    mun_list <- unique(df.pop$municipio_geocodigo)
  } else {
    df.pop <- df.pop[df.pop$municipio_geocodigo %in% mun_list,]
  }
  mun_list <- split(mun_list, ceiling(seq_along(mun_list)/300))
  
  # Prepare output data table
  thresholds.table <-   data.table('municipio_geocodigo'=integer(), 'ano_inicio'=integer(), 'ano_fim'=integer(),
                                   'pre'=double(), 'pos'=double(), 'muitoalta'=double())
  
  # Run by chuncks:
  for (mun_chunck in mun_list){
    # Read historical cases table
    df.inc <- read.cases(start_year, end_year, mun_list=mun_chunck,con=con)
    effec_start_year <- min(round(df.inc$SE/100))
    # Build incidence
    df.inc <- merge.data.frame(df.inc, df.pop, by='municipio_geocodigo')
    df.inc['inc'] <- df.inc$casos * 100000.0 / df.inc$populacao
    
    # Store only necessary data, separating seasons by columns
    dfsimple <- df.inc[df.inc$SE > max(df.inc$SE[df.inc$SE<(effec_start_year+1)*100])-12 &
                         df.inc$SE < (effec_start_year+1)*100+41, c('municipio_geocodigo', 'SE', 'inc')]
    effec_start_year_lbl <- paste0(effec_start_year,'-',effec_start_year+1)
    dfsimple <- rename(dfsimple, c('SE'=paste0('SE',effec_start_year_lbl), 'inc'=effec_start_year_lbl))
    seasons <- c(effec_start_year_lbl)
    for (i in (effec_start_year+1):end_year){
      if (max(df.inc$SE) >= (i+1)*100 + 41){
        dfsimple <- bindseason(df.inc, dfsimple, i)
        seasons <- cbind(seasons, paste0(i,'-',i+1))
      }
    }
    
    # Apply mem method
    thresholds.tab <- data.table(municipio_geocodigo=mun_chunck)
    base.cols <- c('municipio_geocodigo', 'pre', 'pos', 'veryhigh')
    thresholds <- applymem(dfsimple, seasons, i.n.max=i.n.max, i.level.threshold=limiar.preseason,
                           i.level.intensity=limiar.epidemico,...)$dfthresholds[base.cols]
    thresholds.tab <- merge(thresholds.tab, thresholds, by='municipio_geocodigo', all=TRUE)
    thresholds.tab[,c('casos_pre', 'casos_pos', 'casos_muitoalta')] <- 
      data.table(t(apply(thresholds.tab[, ..base.cols], 1, function(x)
        df.pop$populacao[df.pop$municipio_geocodigo == x[1]]*as.numeric(x[2:4])/100000)))
    thresholds.tab[,c('casos_pre', 'casos_pos', 'casos_muitoalta')] <- round(thresholds.tab[,c('casos_pre',
                                                                                               'casos_pos',
                                                                                               'casos_muitoalta')])
    thresholds.tab <- rename(thresholds.tab, replace=c('veryhigh'='muitoalta'))

    thresholds.tab[!is.na(thresholds.tab$pre) & thresholds.tab$casos_pre < 5,
                   c('pre', 'pos', 'muitoalta', 'casos_pre', 'casos_pos', 'casos_muitoalta')] <- NA
    
    if (length(thresholds.tab$pre[is.na(thresholds.tab$pre)]) > 0){
      thresholds.tab$casos_pre[is.na(thresholds.tab$pre)] <- 5
      thresholds.tab$casos_pos[is.na(thresholds.tab$pre)] <- 5
      thresholds.tab$casos_muitoalta[is.na(thresholds.tab$pre)] <- 10
      thresholds.tab[is.na(thresholds.tab$pre), c('pre', 'pos', 'muitoalta')] <- 
        data.table(t(apply(thresholds.tab[is.na(thresholds.tab$pre), c('municipio_geocodigo', 'casos_pre', 'casos_pos',
                                                                       'casos_muitoalta')], 1,
        function(x) 100000*as.numeric(x[2:4])/df.pop$populacao[df.pop$municipio_geocodigo==x[1]])))
    }
    
    thresholds.tab$ano_inicio <- start_year
    thresholds.tab$ano_fim <- end_year
    thresholds.tab <- thresholds.tab[, c('municipio_geocodigo', 'ano_inicio', 'ano_fim','pre', 'pos', 'muitoalta')]
    thresholds.table <- rbindlist(list(thresholds.table, thresholds.tab))
    
  }
  thresholds.table <- rename(thresholds.table, replace=c('pre'='limiar_preseason', 'pos'='limiar_posseason',
                                                         'muitoalta'='limiar_epidemico'))
  
  if (write=='db'){
    tgt.cols <- c('municipio_geocodigo', 'limiar_preseason', 'limiar_posseason', 'limiar_epidemico')
    write.parameters(params=tgt.cols, tab=thresholds.table[, ..tgt.cols], senha=password)
  } else {
    return(thresholds.table)
  }
  
}
