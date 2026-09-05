# PROJETO ALERTA DENGUE -------------------------------------
# Funcoes de calculo do alerta
# Claudia Codeco 2015
#


#setCriteria -------------------------------------------------------------------
#'@title Define rules to issue a four level alert Green-Yellow-Orange-Red.
#'@description The criteria for transition between colors (alert levels) can be
#'chosen from existing rules or can be specified by the user. The built in rules are:
#'Af (minimum temperature defines yellow) and Aw (humidity does), AsAw (temp min and max humidity together).
#'@noRd
#'@param rule either a built-in rule ("Af", "Aw", "AsAw","Awi","AsAwi") or a list with three elements defining criteria for
#'transition to yellow (level 2), orange (level 3) and red (level 4). See description.
#'@param delays list with three elements, each one is a vector: c(delay to turn on, delay to turn off)
#'@param values named vector of values for the critical parameters. Use character.
#'@return list with rules. To be useful, this list must contain variables that match those in the data.
#'@examplesIf FALSE
#'# Defining values manually
#'val <- c("varcli" ="temp_min", "limiar_preseason"="10","limiar_epidemico"="100", "clicrit"="22"
#', "clicrit2" = "80", varcli2 = "umid_max")
#'setCriteria(rule="Af",values=val)
#'setCriteria(rule="AsAw",values=val)
#'# Using infodengue parameters:
#'val <- read.parameters(1200401)
#'setCriteria(rule=val$codmodelo, values=val)

.define_alert_rules_impl <- function(rule=NULL, values=NULL,
                        delays = list(delayy = c(0,0), delayo = c(0,0), delayr = c(0,0))){

      # checking input
      if(!is.null(rule)) assert_that(rule %in% c("Af","Aw","AsAw","Awi","AsAwi"),
                                     msg = "setcriteria: rule unknown. Try Af, Aw, Awi, AsAw or AsAwi")
      if(is.null(rule)) {
            assert_that(!is.null(values), msg = "setcriteria: if rule is null, values must be provided.")
            assert_that(any(names(values) %in% c("varcli", "clicrit", "limiar_preseason",
                                                 "limiar_epidemico")), msg = "setcriteria: elements missing from arg values")
      }


      # pre-defined rules
      if(!is.null(rule)){

            if(rule[1] == "Af"){
                  criteria <- list(
                        crity = c("temp_min > temp_crit & inc > 0", 3, 0), #3,2
                        crito = c("p1 > 0.95 & inc > limiar_preseason", 2, 1), #3,2
                        critr = c("inc > limiar_epidemico & casos > 10", 2, 1) #2,2
                  )}
            if (rule[1] == "Aw"){
                  criteria = list(
                        crity = c("umid_max > umid_crit & inc > 0", 3, 0), #3,2
                        crito = c("p1 > 0.95 & inc > limiar_preseason", 2, 0), #3,2
                        critr = c("inc > limiar_epidemico & casos > 10", 2, 0) #2,2
                  )}
         if (rule[1] == "Awi"){
            criteria = list(
               crity = c("umid_min > umid_crit & inc > 0", 3, 0), #3,2
               crito = c("p1 > 0.95 & inc > limiar_preseason", 2, 0), #3,2
               critr = c("inc > limiar_epidemico & casos > 10", 2, 0) #2,2
            )}
         if(rule[1] == "AsAw"){
                  criteria = list(
                        crity = c("temp_min > temp_crit & umid_max > umid_crit & inc > 0", 3, 0), #3,2
                        crito = c("p1 > 0.95 & inc > limiar_preseason", 3, 0), #3,2
                        critr = c("inc > limiar_epidemico & casos > 10", 2, 0) #2,2
                  )}
         if(rule[1] == "AsAwi"){
            criteria = list(
               crity = c("temp_min > temp_crit & umid_min > umid_crit & inc > 0", 3, 0), #3,2
               crito = c("p1 > 0.95 & inc > limiar_preseason", 3, 0), #3,2
               critr = c("inc > limiar_epidemico & casos > 10", 2, 0) #2,2
            )}
            # user defined rules
      } else {
         message("setcriteria function using user defined criteria")
            criteria<-lapply(1:3, function(x) c(rule[[x]], delays[[x]]))
            names(criteria) <- c("crity","crito","critr")
      }

      # substituting values (very bad coding, should be improved)
      if(!is.null(values)) {  #used in the pipeline
            if (!("varcli2" %in% names(values))) values[["varcli2"]] <- "xx"

            if (rule[1] %in% c("Af", "AsAw","AsAwi")){ # reading temp
                  assert_that(values[["varcli"]] == "temp_min" | values[["varcli2"]] == "temp_min",
                              msg = "setcriteria: Af, AsAw and AsAwi require temp_min")
                  tm <- names(values)[which(values == "temp_min")]
                  if(tm == "varcli")  values <- c(values, "temp_crit" = values[["clicrit"]])
                  if(tm == "varcli2") values[["temp_crit"]] <- c(values, "temp_crit" = values[["clicrit2"]])
            }

            if (rule[1] %in% c("Aw", "AsAw")){ # reading umidmax
                  assert_that(values[["varcli"]] == "umid_max" | values[["varcli2"]] == "umid_max",
                              msg = "setcriteria: Aw and AsAw require umid_max")
                  um <- names(values)[which(values == "umid_max")]
                  if(um == "varcli")   values <- c(values, "umid_crit" = values[["clicrit"]])
                  if(um == "varcli2")  values <- c(values, "umid_crit" = values[["clicrit2"]])
            }

            if (rule[1] %in% c("Awi", "AsAwi")){ # reading umidmin
               assert_that(values[["varcli"]] == "umid_min" | values[["varcli2"]] == "umid_min",
                           msg = "setcriteria: Awi and AsAwi require umid_min")
               um <- names(values)[which(values == "umid_min")]
               if(um == "varcli")   values <- c(values, "umid_crit" = values[["clicrit"]])
               if(um == "varcli2")  values <- c(values, "umid_crit" = values[["clicrit2"]])
            }

            #if(class(values) == "data.frame")
            #      { # handling values from read.parameters
            values <- unlist(sapply(names(values),function(x) values[[x]]))
            #      }


            criteria <- lapply(criteria, function(x) c(str_replace_all(x[1], values), x[c(2,3)]))
      }

      criteria
}


#fouralert ---------------------------------------------------------------------
#'@title Define conditions to issue a four level alert Green-Yellow-Orange-Red.
#'@description Yellow is raised when environmental conditions required for
#'positive mosquito population growth are detected, green otherwise.Orange
#'indicates evidence of sustained transmission, red indicates evidence of
#'an epidemic scenario.
#'@noRd
#'@param obj dataset with data to feed the alert, containing the variables specified in crit.
#'@param crit criteria for the alert colors. See setCriteria()
#'@param dy if inc > 0, and rt was orange or red at least once in the past
#'dy weeks -> level yellow. Default: dy=4
#'@param miss how missing data is treated. "last" if last value is repeated.
#'It is currently the only option
#'@return returns an object of class "alerta" containing four elements: the data,
#'the alert indices, and the rules used to define the indices.
#'@examplesIf FALSE
#' # Parameters of the alert model
#'val = c(varcli ="temp_min", "clicrit"="22","limiar_preseason"="10","limiar_epidemico"="100")
#'criteria = setCriteria(rule="Af",values=val)
#'# Get, organize data
#'cas = getCases(cities = 3200300, cid10 = "A90") %>%
#'      Rt(count = "casos",gtdist="normal", meangt=3, sdgt = 1) %>%
#'      mutate(inc = casos/pop*100000)
#'cli = getWU(stations = 'SBGL', vars="temp_min") %>%
#'      mutate(temp_min = nafill(temp_min, rule = "arima"))
#'tw = getTweet(cities = 3200300)
#'# Calculate alert
#'ale <- plyr::join_all(list(cas,cli,tw), by="SE")
#'resf <- fouralert(ale, crit = criteria)


.classify_alerts_impl <- function(obj, crit, miss="last",dy=4){

      # checking input
      assert_that(all(names(crit) %in% c("crity", "crito", "critr")) &
                        all(sapply(crit,length) %in% 3),
                  msg = "fouralert: argument crit is mispecified")


      # criteria
      #cyellow = crit[[1]]; corange = crit[[2]]; cred = crit[[3]]
      parsed_rules <- lapply(crit, function(x) parse(text=x[1]))
      delay_turnon <- lapply(crit, function(x) as.numeric(x[[2]]))
      delay_turnoff <- lapply(crit, function(x) as.numeric(x[[3]]))

      # checking delays
      #assert_that(all(sapply(c(delay_turnon, delay_turnoff), is.count)),
      #            msg = "fouralert: delays are mispecified")

      # fun to accumulate conditions
      accumcond <- function(vec,lag){
            if (lag ==1 )return(vec)
            zoo::rollapply(vec, lag, sum, align = "right", fill = NA)
      }

      # fun assert condition (week and accumulated)
      assertcondition <- function(dd, nivel){
            condtrue <- with(dd, as.numeric(eval(parsed_rules[[nivel]])))
            mi <- which(is.na(condtrue)) # missing conditions
            if (miss == "last"){
                  #if(le %in% mi) message("missing condition, repeating last value")
                  for (i in mi[mi!=1]) condtrue[i] <- condtrue[i-1]
            }
            # counting accumulated conditions
            ncondtrue <- accumcond(condtrue, delay_turnon[[nivel]])
            cbind(condtrue, ncondtrue)
      }
      #
      le <- nrow(obj)

      indices <- data.frame(cbind(assertcondition(obj, 1),
                                  assertcondition(obj, 2),
                                  assertcondition(obj, 3)))
      names(indices) <- c("cytrue", "nytrue","cotrue", "notrue","crtrue", "nrtrue")

      # setting the alert level when delay_on is reached(1 = green, 2 = yellow, 3 = orange, 4 = red)
      indices$level <- 1
      indices$level[indices$nytrue == delay_turnon[1]] <-2
      indices$level[indices$notrue == delay_turnon[2]] <-3
      indices$level[indices$nrtrue == delay_turnon[3]] <-4



      # delayed turnoff
      delayturnoff <- function(level){
            delay_level = delay_turnoff[[(level-1)]]# as.numeric(as.character(cond[3]))

            ifelse (delay_level == 0, return(indices),
                    {pos <- which(indices$level==level) %>% # weeks with alert at level delay_level
                          lapply(.,function(x)x+seq(0,delay_level)) %>% # current and subsequent weeks
                          unlist() %>%
                          unique()
                    pos <- pos[pos<=le] # remove inexisting rows
                    indices$level[pos] <- level #unlist(lapply(indices$level[pos], function(x) max(x,2)))
                    return(indices)
                    })
      }

      indices <- delayturnoff(level=4)
      indices <- delayturnoff(level=3)
      indices <- delayturnoff(level=2)

      # from orange-red to yellow:
      # if rt was orange or red at least once in the past dy weeks -> level yellow
      #contains_34 <- which(zoo::rollapply(indices$level,list(c(-dy:0)),
      #                                    function(x) any(x>=3), fill=NA))

      # to visualize how it works, descomment the following lines
      #indices$dy <- NA
      #indices$dy[contains_34]<-pmax(indices$level[contains_34], rep(2, length(contains_34)))

      #indices$level[contains_34]<-pmax(indices$level[contains_34],
      #                                 rep(2, length(contains_34)))

      ale <- list(data=obj, indices=indices, crit = crit, n=4)
      class(ale)<-"alerta"
      return(ale)
}

#pipe_infodengue ---------------------------------------------------------------------
#'@title pipeline used by infodengue
#'@description wrap of functions used by Infodengue.
#'@export
#'@param cities In general, a vector of 7-digit geocodes. If it is a data.frame containing geocodes
#' and all parameters, these will replace the database's parameters.
#'@param cid10 default is A90 (dengue). Chik = A92.0, Zika = A92.8
#'@param narule how to treat missing climate data. Do nothing (default), "zero" fills
#'with zeros, "linear" for linear interpolation, "arima" for inter and extrapolation.
#'@param finalday Explicit last reporting date. Required when `datarelatorio`
#'is not supplied; otherwise it is derived from that epidemiological week.
#'@param iniSE first date of the disease data. Default = 201501. Minimum = 201001.
#'@param datarelatorio epidemiological week
#'@param nowcasting `"bayesian"` for the dynamic model or `"none"` to skip nowcasting.
#'@param completetail if sinan data is older than final_day, fill in the tail with NA (default) or 0.
#'@param dataini "notif" (default) or "sinpri"
#'@param writedb TRUE if it should write into the database, default is FALSE.
#'@param datasource posgreSQL connection to project's database
#'@param workers Positive number of workers. Defaults to portable serial execution.
#'@param seed Optional seed for stochastic nowcasting.
#'@param verbose Whether to emit progress messages.
#'@param version_date Explicit model-version date used when writing results.
#'@return data.frame with the week condition and the number of weeks within the
#'last lag weeks with conditions = TRUE.
#'@examplesIf FALSE
#'cidades <- getCidades(uf = "Rio de Janeiro",datasource = connection)
#'t1 <- Sys.time()
#'res <- pipe_infodengue(cities = 3304557, cid10 = "A90",
#'nowcasting="bayesian", dataini= "sinpri", completetail = 0, datarelatorio = 202419)
#'tail(tabela_historico(res))
#'t2 <- Sys.time()
#'message(paste("total time was", t2-t1))
#'tail(tabela_historico(res))
#'res <- pipe_infodengue(cities = 4209102 , cid10 = "A90",
                       #'nowcasting="none", dataini= "sinpri", completetail = 0,
                       #' datarelatorio = 202408)

pipe_infodengue <- function(cities, cid10="A90", datarelatorio, finalday = NULL,
                            iniSE = 201001, nowcasting="none", narule=NULL,
                            writedb = FALSE, datasource, completetail = NA,
                            dataini = "notific", workers = 1L, seed = NULL,
                            verbose = FALSE, version_date = finalday){

      .deprecate_api("pipe_infodengue()", "run_alert_pipeline()")
      .db_validate_connection(datasource)


      if(missing(datarelatorio)) {
            if (is.null(finalday)) {
                  stop("Supply an explicit `datarelatorio` or `finalday`.", call. = FALSE)
            }
            datarelatorio <- data2SE(finalday, format = "%Y-%m-%d")
      } else { # if datarelatorio & finalday are given, priority is datarelatorio
            finalday <- SE2date(datarelatorio)$ini+6
      }

      # If cities is a vector of geocodes, the pipeline reads the parameters from the dataframe
      if (class(cities) %in% c("integer","numeric")) {
            pars_table <- fetch_alert_parameters(conn = datasource, geocodes = cities,
                                                  disease = cid10)
            if (isTRUE(verbose)) message("reading parameters from database")
      } else { # if city contains data already
            if(all(c("municipio_geocodigo","limiar_preseason",
                     "limiar_posseason","limiar_epidemico",
                     "varcli","clicrit","varcli2","clicrit2","cid10","codmodelo") %in% names(cities))) {
                  if (isTRUE(verbose)) message("using user's provided parameters")

                  pars_table <- cities
            }
            else stop("`cities` must be geocodes or a complete parameter data frame.",
                      call. = FALSE)
      }

      cidades <- pars_table$municipio_geocodigo
      if (isTRUE(verbose)) message("running analysis for ", length(cidades), " municipalities")

      # Reading the meteorological data
      #print('Obtendo os dados de clima...')
      varscli <- c("umid_max", "temp_min", "umid_min","umid_med","temp_med","temp_max")
      cli <- fetch_climate(conn = datasource, geocodes = cidades,
                           climate_vars = varscli,
                           start_date = epiweek_start(iniSE), end_date = finalday)
      if (isTRUE(verbose)) message("climate data loaded")
      casos <- fetch_cases(
            conn = datasource, geocodes = cidades, disease = cid10,
            start_date = epiweek_start(iniSE), end_date = finalday,
            case_date = if (dataini == "notific") "notification" else "symptom_onset",
            complete_tail = completetail, verbose = verbose
      )
      if (isTRUE(verbose)) message("case data loaded")
      case_records <- attr(casos, "case_records")

      inputs <- new_alert_inputs(
            cases = casos, climate = cli, case_records = case_records,
            metadata = list(report_week = datarelatorio, source = "legacy_pipeline")
      )
      res <- run_alert_pipeline(
            inputs = inputs, parameters = pars_table, report_week = datarelatorio,
            nowcast = nowcasting, workers = workers, seed = seed, verbose = verbose
      )

      if (writedb == TRUE) {
            write_alert_results(datasource, res, version_date = version_date,
                                conflict = "update")
      }

      res
}


# calc.alerta -------------------------------------------
#'@title Calculates the attention levels for one area.
#'@description Function to compute the alert levels for one spatial unit, defined by level.
#'Internal function used by the dengue_pipeline.
#'@param x filter variable indicating geocode at municipal, state or other level.
#'@param pars List containing input data, report week, nowcasting mode, parameters, and case records.
#'@param level default is municipio. Other options are: regional, macrorregiao
#'and uf (not implemented yet)
#'@param ... Reserved for compatibility.
#'@return data.frame with the data with alert level.

calc.alerta <- function(x, pars, level = "municipio",...){  #x = cities[i]

      d <- pars[[1]]
      datarelatorio <- pars[[2]]
      nowcasting <- pars[[3]]
      pars_table <- pars[[4]]
      caselist <- pars[[5]]

      # casos + nowcasting + Rt + incidencia

      if(level == "municipio"){
            cas.x <- d %>%
                  filter(cidade == x) %>%
                  nowcast_cases(method = "none",
                                  report_week = datarelatorio,
                                  nyears = 1)

            if(nowcasting == "bayesian"){  # handling errors in bayesian nowcast
                  try(cas.x <- d %>%
                            filter(cidade == x) %>%
                            nowcast_cases(method = nowcasting, case_records = caselist,
                                            report_week = datarelatorio,
                                            nyears = 1))
            }

            cas.x <- cas.x %>%
                  estimate_rt(count = "tcasesmed", distribution="normal",
                              mean_generation_time=3, sd_generation_time = 1) %>%
                        mutate(inc = tcasesmed/pop*100000)  %>%
                        arrange(SE)


      assert_that(nrow(cas.x)>0, msg = "check alertapipeline. error makes nrow = 0")
      # build rules
      crit.x <- pars_table[pars_table$municipio_geocodigo==x,] # parameters
      crit.x.vector <- structure(as.character(crit.x), names = as.character(names(crit.x))) # dataframe -> vector
      criteriaU <- define_alert_rules(rule = crit.x$codmodelo, values = crit.x.vector)

      # Apply alert rules
      y <- classify_alerts(cas.x, rules = criteriaU)
      }
      y
}


#tabela_historico --------------------------------------------------------------------
#'@title Convert the alert object into a data.frame and calculate indicators
#'@description Function to organize the alert results for easy reading and inserting
#'in the database. Also computes receptivity, transmission and incidence levels.
#'@noRd
#'@param obj object created by the pipeline.
#'@param iniSE first week of the table. Default is the first date in obj.
#'@param lastSE last week of the table. Default is the last date in obj.
#'@param type "notified", if it should return total counts, disregarding the
#'final classification. For compatibility reasons.
#'@param versao Explicit model-version date.
#'@param parameters Parameter data frame. Pipeline results carry it as an attribute;
#'direct alert objects must supply it explicitly.
#'@return data.frame with the data to be written.
#'@examplesIf FALSE
#'# Several cities at once:
#'cidades <- getCidades(uf = "Mato Grosso", datasource = connection)
#'res <- pipe_infodengue(cities = cidades$municipio_geocodigo[1:3], cid10 = "A90",
#'finalday= "2018-01-10")
#'restab <- tabela_historico_comp(res, iniSE = 202301, type = "all")
#'tail(restab)
#'# One city:
#'res <- pipe_infodengue(cities = 3304557, cid10 = "A90",
#'finalday= "2015-01-10")
#'restab <- tabela_historico(res)
#'tail(restab)

.as_alert_history_impl <- function(obj, iniSE, lastSE, type = "all", versao,
                             parameters = if (inherits(obj, "alerttools_result")) obj$parameters else attr(obj, "parameters")){

      if (missing(versao)) stop("tabela_historico requires an explicit `versao` date.", call. = FALSE)

      # --------- create single data.frame ------------------#
      # if object created by pipe_infodengue():
      if (inherits(obj, "alerttools_result")) {
            data <- obj$data
            indices <- obj$alerts
      } else if(class(obj)=="list" & class(obj[[1]])=="alerta"){
            data <- transpose(obj)[[1]] %>% bind_rows()   # unlist data
            indices <- transpose(obj)[[2]] %>% bind_rows()  #unlist indices

      } else if (class(obj)=="alerta"){ #if object created directly by fouralert()
            data <- obj$data
            indices <- obj$indices
      }
      d <- cbind(data, indices)

      # defining the id (SE+julian(versaomodelo)+geocodigo+localidade)
      location_id <- if ("Localidade_id" %in% names(data)) data$Localidade_id else data$localidade
      gera_id <- function(x) paste(data$cidade[x], location_id[x], data$SE[x],
                                   as.character(julian(versao)), sep="")
      d$id <- sapply(1:nrow(data), gera_id)


      # ---------- filtering dates -------------------------#
      if(missing(iniSE)) iniSE <- 0  # sem filtro
      if(missing(lastSE)) lastSE <- 300000 # sem filtro

      d <- d %>%
            filter(SE >= iniSE & SE <= lastSE) %>%
            rename(municipio_geocodigo = cidade,
                   municipio_nome = nome,
                   casos_est = tcasesmed,
                   casos_est_min = tcasesICmin,
                   casos_est_max = tcasesICmax,
                   casprov = cas_prov,
                   nivel = level,
                   temp_min = temp_min,
                   temp_med = temp_med,
                   temp_max = temp_max,
                   umid_min = umid_min,
                   umid_med = umid_med,
                   umid_max = umid_max) %>%
            mutate(p_rt1 = ifelse(is.na(p1),0,p1),
                   p_inc100k =casos_est/pop*1e5,
                   Localidade_id  = ifelse(is.na(localidade),0,localidade),
                   data_iniSE = SE2date(SE)$ini,
                   versao_modelo = as.character(versao))
      d$Rt[is.na(d$Rt)] <- 0

      if (is.null(parameters)) {
            stop("tabela_historico requires `parameters`; database access is not implicit.",
                 call. = FALSE)
      }
      required_parameters <- c("municipio_geocodigo", "limiar_preseason", "limiar_epidemico")
      assert_that(all(required_parameters %in% names(parameters)),
                  msg = "tabela_historico: invalid parameter schema")
      pars <- parameters

      d <- d %>%   # new stuff
            rename(
                  receptivo = cytrue,  # weeks with receptive conditions
                  transmissao = cotrue)   # weeks with sustained transm

      d1 <- d %>%
            left_join(pars[c("municipio_geocodigo", "limiar_preseason",
                             "limiar_epidemico")], by = "municipio_geocodigo") %>%
            mutate(  # compating estimated incidence with thresholds
                  nivel_inc = case_when(
                        p_inc100k < limiar_preseason ~ 0,
                        p_inc100k >= limiar_preseason & p_inc100k < limiar_epidemico ~ 1,
                        p_inc100k >= limiar_epidemico ~ 2
                  )
            )
      # --------- checking all required variables ------------#
      varnames <-c("data_iniSE", "SE", "CID10","casos", "casos_est",
                   "casos_est_min", "casos_est_max", "municipio_geocodigo", "casprov",
                   "p_rt1", "p_inc100k", "Localidade_id", "nivel", "id", "versao_modelo",
                   "municipio_nome", "tweet", "Rt", "pop", "temp_min","temp_med",
                   "temp_max","umid_min","umid_med","umid_max", "receptivo",
                   "transmissao", "nivel_inc")

      if(all(varnames %in% names(d1))) {
            dfinal <- d1[,varnames]
            return(dfinal)
      } else {
            message(paste("historico_alerta is not returning the required variables",
                          varnames[!varnames %in% names(d1)]))
            return(NULL)
      }
}


#tabela_historico_intra --------------------------------------------------------------------
#'@title Convert the alert object into a data.frame and calculate indicators for cities with subdivisions
#'@description Function to organize the alert results for easy reading and inserting
#'in the database. Specific for municipalities with subdivisions.
#'@export
#'@param obj object created by the pipeline.
#'@param iniSE first week of the table. Default is the first date in obj.
#'@param lastSE last week of the table. Default is the last date in obj.
#'@param versao Explicit model-version date.
#'@param parameters Parameter data frame. It is never read implicitly from a database.
#'@return data.frame with the data to be written.
#'@examplesIf FALSE
#'# NOT RUN without connection
#'# Rio de Janeiro
#'alerio <- pipe_infodengue_intra(city = 3304557, datarelatorio=202105,
#'delaymethod="bayesian", cid10 = "A90", dataini = "sinpri")
#'restab <- tabela_historico_intra(alerio, iniSE = 201801,
#'                                 versao = as.Date("2026-09-04"))
#'tail(restab)

tabela_historico_intra <- function(obj, iniSE, lastSE, versao,
                                   parameters = attr(obj, "parameters")){

      if (missing(versao)) stop("tabela_historico_intra requires an explicit `versao` date.", call. = FALSE)

      assert_that(class(obj) == "alerta_intra", msg = "tabela_historico_mun: obj must be of class alerta_intra.")

      # --------- create single data.frame ------------------#
      data <- transpose(obj)[[1]] %>% bind_rows()   # unlist data
      indices <- transpose(obj)[[2]] %>% bind_rows()  #unlist indices

      d <- cbind(data, indices)

      # defining the id (SE+julian(versaomodelo)+geocodigo+localidade)
      gera_id <- function(x) paste(data$cidade[x], data$Localidade_id[x], data$SE[x],
                                   as.character(julian(versao)), sep="")
      d$id <- sapply(1:nrow(data), gera_id)

      # ---------- filtering dates -------------------------#
      if(missing(iniSE)) iniSE <- 0
      if(missing(lastSE)) lastSE <- 300000

      d <- d %>%
            filter(SE >= iniSE & SE <= lastSE) %>%
            rename(municipio_geocodigo = cidade,
                   municipio_nome = nome,
                   casos_est = tcasesmed,
                   casos_est_min = tcasesICmin,
                   casos_est_max = tcasesICmax,
                   nivel = level) %>%
            mutate(p_rt1 = ifelse(is.na(p1),0,p1),
                   p_inc100k =casos_est/populacao*1e5,
                   Localidade_id  = ifelse(is.na(localidade),0,localidade),
                   data_iniSE = SE2date(SE)$ini,
                   versao_modelo = as.character(versao))
      d$Rt[is.na(d$Rt)] <- 0

      if (is.null(parameters)) {
            stop("tabela_historico_intra requires `parameters`; database access is not implicit.",
                 call. = FALSE)
      }
      required_parameters <- c("municipio_geocodigo", "limiar_preseason", "limiar_epidemico")
      assert_that(all(required_parameters %in% names(parameters)),
                  msg = "tabela_historico_intra: invalid parameter schema")
      pars <- parameters

      d <- d %>%   # new stuff
            rename(
                  receptivo = cytrue,  # weeks with receptive conditions
                  transmissao = cotrue)   # weeks with sustained transm

      d1 <- d %>%
            left_join(pars[c("municipio_geocodigo", "limiar_preseason",
                             "limiar_epidemico")], by = "municipio_geocodigo") %>%
            mutate(  # compating estimated incidence with thresholds
                  nivel_inc = case_when(
                        p_inc100k < limiar_preseason ~ 0,
                        p_inc100k >= limiar_preseason & p_inc100k < limiar_epidemico ~ 1,
                        p_inc100k > limiar_epidemico ~ 2
                  )
            )

      d1
}


#write_alerta --------------------------------------------------------------------
#'@title Write historico_alerta into the database.
#'@description Function to write the pipeline results into the database.
#'Receives the object created by the function historico.alerta. If writetofile == TRUE,
#'it saves the sql command in a text file. If FALSE, it will write directly in the database
#'using the connection.
#'@noRd
#'@param d object created by tabela_historico()
#'@param writetofile TRUE if an sql object will be the output; FALSE if not.
#'@param arq file name to store sql object
#'@param datasource posgreSQL conn to project's database
#'@param conflict Conflict policy: `update`, `error`, or `ignore`.
#'@return the same data.frame from the input
#'@examplesIf FALSE
#'# Parameters for the model
#'cidades <- getCidades(regional = "Norte",uf = "Rio de Janeiro",datasource = connection)
#'res <- pipe_infodengue(cities = cidades$municipio_geocodigo[1], cid10 = "A90",
#'datarelatorio = 202407, iniSE = 202301, nowcasting="none")
#'restab <- tabela_historico(res)
#'res2 <- pipe_infodengue(cities = cidades$municipio_geocodigo[1], cid10 = "A90",
#'datarelatorio = 202406, iniSE = 202301, nowcasting="none")
#'restab2 <- tabela_historico(res2)
#'# NOT RUN
#'t1 <- Sys.time()
#'write_alerta(restab[1:10,], writetofile = TRUE)
#'t2 <- Sys.time() - t1

.write_alert_results_impl <- function(d, writetofile = FALSE, datasource, arq = "output.sql",
                         conflict = c("update", "error", "ignore"), verbose = TRUE) {

   # check input
   assert_that(class(d) == "data.frame", msg = "write_alerta: d is not a data.frame. d should
                  be an output from tabela_historico.")

   conflict <- match.arg(conflict)
   if (!writetofile) .db_validate_connection(datasource)

   cid10 = unique(d$CID10)
   assert_that(length(cid10) == 1, msg = "write_alerta: d must contain only one cid10")

   dcolumns <- c("SE", "data_iniSE", "casos_est", "casos_est_min", "casos_est_max",
                 "casos","casprov","municipio_geocodigo","p_rt1","p_inc100k","Localidade_id",
                 "nivel","id","versao_modelo","municipio_nome","Rt", "pop", "tweet",
                 "receptivo","transmissao","nivel_inc","temp_min","temp_med","temp_max",
                 "umid_min","umid_med","umid_max")

   if(!("temp_min" %in% names(d))) d$temp_min <- NA
   if(!("umid_max" %in% names(d))) d$umid_max <- NA

   assert_that(all(dcolumns %in% names(d)), msg = "write_alerta: check if d contains required
                                                           columns")

   # nomes das tabelas para salvar os historicos:
   if(cid10=="A90") table_key <- "alert_dengue"
   if(cid10=="A92.0") table_key <- "alert_chik"
   if(cid10=="A92.8") table_key <- "alert_zika"
   if(!(cid10 %in% c("A90", "A92.0", "A92.8"))) stop(paste("unknown disease destination for", cid10))

   # ------ vars to write

   dados <- data.frame(
      SE = d$SE, data_iniSE = d$data_iniSE,
      casos_est = d$casos_est, casos_est_min = d$casos_est_min,
      casos_est_max = d$casos_est_max, casos = d$casos, casprov = d$casprov,
      municipio_geocodigo = d$municipio_geocodigo, p_rt1 = d$p_rt1,
      p_inc100k = d$p_inc100k, Localidade_id = d$Localidade_id,
      nivel = d$nivel, id = d$id, versao_modelo = d$versao_modelo,
      municipio_nome = d$municipio_nome, tweet = d$tweet, Rt = d$Rt, pop = d$pop,
      tempmin = d$temp_min, tempmed = d$temp_med, tempmax = d$temp_max,
      umidmin = d$umid_min, umidmed = d$umid_med, umidmax = d$umid_max,
      receptivo = d$receptivo, transmissao = d$transmissao,
      nivel_inc = d$nivel_inc, check.names = FALSE
   )
   key <- c("SE", "municipio_geocodigo", "Localidade_id")

   if (writetofile) {
      driver <- DBI::ANSI()
      config <- .db_table_config[[table_key]]$postgres
      table_sql <- as.character(DBI::dbQuoteIdentifier(
         driver, DBI::Id(schema = config[[1]], table = config[[2]])
      ))
      column_sql <- as.character(DBI::dbQuoteIdentifier(driver, names(dados)))
      row_sql <- apply(dados, 1L, function(row) {
         values <- Map(function(value, template) {
            if (is.na(value)) return(DBI::SQL("NULL"))
            if (inherits(template, "Date")) value <- as.Date(value)
            DBI::dbQuoteLiteral(driver, value)
         }, as.list(row), dados[1, , drop = FALSE])
         paste0("(", paste(as.character(values), collapse = ", "), ")")
      })
      sql <- paste("INSERT INTO", table_sql,
                   paste0("(", paste(column_sql, collapse = ", "), ") VALUES"),
                   paste(row_sql, collapse = ", "))
      if (conflict != "error") {
         key_sql <- as.character(DBI::dbQuoteIdentifier(driver, key))
         sql <- paste(sql, "ON CONFLICT", paste0("(", paste(key_sql, collapse = ", "), ")"))
         if (conflict == "ignore") {
            sql <- paste(sql, "DO NOTHING")
         } else {
            update_columns <- setdiff(names(dados), key)
            update_sql <- as.character(DBI::dbQuoteIdentifier(driver, update_columns))
            sql <- paste(sql, "DO UPDATE SET",
                         paste0(update_sql, " = excluded.", update_sql, collapse = ", "))
         }
      }
      writeLines(paste0(sql, ";"), arq, useBytes = TRUE)
      if (isTRUE(verbose)) message("writing alerta into file ", arq)
   } else {
      if (isTRUE(verbose)) message("writing alerta into ", table_key)
      .repo_write_rows(datasource, table_key, dados, key = key, conflict = conflict)
   }
   invisible(d)
}
