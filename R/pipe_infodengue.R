# PROJETO ALERTA DENGUE -------------------------------------
# Funcoes de calculo do alerta 
# Claudia Codeco versao 2022

#pipe_infodengue_22 ---------------------------------------------------------------------
#'@title pipeline used by infodengue 
#'@description wrap of functions used by Infodengue.
#'@export
#'@param cities In general, a vector of 7-digit geocodes. If it is a data.frame containing geocodes
#' and all parameters, these will replace the database's parameters. 
#'@param cid10 default is A90 (dengue). Chik = A92.0, Zika = A92.8
#'@param narule how to treat missing climate data. Do nothing (default), "zero" fills 
#'with zeros, "linear" for linear interpolation, "arima" for inter and extrapolation.
#'@param finalday if provided, uses only disease data reported up to that day
#'@param iniSE first date of the disease data. Default = 201501. Minimum = 201001. 
#'@param datarelatorio epidemiological week
#'@param nowcasting   "bayesian" for the dynamic model; "none" for not doing nowcast (default) 
#'@param completetail if sinan data is older than final_day, fill in the tail with NA (default) or 0.  
#'@param dataini "notif" (default) or "sinpri" 
#'@param yellow_rule decision rule for yellow alert (climate receptivity) 
#'@param orange_rule number of weeks with Rt > 1 above which orange is raised
#'@param writedb TRUE if it should write into the database, default is FALSE.
#'@param datasource posgreSQL connection to project's database
#'@return data.frame with the week condition and the number of weeks within the 
#'last lag weeks with conditions = TRUE.
#'@examples
#'d <- pipe_infodengue_22(state = "Acre", cid10 = "A90", iniSE = 201801,
#'finalday= "2021-08-12",nowcasting="bayesian", completetail = 0, dataini = "sinpri",
#'recept_rules = AlertTools::regras)
#'restab <- tabela_historico(d)

pipe_infodengue_22 <- function(state, cid10="A90", datarelatorio, finalday = Sys.Date(), 
                               iniSE = 201001, nowcasting="none", narule=NULL,
                               writedb = FALSE, datasource = con, recept_rules, orange_rule = 3, 
                               completetail = NA, dataini = "notific"){
      
      # check inputs
      if(missing(datarelatorio)) {
            datarelatorio <- data2SE(finalday, format = "%Y-%m-%d") 
      } else { # if datarelatorio & finalday are given, priority is datarelatorio
            finalday <- SE2date(datarelatorio)$ini+6
      }
      if(missing(recept_rules)){
            print("receptivity rules not provided, using standard:")
            recept_rules <- "temp_min_1 > 22 & umid_min_1 > 60 & inc_3 > 0"
            print(recept_rules)
      }else{
            assert_that(any(c("regional","municipio_geocodigo") %in% names(recept_rules)),
                        msg = "pipeline: recept_rules argument must contain either
                        regional or municipio_geocodigo")
      }
      
      # get_municipios and regionais
      mun <- getRegionais(uf = state, output = "complete")
      
      # reads the alert parameters 
      pars_table <- read.parameters(cities = mun$municipio_geocodigo, cid10 = cid10)
      
      # number of cities 
      nlugares <- nrow(pars_table)
      cidades <- pars_table$municipio_geocodigo
      print(paste("sera'feita analise de",nlugares,"cidade(s):"))
      print(cidades)      
      
      # stations
      estacoes <- na.omit(unique(pars_table$estacao_wu_sec))
      print("usando dados de clima das estacoes:")
      print(estacoes)
      
      # Reading the meteorological data
      varscli <- c("temp_min","temp_max","umid_min","umid_max")
      cliwu <- getWU(stations = estacoes, vars = varscli, 
                     finalday = finalday, imput = TRUE)
      
      # Reading Cases
      print("Obtendo dados de notificacao ...")
      casos <- getCases(mun$municipio_geocodigo, lastday = finalday, 
                        cid10 = cid10, type = "all", # novo
                        dataini = dataini, completetail = completetail) 
      message("getCases done")
      
      # Merge clima + casos
      d <- casos %>%
            left_join(mun[, c("municipio_geocodigo", "regional", "codigo_regional",
                              "macroregional", "codigo_macroregional"), ], 
                      by = c("cidade" = "municipio_geocodigo")) %>%
            left_join(pars_table[, c("municipio_geocodigo","estacao_wu_sec",
                                     "limiar_epidemico","limiar_preseason","limiar_posseason")],
                      by = c("cidade" = "municipio_geocodigo")) %>% 
            left_join(cliwu, by = c("SE"="SE", "estacao_wu_sec" = "estacao")) 
      
      # Reading and merging tweets 
      d$tweet <- 0
      if(cid10 == "A90"){
            print("Reading tweets...")
            dT = getTweet(mun$municipio_geocodigo, lastday = finalday, cid10 = "A90") %>% # incluir dataini
                  rename(cidade = Municipio_geocodigo)
            
            d <- d %>% left_join(dT)
      }
      
      # Data collection completed . Nowcasting:
      
      ## FUN  calc.nowcasting (internal)
      calc.nowcast <- function(x){  #x = cities[i]
            # casos + nowcasting + Rt + incidencia 
            d.x <- d %>% 
                  filter(cidade == x) %>%
                  adjustIncidence(method = "none",  
                                  nowSE = datarelatorio, 
                                  nyears = 1) 
            
            if(nowcasting != "none"){  # handling errors in bayesian nowcast
                  try(d.x <- d %>% 
                            filter(cidade == x) %>%
                            adjustIncidence(method = nowcasting,  
                                            nowSE = datarelatorio, 
                                            nyears = 1)) 
            }
            d.x     
      }
      
      out <- lapply(mun$municipio_geocodigo, calc.nowcast)
      d <- out %>% bind_rows()
      assert_that(nrow(d) > 0, msg = "check organize data. returning null or empty")
      rm(out)
      
      # calculating nowcasted incidence
      d <- d %>% 
            mutate(inc = tcasesmed/pop * 100000, # this is the one used in the receptivity model
                   inc_min = tcasesICmin/pop * 100000,
                   inc_max = tcasesICmax/pop * 100000)
      
      # Rt
      print("Rt now")
      d <- d %>%
            Rt(count = "tcasesmed", gtdist="normal", group = "cidade", meangt=3, sdgt = 1) %>%
            mutate(inc = tcasesmed/pop*100000)  
      d$Rt[is.na(d$Rt) & d$tcasesmed == 0] <- 0  #Rt = 0 if cases = 0 and Rt = NA
      
      # compute lagged variables for the receptivity  (clima + inc)
      d <- lag_variables(d)      
      
      # compute municipal-level receptivity 
      print("computing receptivity...")
      
      if(length(recept_rules) == 1){
            d <- is_receptive(d, crit = recept_rules) # using default rule  
      } else {
            res = list()
            for(i in 1:nrow(recept_rules)){
                  res[[i]] = is_receptive(d[d$codigo_regional == recept_rules$regional[i],], 
                                          crit = recept_rules$regra[i])
            }
            d <- bind_rows(res)
            rm(res)
      } 
      
      # Regional is receptive if any municipality in the regional is receptive 
      receptivo.reg <- d %>%
            group_by(regional, SE) %>%
            summarise(recept.reg = ifelse(any(receptivo == 1), 1, 0))
      d <- d %>%
            left_join(receptivo.reg)
      
      # Transmission
      message("computing transmission...")
      d <- d %>% 
            count_transmission_weeks() %>%
            mutate(transmissao = ifelse(weeks_transmission >= orange_rule, 1, 0))
      
      # Threshold crossing  
      #message("computing thresholds...")
      d <- d %>%
            weeks_above_threshold(var = "inc", varname = "weeks_epidemic", maxw = 12,
                                  threshold = pars_table[, c("municipio_geocodigo","limiar_epidemico")]) %>%
            weeks_above_threshold(var = "inc", varname = "w_above_preseason", maxw = 12,
                                  threshold = pars_table[, c("municipio_geocodigo","limiar_preseason")]) %>%
            weeks_above_threshold(var = "inc", varname = "w_above_posseason", maxw = 12,
                                  threshold = pars_table[, c("municipio_geocodigo","limiar_posseason")]) 
      
      
      # Alert levels
      d <- d %>%
            group_by(SE, cidade) %>%
            mutate(level = case_when(
                  recept.reg == 1 ~ 2,
                  transmissao == 1 ~ 3,
                  weeks_epidemic >= 2 ~ 4,
                  TRUE ~ 1
            ))
      
      # output object
      out <- list() 
      out[["data"]] <- d %>% 
            select(SE, cidade, nome, pop, regional, codigo_regional, CID10, casos, cas_prov, cas_lab, 
                   tcasesmed, tcasesICmin, tcasesICmax, inc, inc_min, inc_max, temp_min, temp_max, 
                   umid_min, umid_max)
      out[["pars"]] <- d %>%
            select(SE, cidade, estacaoWU = estacao_wu_sec, limiar_epidemico, limiar_preseason,
                   limiar_posseason)
      out[["ind"]] <- d %>%
            select(SE, cidade, level, receptivo, recept.reg,weeks_transmission, transmissao, 
                   weeks_epidemic, w_above_preseason, w_above_posseason)
      
      class(out) <- "alerta"
      out 
}

