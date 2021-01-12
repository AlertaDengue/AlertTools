# PROJETO ALERTA DENGUE -------------------------------------
# Auxiliar functions for nowcasting
# Leo Bastos 2020 
# -----------------------------------------------------------

# Auxiliar function, sampling from a negative binomial likelihood
ff <- function(x, idx){
  rnbinom(n = idx, mu = exp(x$latent[idx]), size = x$hyperpar[1])
}

# Auxiliar function selecionando um pedaco do dataset
gg <- function(x, dados, idx, Fim.sat, Dmax){
  data.aux <- dados
  data.aux$Casos[idx] <- x
  data.aggregated <- data.aux %>%
    # Selecionando apenas os dias faltantes a partir
    # do domingo da respectiva ultima epiweek
    # com dados faltantes
    filter(Date >= Fim.sat - Dmax*7  ) %>%
    group_by(Date) %>% 
    dplyr::summarise( 
      Casos = sum(Casos) 
    )
  data.aggregated
}


# Algorithm to get samples for the predictive distribution for the number of cases

nowcasting <- function(output.day, dadosRio.ag, Dm, Fim){
  index.missing = which(is.na(dadosRio.ag$Casos))
  
  
  # Step 1: Sampling from the approximate posterior distribution using INLA
  srag.samples.list <- inla.posterior.sample(n = 1000, output.day)
  
  # Step 2: Sampling the missing triangle (in vector form) from the likelihood using INLA estimates
  vector.samples <- lapply(X = srag.samples.list, 
                           FUN = ff,
                           idx = index.missing
  )
  
  # Step 3: Calculate N_t for each triangle sample {N_t : t=Tactual-Dmax+1,...Tactual}
  tibble.samples <- lapply( X = vector.samples,
                            FUN = gg,
                            dados = dadosRio.ag, 
                            idx = index.missing,
                            Fim.sat = Fim, 
                            Dmax = Dm
  )
  
  # Nowcasting
  srag.pred <- bind_rows(tibble.samples, .id = "sample")
  
  srag.pred
}


# Running INLA for the nowcasting model
nowcast.INLA <- function(dados.ag, model.day,...){
  
  output <- tryCatch(
    {inla(formula = model.day, 
                 family = "nbinomial", 
                 data = dados.ag,
                 num.threads = 4,
                 control.predictor = list(link = 1, compute = T),
                 control.compute = list( config = T),
                 ...
                 # control.family = list( 
                 # hyper = list("theta" = list(
                 #   prior = "loggamma", param = c(1, 0.1)))
                 #   )
  )},
  error=function(cond) {
    message("nowcast failed")
    message("Here's the original error message:")
    message(cond)
    # Choose a return value in case of error
    return(NULL)
  },
  warning=function(cond) {
    message("nowcast failed")
    message("Here's the original warning message:")
    message(cond)
    # Choose a return value in case of warning
    return(NULL)
  })
  
  output
}


# Plot nowcasting
plot.nowcast <- function(dadosRio.ag, pred.summy, nowcast = T){
  
  dadosRio.ag.day.plot <- dadosRio.ag %>% group_by(Date) %>%
    dplyr::summarise( Casos = sum(Casos, na.rm = T)) %>% 
    left_join(pred.summy, by = "Date") %>% 
    mutate(
      Casos = ifelse(Date > Fim, NA, Casos),
      Forecast = ifelse(is.na(Mean), NA, 0) + ifelse(Date > Fim, 1, 0)
    )
  
  # Time series
  p0.day <- dadosRio.ag.day.plot %>% 
    ggplot(aes(x = Date, y = Casos, 
               color = "Casos notificados", 
               linetype = "Casos notificados")) + 
    geom_line(size = 1, na.rm = T) 
  
  if(nowcast){
    p0.day <- p0.day +   
      geom_ribbon( aes( ymin=LI, ymax=LS), fill = 'gray', 
                   color = 'gray', alpha = 0.5, 
                   show.legend = F) + 
      geom_line(aes(x = Date, y = Median, 
                    colour = "Nowcasting", 
                    linetype = "Nowcasting"), 
                size = 1, na.rm = T) +
      scale_colour_manual(name = "", 
                          values = c("black", "black"), 
                          guide = guide_legend(reverse=F)) +
      scale_linetype_manual(name = "", 
                            values = c("solid", "dotted"), 
                            guide = guide_legend(reverse=F))
  }
  
  p0.day <- p0.day + 
    #ylab("Casos hospitalização de SRAG") + 
    #xlab("Tempo") +
    theme_bw( base_size = 14) +
    theme( legend.position = c(0.2, 0.8), legend.title = element_blank()) 
  
  p0.day
}
