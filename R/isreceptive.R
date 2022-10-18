#is.receptive -------------------------------------------------------------------
#'@title Check if conditions are receptive for transmission.
#'@description receptivity is defined as the combination of suspected viral 
#' activity and favorable climate. 
#'@export 
#'@param rule string with a condition, ex: 'temp_min_1 > 22 & inc_3 > 1.1 | temp_min_1 < 22 & umid_max_2 > 90'.
#'where temp_min, inc, umid_max are variables in the data, while _i at the end indicate the time lag.
#'@param data data.frame containing at least the variables mentioned in the rule,
#' without the lagging. Lagging works only for inc_3, temp_min, temp_max, umid_min,
#' umid_max, with 1 to 3 weeks of lagging.
#'@return vector with three possible values: 1 - receptive, 2 - non_receptive, or NA if not enough information      
#'the data with the lagged variables and a summary of the data quality.
#'@examples
#'rul <- c("temp_min_1 > 22 & inc_3 > 0")
#'dat <- data.frame(temp_min = rnorm(100,25,5), umid_max = rnorm(100,70, 8), inc = rpois(100, 2)/1000)
#'rec <- is.receptive(rule = rul, data = dat)
#'Using infodengue rules:
#'val <- read.parameters(1200401)
#'setCriteria(rule=val$rule, data = data)

is.receptive <- function(rule= NULL,  data = NULL){
  
  # checking input
  assert_that(class(rule) == "character",
                                 msg = "is.receptive: rule should be a string")
  assert_that(class(data) == "data.frame",
              msg = "is.receptive: data should be a data.frame")

  out <- data
  # lagging
  data$inc_3 = lag(data$inc, 3)
  if("temp_min" %in% names(data)) 
    data <- data %>% mutate(temp_min_1 = lag(data$temp_min, 1),
                            temp_min_2 = lag(data$temp_min, 2),
                            temp_min_3 = lag(data$temp_min, 3))
  if("temp_max" %in% names(data)) 
    data <- data %>% mutate(temp_max_1 = lag(data$temp_max, 1),
                            temp_max_2 = lag(data$temp_max, 2),
                            temp_max_3 = lag(data$temp_max, 3))
  if("umid_min" %in% names(data)) 
    data <- data %>% mutate(umid_min_1 = lag(data$umid_min, 1),
                            umid_min_2 = lag(data$umid_min, 2),
                            umid_min_3 = lag(data$umid_min, 3))
  if("umid_max" %in% names(data)) 
    data <- data %>% mutate(umid_max_1 = lag(data$umid_max, 1),
                            umid_max_2 = lag(data$umid_max, 2),
                            umid_max_3 = lag(data$umid_max, 3))
  
  
    # testando a condicao
    vars <- c("inc_3","temp_min_1","temp_min_2","temp_min_3","umid_max_1","umid_max_2","umid_max_3")
    data$rule <- rul
    for(v in vars) {
      data$val <- as.character(data[, v])
      data <- data %>%
        mutate(rule = str_replace_all(rule, v, val))
     }
    
    data <- data %>% 
      mutate(recept = eval(parse(text = rule)))
    
    out$recept <- as.numeric(sapply(data$rule, function(x) eval(parse(text = x))))
    
   out
}
