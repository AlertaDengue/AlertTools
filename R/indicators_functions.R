# PROJETO ALERTA DENGUE -------------------------------------
# Funcoes para calculo de indicadores
# Claudia Codeco 2020
# -----------------------------------------------------------


# calc_receptivity ---------------------------------------------------------------------
#'@description Calculate climate receptivity  
#'@title Compute indicators
#'@export
#'@param ale alert object from one city
#'@return vector of receptivity 
#'@examples
#'res <- pipe_infodengue(cities = 3304557, cid10 = "A90", 
#'finalday= "2020-01-23",nowcasting="none")
#'calc_indicators(res)

# calc_indicators <- function(ale){
#       
#       # check input
#       assert_that(class(ale[[1]]) == "alerta", msg = "receptivity: ale is not an alert object.")
#       
#       d <- cbind(ale[[1]]$data, ale[[1]]$indice) %>%
#             mutate(
#                   receptividade = case_when(
#                         cytrue == 0 ~ "unlike",
#                         cytrue %in% 1:2 ~ "likely",
#                         cytrue == 3 ~ "certain"
#                   ),
#                   transmissao = case_when(
#                         cotrue == 0 ~ "unlike",
#                         cotrue %in% 1:2 ~ "likely",
#                         cotrue == 3  ~ "highly likely"  ),
#                   incidencia = case_when(
#                         crtrue == 0 ~ "low",
#                         crtrue %in% 1:2 ~ "recently high",
#                         crtrue == 3  ~ "high" )
#             )  %>%
#             select(SE, casos, tcasesmed, receptividade, transmissao, incidencia) 
#       d
# }

