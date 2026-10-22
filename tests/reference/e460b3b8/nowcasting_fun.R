# Unmodified function expressions from e460b3b8c15ab28e8755f482d576cf7280803b34:R/nowcasting_fun.R
ff <- function(x, idx) {
    rnbinom(n = idx, mu = exp(x$latent[idx]), size = x$hyperpar[1])
}
gg <- function(x, dados, idx, Fim.sat, Dmax) {
    data.aux <- dados
    data.aux$Casos[idx] <- x
    data.aggregated <- data.aux %>% filter(Date >= Fim.sat - Dmax * 7) %>% group_by(Date) %>% dplyr::summarise(Casos = sum(Casos))
    data.aggregated
}
nowcasting <- function(output.day, dadosRio.ag, Dm, Fim) {
    index.missing = which(is.na(dadosRio.ag$Casos))
    srag.samples.list <- inla.posterior.sample(n = 1000, output.day)
    vector.samples <- lapply(X = srag.samples.list, FUN = ff, idx = index.missing)
    tibble.samples <- lapply(X = vector.samples, FUN = gg, dados = dadosRio.ag, idx = index.missing,
        Fim.sat = Fim, Dmax = Dm)
    srag.pred <- bind_rows(tibble.samples, .id = "sample")
    srag.pred
}
nowcast.INLA <- function(dados.ag, model.day, ...) {
    output <- tryCatch({
        inla(formula = model.day, family = "nbinomial", data = dados.ag, num.threads = 4, verbose = FALSE,
            control.predictor = list(link = 1, compute = T), control.compute = list(config = T), ...)
    }, error = function(cond) {
        message("nowcast failed")
        message("Here's the original error message:")
        message(cond)
        return(NULL)
    }, warning = function(cond) {
        message("nowcast failed")
        message("Here's the original warning message:")
        message(cond)
        return(NULL)
    })
    output
}
