# Unmodified function expressions from e460b3b8c15ab28e8755f482d576cf7280803b34:R/alert_functions.R
setCriteria <- function(rule = NULL, values = NULL, delays = list(delayy = c(0, 0), delayo = c(0, 0),
    delayr = c(0, 0))) {
    if (!is.null(rule))
        assert_that(rule %in% c("Af", "Aw", "AsAw", "Awi", "AsAwi"), msg = "setcriteria: rule unknown. Try Af, Aw, Awi, AsAw or AsAwi")
    if (is.null(rule)) {
        assert_that(!is.null(values), msg = "setcriteria: if rule is null, values must be provided.")
        assert_that(any(names(values) %in% c("varcli", "clicrit", "limiar_preseason", "limiar_epidemico")),
            msg = "setcriteria: elements missing from arg values")
    }
    if (!is.null(rule)) {
        if (rule[1] == "Af") {
            criteria <- list(crity = c("temp_min > temp_crit & inc > 0", 3, 0), crito = c("p1 > 0.95 & inc > limiar_preseason",
                2, 1), critr = c("inc > limiar_epidemico & casos > 10", 2, 1))
        }
        if (rule[1] == "Aw") {
            criteria = list(crity = c("umid_max > umid_crit & inc > 0", 3, 0), crito = c("p1 > 0.95 & inc > limiar_preseason",
                2, 0), critr = c("inc > limiar_epidemico & casos > 10", 2, 0))
        }
        if (rule[1] == "Awi") {
            criteria = list(crity = c("umid_min > umid_crit & inc > 0", 3, 0), crito = c("p1 > 0.95 & inc > limiar_preseason",
                2, 0), critr = c("inc > limiar_epidemico & casos > 10", 2, 0))
        }
        if (rule[1] == "AsAw") {
            criteria = list(crity = c("temp_min > temp_crit & umid_max > umid_crit & inc > 0", 3, 0),
                crito = c("p1 > 0.95 & inc > limiar_preseason", 3, 0), critr = c("inc > limiar_epidemico & casos > 10",
                  2, 0))
        }
        if (rule[1] == "AsAwi") {
            criteria = list(crity = c("temp_min > temp_crit & umid_min > umid_crit & inc > 0", 3, 0),
                crito = c("p1 > 0.95 & inc > limiar_preseason", 3, 0), critr = c("inc > limiar_epidemico & casos > 10",
                  2, 0))
        }
    }
    else {
        message("setcriteria function using user defined criteria")
        criteria <- lapply(1:3, function(x) c(rule[[x]], delays[[x]]))
        names(criteria) <- c("crity", "crito", "critr")
    }
    if (!is.null(values)) {
        if (!("varcli2" %in% names(values)))
            values[["varcli2"]] <- "xx"
        if (rule[1] %in% c("Af", "AsAw", "AsAwi")) {
            assert_that(values[["varcli"]] == "temp_min" | values[["varcli2"]] == "temp_min", msg = "setcriteria: Af, AsAw and AsAwi require temp_min")
            tm <- names(values)[which(values == "temp_min")]
            if (tm == "varcli")
                values <- c(values, temp_crit = values[["clicrit"]])
            if (tm == "varcli2")
                values[["temp_crit"]] <- c(values, temp_crit = values[["clicrit2"]])
        }
        if (rule[1] %in% c("Aw", "AsAw")) {
            assert_that(values[["varcli"]] == "umid_max" | values[["varcli2"]] == "umid_max", msg = "setcriteria: Aw and AsAw require umid_max")
            um <- names(values)[which(values == "umid_max")]
            if (um == "varcli")
                values <- c(values, umid_crit = values[["clicrit"]])
            if (um == "varcli2")
                values <- c(values, umid_crit = values[["clicrit2"]])
        }
        if (rule[1] %in% c("Awi", "AsAwi")) {
            assert_that(values[["varcli"]] == "umid_min" | values[["varcli2"]] == "umid_min", msg = "setcriteria: Awi and AsAwi require umid_min")
            um <- names(values)[which(values == "umid_min")]
            if (um == "varcli")
                values <- c(values, umid_crit = values[["clicrit"]])
            if (um == "varcli2")
                values <- c(values, umid_crit = values[["clicrit2"]])
        }
        values <- unlist(sapply(names(values), function(x) values[[x]]))
        criteria <- lapply(criteria, function(x) c(str_replace_all(x[1], values), x[c(2, 3)]))
    }
    criteria
}
fouralert <- function(obj, crit, miss = "last", dy = 4) {
    assert_that(all(names(crit) %in% c("crity", "crito", "critr")) & all(sapply(crit, length) %in% 3),
        msg = "fouralert: argument crit is mispecified")
    parsed_rules <- lapply(crit, function(x) parse(text = x[1]))
    delay_turnon <- lapply(crit, function(x) as.numeric(x[[2]]))
    delay_turnoff <- lapply(crit, function(x) as.numeric(x[[3]]))
    accumcond <- function(vec, lag) {
        if (lag == 1)
            return(vec)
        zoo::rollapply(vec, lag, sum, align = "right", fill = NA)
    }
    assertcondition <- function(dd, nivel) {
        condtrue <- with(dd, as.numeric(eval(parsed_rules[[nivel]])))
        mi <- which(is.na(condtrue))
        if (miss == "last") {
            for (i in mi[mi != 1]) condtrue[i] <- condtrue[i - 1]
        }
        ncondtrue <- accumcond(condtrue, delay_turnon[[nivel]])
        cbind(condtrue, ncondtrue)
    }
    le <- nrow(obj)
    indices <- data.frame(cbind(assertcondition(obj, 1), assertcondition(obj, 2), assertcondition(obj,
        3)))
    names(indices) <- c("cytrue", "nytrue", "cotrue", "notrue", "crtrue", "nrtrue")
    indices$level <- 1
    indices$level[indices$nytrue == delay_turnon[1]] <- 2
    indices$level[indices$notrue == delay_turnon[2]] <- 3
    indices$level[indices$nrtrue == delay_turnon[3]] <- 4
    delayturnoff <- function(level) {
        delay_level = delay_turnoff[[(level - 1)]]
        ifelse(delay_level == 0, return(indices), {
            pos <- which(indices$level == level) %>% lapply(., function(x) x + seq(0, delay_level)) %>%
                unlist() %>% unique()
            pos <- pos[pos <= le]
            indices$level[pos] <- level
            return(indices)
        })
    }
    indices <- delayturnoff(level = 4)
    indices <- delayturnoff(level = 3)
    indices <- delayturnoff(level = 2)
    ale <- list(data = obj, indices = indices, crit = crit, n = 4)
    class(ale) <- "alerta"
    return(ale)
}
calc.alerta <- function(x, pars, level = "municipio", ...) {
    d <- pars[[1]]
    datarelatorio <- pars[[2]]
    nowcasting <- pars[[3]]
    pars_table <- pars[[4]]
    caselist <- pars[[5]]
    if (level == "municipio") {
        cas.x <- d %>% filter(cidade == x) %>% adjustIncidence(method = "none", nowSE = datarelatorio,
            nyears = 1)
        if (nowcasting == "bayesian") {
            try(cas.x <- d %>% filter(cidade == x) %>% adjustIncidence(method = nowcasting, datas = caselist,
                nowSE = datarelatorio, nyears = 1))
        }
        cas.x <- cas.x %>% Rt(count = "tcasesmed", gtdist = "normal", meangt = 3, sdgt = 1) %>% mutate(inc = tcasesmed/pop *
            1e+05) %>% arrange(SE)
        assert_that(nrow(cas.x) > 0, msg = "check alertapipeline. error makes nrow = 0")
        crit.x <- pars_table[pars_table$municipio_geocodigo == x, ]
        crit.x.vector <- structure(as.character(crit.x), names = as.character(names(crit.x)))
        criteriaU <- setCriteria(rule = crit.x$codmodelo, values = crit.x.vector)
        y <- fouralert(cas.x, crit = criteriaU)
    }
    y
}
tabela_historico <- function(obj, iniSE, lastSE, type = "all", versao = Sys.Date()) {
    if (class(obj) == "list" & class(obj[[1]]) == "alerta") {
        data <- transpose(obj)[[1]] %>% bind_rows()
        indices <- transpose(obj)[[2]] %>% bind_rows()
    }
    else if (class(obj) == "alerta") {
        data <- obj$data
        indices <- obj$indices
    }
    d <- cbind(data, indices)
    gera_id <- function(x) paste(data$cidade[x], data$Localidade_id[x], data$SE[x], as.character(julian(versao)),
        sep = "")
    d$id <- sapply(1:nrow(data), gera_id)
    if (missing(iniSE))
        iniSE <- 0
    if (missing(lastSE))
        lastSE <- 3e+05
    d <- d %>% filter(SE >= iniSE & SE <= lastSE) %>% rename(municipio_geocodigo = cidade, municipio_nome = nome,
        casos_est = tcasesmed, casos_est_min = tcasesICmin, casos_est_max = tcasesICmax, casprov = cas_prov,
        nivel = level, temp_min = temp_min, temp_med = temp_med, temp_max = temp_max, umid_min = umid_min,
        umid_med = umid_med, umid_max = umid_max) %>% mutate(p_rt1 = ifelse(is.na(p1), 0, p1), p_inc100k = casos_est/pop *
        1e+05, Localidade_id = ifelse(is.na(localidade), 0, localidade), data_iniSE = SE2date(SE)$ini,
        versao_modelo = as.character(versao))
    d$Rt[is.na(d$Rt)] <- 0
    pars <- read.parameters(d$municipio_geocodigo, cid10 = d$CID10[1])
    d <- d %>% rename(receptivo = cytrue, transmissao = cotrue)
    d1 <- d %>% left_join(pars[c("municipio_geocodigo", "limiar_preseason", "limiar_epidemico")]) %>%
        mutate(nivel_inc = case_when(p_inc100k < limiar_preseason ~ 0, p_inc100k >= limiar_preseason &
            p_inc100k < limiar_epidemico ~ 1, p_inc100k >= limiar_epidemico ~ 2))
    varnames <- c("data_iniSE", "SE", "CID10", "casos", "casos_est", "casos_est_min", "casos_est_max",
        "municipio_geocodigo", "casprov", "p_rt1", "p_inc100k", "Localidade_id", "nivel", "id", "versao_modelo",
        "municipio_nome", "Rt", "pop", "temp_min", "temp_med", "temp_max", "umid_min", "umid_med", "umid_max",
        "receptivo", "transmissao", "nivel_inc")
    if (all(varnames %in% names(d1))) {
        dfinal <- d1[, varnames]
        return(dfinal)
    }
    else {
        message(paste("historico_alerta is not returning the required variables", varnames[!varnames %in%
            names(d1)]))
        return(NULL)
    }
}
write_alerta <- function(d, writetofile = FALSE, datasource = con, arq = "output.sql") {
    assert_that(class(d) == "data.frame", msg = "write_alerta: d is not a data.frame. d should\n                  be an output from tabela_historico.")
    if (writetofile == FALSE)
        assert_that(class(datasource) == "PostgreSQLConnection", msg = "write_alerta: \n                 works only for writing into Infodengue's server")
    cid10 = unique(d$CID10)
    assert_that(length(cid10) == 1, msg = "write_alerta: d must contain only one cid10")
    dcolumns <- c("SE", "data_iniSE", "casos_est", "casos_est_min", "casos_est_max", "casos", "casprov",
        "municipio_geocodigo", "p_rt1", "p_inc100k", "Localidade_id", "nivel", "id", "versao_modelo",
        "municipio_nome", "Rt", "pop", "receptivo", "transmissao", "nivel_inc", "temp_min", "temp_med",
        "temp_max", "umid_min", "umid_med", "umid_max")
    if (!("temp_min" %in% names(d)))
        d$temp_min <- NA
    if (!("umid_max" %in% names(d)))
        d$umid_max <- NA
    assert_that(all(dcolumns %in% names(d)), msg = "write_alerta: check if d contains required\n                                                           columns")
    if (cid10 == "A90") {
        tabela <- "Historico_alerta"
        constr.unico = "alertas_unicos"
    }
    if (cid10 == "A92.0") {
        tabela <- "Historico_alerta_chik"
        constr.unico = "alertas_unicos_chik"
    }
    if (cid10 == "A92.8") {
        tabela <- "Historico_alerta_zika"
        constr.unico = "alertas_unicos_zika"
    }
    if (!(cid10 %in% c("A90", "A92.0", "A92.8")))
        stop(paste("não sei onde salvar histórico para o agravo", cid10))
    dados <- d %>% select(all_of(dcolumns))
    varnamesforsql <- c("\"SE\"", "\"data_iniSE\"", "casos_est", "casos_est_min", "casos_est_max", "casos",
        "casprov", "municipio_geocodigo", "p_rt1", "p_inc100k", "\"Localidade_id\"", "nivel", "id", "versao_modelo",
        "municipio_nome", "\"Rt\"", "pop", "tempmin", "tempmed", "tempmax", "umidmin", "umidmed", "umidmax",
        "receptivo", "transmissao", "nivel_inc")
    varnames.sql <- str_c(varnamesforsql, collapse = ",")
    updates = str_c(paste(varnamesforsql, "=excluded.", varnamesforsql, sep = ""), collapse = ",")
    escreve_linha <- function(li) {
        vetor <- dados[li, ]
        vetor$municipio_nome = gsub(vetor$municipio_nome, pattern = "'", replacement = "''")
        linha = paste0(vetor$SE, ",'", as.character(vetor$data_iniSE), "',", str_c(vetor[1, c("casos_est",
            "casos_est_min", "casos_est_max", "casos", "casprov", "municipio_geocodigo", "p_rt1", "p_inc100k",
            "Localidade_id", "nivel", "id")], collapse = ","), ",'", as.character(vetor$versao_modelo),
            "','", as.character(vetor$municipio_nome), "',", str_c(vetor[1, c("Rt", "pop", "temp_min",
                "temp_med", "temp_max", "umid_min", "umid_med", "umid_max")], collapse = ","), ",", str_c(vetor[1,
                c("receptivo", "transmissao", "nivel_inc")], collapse = ","))
        linha = gsub("NA", "NULL", linha)
        linha = gsub("NaN", "NULL", linha)
        insert_sql = paste("INSERT INTO \"Municipio\".\"", tabela, "\" (", varnames.sql, ") VALUES (",
            linha, ") \n                                    ON CONFLICT ON CONSTRAINT ", constr.unico,
            "  \n                                     DO UPDATE SET ", updates, ";", sep = "")
        if (writetofile == FALSE) {
            try(dbGetQuery(datasource, insert_sql))
        }
        insert_sql
    }
    if (writetofile) {
        f <- file(arq, open = "w", encoding = "utf8")
        for (i in 1:nrow(d)) writeLines(escreve_linha(i), f)
        message("writing alerta into file ", arq)
        close(f)
    }
    else {
        print(paste("writing alerta into table", tabela))
        try(dbGetQuery(datasource, "BEGIN TRANSACTION;"))
        1:nrow(d) %>% map(escreve_linha)
        try(dbGetQuery(datasource, "COMMIT TRANSACTION;"))
    }
}
