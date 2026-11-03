# Unmodified function expressions from e460b3b8c15ab28e8755f482d576cf7280803b34:R/adjust_incidence_functions.R
adjustIncidence <- function(obj, datas, method = "none", pdig = plnorm((1:20) * 7, 2.5016, 1.1013), Dmax = 10,
    nyears = 2, datasource = con, nowSE, safelimit = 5, ...) {
    city <- unique(obj$cidade)
    cid <- obj$CID10[1]
    assert_that(length(city) == 1, msg = "adjustIncidence only works for one city at a time.")
    le = nrow(obj)
    if (missing(nowSE)) {
        stop("adjustIncidence: nowSE must be provided")
    }
    else {
        assert_that(nowSE <= max(obj$SE), msg = "adjustIncidence: lastSE larger than max(obj$SE). Check input.")
        obj <- subset(obj, SE <= nowSE)
    }
    obj$tcasesICmin <- obj$casos
    obj$tcasesmed <- obj$casos
    obj$tcasesICmax <- obj$casos
    if (sum(tail(obj$casos, n = 52), na.rm = TRUE) <= 50 | sum(tail(obj$casos, n = 5), na.rm = TRUE) <=
        5) {
        message("less than 50 cases in the last 12 months or less than 5 cases \n            in the last month. Nowcasting not done")
        return(obj)
    }
    if (method == "bayesian") {
        message(paste("computing nowcasting for city ", city, " date ", nowSE, "..."))
        nowday <- SE2date(nowSE)$ini + 6
        if (missing(datas)) {
            dados <- getdelaydata(cities = city, nyears = nyears, cid10 = cid, lastday = nowday, datasource = datasource)
        }
        else {
            dados <- datas %>% filter(municipio_geocodigo == city)
            assert_that(dim(dados)[1] > 0, msg = "adjustIncidence: city not in dataset")
        }
        resfit <- bayesnowcasting(dados, Dmax = Dmax, nowSE = nowSE)
        if (!is.null(resfit)) {
            if (tail(resfit$Median, n = 1) > safelimit * sum(tail(obj$casos, n = 5), na.rm = TRUE)) {
                message("nowcast estimate is too large, returning the original count.")
            }
            else {
                if (tail(resfit$LS, n = 1) > 10 * safelimit * sum(tail(obj$casos, n = 5), na.rm = TRUE)) {
                  message("upper limit nowcasting too large, returning t_cases_max = NA")
                  resfit$LS <- NA
                  expl <- which(resfit$LS > 10000)
                  resfit$LS[expl] <- NA
                  message("upper limit nowcarting too large in some dates, \n                   returning t_cases_max = NA")
                }
                message("bayesnowcasting done")
                for (se in resfit$SE) {
                  rse <- which(resfit$SE == se)
                  obj[obj$SE == se, c("tcasesICmin", "tcasesmed", "tcasesICmax")] <- c(resfit$LI[rse],
                    resfit$Median[rse], resfit$LS[rse])
                }
            }
        }
        else {
            message("nowcasting failed, returning the original count")
        }
    }
    if (method == "none")
        message("nowcasting not done, returning the original counts")
    obj
}
bayesnowcasting <- function(d, Dmax = 10, nowSE, nweeks = 50, interacao = FALSE, tweet = F) {
    if (is.null(names(d))) {
        message("bayesnowcasting: no data, returning NULL")
        return(NULL)
    }
    if (nrow(d) < 50) {
        message("bayesnowcasting: few data, returning NULL")
        return(NULL)
    }
    if (missing(nowSE))
        stop("bayesnowcasting requires definition of nowcast week ")
    assert_that(all(c("municipio_geocodigo", "dt_notific", "dt_sin_pri", "dt_digita") %in% names(d)),
        msg = "bayesnowcasting requires data with columns\n              municipio_geocodigo, dt_notific, dt_sin_pri, dt_digita")
    d$ininotif <- d$dt_notific - d$dt_sin_pri
    wrongdates <- which(d$ininotif > 365 | d$ininotif < 0 | is.na(d$dt_sin_pri))
    if (length(wrongdates) > 0) {
        message(paste(length(wrongdates), "registros com datas de inicio de sintomas invalidas"))
        d <- d[-wrongdates, ]
    }
    Fim <- SE2date(nowSE)$ini + 6
    Inicio <- Fim - nweeks * 7
    d <- d[-(which(d$dt_sin_pri < Inicio)), ]
    if (is.null(names(d))) {
        message("bayesnowcasting: no valid data, returning NULL")
        return(NULL)
    }
    if (nrow(d) < 50) {
        message("bayesnowcasting: few valid data, returning NULL")
        return(NULL)
    }
    message(paste("nowcast will be calibrated with ", nrow(d), "cases"))
    d <- d %>% mutate(dt_sinpri_epiweek = epiweek(dt_sin_pri), dt_sinpri_aux = as.numeric(format(as.Date(dt_sin_pri),
        "%w")), dt_sinpri_week = dt_sin_pri + 3 - dt_sinpri_aux, dt_sinpri_epiyear = epiyear(dt_sin_pri),
        dt_sinpri_SE = dt_sinpri_epiyear * 100 + dt_sinpri_epiweek, dt_digita_epiweek = epiweek(dt_digita),
        dt_digita_epiyear = epiyear(dt_digita), dt_digita_SE = dt_digita_epiyear * 100 + dt_digita_epiweek,
        delay_epiweek = ifelse(dt_digita_epiyear == dt_sinpri_epiyear, as.numeric(dt_digita_epiweek -
            dt_sinpri_epiweek), as.numeric(dt_digita_epiweek - dt_sinpri_epiweek) + 52 * (dt_digita_epiyear -
            dt_sinpri_epiyear)))
    obs <- d %>% group_by(dt_sinpri_week) %>% summarize(casos = n())
    Fim_now <- SE2date(nowSE)$ini + 3
    Ini_now <- Fim_now - nweeks * 7
    tibble(Date = c(Ini_now, Fim_now)) %>% mutate(Weekday = weekdays(Date))
    tbl.dates <- tibble(dt_sinpri_week = seq(Ini_now, Fim_now, by = 7)) %>% rowid_to_column(var = "Time")
    Today <- max(tbl.dates$Time)
    dados.ag <- d %>% filter(dt_digita <= Fim & dt_digita >= Ini_now) %>% mutate(delay_epiweek = ifelse(delay_epiweek >
        Dmax, NA, delay_epiweek)) %>% drop_na(delay_epiweek) %>% group_by(dt_sinpri_week, delay_epiweek) %>%
        dplyr::summarise(Casos = n()) %>% spread(key = delay_epiweek, value = Casos) %>% full_join(y = tbl.dates,
        by = "dt_sinpri_week") %>% gather(key = "Delay", value = Casos, -dt_sinpri_week, -Time) %>% mutate(Delay = as.numeric(Delay),
        Casos = ifelse(test = (Time + Delay) <= Today, yes = replace_na(Casos, 0), no = NA)) %>% dplyr::rename(Date = dt_sinpri_week) %>%
        dplyr::arrange(Date)
    if (interacao & !tweet) {
        model.dengue <- Casos ~ 1 + f(Time, model = "rw2", constr = T) + f(Delay, model = "rw1", constr = T) +
            f(TimeDelay, model = "iid", constr = T)
    }
    if (!interacao & !tweet) {
        model.dengue <- Casos ~ 1 + f(Time, model = "rw2", constr = T) + f(Delay, model = "rw1", constr = T)
    }
    if (interacao & tweet) {
        model.dengue <- Casos ~ 1 + tweet + f(Time, model = "rw2", constr = T) + f(Delay, model = "rw1",
            constr = T) + f(TimeDelay, model = "iid", constr = T)
    }
    if (!interacao & tweet) {
        model.dengue <- Casos ~ 1 + tweet + f(Time, model = "rw2", constr = T) + f(Delay, model = "rw1",
            constr = T)
    }
    output.dengue <- nowcast.INLA(model.day = model.dengue, dados.ag = dados.ag %>% mutate(TimeDelay = paste(Time,
        Delay)))
    pred.dengue <- nowcasting(output.dengue, dados.ag, Dm = Dmax, Fim = Fim)
    pred.dengue.summy <- pred.dengue %>% group_by(Date) %>% dplyr::summarise(Mean = mean(Casos, na.rm = TRUE),
        Median = median(Casos, na.rm = TRUE), LI = quantile(Casos, probs = 0.025, na.rm = TRUE), LS = quantile(Casos,
            probs = 0.975, na.rm = TRUE)) %>% left_join(obs, by = c(Date = "dt_sinpri_week"))
    pred.dengue.summy$SE <- daySEday(pred.dengue.summy$Date)$SE
    pred.dengue.summy
}
