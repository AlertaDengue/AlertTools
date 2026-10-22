# Unmodified function expressions from e460b3b8c15ab28e8755f482d576cf7280803b34:R/utility_tools.R
data2SE <- function(days, format = "%d/%m/%Y") {
    sem <- rep(NA, length(days))
    days <- as.Date(as.character(days), format = format)
    for (i in 1:length(days)) {
        sem[i] <- episem(days[i])
    }
    sem
}
episem <- function(x, format = "%Y-%m-%d", separa = "", retorna = "YW") {
    if (class(x) != "Date") {
        x <- as.Date(x, format = format)
    }
    if (is.na(x) == T) {
        message("episem: Date not valid, returning NA")
        return(NA)
    }
    year <- function(dt) {
        as.numeric(format(dt, "%Y"))
    }
    wday <- function(dt) {
        as.numeric(format(dt, "%w"))
    }
    passado <- function(dt, diff = 1) {
        as.Date(paste(as.numeric(format(dt, "%Y")) - diff, format(dt, "%m-%d"), sep = "-"))
    }
    ano <- year(x)
    dia1 <- as.Date(paste(ano, "01", "01", sep = "-"), format = "%Y-%m-%d")
    diasem <- wday(dia1)
    fwd <- ifelse(diasem <= 3, dia1 - diasem, dia1 + (7 - diasem))
    fwd <- as.Date(fwd, origin = "1970-01-01")
    if (x < fwd) {
        dia1 <- passado(dia1)
        diasem <- wday(dia1)
        fwd <- ifelse(diasem <= 3, dia1 - diasem, dia1 + (7 - diasem))
        fwd <- as.Date(fwd, origin = "1970-01-01")
    }
    diafim <- as.Date(paste(ano, "12", "31", sep = "-"))
    diasem <- wday(diafim)
    ewd <- ifelse(diasem < 3, diafim - diasem - 1, diafim + 6 - diasem)
    ewd <- as.Date(ewd, origin = "1970-01-01")
    if (x > ewd)
        fwd <- ewd + 1
    epiweek <- floor(as.numeric(x - fwd)/7) + 1
    if (epiweek == 0)
        epiweek <- 1
    epiyear <- year(fwd + 180)
    if (retorna == "YW") {
        out <- sprintf("%4d%s%02d", epiyear, separa, epiweek)
    }
    else if (retorna == "Y") {
        out <- epiyear
    }
    else {
        out <- epiweek
    }
    if (separa == "") {
        return(as.numeric(out))
    }
    else {
        return(out)
    }
}
SE2date <- function(se) {
    if (!class(se[1]) %in% c("numeric", "integer"))
        stop("se should be numeric or integer")
    res <- data.frame(SE = se, ini = as.Date("1970-01-01"))
    for (i in 1:length(res$SE)) res$ini[i] <- SE$Inicio[SE$SE == res$SE[i]]
    res
}
sevendigitgeocode <- function(dig) {
    ndig = nchar(as.character(dig))
    if (ndig == 7)
        return(dig)
    if (ndig != 6)
        stop("this funtion receives 6 digits geocodes only")
    digspatologicos <- c(220191, 220198, 220225, 261153, 311783, 315213, 430587, 520396, 520393)
    if (dig %in% digspatologicos) {
        pos <- which(digspatologicos == dig)
        dig7 <- c(2201919, 2201988, 2202251, 2611533, 3117836, 3152131, 4305871, 5203962, 5203939)[pos]
        return(dig7)
    }
    peso <- c(1, 2, 1, 2, 1, 2, 0)
    soma <- 0
    digchar <- strsplit(as.character(dig), "")[[1]]
    ndig <- length(digchar)
    for (i in 1:6) {
        valor <- as.integer(digchar[i]) * peso[i]
        nvalor <- ifelse(valor < 10, valor, trunc(valor/10) + valor%%10)
        soma <- soma + nvalor
    }
    dv <- ifelse(soma%%10 == 0, 0, 10 - (soma%%10))
    return(dig * 10 + dv)
}
