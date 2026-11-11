# Unmodified function expressions from e460b3b8c15ab28e8755f482d576cf7280803b34:R/Rt_functions.R
Rt <- function(obj, count = "casos", gtdist, meangt, sdgt, CI = "beta", alpha = 0.95, a0 = 2, b0 = 3) {
    if (!any(c(count, "SE") %in% names(obj)))
        stop("obj must be a data.frame with variables \n                                              SE and var, at least. Consider using getCases")
    y <- obj[, count]
    le <- length(y)
    if (le < 2 * meangt)
        warning("you need a time series                           \n                             with size at least 2 generation intervals to estimate Rt")
    if (gtdist == "normal")
        ga <- rev(dnorm(x = 1:le, mean = meangt, sd = sdgt))
    if (gtdist == "delta") {
        ga <- rep(0, le)
        ga[le - meangt] <- 1
    }
    obj$Rt <- NA
    obj$lwr <- NA
    obj$upr <- NA
    obj$p1 <- NA
    for (t in ceiling(2 * meangt):le) {
        num = y[t]
        deno = sum(y[1:t] * ga[(le - t + 1):le])
        obj$Rt[t] <- num/deno
        if (CI == "beta") {
            obj$p1[t] <- 1 - pbeta(0.5, shape1 = num, shape2 = deno)
            obj[t, c("lwr", "upr")] <- ll(betaconf(alpha = alpha, x = num, n = num + deno, a = a0, b = b0))
        }
    }
    obj
}
betaconf <- function(alpha = 0.95, x, n, a = 1, b = 1, CP = "FALSE") {
    if (CP == "TRUE") {
        lower <- 1 - qbeta((1 - alpha)/2, n + x - 1, x)
        upper <- 1 - qbeta((1 + alpha)/2, n - x, x + 1)
    }
    else {
        lower <- qbeta((1 - alpha)/2, a + x, b + n - x)
        upper <- qbeta((1 + alpha)/2, a + x, b + n - x)
    }
    return(c(lower, upper))
}
ll <- function(x) {
    x/(1 - x)
}
