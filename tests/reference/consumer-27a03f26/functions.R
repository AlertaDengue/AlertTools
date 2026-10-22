# Unmodified expressions from AlertaDengueAnalise/main/main_BR.R
# Commit 27a03f26a9bf366597ef135e7da8d2f620ea9a8e
safe_gc <- function() {
    invisible(gc(verbose = FALSE))
}
combine_ale_parts <- function(parts) {
    parts <- Filter(Negate(is.null), parts)
    if (length(parts) == 0) {
        return(NULL)
    }
    do.call(c, c(parts, recursive = FALSE))
}
run_city_pipeline <- function(city, cid10, now_mode, disease_label, sig) {
    log_msg("[state] ", sig, " ", disease_label, " city=", city, " started")
    result <- tryCatch({
        withCallingHandlers(pipe_infodengue(city, cid10 = cid10, nowcasting = now_mode, finalday = report_end_date,
            narule = "arima", iniSE = 201001, dataini = "sinpri", completetail = 0), warning = function(w) {
            log_msg("[state] ", sig, " ", disease_label, " city=", city, " warning: ", conditionMessage(w),
                level = "WARN")
            invokeRestart("muffleWarning")
        })
    }, error = function(e) {
        log_msg("[state] ", sig, " ", disease_label, " city=", city, " failed: ", conditionMessage(e),
            level = "ERROR")
        NULL
    })
    if (is.null(result)) {
        log_msg("[state] ", sig, " ", disease_label, " city=", city, " returned no result after fallback execution",
            level = "WARN")
    }
    result
}
run_disease_pipeline_by_city <- function(cidades, cid10, now_mode, disease_label, sig) {
    log_msg(" - ", disease_label, ": retrying municipality-level execution after state-level failure")
    ale_parts <- vector("list", length(cidades))
    for (j in seq_along(cidades)) {
        city <- cidades[[j]]
        ale_parts[[j]] <- run_city_pipeline(city, cid10, now_mode, disease_label, sig)
        safe_gc()
    }
    ale <- combine_ale_parts(ale_parts)
    rm(ale_parts)
    safe_gc()
    if (is.null(ale)) {
        stop("All municipality executions failed for ", sig, " / ", disease_label, call. = FALSE)
    }
    restab <- tabela_historico(ale, iniSE = report_epiweek - window_weeks)
    list(ale = ale, restab = restab)
}
run_disease_pipeline <- function(cidades, cid10, now_mode, disease_label, sig) {
    tryCatch({
        ale <- pipe_infodengue(cidades, cid10 = cid10, nowcasting = now_mode, finalday = report_end_date,
            narule = "arima", iniSE = 201001, dataini = "sinpri", completetail = 0)
        restab <- tabela_historico(ale, iniSE = report_epiweek - window_weeks)
        list(ale = ale, restab = restab)
    }, error = function(e) {
        log_msg("[state] ", sig, " ", disease_label, " failed in state-level execution: ", conditionMessage(e),
            level = "ERROR")
        log_msg("[state] ", sig, " ", disease_label, " fallback: municipality-level isolation", level = "WARN")
        run_disease_pipeline_by_city(cidades, cid10, now_mode, disease_label, sig)
    })
}
load_state_result <- function(path) {
    env <- new.env(parent = emptyenv())
    load(path, envir = env)
    if (!exists("res", envir = env, inherits = FALSE)) {
        stop("Missing object 'res' inside: ", path, call. = FALSE)
    }
    env$res
}
append_part <- function(parts, value) {
    if (!is.null(value)) {
        parts[[length(parts) + 1]] <- value
    }
    parts
}
normalize_restab <- function(x) {
    if (is.null(x)) {
        return(NULL)
    }
    if (is.list(x) && !is.data.frame(x)) {
        return(dplyr::bind_rows(x))
    }
    x
}
normalize_ale <- function(x) {
    if (is.null(x)) {
        return(NULL)
    }
    tr <- transpose(x)
    data <- dplyr::bind_rows(tr[[1]])
    idx <- dplyr::bind_rows(tr[[2]])
    cbind(data, idx)
}
bind_parts <- function(parts) {
    parts <- Filter(Negate(is.null), parts)
    if (length(parts) == 0) {
        return(NULL)
    }
    dplyr::bind_rows(parts)
}
cap_max <- function(df) {
    if (is.null(df)) {
        return(NULL)
    }
    if ("casos_est_max" %in% names(df)) {
        df$casos_est_max[df$casos_est_max > 10000] <- NA
    }
    df
}
