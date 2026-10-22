# Unmodified expressions from AlertaDengueAnalise/main/main_BR.R
# Commit 27a03f26a9bf366597ef135e7da8d2f620ea9a8e
if (!is.null(restab_den)) {
    out_sql <- file.path(sql_dir, "output_dengue.sql")
    log_msg("Writing SQL dengue: ", out_sql)
    write_alerta(restab_den, writetofile = TRUE, arq = out_sql)
} else {
    log_msg("No dengue restab found. Skipping dengue SQL.", level = "WARN")
}
if (!is.null(restab_chik)) {
    out_sql <- file.path(sql_dir, "output_chik.sql")
    log_msg("Writing SQL chik: ", out_sql)
    write_alerta(restab_chik, writetofile = TRUE, arq = out_sql)
} else {
    log_msg("No chik restab found. Skipping chik SQL.", level = "WARN")
}
if (!is.null(restab_zika)) {
    out_sql <- file.path(sql_dir, "output_zika.sql")
    log_msg("Writing SQL zika: ", out_sql)
    write_alerta(restab_zika, writetofile = TRUE, arq = out_sql)
} else {
    log_msg("No zika restab found. Skipping zika SQL.", level = "WARN")
}
