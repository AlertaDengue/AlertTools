# Pure keyed frame comparator shared by real-INLA and historical validation.
compare_epidemiological_frames <- function(reference, candidate, tolerance = 1e-7) {
  if (nrow(reference) != nrow(candidate) ||
      anyDuplicated(names(reference)) || anyDuplicated(names(candidate)) ||
      !setequal(names(reference), names(candidate))) {
    stop("Epidemiological output schema/cardinality mismatch.")
  }
  if ("SE" %in% names(reference)) {
    municipality <- intersect(c("municipio_geocodigo", "cidade"), names(reference))
    keys <- c(head(municipality, 1L), intersect("CID10", names(reference)), "SE",
              intersect(c("Localidade_id", "localidade"), names(reference)))
    if (anyDuplicated(reference[keys]) || anyDuplicated(candidate[keys]) ||
        anyNA(reference[keys]) || anyNA(candidate[keys])) {
      stop("Epidemiological output keys must be unique and complete.")
    }
    reference <- reference[do.call(order, reference[keys]), , drop = FALSE]
    candidate <- candidate[do.call(order, candidate[keys]), , drop = FALSE]
  }
  do.call(rbind, lapply(names(reference), function(column) {
    old <- reference[[column]]
    new <- candidate[[column]]
    both_na <- is.na(old) & is.na(new)
    numeric <- is.numeric(old) && is.numeric(new)
    if (numeric) {
      finite <- is.finite(old) & is.finite(new)
      equal <- both_na | (finite & abs(old - new) <= tolerance) |
        (is.infinite(old) & is.infinite(new) & sign(old) == sign(new))
      maximum <- if (any(finite)) max(abs(old[finite] - new[finite])) else 0
    } else {
      equal <- both_na | (!is.na(old) & !is.na(new) & as.character(old) == as.character(new))
      maximum <- NA_real_
    }
    equal[is.na(equal)] <- FALSE
    data.frame(metric = column, rows = length(old),
               differences = sum(!equal), max_abs_difference = maximum,
               tolerance = if (numeric) tolerance else 0,
               status = if (all(equal)) "match" else "mismatch")
  }))
}
