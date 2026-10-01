# Algorithmic epidemiological calendar --------------------------------------

.epi_year_start <- function(year) {
  year <- as.integer(year)
  january_fourth <- as.Date(sprintf("%04d-01-04", year))
  january_fourth - as.integer(format(january_fourth, "%w"))
}

.validate_epiweek <- function(x) {
  if (!is.numeric(x) || any(!is.finite(x) & !is.na(x)) ||
      any(x != floor(x), na.rm = TRUE)) {
    stop("Epidemiological weeks must be finite integers in YYYYWW format.", call. = FALSE)
  }
  valid <- !is.na(x)
  year <- floor(x[valid] / 100)
  week <- x[valid] %% 100
  maximum <- as.integer((.epi_year_start(year + 1L) - .epi_year_start(year)) / 7)
  invalid <- year < 1L | week < 1L | week > maximum
  if (any(invalid)) {
    stop("Invalid epidemiological week: ", paste(x[valid][invalid], collapse = ", "), call. = FALSE)
  }
  invisible(x)
}

#' Convert dates to epidemiological weeks
#'
#' Uses the Brazilian Sunday-to-Saturday convention in which epidemiological
#' week 1 is the week containing 4 January.
#'
#' @param x A vector coercible to `Date`.
#' @param format Format used for character input.
#' @return An integer vector in `YYYYWW` format.
#' @export
#' @examples
#' as_epiweek(as.Date(c("2020-12-27", "2021-01-03")))
as_epiweek <- function(x, format = "%Y-%m-%d") {
  dates <- if (inherits(x, "Date")) x else as.Date(x, format = format)
  result <- rep(NA_integer_, length(dates))
  valid <- !is.na(dates)
  if (!any(valid)) return(result)

  calendar_year <- as.integer(format(dates[valid], "%Y"))
  start <- .epi_year_start(calendar_year)
  epi_year <- calendar_year
  before <- dates[valid] < start
  epi_year[before] <- epi_year[before] - 1L
  next_start <- .epi_year_start(epi_year + 1L)
  after <- dates[valid] >= next_start
  epi_year[after] <- epi_year[after] + 1L
  start <- .epi_year_start(epi_year)
  week <- as.integer(as.numeric(dates[valid] - start) %/% 7) + 1L
  result[valid] <- epi_year * 100L + week
  result
}

#' Return the start date of epidemiological weeks
#'
#' @param x Epidemiological weeks in `YYYYWW` format.
#' @return A `Date` vector containing Sundays.
#' @export
#' @examples
#' epiweek_start(c(202052, 202101))
epiweek_start <- function(x) {
  .validate_epiweek(x)
  result <- as.Date(rep(NA_real_, length(x)), origin = "1970-01-01")
  valid <- !is.na(x)
  year <- floor(x[valid] / 100)
  week <- x[valid] %% 100
  result[valid] <- .epi_year_start(year) + (week - 1L) * 7L
  result
}

#' Create an inclusive epidemiological-week sequence
#'
#' @param from First epidemiological week in `YYYYWW` format.
#' @param to Last epidemiological week in `YYYYWW` format.
#' @return An integer vector of epidemiological weeks.
#' @export
#' @examples
#' epiweek_seq(202052, 202101)
epiweek_seq <- function(from, to) {
  .validate_epiweek(c(from, to))
  if (length(from) != 1L || length(to) != 1L || is.na(from) || is.na(to)) {
    stop("`from` and `to` must be non-missing scalar epidemiological weeks.", call. = FALSE)
  }
  start <- epiweek_start(from)
  end <- epiweek_start(to)
  if (start > end) stop("`from` must not be after `to`.", call. = FALSE)
  as_epiweek(seq(start, end, by = "7 days"))
}
