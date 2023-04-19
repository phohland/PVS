#' Forms dates to days for hRELSA calculation
#'
#' The \code{hrelsa_days} function transforms dates into days as integer
#' either with the first date of each patient as day 1
#' or with the first date of the whole data set as day 1
#' a day column will be added
#'
#' @param raw data set
#' @param format either "day", "year" or "timecode" (a transformation to years is possible as well)
#' @param formthis date variable
#' @param newdayone for each patient a day 1? (TRUE/FALSE)
#'
#' @return \code{raw} data set with day column added
#'
#' @export
#'

hrelsa_days <- function (raw,
                         format = "day",
                         date_format = "%d%b%Y",
                         formthis = NULL,
                         newdayone = TRUE) {
  # Searching for errors ----------------------------------------------------

  abort <- FALSE
  if (is.null(raw)) {
    warning("There was no raw data set found. The days were not calculated.")
    abort <- TRUE
  }
  if (format != "day" && format != "year" && format != "timecode") {
    warning("Only the formats 'day', 'year' and 'timecode' are available.
            The days were not calculated.")
    abort <- TRUE
  }
  if (length(formthis) != 1) {
    warning("There was more than one or no variable for the dates.
            The days were not calculated.")
    abort <- TRUE
  }
  if (!(formthis %in% colnames(raw))) {
    warning("The date column was not found. The days were not calculated.")
    abort <- TRUE
  }
  if (!("id" %in% colnames(raw))) {
    warning("The id column needs to be called 'id'.
            The days were not calculated.")
    abort <- TRUE
  }

  # Function code -----------------------------------------------------------

  if (abort) {

  } else {

    if (format != "timecode") {
      col_id <- which(names(raw) == formthis)
      raw[col_id] <- as.Date(unlist(raw[col_id], use.names = FALSE), date_format)
    } else {
      col_id <- which(names(raw) == formthis)
      raw$dummy <- raw[col_id]
      raw[col_id] <- as.numeric(unlist(raw[col_id], use.names = FALSE))
    }

    if (newdayone) {
      n <- unique(raw$id)
      col <- which(names(raw) == formthis)
      ret <- NULL
      for (i in 1:length(n)) {
        pat <- filter(raw, id == n[i])
        pat <- arrange(pat, pat[col])
        pat$day <- 0
        first_day <- pat[1, col]
        for (o in 1:length(pat$id)) {
          if (o == 1) {
            pat$day[o] <- 1
          } else {
            if (format == "year") {
              pat$day[o] <-
                as.integer((as.numeric(format(
                  pat[[o, col]], "%Y"
                )) - as.numeric(format(
                  first_day[[1, 1]], "%Y"
                ))))
              if (pat$day[o] == 0) {
                pat$day[o] <- 1
              }
            } else if (format == "day" || format == "timecode") {
              #pat$day[o] <- as.integer((pat[o, col] - first_day))
              pat$day[o] <- as.numeric((pat[o, col] - first_day))
            }
          }
        }
        ret <- rbind(ret, pat)
      }
      ret <- arrange(ret, id, day)
      raw <- ret
    } else {
      n <- unique(raw$id)
      col <- which(names(raw) == formthis)
      ret <- NULL
      raw <- arrange(raw, raw[col])
      first_day <- raw[1, col]
      for (i in 1:length(n)) {
        pat <- filter(raw, id == n[i])
        pat <- arrange(pat, pat[col])
        pat$day <- 0
        for (o in 1:length(pat$id)) {
          if (pat[o, col] == first_day) {
            pat$day[o] <- 1
          } else {
            if (format == "year") {
              pat$day[o] <-
                as.integer((as.numeric(format(
                  pat[[o, col]], "%Y"
                )) - as.numeric(format(
                  first_day[[1, 1]], "%Y"
                ))))
              if (pat$day[o] == 0) {
                pat$day[o] <- 1
              }
            } else if (format == "day" || format == "timecode") {
              #pat$day[o] <- as.integer((pat[o, col] - first_day))
              pat$day[o] <- as.numeric((pat[o, col] - first_day))
            }
          }
        }
        ret <- rbind(ret, pat)
      }
      ret <- arrange(ret, id, day)
      raw <- ret
    }

      if (format == "timecode") {
        raw[col_id] <- raw$dummy
        raw <- raw[, -which(names(raw) == "dummy")]
      }
      colnames(raw)[which(names(raw) == "day")] <- "time"

  }
  return(raw)
}
