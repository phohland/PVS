#' X
#'
#' The \code{hrelsa_adaptive_baselines} X
#'
#' @param raw X
#'
#' @return \code{X} X
#'
#' @export
#'

hrelsa_adaptive_baselines <-
  function(dat,
           vars = NULL,
           turnvars = NULL,
           realtime = NULL,

           dob_dat = NULL,
           dob_data_id_col = NULL,
           dob_data_dob_col = NULL,

           norm_dat = NULL,
           norm_dat_names = NULL
  ) {
    # Searching for errors ----------------------------------------------------

    abort <- FALSE
    if (is.null(dat)) {
      warning("There was no data set found. The baseline was not calculated.")
      abort <- TRUE
    }
    if (is.null(vars)) {
      warning("There was no variable for the baseline calculation.
              The baseline was not calculated.")
      abort <- TRUE
    }
    if ((is.null(dob_dat) || is.null(norm_dat))) {
      warning(
        "When the adaptive baselines are wished, there are data regarding
        the dates of birth and the norm values needed.
        The baseline was not calculated."
      )
      abort <- TRUE
    }
    if (length(vars) != (length(norm_dat_names) - 1)) {
      warning(
        "The column names of the normal values need to have the same length.
        The baseline was not calculated."
      )
      abort <- TRUE
    }

    # Normalization -----------------------------------------------------------

    dat <- as_tibble(dat)
    n   <- unique(dat$id)
    dat <- arrange(dat, id, time)

    col_time <- which(names(dat) == realtime)
    names(dat)[col_time] <- "timepoint"

    names(dob_dat)[dob_data_id_col] <- "id"
    names(dob_dat)[dob_data_dob_col] <- "dob"

    dat$age <- 0

    # Calculates the age regarding the dob of the extern table and the current time point
    for (l in 1:nrow(dat)) {
      timepoint <- dat$timepoint[l]
      dob <- dob_dat$dob[dob_dat$id == as.character(dat$id[l])]

      dat$age[l] <- difftime(timepoint, dob, units = "days") / 365.25
    }


    for (j in 1:length(vars)) {
      norm            <- NULL
      for (i in 1:length(n)) {

          norm_table      <- dat[dat$id == n[i], vars[j]]
          age <- dat$age[dat$id == n[i]]
          norm_table$age <- NA
          norm_table$age <- age

          for (l in 1:nrow(norm_table)) {
            pat_age <- as.numeric(norm_table[l, 2])
            target.index <- which(abs(norm_dat[, 1] - pat_age) == min(abs(norm_dat[, 1] - pat_age)))
            if (length(target.index) > 1) {
              target.index <- target.index[1]
            }
            norm_table[l, 1] <- norm_dat[target.index, j + 1]
          }

          # Here is vere the normalization with each base values happens
          mymeans       <-
            (dat[dat$id == n[i], vars[j]] / norm_table[ , 1]) * 100

          norm          <- rbind(norm, mymeans)

          # check if there are multiple entries per id
          # (e.g., multiple measurements per time)
          n_occur       <- NULL
          n_occur       <-
            data.frame(table(dat[dat$id %in% n[i], "time"]))
          if (sum(n_occur$Freq) > length(n_occur[,1])) {
            warning(
              "There are multiple daily entries per id.
                      Normalization probably incorrect."
            )
          }

      }

      dat[, which(names(dat) == vars[j])] <- norm
    }


# Baseline ----------------------------------------------------------------

      maxsev <- apply(dat[, vars], 2, min, na.rm = TRUE)

    if (length(turnvars) > 0) {
      maxsev[turnvars] <- apply(dat[, turnvars], 2, max, na.rm = TRUE)
    }

    # Fetch maximum delta between 100% and highest severity
    maxdelta            <- abs(100 - maxsev)

    # Model characteristics
    ristics           <-
      data.frame(
        n          = length(unique(dat$id)),
        treatments = length(unique(dat$treatment)),
        conditions = length(unique(dat$condition)),
        variables  = dim(dat[, vars])[2]
      )

    # Giving the baseline table, max severity, max delta and
    # the characteristics out
    # For just z variables without baseline table
      ret <-
        list(
          pre = dat[ , -(which(names(dat) %in% c("age", "timepoint")))],
          maxsev = maxsev,
          maxdelta = maxdelta,
          ristics = ristics
        )

    cat(
      "The baseline has been calculated with",
      length(vars),
      "variables.",
      length(turnvars),
      "of them are turned variables."
    )

  return(ret)
}
