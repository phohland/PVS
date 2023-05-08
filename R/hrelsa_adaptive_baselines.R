#' Defines baselines and maximum severity values for hRELSA calculation using
#' the adaptive reference method
#' Here is the normalization included as well
#'
#' The \code{hrelsa_adaptive_baselines} function normalizes the data,
#' calculates baselines, maximum severity, maximum delta and some characteristics
#'
#' @param dat formatted data set
#' @param vars variables to work with
#' @param turnvars turned variables
#' @param ambivars ambivalent variables
#' @param realtime the real time point for age calculcation, the real time needs to be included in dat
#' @param dob_dat data with date of birth in it
#' @param dob_data_id_col column id of id in dob data set
#' @param dob_data_dob_col column id of date of birth in dob data set
#' @param norm_dat data set with normal / reference values
#' @param norm_dat_names names of the norm dat data set
#'
#' @return \code{ret} list with reference variables set to 100 %,
#' maximum reached values and some characteristics
#'
#' @export
#'

hrelsa_adaptive_baselines <-
  function(dat,
           reference_dat,
           vars = NULL,
           turnvars = NULL,
           ambivars = NULL,
           realtime = NULL,

           dob_dat = NULL,
           dob_data_id_col = NULL,
           dob_data_dob_col = NULL,

           norm_dat = NULL,
           norm_dat_names = NULL
  ) {
    # Searching for errors ----------------------------------------------------

    abort <- FALSE
    if (is.null(dat) || is.null(reference_dat)) {
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

    pre <- as_tibble(dat)
    n   <- unique(pre$id)
    pre <- arrange(pre, id, time)

    col_time <- which(names(pre) == realtime)
    names(pre)[col_time] <- "timepoint"

    names(dob_dat)[dob_data_id_col] <- "id"
    names(dob_dat)[dob_data_dob_col] <- "dob"

    pre$age <- 0

    # Calculates the age regarding the dob of the extern table and the current time point
    for (l in 1:nrow(pre)) {
      timepoint <- pre$timepoint[l]
      dob <- dob_dat$dob[dob_dat$id == as.character(pre$id[l])]

      pre$age[l] <- difftime(timepoint, dob, units = "days") / 365.25
    }


    for (j in 1:length(vars)) {
      norm            <- NULL
      for (i in 1:length(n)) {

          norm_table      <- pre[pre$id == n[i], vars[j]]
          age <- pre$age[pre$id == n[i]]
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
            (pre[pre$id == n[i], vars[j]] / norm_table[ , 1]) * 100

          # ambivar normalization
          if (vars[j] %in% ambivars) {
            for (b in 1:nrow(mymeans)) {
              if (!(is.na(mymeans[b, 1]))) {
                if (mymeans[b, 1] < 100) {
                  mymeans[b, 1] <- 100 + (100 - mymeans[b, 1])
                }
              }
            }
          }

          norm          <- rbind(norm, mymeans)

          # check if there are multiple entries per id
          # (e.g., multiple measurements per time)
          n_occur       <- NULL
          n_occur       <-
            data.frame(table(pre[pre$id %in% n[i], "time"]))
          if (sum(n_occur$Freq) > length(n_occur[,1])) {
            warning(
              "There are multiple daily entries per id.
                      Normalization probably incorrect."
            )
          }

      }

      pre[, which(names(pre) == vars[j])] <- norm
    }


# Baseline ----------------------------------------------------------------

    ref_pre <- pre[pre$id %in% as.character(reference_dat$id), ]

    maxsev <- apply(ref_pre[, vars], 2, min, na.rm = TRUE)

    if (length(turnvars) > 0) {
      maxsev[turnvars] <- apply(ref_pre[, turnvars], 2, max, na.rm = TRUE)
    }

    # Ambivar implementation
    if (length(ambivars) > 0) {
      maxsev[ambivars] <- apply(ref_pre[, ambivars], 2, max, na.rm = TRUE)
    }

    # Fetch maximum delta between 100% and highest severity
    maxdelta            <- abs(100 - maxsev)

    # Model characteristics
    ristics           <-
      data.frame(
        n          = length(unique(ref_pre$id)),
        treatments = length(unique(ref_pre$treatment)),
        conditions = length(unique(ref_pre$condition)),
        variables  = dim(ref_pre[, vars])[2]
      )

    # Giving the baseline table, max severity, max delta and
    # the characteristics out
    # For just z variables without baseline table
      ret <-
        list(
          pre = pre[ , -(which(names(pre) %in% c("age", "timepoint")))],
          maxsev = maxsev,
          maxdelta = maxdelta,
          ristics = ristics
        )

    cat(
      "The baseline has been calculated with",
      length(vars),
      "variables.",
      length(turnvars),
      "of them are turned variables and",
      length(ambivars),
      "of them are ambivalent variables."
    )

  return(ret)
}
