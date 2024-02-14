#' PVS
#'
#' The \code{pvs_adaptive_baselines_sex} function normalizes the data,
#' calculates baselines, maximum severity, maximum delta and some characteristics
#' for data using the adaptive reference method
#'
#' @param dat data set
#' @param reference_dat reference data set
#' @param max_sev if no reference data set is given, set max severity percentage
#' @param vars variables
#' @param turnvars turned variables
#' @param ambivars ambivalent variables
#' @param sex_dat data with date of birth in it
#' @param sex_data_id_col column id of id in sex data set
#' @param sex_data_sex_col column id of sex in sex data set
#' @param norm_dat data set with normal / reference values
#' @param norm_dat_names names of the norm dat data set
#'
#' @return \code{ret} list with reference variables set to 100 %,
#' maximum reached values and some characteristics
#'
#' @export
#'

pvs_adaptive_baselines_range <-
  function(dat,
           reference_dat = NULL,
           max_sev = NULL,
           vars = NULL,
           turnvars = NULL,
           ambivars = NULL,

           sex_dat = NULL,
           sex_data_id_col = NULL,
           sex_data_sex_col = NULL,

           norm_dat = NULL
  ) {
    # Searching for errors ----------------------------------------------------

    abort <- FALSE
    if (is.null(dat)) {
      warning("There was no data set found. The baseline was not calculated.")
      abort <- TRUE
    }
    if (is.null(reference_dat) & is.null(max_sev)) {
      warning("There was neither a reference data set nor a maximum severity percentage found.
              The baseline was not calculated.")
      abort <- TRUE
    }
    if (is.null(vars)) {
      warning("There was no variable for the baseline calculation.
              The baseline was not calculated.")
      abort <- TRUE
    }
    if ((is.null(sex_dat) || is.null(norm_dat))) {
      warning(
        "When the adaptive baselines are wished, there are data regarding
        the sexes and the norm values needed.
        The baseline was not calculated."
      )
      abort <- TRUE
    }

    # Normalization -----------------------------------------------------------

    pre <- as_tibble(dat)
    pre <- arrange(pre, id, time)
    n   <- unique(pre$id)

    names(sex_dat)[sex_data_id_col] <- "id"
    names(sex_dat)[sex_data_sex_col] <- "sex"

    # Fetch sex of every patient
    pre$sex <- 0
    for (l in 1:nrow(pre)) {
      sex <- sex_dat$sex[sex_dat$id == as.character(pre$id[l])]

      pre$sex[l] <- sex
    }


    for (j in 1:length(vars)) {
      norm            <- NULL
      for (i in 1:length(n)) {

          norm_table      <- pre[pre$id == n[i], vars[j]]
          sex <- pre$sex[pre$id == n[i]]
          norm_table$sex <- NA
          norm_table$sex <- sex

          for (l in 1:nrow(norm_table)) {
            pat_sex <- as.numeric(norm_table[l, 2])
            target.index <- which(abs(norm_dat[, 1] - pat_sex) == min(abs(norm_dat[, 1] - pat_sex)))
            if (length(target.index) > 1) {
              target.index <- target.index[1]
            }

            if (!is.na(norm_table[l, 1])) {
              if ((norm_table[l, 1] < norm_dat[target.index, j + 2])) { # Lower as LOWER
                norm_table[l, 1] <- norm_dat[target.index, j + 2]
              } else if ((norm_table[l, 1] > norm_dat[target.index + 1, j + 2])) { # Higher as UPPER
                norm_table[l, 1] <- norm_dat[target.index + 1, j + 2]
              }
            }

          }

          # Here is where the normalization with each base values happens
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

    if (!is.null(reference_dat)) {

      ref_pre <- pre[pre$id %in% as.character(reference_dat$id), ]

      maxsev <- apply(ref_pre[, vars], 2, min, na.rm = TRUE)

      if (length(turnvars) > 0) {
        maxsev[turnvars] <- apply(ref_pre[, turnvars], 2, max, na.rm = TRUE)
      }

      # Ambivar implementation
      if (length(ambivars) > 0) {
        maxsev[ambivars] <- apply(ref_pre[, ambivars], 2, max, na.rm = TRUE)
      }

    } else if (!is.null(max_sev)) {

      maxsev <- pre[1,vars]
      maxsev[1,] <- max_sev

    }

    # Fetch maximum delta between 100% and highest severity
    maxdelta            <- abs(100 - maxsev)

    # Model characteristics
    ristics           <-
      data.frame(
        n          = length(unique(ref_pre$id)),
        variables  = dim(ref_pre[, vars])[2]
      )

    # Giving the baseline table, max severity, max delta and
    # the characteristics out
    # For just z variables without baseline table
      ret <-
        list(
          pre = pre[ , -(which(names(pre) %in% c("sex")))],
          maxsev = maxsev,
          maxdelta = maxdelta,
          ristics = ristics,
          sex_pre = pre
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
