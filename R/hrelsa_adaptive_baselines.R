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
           realtime = NULL,

           dob_dat = NULL,
           dob_data_id_col = NULL,
           ddob_data_dob_col = NULL,

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
    if (length(vars) != length(norm_dat_names)) {
      warning(
        "The column names of the normal values need to have the same length.
        The baseline was not calculated."
      )
      abort <- TRUE
    }

    # Function code -----------------------------------------------------------

    dat <- as_tibble(dat)
    n   <- unique(dat$id)
    dat <- arrange(dat, id, time)

    col_time <- which(names(dat) == realtime)
    names(dat)[col_time] <- "timepoint"

    names(dob_dat)[dob_data_id_col] <- "id"
    names(dob_dat)[dob_data_dob_col] <- "dob"

    dat$age <- 0

    for (l in 1:nrow(dat)) {
      timepoint <- dat$timepoint[l]
      dob <- dob_dat$dob[dob_dat$id == as.character(dat$id[l])]

      dat$age[l] <- difftime(timepoint, dob, units = "days") / 365.25
    }


    for (j in 1:length(vars)) {
      norm            <- NULL
      for (i in 1:length(n)) {

          # Here happens the normal variable normalization
          mydayone      <- set[set$id == n[i], normthese[j]]
          mydayone[,1]  <-
            set[set$id == n[i] &
                  set$time == ontime, normthese[j]]
          mymeans       <-
            (set[set$id %in% n[i], normthese[j]] / mydayone) * 100

          mytime         <- set[set$id == n[i], "time"]
          mytier        <- set[set$id == n[i], "id"]
          norm          <-
            rbind(norm,
                  data.frame(
                    id = mytier,
                    time = mytime,
                    act = mymeans
                  ))

          # check if there are multiple entries per id
          # (e.g., multiple measurements per time)
          n_occur       <- NULL
          n_occur       <-
            data.frame(table(set[set$id %in% n[i], "time"]))
          if (sum(n_occur$Freq) > length(n_occur[,1])) {
            warning(
              "There are multiple daily entries per id.
                      Normalization probably incorrect."
            )
          }

      }

      set[, which(names(set) == normthese[j])] <- norm[, 3]
    }


  return(raw)
}
