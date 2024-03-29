#' PVS
#'
#' The \code{pvs_norm} function normalizes defined variables to 100 %
#' at a given time point
#'
#' when z variables are involved they have a special way of being normalized
#' z_norm = (|z| + 1) * 100
#' when normalizing z variables the reference time point does not matter
#' baseline for z variables is 0 (0 <- 100 %)
#'
#' @param set data set
#' @param normthese variables in the data set that shall be normalized
#' @param zvars z variables (need to be contained in normthese as well)
#' @param ambivars ambivalent variables
#' @param ontime unique reference time point in data set (e.g. 1 for day 1)
#'
#' @return \code{set} data set with normalized values
#'
#' @export
#'

pvs_norm <-
  function(set,
           normthese = NULL,
           zvars = NULL,
           ambivars = NULL,
           ontime = NULL) {
    # Searching for errors ----------------------------------------------------

    abort <- FALSE
    if (is.null(set)) {
      warning("There was no data set found. The data set was not normalized.")
      abort <- TRUE
    }
    if (is.null(normthese)) {
      warning("There was no variable to normalize.
              The data set was not normalized.")
      abort <- TRUE
    }
    if (is.null(ontime) && (FALSE %in% (zvars == normthese))) {
      warning("There was no reference time point (ontime) found.
              The data set was not normalized.")
      abort <- TRUE
    }

    # Function code -----------------------------------------------------------

    if (abort) {

    } else {
      set <- as_tibble(set)
      n   <- unique(set$id)
      set <- arrange(set, id, time)

      for (j in 1:length(normthese)) {
        norm            <- NULL
        for (i in 1:length(n)) {
          if (normthese[j] %in% zvars) {
            # Here the z variable normalization happens
            # z_norm = (|z| + 1) * 100
            mymeans       <-
              (abs(set[set$id %in% n[i], normthese[j]]) + 1) * 100

            mytime         <- set[set$id == n[i], "time"]
            mytier        <- set[set$id == n[i], "id"]
            norm          <-
              rbind(norm,
                    data.frame(
                      id = mytier,
                      time = mytime,
                      act = mymeans
                    ))

          } else {
            # Here the normal variable normalization happens
            mydayone      <- set[set$id == n[i], normthese[j]]
            mydayone[,1]  <-
              set[set$id == n[i] &
                    set$time == ontime, normthese[j]]
            mymeans       <-
              (set[set$id %in% n[i], normthese[j]] / mydayone) * 100

            # ambivar normalization
            if (normthese[j] %in% ambivars) {
              for (b in 1:nrow(mymeans)) {
                if (!(is.na(mymeans[b, 1]))) {
                  if (mymeans[b, 1] < 100) {
                    mymeans[b, 1] <- 100 + (100 - mymeans[b, 1])
                  }
                }
              }
            }

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

        }

        set[, which(names(set) == normthese[j])] <- norm[, 3]
      }

    }

    return(set)
  }
