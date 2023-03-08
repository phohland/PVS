#' Evaluates a analysis of the hRELSAed data set
#'
#' The \code{hrelsa_analysis} evaluates relevant informations about the patients
#' with the highest severity
#'
#' @param final hRELSAed data set (by hrelsa_final)
#'
#' @return \code{analysis} Outputs some basic data set informations, and the
#' analysis of wfactor, wf and rms
#'
#' @export
#'

hrelsa_analysis <- function(set) {
  # Searching for errors ----------------------------------------------------

  abort <- FALSE
  if (is.null(set)) {
    warning("There was no data set found. The baseline was not calculated.")
    abort <- TRUE
  }

  # Function code -----------------------------------------------------------

  analysis <- NULL
  if (abort) {

  } else {
    # basic data set informations
    n <- length(unique(set$id))
    treatments <- as.character(unique(set$treatment))
    conditions <- as.character(unique(set$condition))
    variables  <- names(set[, (which(names(set) == "time") + 1):(length(names(set)) - 2)])

    subset <- set[, (which(names(set) == "time") + 1):(length(names(set)) - 2)]

    # wfactor analysis
    max_wfactor <- max(subset, na.rm = TRUE)
    max_wfactor_where <-
      which(subset == max_wfactor, arr.ind = TRUE)
    max_wfactor_rdy <- set[max_wfactor_where[, 1], ]

    # wf analysis
    max_wf <- max(set$wf, na.rm = TRUE)
    max_wf_rdy <- filter(set, set$wf == max_wf)
    max_wf_pat_rdy <- filter(set, set$id == (max_wf_rdy$id[1]))

    # rms analysis
    max_rms <- max(set$rms, na.rm = TRUE)
    max_rms_rdy <- filter(set, set$rms == max_rms)
    max_rms_pat_rdy <- filter(set, set$id == (max_rms_rdy$id[1]))

    wf_rms_same <- (max_wf_rdy$id[1] == max_rms_rdy$id[1])

    # Forming final output
    if (wf_rms_same) {
      analysis <- list(
        amount_of_unique_patients = n,
        treatments = treatments,
        conditions = conditions,
        variables = variables,
        max_severity_of_each_variable = as_tibble(max_wfactor_rdy),
        the_same_patient_has_max_wf_and_rms = wf_rms_same,
        max_severity = as_tibble(max_wf_rdy),
        max_severity_whole_patient = as_tibble(max_wf_pat_rdy)
      )
    } else {
      analysis <- list(
        amount_of_unique_patients = n,
        treatments = treatments,
        conditions = conditions,
        variables = variables,
        max_severity_of_each_variable = as_tibble(max_wfactor_rdy),
        max_severity_wf = as_tibble(max_wf_rdy),
        max_severity_wf_whole_patient = as_tibble(max_wf_pat_rdy),
        max_severity_rms = as_tibble(max_rms_rdy),
        max_severity_rms_whole_patient = as_tibble(max_rms_pat_rdy)
      )
    }

  }

  return(analysis)
}
