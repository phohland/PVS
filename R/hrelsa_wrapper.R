#' hRELSA Wrapper
#'
#' The \code{hrelsa_wrapper} 
#'
#' @param x 
#'
#' @return \code{robj}
#'
#' @import dplyr
#' @export
#'

## normthese = all variables

relsa_wrapper <- function(
    # input file
  data_path = NULL,
  data_seperation = NULL,
  data_decimal = NULL,
  data_sheet = NULL,
  
    # data formation
  id = NULL,
  treatment = NULL,
  condition = NULL,
  day = NULL,
  form_to_day = NULL,
  new_day_one = NULL,
  vars = NULL,
  turnvars = NULL,
  zvars = NULL,
  dropvars = NULL,
  ){
  
  # Searching for errors ----------------------------------------------------
  
  abort <- FALSE
  data_ending <- file_ext(data_path)
  just_one_condition <- FALSE
  if (is.null(treatment) || is.null(condition)) {
    just_one_condition <- TRUE
  }
  
  # Warnings leading to abort
  if (is.null(data_path)) {
    warning("There was no data path found. Please insert your data path including
            the file ending in the following way: data_path = 'example_file.csv'.
            The hRELSA was not calculated.")
    abort <- TRUE
  }
  if (!(file.exists(data_path))) {
    warning("The inserted file was not found. Please insert your data path in the
            following way: data_path = 'example_path'.
            The hRELSA was not calculated.")
    abort <- TRUE
  }
  if (!(data_ending %in% c("csv", "xls", "xlsx"))) {
    warning("The inserted file has no csv-, xls- or xlsx-ending. A file with one
            of those endings is necessairy.
            The hRELSA was not calculated.")
    abort <- TRUE
  }
  if (is.null(id)) {
    warning("There was no id column name found. Please insert your id column name
            in the following way: id = 'id'.
            The hRELSA was not calculated.")
    abort <- TRUE
  }
  if (is.null(treatment) && is.null(condition)) {
    warning("There was neither a treatment nor a condition column name found.
            Please insert at least one of them in the following way:
            treatment = 'treatment'
            or condition = 'condition'.
            The hRELSA was not calculated.")
    abort <- TRUE
  }
  if (is.null(day) && is.null(form_to_day)) {
    warning("There was neither a day nor a column to form to a day column found.
            Please insert at least one of them in the following way:
            day = 'day'
            or form_to_day = 'exam_date'.
            The hRELSA was not calculated.")
    abort <- TRUE
  }
  if (is.null(vars)) {
    warning("There were no variable column names found. Please insert your variable column names
            in the following way: vars = c('var1','var2').
            The hRELSA was not calculated.")
    abort <- TRUE
  }
  
  
  # Warnings leading to no abort
  if (is.null(data_seperation)) {
    data_seperation <- ";"
    cat("While processing the inserted csv file the assumption that the data is
        seperated with a ';' has been made.
        If the file is not seperated that way, please use: data_seperation = ''.")
  }
  if (is.null(data_decimal)) {
    data_decimal <- "."
    cat("While processing the inserted csv file the assumption that the data 
        decimal seperation is '.' has been made.
        If the decimal seperation is not that way, please use: data_decimal = '.'")
  }
  if (is.null(data_sheet)) {
    data_sheet <- 1
    cat("While processing the inserted excel file the assumption that the data 
        is in sheet 1 has been made.
        If the data is in another sheet, please use e.g.: data_sheet = 2")
  }
  if (!(is.null(day)) && !(is.null(form_to_day))) {
    warning("You can't form a day column if you already have one. Your own day
            column has been used.")
    form_to_day <- NULL
    new_day_one <- NULL
  }
  if (is.null(new_day_one) && !(is.null(form_to_day))) {
    new_day_one <- TRUE
    cat("While processing the inserted time column the assumption that each
        patient should get a new day one at his first time point has been made.
        If only one day one at the earliest time point of the data set is wished, 
        please use: new_day_one = FALSE")
  }
  
  
  # Function code -----------------------------------------------------------
  
  if (abort) {
    
  } else {
    
  if (data_ending == "csv") {
    raw <- read.csv(data_path, sep = data_seperation, header = TRUE, dec = data_decimal) 
  } else if (data_ending == "xlsx" || data_ending == "xls") {
    raw <- read_excel(data_path, sheet = data_sheet) 
  }
  
  raw <- as_tibble(raw)
  raw <- raw %>% clean_names
  
  if (is.null(day)) {
    raw <- hrelsa_days(raw, format = "day", formthis = form_to_day, newdayone = new_day_one)
  }
  
  }
}
