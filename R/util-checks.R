check_dx <- function(dx){
  if (is.character(dx) != TRUE) {
    warning("Your diagnoses were not character variable(s) and have been converted to such. To avoid this warning, please convert diagnoses to characters before this step.")
    dx <- as.character(dx)
  }
}


check_date <- function(date){
  if (lubridate::is.Date(date) == FALSE) {
    stop("Your date is not a date variable, and needs to be converted as such")
  }
}
