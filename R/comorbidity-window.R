#' Limit our comorbidities / multimorbidity measures to a specific time window.
#'
#' \code{comorbidity_window} returns a dataset of claims which fall within a specific timeframe.
#'
#' This function takes prepared data, using the 'prepare_data' function, along with an identification dataset to limit
#' the claims of interest to a specific time window.
#'
#' @param dat dataset
#' @param id_dat  dataset with our other identifying variables, this should be 1 row per person
#' @param id  ID variable which will be used to match and merge
#' @param id_date name of the date of interest from the identification dataset, for example a date of diagnosis
#' @param claims_date name for the variable in the claims data (dat) which is the date of the claim
#' @param time_pre number to limit how many days, pre diagnosis, should be included. Default will be
#'    infinity (all claims)
#' @param time_post similar to time_pre, but this will be after the date of interest
#'
#' @return dataframe with which has limited the claims to a specific window
#'
#' @examples
#' comorbidity_window(id_dat = id, dat = prepared_data, id = patient_id,
#' id_date = date_of_interest9, claims_date = claim_date, time_pre = 60)
#'
#'
#' @export

#' @importFrom rlang .data
comorbidity_window <- function(dat = NULL,
                               id_dat = NULL,
                               id = NULL,
                               id_date = NULL,
                               claims_date = NULL,
                               time_pre = Inf,
                               time_post = Inf) {

  id <- rlang::quo_name(rlang::enquo(id))
  id_date2 <- rlang::quo_name(rlang::enquo(id_date))
  claims_date2 <- rlang::quo_name(rlang::enquo(claims_date))

  if (class(id_dat[[id]]) != class(dat[[id]])) {
    stop("The format of the two ID variables do not match.") # our first check is to make sure the variable types for ID match
  }

  else if(lubridate::is.Date(id_dat[[id_date2]]) == FALSE | lubridate::is.Date(dat[[claims_date2]] == FALSE)) {
    stop("One, or both, of your date variables is not a true date.") # Now we check to make sure our dates are both dates
  }

  else {
    df1 <- dplyr::right_join(dat, id_dat, by = {{id}}) # we join our tables here
    df2 <- dplyr::filter(df1, (as.numeric({{ id_date }} - {{ claims_date }} ) <= time_pre) &
                    (as.numeric( {{ claims_date }} - {{ id_date }} ) <= time_post)) # finally, we limit to our comorbidity window

    return(df2) }

  }
