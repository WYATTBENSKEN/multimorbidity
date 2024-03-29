% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/comorbidity-window.R
\name{comorbidity_window}
\alias{comorbidity_window}
\title{Limit our comorbidities / multimorbidity measures to a specific time window.}
\usage{
comorbidity_window(
  dat = NULL,
  id_dat = NULL,
  id = NULL,
  id_date = NULL,
  claims_date = NULL,
  time_pre = Inf,
  time_post = Inf
)
}
\arguments{
\item{dat}{dataset}

\item{id_dat}{dataset with our other identifying variables, this should be 1 row per person}

\item{id}{ID variable which will be used to match and merge}

\item{id_date}{name of the date of interest from the identification dataset, for example a date of diagnosis}

\item{claims_date}{name for the variable in the claims data (dat) which is the date of the claim}

\item{time_pre}{number to limit how many days, pre diagnosis, should be included. Default will be
infinity (all claims)}

\item{time_post}{similar to time_pre, but this will be after the date of interest}
}
\value{
dataframe with which has limited the claims to a specific window
}
\description{
\code{comorbidity_window} returns a dataset of claims which fall within a specific timeframe.
}
\details{
This function takes prepared data, using the 'prepare_data' function, along with an identification dataset to limit
the claims of interest to a specific time window.
}
\examples{
comorbidity_window(id_dat = id, dat = prepared_data, id = patient_id,
id_date = date_of_interest9, claims_date = claim_date, time_pre = 60)


}
