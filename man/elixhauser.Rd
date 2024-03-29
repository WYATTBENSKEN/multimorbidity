% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/elixhauser.R
\name{elixhauser}
\alias{elixhauser}
\title{Elixhauser Comorbidities}
\usage{
elixhauser(
  dat = NULL,
  id = NULL,
  dx = "dx",
  version = 19,
  version_var = NULL,
  outpatient_two = "no"
)
}
\arguments{
\item{dat}{dataset which has been properly prepared using 'prepare_data()'}

\item{id}{variable of the unique patient identifier}

\item{dx}{the column with the diagnoses (defaults to 'dx')}

\item{version}{which version(s) of ICD your data contain (ICD-9 only: 9, ICD-10 only: 10,
Both: 19)}

\item{version_var}{variable which denotes if the diagnoses on that row are ICD-9 (9) or
ICD-10 (10)}

\item{outpatient_two}{whether or not it should be required for there to be two outpatient
claims for a diagnosis for a patient to be positively coded with that diagnosis.}
}
\value{
dataframe with one row per patient, and a column for their patient id, a column with each
Elixhauser comorbidity, and a column with their Elixhauser index for readmission and death
}
\description{
\code{elixhauser} returns a summary dataset containing the Elixhauser comorbidities for
each patient.
}
\details{
This function uses data which has been properly prepared to identify and flag the
Elixhauser comorbidities. See full package documentation for additional details.
}
\examples{
elixhauser(dat = prepared_data, id = patient_id, dx = dx, version = 19,
version_var = version, outpatient_two = "yes")


}
