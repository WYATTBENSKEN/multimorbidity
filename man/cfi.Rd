% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cfi.R
\name{cfi}
\alias{cfi}
\title{Claims-based Frailty Index (CFI)}
\usage{
cfi(
  dat = NULL,
  id = NULL,
  dx = "dx",
  version = 19,
  version_var = NULL,
  hcpcs = "yes"
)
}
\arguments{
\item{dat}{dataset which has been properly prepared using 'prepare_data()'}

\item{id}{variable of the unique patient identifier}

\item{dx}{the column with the diagnoses and procedures (defaults to 'dx')}

\item{version}{which version(s) of ICD your data contain (ICD-9 only: 9, ICD-10 only: 10,
Both: 19)}

\item{version_var}{variable which denotes if the diagnoses on that row are ICD-9 (9) or
ICD-10 (10)}

\item{hcpcs}{whether or not HCPCS variables are included ("yes" or "no", where "yes" is the default)}
}
\value{
dataframe with one row per patient, and a column for their patient id and a column with their frailty index.
}
\description{
\code{cfi} returns a summary dataset containing the deficit-accumulation frailty index
for each patient.
}
\details{
This function uses data which has been properly prepared to calculate the claims-based frailty index (CFI) developed by Kim et al. for each patient.
As this algorithm was never developed to require two diagnosis codes, and is weighted, we have excluded that feature from this function.
See full package documentation for additional details. This function is based largely on the code available via the \href{https://dataverse.harvard.edu/dataverse/cfi}{Harvard Dataverse}.
}
\examples{
cfi(dat = prepared_data, id = patient_id, dx = dx, version = 19, version_var = version)


}
