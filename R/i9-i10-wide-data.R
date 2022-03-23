#' Example diagnosis data.
#'
#' A dataset with fake patient data for 5 patients, with both inpatient and
#' outpatient data, as well as HCPCS codes, and ICD9 and ICD10.
#'
#' @format A data frame with 58 rows and 11 variables:
#' \describe{
#'   \item{patient_id}{patient_id}
#'   \item{sex}{patient's sex (male or female)}
#'   \item{date_of_serv}{the date of service for the fake claim}
#'   \item{visit_type}{inpatient (ip) or outpatient(ot)}
#'   \item{dx1}{first diagnosis}
#'   \item{dx2}{second diagnosis}
#'   \item{dx3}{third diagnosis}
#'   \item{dx4}{fourth diagnosis}
#'   \item{dx5}{fifth diagnosis}
#'   \item{hcpcs}{HCPCS code}
#'   \item{icd_version}{Which version of ICD the row is. 9 = ICD-9, 10 = ICD-10}
#' }
#' @source This was created by the package author.
#'
#' @docType data
#'
#' @usage data(i9_i10_comb)
#'
"i9_i10_comb"
