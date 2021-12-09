#' Prepare our claims data for analysis
#'
#' \code{prepare_data} returns a dataset which has been transformed and prepared for subsequent functions in this
#'     package.
#'
#' This function takes our raw claims data, in a number of different forms, and prepares it in a way which allows the
#'    other functions in this package to easily work with it. It is recommended to run this package on all data
#'    regardless of setup.
#'
#' @param dat dataset
#' @param style long, the default, is one diagnosis column per row whereas wide is multiple diagnosis columns
#' @param id unique patient identifier variable name
#' @param prefix_dx the variable prefix for the diagnosis columns (defaults to "dx"), in quotes
#' @param hcpcs whether or not HCPCS variables are included ("yes" or "no", where "no" is the default)
#' @param prefix_hcpcs if HCPCS are included, the variable prefix in quotes
#' @param version_var  variable which denotes if the diagnoses on that row are ICD-9 (9) or ICD-10 (10)
#' @param type_name variable to denote if the claim is inpatient (ip) or outpatient (ot)
#' @param date variable with the date of the claim
#'
#' @return dataframe with multiple rows per patient, which has re-structured their claims
#'
#' @examples
#' prepare_data(dat = i9_i10_comb, id = patient_id, style = "wide",
#' prefix_dx = "dx", hcpcs = "yes", prefix_hcpcs = "hcpcs", version_var = icd_version,
#' type_name = visit_type, date = date_of_serv)
#'
#' @export


#' @importFrom rlang .data
prepare_data <- function(dat = NULL,
                         style = "long",
                         id = NULL,
                         prefix_dx = "dx",
                         hcpcs = "no",
                         prefix_hcpcs,
                         version_var,
                         type_name,
                         date) {

  date2 <- rlang::quo_name(rlang::enquo(date))
  version2 <- rlang::quo_name(rlang::enquo(version_var))
  type2 <- rlang::quo_name(rlang::enquo(type_name))
  id2 <- rlang::quo_name(rlang::enquo(id))

  if (style == "wide" | style == "Wide") {
    dat_dx <- tidyr::pivot_longer(dat, dplyr::starts_with(prefix_dx), values_to = "dx") # here we reshape our diagnoses
    var1 <- c(id2, date2, "dx", version2, type2)
    dat_dx <- dat_dx[tidyselect::all_of(var1)]
    dat_dx <- dplyr::rename(dat_dx, "claim_date" = date2)
    dat_dx <- dplyr::rename(dat_dx, "version" = version2)
    dat_dx <- dplyr::rename(dat_dx, "type" = type2)

    if (hcpcs == "yes" | hcpcs == "Yes"){
      dat_hcpcs <- tidyr::pivot_longer(dat, dplyr::starts_with(prefix_hcpcs), values_to = "dx")
      var2 <- c(id2, date2, "dx", type2)
      dat_hcpcs <- dat_hcpcs[tidyselect::all_of(var2)]
      dat_hcpcs <- dplyr::rename(dat_hcpcs, "claim_date" = date2)
      dat_hcpcs <- dplyr::rename(dat_hcpcs, "type" = type2)
      dat_hcpcs <- dplyr::mutate(dat_hcpcs, version = 1)

      dat2 <- dplyr::bind_rows(dat_dx, dat_hcpcs) }# merging Dx and HCPCS/CPT datasets

    else {
      dat2 <- dat_dx}
  }

  else { # if the data is already long we change the variable names for later steps
    dat2 <- dplyr::rename(dat, "dx" = prefix_dx)
    dat2 <- dplyr::rename(dat2, "claim_date" = date2)
    dat2 <- dplyr::rename(dat2, "version" = version2)
    dat2 <- dplyr::rename(dat2, "type" = type2)
    var3 <- c(id2, "claim_date", "dx", "version", "type")
    dat2 <- dat2[tidyselect::all_of(var3)]
  }

  check_dx(dat2$dx)
  check_date(dat2$claim_date)

  dat2 <- dplyr::filter(dat2, !is.na(.data$dx)) # removes any missing diagnosis rows
  dat2 <- dplyr::filter(dat2, .data$dx != "") # removes empty string rows

  return(dat2)

}
