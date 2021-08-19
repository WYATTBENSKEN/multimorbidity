#' Claims-based Frailty Index (CFI)
#'
#' \code{cfi} returns a summary dataset containing the deficit-accumulation frailty index
#'     for each patient.
#'
#' This function uses data which has been properly prepared to calculate the claims-based frailty index (CFI) developed by Kim et al. for each patient.
#'     As this algorithm was never developed to require two diagnosis codes, and is weighted, we have excluded that feature from this function.
#'     See full package documentation for additional details. This function is based largely on the code available via the [Harvard Dataverse](https://dataverse.harvard.edu/dataverse/cfi).
#'
#' @return dataframe with one row per patient, and a column for their patient id and a column with their frailty index.
#'
#' @param dat dataset which has been properly prepared using 'prepare_data()'
#' @param id variable of the unique patient identifier
#' @param dx the column with the diagnoses and procedures (defaults to 'dx')
#' @param version which version(s) of ICD your data contain (ICD-9 only: 9, ICD-10 only: 10,
#'     Both: 19)
#' @param version_var variable which denotes if the diagnoses on that row are ICD-9 (9) or
#'     ICD-10 (10)
#' @param hcpcs whether or not HCPCS variables are included ("yes" or "no", where "yes" is the default)
#'
#' @examples
#' cfi(dat = prepared_data, id = patient_id, dx = dx, version = 19, version_var = version)
#'
#'
#' @export

#' @importFrom rlang .data
cfi <- function(dat = NULL,
                id = NULL,
                dx = "dx",
                version = 19,
                version_var = NULL,
                hcpcs = "yes"){


  id2 <- rlang::quo_name(rlang::enquo(id))
  ModelIntercept = 0.10288

  # Readin Data ----

  dx9lookup <- multimorbidity::cfi_dx9lookup
  dx10lookup <- multimorbidity::cfi_dx10lookup
  pxlookup <- multimorbidity::cfi_pxlookup
  weightlookup <- multimorbidity::cfi_weightlookup

  # Deduplicate files ----
    # As each diagnosis is only used once, we can start by de-duplicating the diagnoses

  dat1 <- dat %>%
    dplyr::select({{id}}, {{dx}}, {{version_var}})

  dat1 <- unique(dat1)

  # CFI ICD-9 ----

  if (version == 9){

    dat1_9 <- dat1 %>%
      dplyr::filter({{version_var}} == 9)

    dat1_9 <- dat1_9 %>%
      dplyr::mutate(dx = as.numeric(dx))

    dat_dx9 <- sqldf::sqldf("select A.*, B.disease_number from
                 dat1_9 A left join dx9lookup B
                 ON (A.dx >= B.start and A.dx < B.stop)")
    dat_dx9[is.na(dat_dx9)] <- 0
    dat_dx9[is.na(dat_dx9)] <- 0

  }

  # CFI - ICD-10 ----

  else if (version == 10){

    dat1_10 <- dat1 %>%
      dplyr::filter({{version_var}} == 10)

    dat_dx10 <- merge(dat1_10, dx10lookup, all.x=TRUE)
    dat_dx10[is.na(dat_dx10)] <- 0

  }


  # CFI - ICD-9 and ICD-10 ----

  else if (version == 19){
    dat1_9 <- dat1 %>%
      dplyr::filter({{version_var}} == 9)
    dat1_10 <- dat1 %>%
      dplyr::filter({{version_var}} == 10)

    dat1_9 <- dat1_9 %>%
      dplyr::mutate(dx = as.numeric(dx))

    dat_dx9 <- sqldf::sqldf("select A.*, B.disease_number from
                 dat1_9 A left join dx9lookup B
                 ON (A.dx >= B.start and A.dx < B.stop)")
    dat_dx9[is.na(dat_dx9)] <- 0

    dat_dx10 <- unique(dat1_10)
    dat_dx10 <- merge(dat_dx10, dx10lookup, all.x=TRUE)
    dat_dx10[is.na(dat_dx10)] <- 0

  }


  # CFI - Procedure Codes ----

  if (hcpcs == "yes"){

  dat1_px <- dat1 %>%
    dplyr::filter({{version_var}} == 1)

  dat1_px <- unique(dat1_px)

  dat_px <- sqldf::sqldf("select A.*, B.disease_number from
                 dat1_px A left join pxlookup B
                 ON (A.dx >= B.start and A.dx <= B.stop)")

  dat_px[is.na(dat_px)] <- 0
  # If a PX value isn't 5 characters or if last character
  # isn't a number, PX should not be scored. Set to 0.
  dat_px <- within(dat_px, disease_number[nchar(dx) != 5 | grepl("[0-9]", substr(dx, nchar(dx), nchar(dx))) == FALSE] <- 0)

  }

  # Assign dummy disease_number = 0 for all study IDs. This will have the effect of assigning the ----
  # default weight (ModelIntercept) for any PatID that is not included in the DX9, DX10 or PX file

  iddata <- dat1 %>%
    dplyr::select(id2)

  iddata <- unique(iddata)
  iddata['disease_number'] = 0

  # Remove duplicates. Each DX/PX should only be weighted once. ----
  if (version == 9 & hcpcs == "yes"){

    # Combine the data, keeping only patient ID and disease number
    diseasedata <- data.frame()
    base_names <- names(iddata)
    list_df <- list(dat_dx9, dat_px, iddata)
    for(item in list_df)
    {
      items <- item[, base_names]
      diseasedata <- rbind(diseasedata, items)
    }

  }

  else if (version == 9 & hcpcs == "no"){
    # Combine the data, keeping only patient ID and disease number
    diseasedata <- data.frame()
    base_names <- names(iddata)
    list_df <- list(dat_dx9, iddata)
    for(item in list_df)
    {
      items <- item[, base_names]
      diseasedata <- rbind(diseasedata, items)
    }

  }

  else if (version == 10 & hcpcs == "yes"){

    # Combine the data, keeping only patient ID and disease number
    diseasedata <- data.frame()
    base_names <- names(iddata)
    list_df <- list(dat_dx10, dat_px, iddata)
    for(item in list_df)
    {
      items <- item[, base_names]
      diseasedata <- rbind(diseasedata, items)
    }

  }

  else if (version == 10 & hcpcs == "no"){
    # Combine the data, keeping only patient ID and disease number
    diseasedata <- data.frame()
    base_names <- names(iddata)
    list_df <- list(dat_dx10, iddata)
    for(item in list_df)
    {
      items <- item[, base_names]
      diseasedata <- rbind(diseasedata, items)
    }

  }

  else if (version == 19 & hcpcs == "yes"){

    # Combine the data, keeping only patient ID and disease number
    diseasedata <- data.frame()
    base_names <- names(iddata)
    list_df <- list(dat_dx9, dat_dx10, dat_px, iddata)
    for(item in list_df)
    {
      items <- item[, base_names]
      diseasedata <- rbind(diseasedata, items)
    }

  }

  else if (version == 19 & hcpcs == "no"){
    # Combine the data, keeping only patient ID and disease number
    diseasedata <- data.frame()
    base_names <- names(iddata)
    list_df <- list(dat_dx9, dat_dx10, iddata)
    for(item in list_df)
    {
      items <- item[, base_names]
      diseasedata <- rbind(diseasedata, items)
    }

  }

  diseasedata <- unique(diseasedata)

  diseasedatasort <- diseasedata

  # Assign weights ----
  # Merge the disease weights on to the disease data and fill non-matches (NA) with 0
  diseasedatasort <- merge(diseasedatasort, weightlookup, all.x=TRUE)
  diseasedatasort[is.na(diseasedatasort)] <- 0

  # Calculate frailty scores by summing the weights of records grouped by patient ID. ----
  # ModelIntercept value added to every score. Default score for those with no DX/PX.

  scores <- diseasedatasort %>%
    dplyr::group_by({{id}}) %>%
    dplyr::summarize(x = sum(.data$weight)) %>%
    dplyr::ungroup()

  scores$x <- scores$x + ModelIntercept
  colnames(scores) <- c(id2, 'frailty_index')

  scores <- dplyr::rename(scores, "id" = id2)

  return(scores)

}
