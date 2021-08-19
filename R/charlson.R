#' Charlson Comorbidities
#'
#' \code{charlson} returns a summary dataset containing the Charlson comorbidities for
#'     each patient.
#'
#' This function uses data which has been properly prepared to identify and flag the
#'     Charlson comorbidities. See full package documentation for additional details.
#'
#' @param dat dataset which has been properly prepared using 'prepare_data()'
#' @param id variable of the unique patient identifier
#' @param dx the column with the diagnoses (defaults to 'dx')
#' @param version which version(s) of ICD your data contain (ICD-9 only: 9, ICD-10 only: 10,
#'     Both: 19)
#' @param version_var variable which denotes if the diagnoses on that row are ICD-9 (9) or
#'     ICD-10 (10)
#' @param outpatient_two whether or not it should be required for there to be two outpatient
#'     claims for a diagnosis for a patient to be positively coded with that diagnosis.
#'
#' @return dataframe with one row per patient, and a column for their patient id, a column
#'     with each Charlson comorbidity, and a column with their Charlson score
#'
#' @examples
#' charlson(dat = prepared_data, id = patient_id, dx = dx, version = 19,
#' version_var = version, outpatient_two = "yes")
#'
#'
#' @export

#' @importFrom rlang .data
charlson <- function(dat = NULL,
                       id = NULL,
                       dx = "dx",
                       version = 19,
                       version_var = NULL,
                       outpatient_two = "no"){


  id2 <- rlang::quo_name(rlang::enquo(id))

  # Charlson ICD-9 ----

  ## Diagnoses Setup ----

  if (version == 9){
    chf9 <- c("39891", "40201", "40211", "40291", "40401", "40403",
              "40411", "40413", "40491", "40493", "4254", "4255", "4257",
              "4258", "4259")
    periph_vasc9 <- c("0930", "4373", "4471", "5571", "5579", "V434",
                      "4431", "4432", "4438", "4439")
    cerebro9 <- c("36234")
    dementia9 <- c("2941", "3312")
    chronic_pulm9 <- c("4168", "4169", "5064", "5081", "5088")
    rheum9 <- c("4465", "7148", "7100", "7101", "7102", "7103", "7104", "7140",
                "7141", "7142", "725")
    mild_liv9 <- c("07022", "07023", "07032", "07033", "07044", "07054", "0706",
                   "0709", "5733", "5734", "5738", "5739", "V427")
    diab_uc9 <- c("2508", "2509")
    hemi_para9 <- c("3341", "3449")
    renal9 <- c("40301", "40311", "40391", "40402", "40403", "40412", "40413",
                "40492", "40493", "5880", "V420", "V451", "5830", "5831", "5832",
                "5833", "5834", "5835", "5836", "5837")
    malig9 <- c("2386")

    myocar9_str <- c("410", "412")
    chf9_str <- c("428")
    periph_vasc9_str <- c("440", "441")
    cerebro9_str <- c("430", "431", "432", "433", "434", "435", "436",
                      "437", "438")
    dementia9_str <- c("290")
    chronic_pulm9_str <- c("491", "492", "493", "494", "495", "496", "500", "501",
                           "502", "503", "504", "505")
    peptic_ulcer9_str <- c("531", "532", "533", "534")
    mild_liv9_str <- c("570", "571")
    diab_uc9_str <- c("2500", "2501", "2052", "2503")
    diab_c9_str <- c("2504", "2505", "2506", "2507")
    hemi_para9_str <- c("342", "343", "3440", "3441", "3442", "3443", "3445", "3446")
    renal9_str <- c("582", "585", "586", "V56")
    malig9_str <- c("140", "141", "142", "143", "144", "145", "146", "147", "148",
                    "149", "150", "151", "152", "153", "154", "155", "156", "157",
                    "158", "159", "160", "161", "162", "163", "164", "165", "166",
                    "167", "168", "169", "170", "171", "172", "174", "175", "176",
                    "177", "178", "179", "180", "181", "182", "183", "184", "186",
                    "187", "188", "189", "190", "191", "192", "193", "194", "195",
                    "200", "201", "202", "203", "204", "205", "206", "207", "208")
    mod_sev_liv9_str <- c("4560", "4561", "4562", "5722", "5723", "5724", "5728")
    met_solid9_str <- c("196", "197", "198", "199")
    hiv9_str <- c("042", "043", "044")

    ## Code ----

    dat1 <- dat %>%
      dplyr::mutate(myocar = dplyr::if_else(stringr::str_starts(dx, paste(myocar9_str, sep = "|", collapse = "|")), 1, 0),
                    chf = dplyr::if_else(dx %in% chf9 | stringr::str_starts(dx, paste(chf9_str)), 1, 0),
                    periph_vasc = dplyr::if_else(dx %in% periph_vasc9 | stringr::str_starts(dx, paste(periph_vasc9_str, sep = "|", collapse = "|")), 1, 0),
                    cerebro = dplyr::if_else(dx %in% cerebro9 | stringr::str_starts(dx, paste(cerebro9_str, sep = "|", collapse = "|")), 1, 0),
                    dementia = dplyr::if_else(dx %in% dementia9 | stringr::str_starts(dx, paste(dementia9_str)), 1, 0),
                    chronic_pulm = dplyr::if_else(dx %in% chronic_pulm9 | stringr::str_starts(dx, paste(chronic_pulm9_str, sep = "|", collapse = "|")), 1, 0),
                    rheum = dplyr::if_else(dx %in% rheum9, 1, 0),
                    peptic_ulcer = dplyr::if_else(stringr::str_starts(dx, paste(peptic_ulcer9_str, sep = "|", collapse = "|")), 1, 0),
                    mild_liv = dplyr::if_else(dx %in% mild_liv9 | stringr::str_starts(dx, paste(mild_liv9_str, sep = "|", collapse = "|")), 1, 0),
                    diab_uc = dplyr::if_else(dx %in% diab_uc9 | stringr::str_starts(dx, paste(diab_uc9_str, sep = "|", collapse = "|")), 1, 0),
                    diab_c = dplyr::if_else(stringr::str_starts(dx, paste(diab_c9_str, sep = "|", collapse = "|")), 1, 0),
                    hemi_para = dplyr::if_else(dx %in% hemi_para9 | stringr::str_starts(dx, paste(hemi_para9_str, sep = "|", collapse = "|")), 1, 0),
                    renal = dplyr::if_else(dx %in% renal9 | stringr::str_starts(dx, paste(renal9_str, sep = "|", collapse = "|")), 1, 0),
                    malig = dplyr::if_else(dx %in% malig9 | stringr::str_starts(dx, paste(malig9_str, sep = "|", collapse = "|")), 1, 0),
                    mod_sev_liv = dplyr::if_else(stringr::str_starts(dx, paste(mod_sev_liv9_str, sep = "|", collapse = "|")), 1, 0),
                    met_solid = dplyr::if_else(stringr::str_starts(dx, paste(met_solid9_str, sep = "|", collapse = "|")), 1, 0),
                    hiv = dplyr::if_else(stringr::str_starts(dx, paste(hiv9_str, sep = "|", collapse = "|")), 1, 0))

    dat1 <- dat1 %>%
      dplyr::mutate(diab_uc = dplyr::if_else(.data$diab_c == 1, 0, .data$diab_uc),
                    mild_liv = dplyr::if_else(.data$mod_sev_liv == 1, 0, .data$mild_liv))

    }

  # Charlson ICD-10 ----

  else if (version == 10){

    ## Diagnoses Setup ----

    myocar10 <- c("I252")
    chf10 <- c("I099", "I110", "I130", "I132", "I255", "I420", "P290")
    periph_vasc10 <- c("I731", "I738", "I739", "I771", "I790", "I792", "K551",
                       "K558", "K559", "Z958", "Z959",
                       "I425", "I426", "I427", "I428", "I429")
    cerebro10 <- c("H340")
    dementia10 <- c("F051", "G311")
    chronic_pulm10 <- c("I278", "I279", "J684", "J701", "J703")
    rheum10 <- c("M351", "M353", "M360", "M315")
    mild_liv10 <- c("K709", "K717", "K760", "K768", "K769", "Z944",
                    "K762", "K763", "K764")
    diab_uc10 <- c("E100", "E101", "E106", "E108", "E109", "E110", "E111",
                   "E116", "E118", "E119", "E120", "E121", "E126", "E128",
                   "E129", "E130", "E131", "E136", "E138", "E139", "E140",
                   "E141", "E146", "E148", "E149")
    diab_c10 <- c("E107", "E117", "E127", "E137", "E147")
    hemi_para10 <- c("G041", "G114", "G801", "G802", "G839")
    renal10 <- c("I120", "I131", "N250", "Z940", "Z992")
    mod_sev_liv10 <- c("I850", "I859", "I864", "I982", "K704", "K711", "K721",
                       "K729", "K765", "K766", "K767")

    myocar10_str <- c("I21", "I22")
    chf10_str <- c("I43", "I50")
    periph_vasc10_str <- c("I70", "I71")
    cerebro10_str <- c("G45", "G46", "I60", "I61", "I62", "I63", "I64", "I65",
                       "I66", "I67", "I68", "I69")
    dementia10_str <- c("F00", "F01", "F02", "F03", "G30")
    chronic_pulm10_str <- c("J40", "J41", "J42", "J43", "J44", "J45", "J47",
                            "J61", "J62", "J63", "J64", "J65", "J66", "J67")
    rheum10_str <- c("M05", "M06", "M32", "M33", "M34")
    peptic_ulcer10_str <- c("K25", "K26", "K27", "K28")
    mild_liv10_str <- c("B18", "K700", "K701", "K702", "K703", "K713", "K714",
                        "K715", "K73", "K74")
    diab_c10_str <- c("E102", "E103", "E104", "E105", "E112", "E113", "E114",
                      "E115", "E122", "E123", "E124", "E125", "E132", "E133",
                      "E134", "E135", "E142", "E143", "E144", "E145")
    hemi_para10_str <- c("G81", "G82", "G830", "G831", "G832", "G833", "G834")
    renal10_str <- c("N032", "N033", "N034", "N035", "N036", "N037", "N052",
                     "N053", "N054", "N055", "N056", "N057", "N18", "N19", "Z490",
                     "Z491", "Z492")
    malig10_str <- c("C00", "C01", "C02", "C03", "C04", "C05", "C06", "C07", "C08", "C09",
                     "C10", "C11", "C12", "C13", "C14", "C15", "C16", "C17", "C18", "C19",
                     "C20", "C21", "C22", "C23", "C24", "C25", "C26",
                     "C30", "C31", "C32", "C33", "C34",
                     "C37", "C38", "C39", "C40", "C41",
                     "C43",
                     "C45", "C46", "C47", "C48", "C49", "C50", "C51", "C52", "C53", "C54",
                     "C55", "C56", "C57", "C58",
                     "C60", "C61", "C62", "C63", "C64", "C65", "C66", "C67", "C68", "C69",
                     "C70", "C71", "C72", "C73", "C74", "C75", "C76",
                     "C81", "C82", "C83", "C84", "C85",
                     "C88",
                     "C90", "C91", "C92", "C93", "C97")
    met_solid10_str <- c("C77", "C78", "C79", "C80")
    hiv10_str <- c("B20", "B21", "B22", "B24")

    # Code ----

    dat1 <- dat %>%
      dplyr::mutate(myocar = dplyr::if_else(stringr::str_starts(dx, paste(myocar10_str, sep = "|", collapse = "|")), 1, 0),
                    chf = dplyr::if_else(dx %in% chf10 | stringr::str_starts(dx, paste(chf10_str, sep = "|", collapse = "|")), 1, 0),
                    periph_vasc = dplyr::if_else(dx %in% periph_vasc10 | stringr::str_starts(dx, paste(periph_vasc10_str, sep = "|", collapse = "|")), 1, 0),
                    cerebro = dplyr::if_else(dx %in% cerebro10 | stringr::str_starts(dx, paste(cerebro10_str, sep = "|", collapse = "|")), 1, 0),
                    dementia = dplyr::if_else(dx %in% dementia10 | stringr::str_starts(dx, paste(dementia10_str, sep = "|", collapse = "|")), 1, 0),
                    chronic_pulm = dplyr::if_else(dx %in% chronic_pulm10 | stringr::str_starts(dx, paste(chronic_pulm10_str, sep = "|", collapse = "|")), 1, 0),
                    rheum = dplyr::if_else(dx %in% rheum10 | stringr::str_starts(dx, paste(rheum10_str, sep = "|", collapse = "|")), 1, 0),
                    peptic_ulcer = dplyr::if_else(stringr::str_starts(dx, paste(peptic_ulcer10_str, sep = "|", collapse = "|")), 1, 0),
                    mild_liv = dplyr::if_else(dx %in% mild_liv10 | stringr::str_starts(dx, paste(mild_liv10_str, sep = "|", collapse = "|")), 1, 0),
                    diab_uc = dplyr::if_else(dx %in% diab_uc10, 1, 0),
                    diab_c = dplyr::if_else(dx %in% diab_c10 | stringr::str_starts(dx, paste(diab_c10_str, sep = "|", collapse = "|")), 1, 0),
                    hemi_para = dplyr::if_else(dx %in% hemi_para10 | stringr::str_starts(dx, paste(hemi_para10_str, sep = "|", collapse = "|")), 1, 0),
                    renal = dplyr::if_else(dx %in% renal10 | stringr::str_starts(dx, paste(renal10_str, sep = "|", collapse = "|")), 1, 0),
                    malig = dplyr::if_else(stringr::str_starts(dx, paste(malig10_str, sep = "|", collapse = "|")), 1, 0),
                    mod_sev_liv = dplyr::if_else(dx %in% mod_sev_liv10, 1, 0),
                    met_solid = dplyr::if_else(stringr::str_starts(dx, paste(met_solid10_str, sep = "|", collapse = "|")), 1, 0),
                    hiv = dplyr::if_else(stringr::str_starts(dx, paste(hiv10_str, sep = "|", collapse = "|")), 1, 0))

    dat1 <- dat1 %>%
      dplyr::mutate(diab_uc = dplyr::if_else(.data$diab_c == 1, 0, .data$diab_uc),
                    mild_liv = dplyr::if_else(.data$mod_sev_liv == 1, 0, .data$mild_liv))
  }

  # Charlson Both ----

    else if (version == 19){

      ## Diagnoses Setup ----

      chf9 <- c("39891", "40201", "40211", "40291", "40401", "40403",
                "40411", "40413", "40491", "40493", "4254", "4255", "4257",
                "4258", "4259")
      periph_vasc9 <- c("0930", "4373", "4471", "5571", "5579", "V434",
                        "4431", "4432", "4438", "4439")
      cerebro9 <- c("36234")
      dementia9 <- c("2941", "3312")
      chronic_pulm9 <- c("4168", "4169", "5064", "5081", "5088")
      rheum9 <- c("4465", "7148", "7100", "7101", "7102", "7103", "7104", "7140",
                  "7141", "7142", "725")
      mild_liv9 <- c("07022", "07023", "07032", "07033", "07044", "07054", "0706",
                     "0709", "5733", "5734", "5738", "5739", "V427")
      diab_uc9 <- c("2508", "2509")
      hemi_para9 <- c("3341", "3449")
      renal9 <- c("40301", "40311", "40391", "40402", "40403", "40412", "40413",
                  "40492", "40493", "5880", "V420", "V451", "5830", "5831", "5832",
                  "5833", "5834", "5835", "5836", "5837")
      malig9 <- c("2386")

      myocar9_str <- c("410", "412")
      chf9_str <- c("428")
      periph_vasc9_str <- c("440", "441")
      cerebro9_str <- c("430", "431", "432", "433", "434", "435", "436",
                        "437", "438")
      dementia9_str <- c("290")
      chronic_pulm9_str <- c("491", "492", "493", "494", "495", "496", "500", "501",
                             "502", "503", "504", "505")
      peptic_ulcer9_str <- c("531", "532", "533", "534")
      mild_liv9_str <- c("570", "571")
      diab_uc9_str <- c("2500", "2501", "2052", "2503")
      diab_c9_str <- c("2504", "2505", "2506", "2507")
      hemi_para9_str <- c("342", "343", "3440", "3441", "3442", "3443", "3445", "3446")
      renal9_str <- c("582", "585", "586", "V56")
      malig9_str <- c("140", "141", "142", "143", "144", "145", "146", "147", "148",
                      "149", "150", "151", "152", "153", "154", "155", "156", "157",
                      "158", "159", "160", "161", "162", "163", "164", "165", "166",
                      "167", "168", "169", "170", "171", "172", "174", "175", "176",
                      "177", "178", "179", "180", "181", "182", "183", "184", "186",
                      "187", "188", "189", "190", "191", "192", "193", "194", "195",
                      "200", "201", "202", "203", "204", "205", "206", "207", "208")
      mod_sev_liv9_str <- c("4560", "4561", "4562", "5722", "5723", "5724", "5728")
      met_solid9_str <- c("196", "197", "198", "199")
      hiv9_str <- c("042", "043", "044")

      myocar10 <- c("I252")
      chf10 <- c("I099", "I110", "I130", "I132", "I255", "I420", "P290")
      periph_vasc10 <- c("I731", "I738", "I739", "I771", "I790", "I792", "K551",
                         "K558", "K559", "Z958", "Z959",
                         "I425", "I426", "I427", "I428", "I429")
      cerebro10 <- c("H340")
      dementia10 <- c("F051", "G311")
      chronic_pulm10 <- c("I278", "I279", "J684", "J701", "J703")
      rheum10 <- c("M351", "M353", "M360", "M315")
      mild_liv10 <- c("K709", "K717", "K760", "K768", "K769", "Z944",
                      "K762", "K763", "K764")
      diab_uc10 <- c("E100", "E101", "E106", "E108", "E109", "E110", "E111",
                     "E116", "E118", "E119", "E120", "E121", "E126", "E128",
                     "E129", "E130", "E131", "E136", "E138", "E139", "E140",
                     "E141", "E146", "E148", "E149")
      diab_c10 <- c("E107", "E117", "E127", "E137", "E147")
      hemi_para10 <- c("G041", "G114", "G801", "G802", "G839")
      renal10 <- c("I120", "I131", "N250", "Z940", "Z992")
      mod_sev_liv10 <- c("I850", "I859", "I864", "I982", "K704", "K711", "K721",
                         "K729", "K765", "K766", "K767")

      myocar10_str <- c("I21", "I22")
      chf10_str <- c("I43", "I50")
      periph_vasc10_str <- c("I70", "I71")
      cerebro10_str <- c("G45", "G46", "I60", "I61", "I62", "I63", "I64", "I65",
                         "I66", "I67", "I68", "I69")
      dementia10_str <- c("F00", "F01", "F02", "F03", "G30")
      chronic_pulm10_str <- c("J40", "J41", "J42", "J43", "J44", "J45", "J47",
                              "J61", "J62", "J63", "J64", "J65", "J66", "J67")
      rheum10_str <- c("M05", "M06", "M32", "M33", "M34")
      peptic_ulcer10_str <- c("K25", "K26", "K27", "K28")
      mild_liv10_str <- c("B18", "K700", "K701", "K702", "K703", "K713", "K714",
                          "K715", "K73", "K74")
      diab_c10_str <- c("E102", "E103", "E104", "E105", "E112", "E113", "E114",
                        "E115", "E122", "E123", "E124", "E125", "E132", "E133",
                        "E134", "E135", "E142", "E143", "E144", "E145")
      hemi_para10_str <- c("G81", "G82", "G830", "G831", "G832", "G833", "G834")
      renal10_str <- c("N032", "N033", "N034", "N035", "N036", "N037", "N052",
                       "N053", "N054", "N055", "N056", "N057", "N18", "N19", "Z490",
                       "Z491", "Z492")
      malig10_str <- c("C00", "C01", "C02", "C03", "C04", "C05", "C06", "C07", "C08", "C09",
                       "C10", "C11", "C12", "C13", "C14", "C15", "C16", "C17", "C18", "C19",
                       "C20", "C21", "C22", "C23", "C24", "C25", "C26",
                       "C30", "C31", "C32", "C33", "C34",
                       "C37", "C38", "C39", "C40", "C41",
                       "C43",
                       "C45", "C46", "C47", "C48", "C49", "C50", "C51", "C52", "C53", "C54",
                       "C55", "C56", "C57", "C58",
                       "C60", "C61", "C62", "C63", "C64", "C65", "C66", "C67", "C68", "C69",
                       "C70", "C71", "C72", "C73", "C74", "C75", "C76",
                       "C81", "C82", "C83", "C84", "C85",
                       "C88",
                       "C90", "C91", "C92", "C93", "C97")
      met_solid10_str <- c("C77", "C78", "C79", "C80")
      hiv10_str <- c("B20", "B21", "B22", "B24")

      # Code -----

      dat <- dat %>%
        dplyr::filter({{version_var}} == 9 | {{version_var}} == 10)

      dat1 <- dat %>%
        dplyr::mutate(myocar = dplyr::if_else(({{version_var}} ==  9 & stringr::str_starts(dx, paste(myocar9_str, sep = "|", collapse = "|"))) |
                                             ({{version_var}} ==  10 & (stringr::str_starts(dx, paste(myocar10_str, sep = "|", collapse = "|")))),
                                           1, 0),
                      chf = dplyr::if_else(({{version_var}} ==  9 & (dx %in% chf9 | stringr::str_starts(dx, paste(chf9_str, sep = "|", collapse = "|")))) |
                                               ({{version_var}} ==  10 & (dx %in% chf10 | stringr::str_starts(dx, paste(chf10_str, sep = "|", collapse = "|")))),
                                             1, 0),
                      periph_vasc = dplyr::if_else(({{version_var}} ==  9 & (dx %in% periph_vasc9 | stringr::str_starts(dx, paste(periph_vasc9_str, sep = "|", collapse = "|")))) |
                                                  ({{version_var}} ==  10 & (dx %in% periph_vasc10 | stringr::str_starts(dx, paste(periph_vasc10_str, sep = "|", collapse = "|")))),
                                                1, 0),
                      cerebro = dplyr::if_else(({{version_var}} ==  9 & (dx %in% cerebro9 | stringr::str_starts(dx, paste(cerebro9_str, sep = "|", collapse = "|")))) |
                                                     ({{version_var}} ==  10 & (dx %in% cerebro10 | stringr::str_starts(dx, paste(cerebro10_str, sep = "|", collapse = "|")))),
                                                   1, 0),
                      dementia = dplyr::if_else(({{version_var}} ==  9 & (dx %in% dementia9 | stringr::str_starts(dx, paste(dementia9_str, sep = "|", collapse = "|")))) |
                                                ({{version_var}} ==  10 & (dx %in% dementia10 | stringr::str_starts(dx, paste(dementia10_str, sep = "|", collapse = "|")))),
                                              1, 0),
                      chronic_pulm = dplyr::if_else(({{version_var}} == 9 & (dx %in% chronic_pulm9 | stringr::str_starts(dx, paste(chronic_pulm9_str, sep = "|", collapse = "|")))) |
                                               ({{version_var}} ==  10 & (dx %in% chronic_pulm10 | stringr::str_starts(dx, paste(chronic_pulm10_str, sep = "|", collapse = "|")))),
                                             1 ,0),
                      rheum = dplyr::if_else(({{version_var}} ==  9 & dx %in% rheum9) |
                                                   ({{version_var}} ==  10 & (dx %in% rheum10 | stringr::str_starts(dx, paste(rheum10_str, sep = "|", collapse = "|")))),
                                                 1, 0),
                      peptic_ulcer = dplyr::if_else(({{version_var}} ==  9 & stringr::str_starts(dx, paste(peptic_ulcer9_str, sep = "|", collapse = "|"))) |
                                               ({{version_var}} ==  10 & stringr::str_starts(dx, paste(peptic_ulcer10_str, sep = "|", collapse = "|"))),
                                             1, 0),
                      mild_liv = dplyr::if_else(({{version_var}} ==  9 & (dx %in% mild_liv9 | stringr::str_starts(dx, paste(mild_liv9_str, sep = "|", collapse = "|")))) |
                                                      ({{version_var}} ==  10 & (dx %in% mild_liv10 | stringr::str_starts(dx, paste(mild_liv10_str, sep = "|", collapse = "|")))),
                                                    1, 0),
                      diab_uc = dplyr::if_else(({{version_var}} ==  9 & (dx %in% diab_uc9 | stringr::str_starts(dx, paste(diab_uc9_str, sep = "|", collapse = "|")))) |
                                                 ({{version_var}} ==  10 & dx %in% diab_uc10),
                                               1, 0),
                      diab_c = dplyr::if_else(({{version_var}} ==  9 & stringr::str_starts(dx, paste(diab_c9_str, sep = "|", collapse = "|"))) |
                                                ({{version_var}} ==  10 & (dx %in% diab_c10 | stringr::str_starts(dx, paste(diab_c10_str, sep = "|", collapse = "|")))),
                                              1, 0),
                      hemi_para = dplyr::if_else(({{version_var}} == 9 & (dx %in% hemi_para9 | stringr::str_starts(dx, paste(hemi_para9_str, sep = "|", collapse = "|")))) |
                                              ({{version_var}} ==  10 & (dx %in% hemi_para10 | stringr::str_starts(dx, paste(hemi_para10_str, sep = "|", collapse = "|")))),
                                            1, 0),
                      renal = dplyr::if_else(({{version_var}} ==  9 & (dx %in% renal9 | stringr::str_starts(dx, paste(renal9_str, sep = "|", collapse = "|")))) |
                                               ({{version_var}} ==  10 & (dx %in% renal10 | stringr::str_starts(dx, paste(renal10_str, sep = "|", collapse = "|")))),
                                             1, 0),
                      malig = dplyr::if_else(({{version_var}} ==  9 & (dx %in% malig9 | stringr::str_starts(dx, paste(malig9_str, sep = "|", collapse = "|")))) |
                                               ({{version_var}} ==  10 & dx %in% stringr::str_starts(dx, paste(malig10_str, sep = "|", collapse = "|"))),
                                             1, 0),
                      mod_sev_liv = dplyr::if_else(({{version_var}} ==  9 & dx %in% stringr::str_starts(dx, paste(mod_sev_liv9_str, sep = "|", collapse = "|"))) |
                                                ({{version_var}} ==  10 & dx %in% mod_sev_liv10),
                                              1, 0),
                      met_solid = dplyr::if_else(({{version_var}} ==  9 & stringr::str_starts(dx, paste(met_solid9_str, sep = "|", collapse = "|"))) |
                                             ({{version_var}} ==  10 & dx %in% stringr::str_starts(dx, paste(met_solid10_str, sep = "|", collapse = "|"))),
                                           1 ,0),
                      hiv = dplyr::if_else(({{version_var}} ==  9 & stringr::str_starts(dx, paste(hiv9_str, sep = "|", collapse = "|"))) |
                                               ({{version_var}} ==  10 & stringr::str_starts(dx, paste(hiv10_str, sep = "|", collapse = "|"))),
                                             1, 0))

      dat1 <- dat1 %>%
        dplyr::mutate(diab_uc = dplyr::if_else(.data$diab_c == 1, 0, .data$diab_uc),
                      mild_liv = dplyr::if_else(.data$mod_sev_liv == 1, 0, .data$mild_liv))


    }

  else {stop("Please specify the version (9 = ICD9, 10 = ICD10, or 19 = ICD9 and ICD10) of your diagnoses codes.")

  }

  # Outpatient Algorithms ----

  ## No OT Limitation ----

  if (outpatient_two == "no"){

    dat2 <- dplyr::rename(dat1, "id" = id2)

    dat2 <- dat2 %>%
      dplyr::mutate(charlson_myocar = .data$myocar,
                    charlson_chf = .data$chf,
                    charlson_periph_vasc = .data$periph_vasc,
                    charlson_cerebro = .data$cerebro,
                    charlson_dementia = .data$dementia,
                    charlson_chronic_pulm = .data$chronic_pulm,
                    charlson_rheum = .data$rheum,
                    charlson_peptic_ulcer = .data$peptic_ulcer,
                    charlson_mild_liv = .data$mild_liv,
                    charlson_diab_uc = .data$diab_uc,
                    charlson_diab_c = .data$diab_c,
                    charlson_hemi_para = .data$hemi_para,
                    charlson_renal = .data$renal,
                    charlson_malig = .data$malig,
                    charlson_mod_sev_liv = .data$mod_sev_liv,
                    charlson_met_solid = .data$met_solid,
                    charlson_hiv = .data$hiv)

  dat2 <- dat2 %>%
    dplyr::select(id, tidyselect::starts_with("charlson"))

  }

  ## OT Limitation ----

  else if (outpatient_two == "yes" | outpatient_two == "Yes"){
    message("Message: You have specified that for a comorbidity to be positvely coded, an individual must have two outpatient claims with it. Please make sure the levels of your variable denoting outpatient type are either 'ot' or 'OT'")

    dat_ot <- dat1 %>%
      dplyr::filter(.data$type == "ot" | .data$type == "OT")

    dat_ip <- dat1 %>%
      dplyr::filter(.data$type != "ot" & .data$type != "OT")

    dat_ot_sum <- dat_ot %>%
      dplyr::group_by({{id}}) %>%
      dplyr::summarize(
        myocar_ot = sum(.data$myocar),
        chf_ot = sum(.data$chf),
        periph_vasc_ot = sum(.data$periph_vasc),
        cerebro_ot = sum(.data$cerebro),
        dementia_ot = sum(.data$dementia),
        chronic_pulm_ot = sum(.data$chronic_pulm),
        rheum_ot = sum(.data$rheum),
        peptic_ulcer_ot = sum(.data$peptic_ulcer),
        mild_liv_ot = sum(.data$mild_liv),
        diab_uc_ot = sum(.data$diab_uc),
        diab_c_ot = sum(.data$diab_c),
        hemi_para_ot = sum(.data$hemi_para),
        renal_ot = sum(.data$renal),
        malig_ot = sum(.data$malig),
        mod_sev_liv_ot = sum(.data$mod_sev_liv),
        met_solid_ot = sum(.data$met_solid),
        hiv_ot = sum(.data$hiv)) %>%
      dplyr::ungroup()

    dat_ip_sum <- dat_ip %>%
      dplyr::group_by({{id}}) %>%
      dplyr::summarize(
        myocar_ip = max(.data$myocar),
        chf_ip = max(.data$chf),
        periph_vasc_ip = max(.data$periph_vasc),
        cerebro_ip = max(.data$cerebro),
        dementia_ip = max(.data$dementia),
        chronic_pulm_ip = max(.data$chronic_pulm),
        rheum_ip = max(.data$rheum),
        peptic_ulcer_ip = max(.data$peptic_ulcer),
        mild_liv_ip = max(.data$mild_liv),
        diab_uc_ip = max(.data$diab_uc),
        diab_c_ip = max(.data$diab_c),
        hemi_para_ip = max(.data$hemi_para),
        renal_ip = max(.data$renal),
        malig_ip = max(.data$malig),
        mod_sev_liv_ip = max(.data$mod_sev_liv),
        met_solid_ip = max(.data$met_solid),
        hiv_ip = max(.data$hiv)) %>%
      dplyr::ungroup()

    dat_ip_sum <- dplyr::rename(dat_ip_sum, "id" = id2)
    dat_ot_sum <- dplyr::rename(dat_ot_sum, "id" = id2)
    dat_comb <- dplyr::full_join(dat_ip_sum, dat_ot_sum, by = "id")

    dat_comb <- dat_comb %>%
      dplyr::mutate(charlson_myocar = dplyr::if_else(.data$myocar_ot >= 2 | .data$myocar_ip >=1, 1, 0, missing = 0),
                    charlson_chf = dplyr::if_else(.data$chf_ot >= 2 | .data$chf_ip >=1, 1, 0, missing = 0),
                    charlson_periph_vasc = dplyr::if_else(.data$periph_vasc_ot >= 2 | .data$periph_vasc_ip >=1, 1, 0, missing = 0),
                    charlson_cerebro = dplyr::if_else(.data$cerebro_ot >= 2 | .data$cerebro_ip >=1, 1, 0, missing = 0),
                    charlson_dementia = dplyr::if_else(.data$dementia_ot >= 2 | .data$dementia_ip >=1, 1, 0, missing = 0),
                    charlson_chronic_pulm = dplyr::if_else(.data$chronic_pulm_ot >= 2 | .data$chronic_pulm_ip >=1, 1, 0, missing = 0),
                    charlson_rheum = dplyr::if_else(.data$rheum_ot >= 2 | .data$rheum_ip >=1, 1, 0, missing = 0),
                    charlson_peptic_ulcer = dplyr::if_else(.data$peptic_ulcer_ot >= 2 | .data$peptic_ulcer_ip >=1, 1, 0, missing = 0),
                    charlson_mild_liv = dplyr::if_else(.data$mild_liv_ot >= 2 | .data$mild_liv_ip >=1, 1, 0, missing = 0),
                    charlson_diab_uc = dplyr::if_else(.data$diab_uc_ot >= 2 | .data$diab_uc_ip >=1, 1, 0, missing = 0),
                    charlson_diab_c = dplyr::if_else(.data$diab_c_ot >= 2 | .data$diab_c_ip >=1, 1, 0, missing = 0),
                    charlson_hemi_para = dplyr::if_else(.data$hemi_para_ot >= 2 | .data$hemi_para_ip >=1, 1, 0, missing = 0),
                    charlson_renal = dplyr::if_else(.data$renal_ot >= 2 | .data$renal_ip >=1, 1, 0, missing = 0),
                    charlson_malig = dplyr::if_else(.data$malig_ot >= 2 | .data$malig_ip >=1, 1, 0, missing = 0),
                    charlson_mod_sev_liv = dplyr::if_else(.data$mod_sev_liv_ot >= 2 | .data$mod_sev_liv_ip >=1, 1, 0, missing = 0),
                    charlson_met_solid = dplyr::if_else(.data$met_solid_ot >= 2 | .data$met_solid_ip >=1, 1, 0, missing = 0),
                    charlson_hiv = dplyr::if_else(.data$hiv_ot >= 2 | .data$hiv_ip >=1, 1, 0, missing = 0))

    dat2 <- dat_comb %>%
      dplyr::select(id, tidyselect::starts_with("charlson"))

  }


  # Final Summary ----

    dat3 <- dat2 %>%
      dplyr::group_by(id) %>%
      dplyr::summarize(
        charlson_myocar = max(.data$charlson_myocar),
        charlson_chf = max(.data$charlson_chf),
        charlson_periph_vasc = max(.data$charlson_periph_vasc),
        charlson_cerebro = max(.data$charlson_cerebro),
        charlson_dementia = max(.data$charlson_dementia),
        charlson_chronic_pulm = max(.data$charlson_chronic_pulm),
        charlson_rheum = max(.data$charlson_rheum),
        charlson_peptic_ulcer = max(.data$charlson_peptic_ulcer),
        charlson_mild_liv = max(.data$charlson_mild_liv),
        charlson_diab_uc = max(.data$charlson_diab_uc),
        charlson_diab_c = max(.data$charlson_diab_c),
        charlson_hemi_para = max(.data$charlson_hemi_para),
        charlson_renal = max(.data$charlson_renal),
        charlson_malig = max(.data$charlson_malig),
        charlson_mod_sev_liv = max(.data$charlson_mod_sev_liv),
        charlson_met_solid = max(.data$charlson_met_solid),
        charlson_hiv = max(.data$charlson_hiv)) %>%
      dplyr::ungroup()

    # Weighting ----

    dat3 <- dat3 %>%
      dplyr::mutate(charlson_score = .data$charlson_myocar + .data$charlson_chf +
                      .data$charlson_periph_vasc + .data$charlson_cerebro +
                      .data$charlson_dementia + .data$charlson_chronic_pulm +
                      .data$charlson_rheum + .data$charlson_peptic_ulcer +
                      .data$charlson_mild_liv + .data$charlson_diab_uc +
                      (.data$charlson_diab_c * 2) + (.data$charlson_hemi_para * 2) +
                      (.data$charlson_renal * 2) + (.data$charlson_malig * 2) +
                      (.data$charlson_mod_sev_liv * 3) + (.data$charlson_met_solid * 6) +
                      (.data$charlson_hiv * 6))


  dat4 <- dat3 %>%
    dplyr::select(id, tidyselect::starts_with("charlson"))

  return(dat4)

}
