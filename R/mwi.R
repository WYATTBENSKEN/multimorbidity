#' Multimorbidity Weighted Index (MWI)
#'
#' \code{cfi} returns a summary dataset containing the multimorbidity weighted index
#'     for each patient.
#'
#' This function uses data which has been properly prepared to calculate the multimorbidity weighted index developed by Wei et al.
#'     As this algorithm was never developed to require two diagnosis codes, and is weighted, we have excluded that feature from this function.
#'     See full package documentation for additional details.
#'
#' @param dat dataset which has been properly prepared using 'prepare_data()'
#' @param id variable of the unique patient identifier
#' @param dx the column with the diagnoses and procedures (defaults to 'dx')
#' @param version which version(s) of ICD your data contain (ICD-9 only: 9, ICD-10 only: 10,
#'     Both: 19)
#' @param version_var variable which denotes if the diagnoses on that row are ICD-9 (9) or
#'     ICD-10 (10)
#'
#' @examples
#' mwi(dat = prepared_data, id = patient_id, dx = dx, version = 9, version_var = version)
#'
#'
#' @export

#' @importFrom rlang .data
mwi <- function(dat = NULL,
                id = NULL,
                dx = "dx",
                version = 19,
                version_var = NULL){


  id2 <- rlang::quo_name(rlang::enquo(id))

  if (version == 9){

    dat1a <- dat %>%
      dplyr::filter({{version_var}} == 9)

    # Unduplicate Diagnosis Codes ----

    dat1 <- unique(dat1a)

    # Setup the specific diseases ----

    angina9_str <- c("413")

    aneurysm9_str <- c("441")

    afib9 <- c("42731")

    aicd9 <- c("3794", "3795", "3796", "3797", "3798",
               "V4502", "V5332", "99604")

    chf9 <- c("42511", "42518", "4252", "4253", "4254", "2455", "4256",
              "4257", "4258", "4259", "4259", "39891", "40201", "40211", "40291",
              "40401", "40403", "40411", "40413", "40491", "40493")
    chf9_str <- c("428")

    bypass9_str <- c("0361")

    hbp9 <- c("40200", "40210", "40290", "40310", "40322", "40390", "40391")
    hbp9_str <- c("401", "405")

    mitral9_str <- c("3949", "4240")

    mi9_str <- c("410", "4110", "4111", "412", "42979")

    periph9 <- c("44381", "44382", "44389", "4439", "4401", "4402", "44020", "44021",
                 "44022", "44023", "44024", "44029", "4403", "44030", "44031", "44032",
                 "4404", "4408", "4409")
    periph9_str <- c("4471", "V434")

    valve9 <- c("42971", "4241", "4242", "4243", "7463", "7464", "7465", "7466")
    valve9_str <- c("3940", "3942", "396", "3970", "V422", "V433")

    diabetes9 <- c("V5867", "V5391", "E9323")
    diabetes9_str <- c("249", "250")

    hyperlipid9_str <- c("2720", "2722", "2724", "2726", "2727")

    hyperthyroid9 <- c("24201", "24211", "24221", "24231", "24241", "24281", "24291")

    hypertri9_str <- c("2721", "2723")

    hypothyroid9_str <- c("243", "244")

    thyroidnod9_str <- c("240", "241", "2420", "2421", "2422", "2423", "2424", "2461")

    barretts9 <- c("53085")

    hepato9 <- c("07022", "07023", "07044", "07054", "07031", "07032", "07033")
    hepato9_str <- c("5710", "5713", "5714", "5718", "5719", "572", "5730", "1305")

    cirrhosis9 <- c("5051", "5059")
    cirrhosis9_str <- c("5712", "5715", "5716", "V427")

    colonpolyp9_str <- c("2113")

    diverticulitis9_str <- c("562")

    gallstones9 <- c("99741")
    gallstones9_str <- c("574")

    hepatitis9 <- c("07030", "07041", "07042", "07043", "07049", "07051", "07052", "07053",
                    "0700", "0701", "0702", "07020", "07021", "07059", "0706", "0707",
                    "07070", "07071", "0709")
    hepatitis9_str <- c("5711", "5731", "5732", "5733", "5734")


    ibs9_str <- c("555", "556", "558")

    pancreatitis9_str <- c("5770", "5771", "5772")

    ulcer9_str <- c("531", "532", "533", "534")

    perniciousanemia9 <- c("2810")

    dvt9_str <- c("415", "4162", "452", "4530", "4531", "4532", "4533",
                  "45350", "4536", "4537", "4539")

    aids9 <- c("07953")
    aids9_str <- c("042")

    solar9 <- c("7020")

    connective9_str <- c("6954", "710")

    gout9_str <- c("274")

    herniated_disc9_str <- c("722")

    hip_frac9 <- c("73314", "73315", "73396")
    hip_frac9_str <- c("8080", "8081", "820")

    hip_replace9 <- c("8151", "8152", "8153",
                      "V4364", "0070", "0071",
                      "0072", "0073")

    knee_replace9 <- c("8154", "8155", "0080", "0081", "0082", "0083", "0084",
                      "V4365")

    osteoarth9_str <- c("715")

    osteoporo9_str <- c("7330")

    rheum9_str <- c("714")

    vertebral9 <- c("73313")
    vertebral9_str <- c("805", "806")

    wrist_frac9_str <- c("814")

    als9_str <- c("3552")

    dementia9 <- c("33182")
    dementia9_str <- c("290", "2941", "2942", "3310", "3311", "3312", "3371",
                       "3315")

    migraine9_str <- c("346")

    ms9_str <- c("340")

    parkinson9_str <- c("332")

    restless9 <- c("3394")

    epilepsy9_str <- c("345")

    cerebro9 <- c("44321", "99702")
    cerebro9_str <- c("430", "431", "432", "433", "434", "437", "438",
                      "8530", "8541", "8520", "8521", "8522", "8523")

    tia9_str <- c("435")

    basal9 <- c("17301", "17311", "17321", "17331", "17341", "17351", "17361",
               "17371", "17381", "17391")

    bladder9_str <- c("188")

    breast9_str <- c("174", "175", "2330")

    cervical9_str <- c("180")

    colon9_str <- c("153")

    leuk9_str <- c("200", "201", "202", "203", "204", '205', "206", "207", "208",
                   "2386", "2733")

    lung9_str <- c("162")

    melanoma9_str <- c("172")

    oth_canc9_str <- c("239")

    ovarian9_str <- c("183")

    prostate9_str <- c("1850")

    squamous9 <- c("17302", "17312", "17322", "17332", "17342", "17352", "17362",
                  "17372", "17382", "17392")

    uterine9_str <- c("1821", "1828", "1790")

    cataract9 <- c("37926", "37931", "37939", "V4561", "74339", "74330", "74331",
                   "74332", "74333", "74334")
    cataract9_str <- c("366", "V431")


    glaucoma9 <- c("36285", "37714")
    glaucoma9_str <- c("365")

    macular_deg9_str <- c("3625")

    periodontal9 <- c("52310", "52311", "52320", "52321", "52322", "52323", "52324",
                      "52325", "52330", "52331", "52332")
    peridontal9_str <- c("5234", "5235", "5238", "5239")

    alcohol_abuse9_str <- c("291", "303", "3050", "V113")

    depression9_str <- c("2962", "2963", "29682", "2980", "3004", "3090", "3091", "311")

    asthma9_str <- c("493")

    copd9_str <- c("490", "491", "492", "4940", "4941", "496", "5181", "5182",
                   "7702", "9587", "99881")

    ckd9_str <- c("28311", "403", "404", "5724", "585", "587", "588", "589",
                  "580", "581", "582", "583")

    cystitits9_str <- c("5951", "5952", "5953", "5954", "5955", "5956", "5957",
                   "5958", "5959")

    calc_kid9_str <- c("592")

    benign_breast9_str <- c("217")

    bph9_str <- c("600")

    ectopic9_str <- c("633")

    endo9_str <- c("617")

    ed9 <- c("30272", "60784")

    polycys9_str <- c("2564")

    premenstrual9_str <- c("6254")

    prostate_surg9 <- c("6021", "6029", "6095", "6096", "6097")

    uterine_fibroid9_str <- c("218")

    # Apply weights and disease number ----

    dat1 <- dat1 %>%
      dplyr::mutate(
        diseaseid = dplyr::case_when(
          stringr::str_starts(dx, paste(angina9_str)) ~ 1,
          stringr::str_starts(dx, paste(aneurysm9_str)) ~ 2,
          dx %in% afib9 ~ 3,
          dx %in% aicd9 ~ 4,
          dx %in% chf9 | stringr::str_starts(dx, paste(chf9_str)) ~ 5,
          stringr::str_starts(dx, paste(bypass9_str)) ~ 6,
          dx %in% hbp9 | stringr::str_starts(dx, paste(hbp9_str, sep = "|", collapse = "|")) ~ 7,
          stringr::str_starts(dx, paste(mitral9_str, sep = "|", collapse = "|")) ~ 8,
          stringr::str_starts(dx, paste(mi9_str, sep = "|", collapse = "|")) ~ 9,
          dx %in% periph9 | stringr::str_starts(dx, paste(periph9_str, sep = "|", collapse = "|")) ~ 10,
          dx %in% valve9 | stringr::str_starts(dx, paste(valve9_str, sep = "|", collapse = "|")) ~ 11,
          dx %in% diabetes9 | stringr::str_starts(dx, paste(diabetes9_str, sep = "|", collapse = "|")) ~ 12,
          stringr::str_starts(dx, paste(hyperlipid9_str, sep = "|", collapse = "|")) ~ 13,
          dx %in% hyperthyroid9 ~ 14,
          stringr::str_starts(dx, paste(hypertri9_str, sep = "|", collapse = "|")) ~ 15,
          stringr::str_starts(dx, paste(hypothyroid9_str, sep = "|", collapse = "|")) ~ 16,
          stringr::str_starts(dx, paste(thyroidnod9_str, sep = "|", collapse = "|")) ~ 17,
          dx %in% barretts9 ~ 18,
          dx %in% hepato9 | stringr::str_starts(dx, paste(hepato9_str, sep = "|", collapse = "|")) ~ 19,
          dx %in% cirrhosis9 | stringr::str_starts(dx, paste(cirrhosis9_str, sep = "|", collapse = "|")) ~ 20,
          stringr::str_starts(dx, paste(colonpolyp9_str)) ~ 21,
          stringr::str_starts(dx, paste(diverticulitis9_str)) ~ 22,
          dx %in% gallstones9 | stringr::str_starts(dx, paste(gallstones9_str)) ~ 23,
          dx %in% hepatitis9 | stringr::str_starts(dx, paste(hepatitis9_str, sep = "|", collapse = "|")) ~ 24,
          stringr::str_starts(dx, paste(ibs9_str, sep = "|", collapse = "|")) ~ 25,
          stringr::str_starts(dx, paste(pancreatitis9_str, sep = "|", collapse = "|")) ~ 26,
          stringr::str_starts(dx, paste(ulcer9_str, sep = "|", collapse = "|")) ~ 27,
          dx %in% perniciousanemia9 ~ 28,
          stringr::str_starts(dx, paste(dvt9_str, sep = "|", collapse = "|")) ~ 29,
          dx %in% aids9 | stringr::str_starts(dx, paste(aids9_str)) ~ 30,
          dx %in% solar9 ~ 31,
          stringr::str_starts(dx, paste(connective9_str, sep = "|", collapse = "|")) ~ 32,
          stringr::str_starts(dx, paste(gout9_str)) ~ 33,
          stringr::str_starts(dx, paste(herniated_disc9_str)) ~ 34,
          dx %in% hip_frac9 | stringr::str_starts(dx, paste(hip_frac9_str, sep = "|", collapse = "|")) ~ 35,
          dx %in% hip_replace9 ~ 36,
          dx %in% knee_replace9 ~ 37,
          stringr::str_starts(dx, paste(osteoarth9_str)) ~ 38,
          stringr::str_starts(dx, paste(osteoporo9_str)) ~ 39,
          stringr::str_starts(dx, paste(rheum9_str)) ~ 40,
          dx %in% vertebral9 | stringr::str_starts(dx, paste(vertebral9_str, sep = "|", collapse = "|")) ~ 41,
          stringr::str_starts(dx, paste(wrist_frac9_str)) ~ 42,
          stringr::str_starts(dx, paste(als9_str)) ~ 43,
          dx %in% dementia9 | stringr::str_starts(dx, paste(dementia9_str, sep = "|", collapse = "|")) ~ 44,
          stringr::str_starts(dx, paste(migraine9_str)) ~ 45,
          stringr::str_starts(dx, paste(ms9_str)) ~ 46,
          stringr::str_starts(dx, paste(parkinson9_str)) ~ 47,
          dx %in% restless9 ~ 48,
          stringr::str_starts(dx, paste(epilepsy9_str)) ~ 49,
          dx %in% cerebro9 | stringr::str_starts(dx, paste(cerebro9_str, sep = "|", collapse = "|")) ~ 50,
          stringr::str_starts(dx, paste(tia9_str)) ~ 51,
          dx %in% basal9 ~ 52,
          stringr::str_starts(dx, paste(bladder9_str)) ~ 53,
          stringr::str_starts(dx, paste(breast9_str, sep = "|", collapse = "|")) ~ 54,
          stringr::str_starts(dx, paste(cervical9_str)) ~ 55,
          stringr::str_starts(dx, paste(colon9_str)) ~ 56,
          stringr::str_starts(dx, paste(leuk9_str, sep = "|", collapse = "|")) ~ 57,
          stringr::str_starts(dx, paste(lung9_str)) ~ 58,
          stringr::str_starts(dx, paste(melanoma9_str)) ~ 58,
          stringr::str_starts(dx, paste(oth_canc9_str)) ~ 60,
          stringr::str_starts(dx, paste(ovarian9_str)) ~ 61,
          stringr::str_starts(dx, paste(prostate9_str)) ~ 62,
          dx %in% squamous9 ~ 63,
          stringr::str_starts(dx, paste(uterine9_str, sep = "|", collapse = "|")) ~ 64,
          dx %in% cataract9 | stringr::str_starts(dx, paste(cataract9_str, sep = "|", collapse = "|")) ~ 65,
          dx %in% glaucoma9 | stringr::str_starts(dx, paste(glaucoma9_str)) ~ 66,
          stringr::str_starts(dx, paste(macular_deg9_str)) ~ 67,
          dx %in% periodontal9 | stringr::str_starts(dx, paste(peridontal9_str, sep = "|", collapse = "|")) ~ 68,
          stringr::str_starts(dx, paste(alcohol_abuse9_str, sep = "|", collapse = "|")) ~ 69,
          stringr::str_starts(dx, paste(depression9_str, sep = "|", collapse = "|")) ~ 70,
          stringr::str_starts(dx, paste(asthma9_str)) ~ 71,
          stringr::str_starts(dx, paste(copd9_str, sep = "|", collapse = "|")) ~ 72,
          stringr::str_starts(dx, paste(ckd9_str, sep = "|", collapse = "|")) ~ 73,
          stringr::str_starts(dx, paste(cystitits9_str, sep = "|", collapse = "|")) ~ 74,
          stringr::str_starts(dx, paste(calc_kid9_str)) ~ 75,
          stringr::str_starts(dx, paste(benign_breast9_str)) ~ 76,
          stringr::str_starts(dx, paste(bph9_str)) ~ 77,
          stringr::str_starts(dx, paste(ectopic9_str)) ~ 78,
          stringr::str_starts(dx, paste(endo9_str)) ~ 79,
          dx %in% ed9 ~ 80,
          stringr::str_starts(dx, paste(polycys9_str)) ~ 81,
          stringr::str_starts(dx, paste(premenstrual9_str)) ~ 82,
          stringr::str_starts(dx, paste(prostate_surg9, sep = "|", collapse = "|")) ~ 83,
          stringr::str_starts(dx, paste(uterine_fibroid9_str)) ~ 84,
          TRUE ~ 85)
      )

    dat1 <- dat1 %>%
      dplyr::mutate(
        weight = dplyr::case_when(
          diseaseid == 1 ~ 2.2,
          diseaseid == 2 ~ 1.96,
          diseaseid == 3 ~ 1.33,
          diseaseid == 4 ~ 1.57,
          diseaseid == 5 ~ 4.77,
          diseaseid == 6 ~ 0.724,
          diseaseid == 19 ~ 0.293,
          diseaseid == 73 ~ 3.98,
          diseaseid == 7 ~ 1.53,
          diseaseid == 8 ~ 0.033,
          diseaseid == 9 ~ 1.73,
          diseaseid == 10 ~ 3.25,
          diseaseid == 11 ~ 0.416,
          diseaseid == 12 ~ 2.67,
          diseaseid == 13 ~ 0.343,
          diseaseid == 14 ~ 0.149,
          diseaseid == 15 ~ 0.692,
          diseaseid == 16 ~ 0.808,
          diseaseid == 17 ~ 0,
          diseaseid == 18 ~ 0.284,
          diseaseid == 20 ~ 4.3,
          diseaseid == 21 ~ 0.01,
          diseaseid == 22 ~ 0.624,
          diseaseid == 23 ~ 0.929,
          diseaseid == 24 ~ 0.147,
          diseaseid == 25 ~ 1.07,
          diseaseid == 26 ~ 0.675,
          diseaseid == 27 ~ 1.08,
          diseaseid == 28 ~ 1.82,
          diseaseid == 29 ~ 1.98,
          diseaseid == 30 ~ 2.91,
          diseaseid == 31 ~ 0,
          diseaseid == 32 ~ 3.02,
          diseaseid == 33 ~ 1.34,
          diseaseid == 34 ~ 3.27,
          diseaseid == 35 ~ 3.56,
          diseaseid == 36 ~ 3.55,
          diseaseid == 37 ~ 9.11,
          diseaseid == 38 ~ 3.52,
          diseaseid == 39 ~ 0.997,
          diseaseid == 40 ~ 3.79,
          diseaseid == 41 ~ 2.07,
          diseaseid == 42 ~ 0,
          diseaseid == 43 ~ 7.45,
          diseaseid == 44 ~ 6.1,
          diseaseid == 45 ~ 0.614,
          diseaseid == 46 ~ 10.6,
          diseaseid == 47 ~ 8.82,
          diseaseid == 48 ~ 2.23,
          diseaseid == 49 ~ 0.841,
          diseaseid == 50 ~ 3.79,
          diseaseid == 51 ~ 1.24,
          diseaseid == 52 ~ 0,
          diseaseid == 53 ~ 0.99,
          diseaseid == 54 ~ 0.886,
          diseaseid == 55 ~ 0.726,
          diseaseid == 56 ~ 1.18,
          diseaseid == 57 ~ 1.32,
          diseaseid == 58 ~ 6.25,
          diseaseid == 59 ~ 0,
          diseaseid == 60 ~ 1.76,
          diseaseid == 61 ~ 1.87,
          diseaseid == 62 ~ 0.402,
          diseaseid == 63 ~ 0,
          diseaseid == 64 ~ 0.753,
          diseaseid == 65 ~ 0.288,
          diseaseid == 66 ~ 0.427,
          diseaseid == 67 ~ 0.564,
          diseaseid == 68 ~ 0.164,
          diseaseid == 69 ~ 1.37,
          diseaseid == 70 ~ 1.29,
          diseaseid == 71 ~ 1.62,
          diseaseid == 72 ~ 4.32,
          diseaseid == 74 ~ 0.879,
          diseaseid == 75 ~ 0.291,
          diseaseid == 76 ~ 0,
          diseaseid == 77 ~ 0,
          diseaseid == 78 ~ 0,
          diseaseid == 79 ~ 0.142,
          diseaseid == 80 ~ 1.22,
          diseaseid == 81 ~ 0.64,
          diseaseid == 82 ~ 0.412,
          diseaseid == 83 ~ 0,
          diseaseid == 84 ~ 0.029,
          diseaseid == 85 ~ 0))

    dat2 <- dat1 %>%
      dplyr::select(id2, .data$diseaseid, .data$weight)

    # Deduplicate by disease number ----

    dat3 <- unique(dat2)

    # Summarize Total Weight ----

    dat4 <- dat3 %>%
      dplyr::group_by({{id}}) %>%
      dplyr::summarize(mwi = sum(.data$weight)) %>%
      dplyr::ungroup()

  }

  else if (version == 10 | version == 19){
    stop("The Multmorbidity Weighted Index has only been developed for ICD-9. Please limit your data to those claims with ICD-9 diagnosis codes, and use version = 9")
  }

  dat4 <- dplyr::rename(dat4, "id" = id2)

return(dat4)

}
