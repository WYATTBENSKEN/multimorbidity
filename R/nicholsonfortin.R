#' Nicholson and Fortin Conditions
#'
#' \code{elixhauser} returns a summary dataset containing the Nicholson and
#'     Fortin Conditions for each patient.
#'
#' This function uses data which has been properly prepared to identify and flag the
#'     Nicholson and Fortin conditions See full package documentation for additional details.
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
#' @return dataframe with one row per patient, and a column for their patient id, a column with each
#'     Nicholson/Fortin comorbidity
#'
#' @examples
#' nicholsonfortin(dat = prepared_data, id = patient_id, dx = dx, version = 19,
#' version_var = version, outpatient_two = "yes")
#'
#'
#' @export

nicholsonfortin <- function(dat = NULL,
                       id = NULL,
                       dx = "dx",
                       version = 19,
                       version_var = NULL,
                       outpatient_two = "no"){


  id2 <- rlang::quo_name(rlang::enquo(id))

  # ICD-9 ----

  ## Diagnoses Setup ----

  if (version == 9){
    htn9_str <- c("401", "402", "403", "404", "405")
    obesity9 <- c("27800", "27801")
    diabetes9_str <- c("250")
    clrd9_str <- c("491", "492", "493", "496")
    hyperlipid9_str <- c("272")
    cancer9_str <- c("140", "141", "142", "143", "144", "145", "146", "147", "148", "149",
                     "150", "151", "152", "153", "154", "155", "156", "157", "158", "159",
                     "160", "161", "162", "163", "164", "165",
                     "170", "171", "172", "173", "174", "175", "175", "176",
                     "179", "180", "181", "182", "183", "184", "185", "186", "187", "188", "189",
                     "190", "191", "192", "192", "193", "194", "195", "196", "197", "198", "199",
                     "200", "201", "202", "203", "204", "205", "206", "207", "208", "209")
    cvd9 <- c("4130", "4131", "4132", "4273", "42731", "42732")
    cvd9_str <- c("412", "440", "441", "442", "443", "444", "445",
                   "446", "447", "448", "449", "427")
    heartfail9_str <- c("428", "394", "395")
    anxietydepress9_str <- c("2962", "2963", "3000")
    arthritis9_str <- c("7141", "7142", "7143", "7150", "7151", "7152", "7153", "7158", "7159")
    stroketia9_str <- c("434", "435")
    thyroid9_str <- c("240", "241", "242", "243", "244", "245", "246")
    ckd9_str <- c("585")
    osteo9_str <- c("733")
    dementia9_str <- c("290", "2940", "2941", "2942")
    musculo9 <- c("72701", "72703", "72704", "72705", "72706", "72709")
    musculo9_str <- c("725", "7230", "7231", "7240", "7241", "7242", "7243", "7244", "7245",
                       "7261", "7263", "7263", "7264", "7265", "7266", "7267", "7269",
                       "7272", "7273",
                       "7290", "7291", "7292", "7294", "7295")
    stomach9 <- c("53081")
    stomach9_str <- c("5300", "5314", "5315", "5316", "5317", "5319")
    colon9_str <- c("5550", "5551", "5552", "5559", "5564", "5565", "5566",
                    "5568", "5569", "5640", "5641")
    liverdx9_str <- c("571")
    urinary9 <- c("5933", "5934", "5935", "59370", "59371", "59372", "59373", "59381", "59382", "59389", "5939",
                  "5950", "5951", "5952", "5959",
                  "5970", "59780", "59781", "59782", "6010", "6011", "6013", "6018", "6019",
                  "6020", "6021", "6022", "6023", "6028", "6029")
    urinary9_str <- c("600")

    ## Code ----

    dat1 <- dat %>%
      dplyr::mutate(htn = dplyr::if_else(stringr::str_starts(dx, paste(htn9_str, sep = "|", collapse = "|")), 1, 0),
                    obesity = dplyr::if_else(dx %in% obesity9, 1, 0),
                    diabetes = dplyr::if_else(stringr::str_starts(dx, paste(diabetes9_str, sep = "|", collapse = "|")), 1, 0),
                    clrd = dplyr::if_else(stringr::str_starts(dx, paste(clrd9_str, sep = "|", collapse = "|")), 1, 0),
                    hyperlipid = dplyr::if_else(stringr::str_starts(dx, paste(hyperlipid9_str, sep = "|", collapse = "|")), 1, 0),
                    cancer = dplyr::if_else(stringr::str_starts(dx, paste(cancer9_str, sep = "|", collapse = "|")), 1, 0),
                    cvd = dplyr::if_else(dx %in% cvd9 | stringr::str_starts(dx, paste(cvd9_str, sep = "|", collapse = "|")), 1, 0),
                    heartfail = dplyr::if_else(stringr::str_starts(dx, paste(heartfail9_str, sep = "|", collapse = "|")), 1, 0),
                    anxietydepress = dplyr::if_else(stringr::str_starts(dx, paste(anxietydepress9_str, sep = "|", collapse = "|")), 1, 0),
                    arthritis = dplyr::if_else(stringr::str_starts(dx, paste(arthritis9_str, sep = "|", collapse = "|")), 1, 0),
                    stroketia = dplyr::if_else(stringr::str_starts(dx, paste(stroketia9_str, sep = "|", collapse = "|")), 1, 0),
                    thyroid = dplyr::if_else(stringr::str_starts(dx, paste(thyroid9_str, sep = "|", collapse = "|")), 1, 0),
                    ckd = dplyr::if_else(stringr::str_starts(dx, paste(ckd9_str, sep = "|", collapse = "|")), 1, 0),
                    osteo = dplyr::if_else(stringr::str_starts(dx, paste(osteo9_str, sep = "|", collapse = "|")), 1, 0),
                    dementia = dplyr::if_else(stringr::str_starts(dx, paste(dementia9_str, sep = "|", collapse = "|")), 1, 0),
                    musculo = dplyr::if_else(dx %in% musculo9 | stringr::str_starts(dx, paste(musculo9_str, sep = "|", collapse = "|")), 1, 0),
                    stomach = dplyr::if_else(dx %in% stomach9 | stringr::str_starts(dx, paste(stomach9_str, sep = "|", collapse = "|")), 1, 0),
                    colon = dplyr::if_else(stringr::str_starts(dx, paste(colon9_str, sep = "|", collapse = "|")), 1, 0),
                    liver = dplyr::if_else(stringr::str_starts(dx, paste(liverdx9_str, sep = "|", collapse = "|")), 1, 0),
                    urinary = dplyr::if_else(dx %in% urinary9 | stringr::str_starts(dx, paste(urinary9_str, sep = "|", collapse = "|")), 1, 0))
  }

  #ICD-10 ----

  else if (version == 10){

    ## Diagnoses Setup ----

    htn10_str <- c("I10", "I11", "I12", "I13", "I14", "I15")
    obesity10_str <- c("E66")
    diabetes10_str <- c("E10", "E11", "E12", "E13", "E14")
    clrd10_str <- c("J40", "J41", "J42", "J43", "J44", "J45", "J46")
    hyperlipid10_str <- c("E78")
    cancer10_str <- c("C")
    cvd10_str <- c("I20", "I25", "I48", "I70", "I71", "I72", "I73", "I74", "I75",
                   "I76", "I77", "I78", "I79")
    heartfail10_str <- c("I05", "I06", "I07", "I08", "I09", "I34", "I35", "I36",
                         "I37", "I38", "I39", "I42", "I43", "I50")
    anxietydepress10_str <- c("F33", "F40", "F41")
    arthritis10 <- c("M059", "M130", "M139")
    arthritis10_str <- c("M15", "M16", "M17", "M18", "M19")
    stroketia10_str <- c("G45", "I62")
    thyroid10_str <- c("E00", "E01", "E02", "E03", "E04", "E05", "E06", "E07")
    ckd10_str <- c("N18", "N19")
    osteo10_str <- c("M81")
    dementia10_str <- c("F00", "F01", "F02", "F03")
    musculo10_str <- c("M40", "M41", "M42", "M43", "M44", "M45", "M46", "M47", "M48", "M49",
                       "M50", "M51", "M52", "M53", "M54", "M60", "M61", "M62", "M63",
                       "M65", "M66", "M67", "M68",
                       "M70", "M71", "M72", "M73", "M74", "M75", "M76", "M77", "M78", "M79")
    stomach10_str <- c("K21")
    stomach10 <- c("K257", "K295")
    colon10_str <- c("K50", "K51", "K52", "K57", "K58")
    liverdx10_str <- c("K70", "K71", "K72", "K73", "K74", "K75", "K76", "K77")
    urinary10_str <- c("N03", "N11", "N18", "N20", "N21", "N22", "N23", "N25",
                       "N26", "N27", "N28", "N29",
                       "N30", "N31", "N32", "N33", "N34", "N35", "N36", "N37", "N38", "N39",
                       "N40", "N41", "N42", "N43", "N44", "N45", "N46", "N47", "N48", "N49",
                       "N50", "N51")

    # Code ----

    dat1 <- dat %>%
      dplyr::mutate(htn = dplyr::if_else(stringr::str_starts(dx, paste(htn10_str, sep = "|", collapse = "|")), 1, 0),
                    obesity = dplyr::if_else(stringr::str_starts(dx, paste(obesity10_str, sep = "|", collapse = "|")), 1, 0),
                    diabetes = dplyr::if_else(stringr::str_starts(dx, paste(diabetes10_str, sep = "|", collapse = "|")), 1, 0),
                    clrd = dplyr::if_else(stringr::str_starts(dx, paste(clrd10_str, sep = "|", collapse = "|")), 1, 0),
                    hyperlipid = dplyr::if_else(stringr::str_starts(dx, paste(hyperlipid10_str, sep = "|", collapse = "|")), 1, 0),
                    cancer = dplyr::if_else(stringr::str_starts(dx, paste(cancer10_str, sep = "|", collapse = "|")), 1, 0),
                    cvd = dplyr::if_else(stringr::str_starts(dx, paste(cvd10_str, sep = "|", collapse = "|")), 1, 0),
                    heartfail = dplyr::if_else(stringr::str_starts(dx, paste(heartfail10_str, sep = "|", collapse = "|")), 1, 0),
                    anxietydepress = dplyr::if_else(stringr::str_starts(dx, paste(anxietydepress10_str, sep = "|", collapse = "|")), 1, 0),
                    arthritis = dplyr::if_else(dx %in% arthritis10 | stringr::str_starts(dx, paste(arthritis10_str, sep = "|", collapse = "|")), 1, 0),
                    stroketia = dplyr::if_else(stringr::str_starts(dx, paste(stroketia10_str, sep = "|", collapse = "|")), 1, 0),
                    thyroid = dplyr::if_else(stringr::str_starts(dx, paste(thyroid10_str, sep = "|", collapse = "|")), 1, 0),
                    ckd = dplyr::if_else(stringr::str_starts(dx, paste(ckd10_str, sep = "|", collapse = "|")), 1, 0),
                    osteo = dplyr::if_else(stringr::str_starts(dx, paste(osteo10_str, sep = "|", collapse = "|")), 1, 0),
                    dementia = dplyr::if_else(stringr::str_starts(dx, paste(dementia10_str, sep = "|", collapse = "|")), 1, 0),
                    musculo = dplyr::if_else(stringr::str_starts(dx, paste(musculo10_str, sep = "|", collapse = "|")), 1, 0),
                    stomach = dplyr::if_else(dx %in% stomach10 | stringr::str_starts(dx, paste(stomach10_str, sep = "|", collapse = "|")), 1, 0),
                    colon = dplyr::if_else(stringr::str_starts(dx, paste(colon10_str, sep = "|", collapse = "|")), 1, 0),
                    liver = dplyr::if_else(stringr::str_starts(dx, paste(liverdx10_str, sep = "|", collapse = "|")), 1, 0),
                    urinary = dplyr::if_else(stringr::str_starts(dx, paste(urinary10_str, sep = "|", collapse = "|")), 1, 0))


  }

  # Both ----

  else if (version == 19){

    ## Diagnoses Setup ----

    # ICD-9

    htn9_str <- c("401", "402", "403", "404", "405")
    obesity9 <- c("27800", "27801")
    diabetes9_str <- c("250")
    clrd9_str <- c("491", "492", "493", "496")
    hyperlipid9_str <- c("272")
    cancer9_str <- c("140", "141", "142", "143", "144", "145", "146", "147", "148", "149",
                     "150", "151", "152", "153", "154", "155", "156", "157", "158", "159",
                     "160", "161", "162", "163", "164", "165",
                     "170", "171", "172", "173", "174", "175", "175", "176",
                     "179", "180", "181", "182", "183", "184", "185", "186", "187", "188", "189",
                     "190", "191", "192", "192", "193", "194", "195", "196", "197", "198", "199",
                     "200", "201", "202", "203", "204", "205", "206", "207", "208", "209")
    cvd9 <- c("4130", "4131", "4132", "4273", "42731", "42732")
    cvd9_str <- c("412", "440", "441", "442", "443", "444", "445",
                   "446", "447", "448", "449", "427")

    heartfail9_str <- c("428", "394", "395")
    anxietydepress9_str <- c("2962", "2963", "3000")
    arthritis9_str <- c("7141", "7142", "7143", "7150", "7151", "7152", "7153", "7158", "7159")
    stroketia9_str <- c("434", "435")
    thyroid9_str <- c("240", "241", "242", "243", "244", "245", "246")
    ckd9_str <- c("585")
    osteo9_str <- c("733")
    dementia9_str <- c("290", "2940", "2941", "2942")
    musculo9 <- c("72701", "72703", "72704", "72705", "72706", "72709")
    musculo9_str <- c("725", "7230", "7231", "7240", "7241", "7242", "7243", "7244", "7245",
                      "7261", "7263", "7263", "7264", "7265", "7266", "7267", "7269",
                      "7272", "7273",
                      "7290", "7291", "7292", "7294", "7295")
    stomach9 <- c("53081")
    stomach9_str <- c("5300", "5314", "5315", "5316", "5317", "5319")
    colon9_str <- c("5550", "5551", "5552", "5559", "5564", "5565", "5566",
                    "5568", "5569", "5640", "5641")
    liverdx9_str <- c("571")
    urinary9 <- c("5933", "5934", "5935", "59370", "59371", "59372", "59373", "59381", "59382", "59389", "5939",
                  "5950", "5951", "5952", "5959",
                  "5970", "59780", "59781", "59782", "6010", "6011", "6013", "6018", "6019",
                  "6020", "6021", "6022", "6023", "6028", "6029")
    urinary9_str <- c("600")

    # ICD-10

    htn10_str <- c("I10", "I11", "I12", "I13", "I14", "I15")
    obesity10_str <- c("E66")
    diabetes10_str <- c("E10", "E11", "E12", "E13", "E14")
    clrd10_str <- c("J40", "J41", "J42", "J43", "J44", "J45", "J46")
    hyperlipid10_str <- c("E78")
    cancer10_str <- c("C")
    cvd10_str <- c("I20", "I25", "I48", "I70", "I71", "I72", "I73", "I74", "I75",
                   "I76", "I77", "I78", "I79")
    heartfail10_str <- c("I05", "I06", "I07", "I08", "I09", "I34", "I35", "I36",
                         "I37", "I38", "I39", "I42", "I43", "I50")
    anxietydepress10_str <- c("F33", "F40", "F41")
    arthritis10 <- c("M059", "M130", "M139")
    arthritis10_str <- c("M15", "M16", "M17", "M18", "M19")
    stroketia10_str <- c("G45", "I62")
    thyroid10_str <- c("E00", "E01", "E02", "E03", "E04", "E05", "E06", "E07")
    ckd10_str <- c("N18", "N19")
    osteo10_str <- c("M81")
    dementia10_str <- c("F00", "F01", "F02", "F03")
    musculo10_str <- c("M40", "M41", "M42", "M43", "M44", "M45", "M46", "M47", "M48", "M49",
                       "M50", "M51", "M52", "M53", "M54", "M60", "M61", "M62", "M63",
                       "M65", "M66", "M67", "M68",
                       "M70", "M71", "M72", "M73", "M74", "M75", "M76", "M77", "M78", "M79")
    stomach10_str <- c("K21")
    stomach10 <- c("K257", "K295")
    colon10_str <- c("K50", "K51", "K52", "K57", "K58")
    liverdx10_str <- c("K70", "K71", "K72", "K73", "K74", "K75", "K76", "K77")
    urinary10_str <- c("N03", "N11", "N18", "N20", "N21", "N22", "N23", "N25",
                       "N26", "N27", "N28", "N29",
                       "N30", "N31", "N32", "N33", "N34", "N35", "N36", "N37", "N38", "N39",
                       "N40", "N41", "N42", "N43", "N44", "N45", "N46", "N47", "N48", "N49",
                       "N50", "N51")
    # Code -----

    dat <- dat %>%
      dplyr::filter({{version_var}} == 9 | {{version_var}} == 10)


    dat1 <- dat %>%
      dplyr::mutate(htn = dplyr::if_else(({{version_var}} == 9 & stringr::str_starts(dx, paste(htn9_str, sep = "|", collapse = "|"))) |
                                           ({{version_var}} == 10 & stringr::str_starts(dx, paste(htn10_str, sep = "|", collapse = "|"))),
                                         1, 0),
                    obesity = dplyr::if_else(({{version_var}} == 9 & dx %in% obesity9) |
                                               ({{version_var}} == 10 & stringr::str_starts(dx, paste(obesity10_str, sep = "|", collapse = "|"))),
                                             1, 0),
                    diabetes = dplyr::if_else(({{version_var}} == 9 & stringr::str_starts(dx, paste(diabetes9_str, sep = "|", collapse = "|"))) |
                                                ({{version_var}} == 10 & stringr::str_starts(dx, paste(diabetes10_str, sep = "|", collapse = "|"))),
                                              1, 0),
                    clrd = dplyr::if_else(({{version_var}} == 9 & stringr::str_starts(dx, paste(clrd9_str, sep = "|", collapse = "|"))) |
                                            ({{version_var}} == 10 & stringr::str_starts(dx, paste(clrd10_str, sep = "|", collapse = "|"))),
                                          1, 0),
                    hyperlipid = dplyr::if_else(({{version_var}} == 9 & stringr::str_starts(dx, paste(hyperlipid9_str, sep = "|", collapse = "|"))) |
                                            ({{version_var}} == 10 & stringr::str_starts(dx, paste(hyperlipid10_str, sep = "|", collapse = "|"))),
                                          1, 0),
                    cancer = dplyr::if_else(({{version_var}} == 9 & stringr::str_starts(dx, paste(cancer9_str, sep = "|", collapse = "|"))) |
                                                  ({{version_var}} == 10 & stringr::str_starts(dx, paste(cancer10_str, sep = "|", collapse = "|"))),
                                                1, 0),
                    cvd = dplyr::if_else(({{version_var}} == 9 & (dx %in% cvd9 | stringr::str_starts(dx, paste(cvd9_str, sep = "|", collapse = "|")))) |
                                              ({{version_var}} == 10 & stringr::str_starts(dx, paste(cvd10_str, sep = "|", collapse = "|"))),
                                            1, 0),
                    heartfail = dplyr::if_else(({{version_var}} == 9 & stringr::str_starts(dx, paste(heartfail9_str, sep = "|", collapse = "|"))) |
                                              ({{version_var}} == 10 & stringr::str_starts(dx, paste(heartfail10_str, sep = "|", collapse = "|"))),
                                            1, 0),
                    anxietydepress = dplyr::if_else(({{version_var}} == 9 & stringr::str_starts(dx, paste(anxietydepress9_str, sep = "|", collapse = "|"))) |
                                                 ({{version_var}} == 10 & stringr::str_starts(dx, paste(anxietydepress10_str, sep = "|", collapse = "|"))),
                                               1, 0),
                    arthritis = dplyr::if_else(({{version_var}} == 9 & stringr::str_starts(dx, paste(arthritis9_str, sep = "|", collapse = "|"))) |
                                                      ({{version_var}} == 10 & (dx %in% arthritis10 | stringr::str_starts(dx, paste(arthritis10_str, sep = "|", collapse = "|")))),
                                                    1, 0),
                    stroketia = dplyr::if_else(({{version_var}} == 9 & stringr::str_starts(dx, paste(stroketia9_str, sep = "|", collapse = "|"))) |
                                                      ({{version_var}} == 10 & stringr::str_starts(dx, paste(stroketia10_str, sep = "|", collapse = "|"))),
                                                    1, 0),
                    thyroid = dplyr::if_else(({{version_var}} == 9 & stringr::str_starts(dx, paste(thyroid9_str, sep = "|", collapse = "|"))) |
                                                 ({{version_var}} == 10 & stringr::str_starts(dx, paste(thyroid10_str, sep = "|", collapse = "|"))),
                                               1, 0),
                    ckd = dplyr::if_else(({{version_var}} == 9 & stringr::str_starts(dx, paste(ckd9_str, sep = "|", collapse = "|"))) |
                                               ({{version_var}} == 10 & stringr::str_starts(dx, paste(ckd10_str, sep = "|", collapse = "|"))),
                                             1, 0),
                    osteo = dplyr::if_else(({{version_var}} == 9 & stringr::str_starts(dx, paste(osteo9_str, sep = "|", collapse = "|"))) |
                                           ({{version_var}} == 10 & stringr::str_starts(dx, paste(osteo10_str, sep = "|", collapse = "|"))),
                                         1, 0),
                    dementia = dplyr::if_else(({{version_var}} == 9 & stringr::str_starts(dx, paste(dementia9_str, sep = "|", collapse = "|"))) |
                                           ({{version_var}} == 10 & stringr::str_starts(dx, paste(dementia10_str, sep = "|", collapse = "|"))),
                                         1, 0),
                    musculo = dplyr::if_else(({{version_var}} == 9 & (dx %in% musculo9 | stringr::str_starts(dx, paste(musculo9_str, sep = "|", collapse = "|")))) |
                                           ({{version_var}} == 10 & stringr::str_starts(dx, paste(musculo10_str, sep = "|", collapse = "|"))),
                                         1, 0),
                    stomach = dplyr::if_else(({{version_var}} == 9 & (dx %in% stomach9 | stringr::str_starts(dx, paste(stomach9_str, sep = "|", collapse = "|")))) |
                                               ({{version_var}} == 10 & (dx %in% stomach10 | stringr::str_starts(dx, paste(stomach10_str, sep = "|", collapse = "|")))),
                                             1, 0),
                    colon = dplyr::if_else(({{version_var}} == 9 & stringr::str_starts(dx, paste(colon9_str, sep = "|", collapse = "|"))) |
                                                ({{version_var}} == 10 & stringr::str_starts(dx, paste(colon10_str, sep = "|", collapse = "|"))),
                                              1, 0),
                    liver = dplyr::if_else(({{version_var}} == 9 & stringr::str_starts(dx, paste(liverdx9_str, sep = "|", collapse = "|"))) |
                                                ({{version_var}} == 10 & stringr::str_starts(dx, paste(liverdx10_str, sep = "|", collapse = "|"))),
                                              1, 0),
                    urinary = dplyr::if_else(({{version_var}} == 9 & (dx %in% urinary9 | stringr::str_starts(dx, paste(urinary9_str, sep = "|", collapse = "|")))) |
                                             ({{version_var}} == 10 & stringr::str_starts(dx, paste(urinary10_str, sep = "|", collapse = "|"))),
                                           1, 0))

  }

  else {stop("Please specify the version (9 = ICD9, 10 = ICD10, or 19 = ICD9 and ICD10) of your diagnoses codes.")

  }

  #  Outpatient Algorithms ----

  ## No OT Limitation ----

  if (outpatient_two == "no"){

    dat2 <- dplyr::rename(dat1, "id" = id2)

    dat2 <- dat2 %>%
      dplyr::mutate(nf_htn = .data$htn,
                    nf_obesity = .data$obesity,
                    nf_diabetes = .data$diabetes,
                    nf_clrd = .data$clrd,
                    nf_hyperlipid = .data$hyperlipid,
                    nf_cancer = .data$cancer,
                    nf_cvd = .data$cvd,
                    nf_heartfail = .data$heartfail,
                    nf_anxietydepress = .data$anxietydepress,
                    nf_arthritis = .data$arthritis,
                    nf_stroketia = .data$stroketia,
                    nf_thyroid = .data$thyroid,
                    nf_ckd = .data$ckd,
                    nf_osteo = .data$osteo,
                    nf_dementia = .data$dementia,
                    nf_musculo = .data$musculo,
                    nf_stomach = .data$stomach,
                    nf_colon = .data$colon,
                    nf_liver = .data$liver,
                    nf_urinary = .data$urinary)

    dat2 <- dat2 %>%
      dplyr::select(id, tidyselect::starts_with("nf"))

  }

  ## OT Limitation ----

  else if ((outpatient_two == "yes" | outpatient_two == "Yes")){
    message("Message: You have specified that for a comorbidity to be positvely coded, an individual must have two outpatient claims with it. Please make sure the levels of your variable denoting outpatient type must be either 'ot' or 'OT'")

    dat_ot <- dat1 %>%
      dplyr::filter(.data$type == "ot" | .data$type == "OT")

    dat_ip <- dat1 %>%
      dplyr::filter(.data$type != "ot" & .data$type != "OT")

    dat_ot_sum <- dat_ot %>%
      dplyr::group_by({{id}}) %>%
      dplyr::summarize(htn_ot = sum(.data$htn),
                    obesity_ot = sum(.data$obesity),
                    diabetes_ot = sum(.data$diabetes),
                    clrd_ot = sum(.data$clrd),
                    hyperlipid_ot = sum(.data$hyperlipid),
                    cancer_ot = sum(.data$cancer),
                    cvd_ot = sum(.data$cvd),
                    heartfail_ot = sum(.data$heartfail),
                    anxietydepress_ot = sum(.data$anxietydepress),
                    arthritis_ot = sum(.data$arthritis),
                    stroketia_ot = sum(.data$stroketia),
                    thyroid_ot = sum(.data$thyroid),
                    ckd_ot = sum(.data$ckd),
                    osteo_ot = sum(.data$osteo),
                    dementia_ot = sum(.data$dementia),
                    musculo_ot = sum(.data$musculo),
                    stomach_ot = sum(.data$stomach),
                    colon_ot = sum(.data$colon),
                    liver_ot = sum(.data$liver),
                    urinary_ot = sum(.data$urinary)) %>%
      dplyr::ungroup()

    dat_ip_sum <- dat_ip %>%
      dplyr::group_by({{id}}) %>%
      dplyr::summarize(
        htn_ip = max(.data$htn),
        obesity_ip = max(.data$obesity),
        diabetes_ip = max(.data$diabetes),
        clrd_ip = max(.data$clrd),
        hyperlipid_ip = max(.data$hyperlipid),
        cancer_ip = max(.data$cancer),
        cvd_ip = max(.data$cvd),
        heartfail_ip = max(.data$heartfail),
        anxietydepress_ip = max(.data$anxietydepress),
        arthritis_ip = max(.data$arthritis),
        stroketia_ip = max(.data$stroketia),
        thyroid_ip = max(.data$thyroid),
        ckd_ip = max(.data$ckd),
        osteo_ip = max(.data$osteo),
        dementia_ip = max(.data$dementia),
        musculo_ip = max(.data$musculo),
        stomach_ip = max(.data$stomach),
        colon_ip = max(.data$colon),
        liver_ip = max(.data$liver),
        urinary_ip = max(.data$urinary)) %>%
      dplyr::ungroup()

    dat_ip_sum <- dplyr::rename(dat_ip_sum, "id" = id2)
    dat_ot_sum <- dplyr::rename(dat_ot_sum, "id" = id2)
    dat_comb <- dplyr::full_join(dat_ip_sum, dat_ot_sum, by = "id")

    dat_comb <- dat_comb %>%
      dplyr::mutate(nf_htn = dplyr::if_else(.data$htn_ot >= 2 | .data$htn_ip >= 1, 1, 0, missing = 0),
                     nf_obesity = dplyr::if_else(.data$obesity_ot >= 2 | .data$obesity_ip >= 1, 1, 0, missing = 0),
                     nf_diabetes = dplyr::if_else(.data$diabetes_ot >= 2 | .data$diabetes_ip >= 1, 1, 0, missing = 0),
                     nf_clrd = dplyr::if_else(.data$clrd_ot >= 2 | .data$clrd_ip >= 1, 1, 0, missing = 0),
                     nf_hyperlipid = dplyr::if_else(.data$hyperlipid_ot >= 2 | .data$hyperlipid_ip >= 1, 1, 0, missing = 0),
                     nf_cancer = dplyr::if_else(.data$cancer_ot >= 2 | .data$cancer_ip >= 1, 1, 0, missing = 0),
                     nf_cvd = dplyr::if_else(.data$cvd_ot >= 2 | .data$cvd_ip >= 1, 1, 0, missing = 0),
                     nf_heartfail = dplyr::if_else(.data$heartfail_ot >= 2 | .data$heartfail_ip >= 1, 1, 0, missing = 0),
                     nf_anxietydepress = dplyr::if_else(.data$anxietydepress_ot >= 2 | .data$anxietydepress_ip >= 1, 1, 0, missing = 0),
                     nf_arthritis = dplyr::if_else(.data$arthritis_ot >= 2 | .data$arthritis_ip >= 1, 1, 0, missing = 0),
                     nf_stroketia = dplyr::if_else(.data$stroketia_ot >= 2 | .data$stroketia_ip >= 1, 1, 0, missing = 0),
                     nf_thyroid = dplyr::if_else(.data$thyroid_ot >= 2 | .data$thyroid_ip >= 1, 1, 0, missing = 0),
                     nf_ckd = dplyr::if_else(.data$ckd_ot >= 2 | .data$ckd_ip >= 1, 1, 0, missing = 0),
                     nf_osteo = dplyr::if_else(.data$osteo_ot >= 2 | .data$osteo_ip >= 1, 1, 0, missing = 0),
                     nf_dementia = dplyr::if_else(.data$dementia_ot >= 2 | .data$dementia_ip >= 1, 1, 0, missing = 0),
                     nf_musculo = dplyr::if_else(.data$musculo_ot >= 2 | .data$musculo_ip >= 1, 1, 0, missing = 0),
                     nf_stomach = dplyr::if_else(.data$stomach_ot >= 2 | .data$stomach_ip >= 1, 1, 0, missing = 0),
                     nf_colon = dplyr::if_else(.data$colon_ot >= 2 | .data$colon_ip >= 1, 1, 0, missing = 0),
                     nf_liver = dplyr::if_else(.data$liver_ot >= 2 | .data$liver_ip >= 1, 1, 0, missing = 0),
                     nf_urinary = dplyr::if_else(.data$urinary_ot >= 2 | .data$urinary_ip >= 1, 1, 0, missing = 0))

    dat2 <- dat_comb %>%
      dplyr::select(id, tidyselect::starts_with("nf"))

  }

  # Final Summary ----

    dat3 <- dat2 %>%
      dplyr::group_by(id) %>%
      dplyr::summarize(
        nf_htn = max(.data$nf_htn),
        nf_obesity = max(.data$nf_obesity),
        nf_diabetes = max(.data$nf_diabetes),
        nf_clrd = max(.data$nf_clrd),
        nf_hyperlipid = max(.data$nf_hyperlipid),
        nf_cancer = max(.data$nf_cancer),
        nf_cvd = max(.data$nf_cvd),
        nf_heartfail = max(.data$nf_heartfail),
        nf_anxietydepress = max(.data$nf_anxietydepress),
        nf_arthritis = max(.data$nf_arthritis),
        nf_stroketia = max(.data$nf_stroketia),
        nf_thyroid = max(.data$nf_thyroid),
        nf_ckd = max(.data$nf_ckd),
        nf_osteo = max(.data$nf_osteo),
        nf_dementia = max(.data$nf_dementia),
        nf_musculo = max(.data$nf_musculo),
        nf_stomach = max(.data$nf_stomach),
        nf_colon = max(.data$nf_colon),
        nf_liver = max(.data$nf_liver),
        nf_urinary = max(.data$nf_urinary)) %>%
    dplyr::ungroup()

  dat4 <- dat3 %>%
    dplyr::select(id, tidyselect::starts_with("nf"))

  dat4 <- dat4 %>%
      dplyr::rename(htn = .data$nf_htn,
                     obesity = .data$nf_obesity,
                     diabetes = .data$nf_diabetes,
                     clrd = .data$nf_clrd,
                     hyperlipid = .data$nf_hyperlipid,
                     cancer = .data$nf_cancer,
                     cvd = .data$nf_cvd,
                     heartfail = .data$nf_heartfail,
                     anxietydepress = .data$nf_anxietydepress,
                     arthritis = .data$nf_arthritis,
                     stroketia = .data$nf_stroketia,
                     thyroid = .data$nf_thyroid,
                     ckd = .data$nf_ckd,
                     osteo = .data$nf_osteo,
                     dementia = .data$nf_dementia,
                     musculo = .data$nf_musculo,
                     stomach = .data$nf_stomach,
                     colon = .data$nf_colon,
                     liver = .data$nf_liver,
                     urinary = .data$nf_urinary)

  return(dat4)

}
