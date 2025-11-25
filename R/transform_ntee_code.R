# Functions to transform NTEE Code

create_ntee_code_dim_table <- function(bmf, lookup, input_ntee_col, year){
  # Params
  TARGET_LENGTH <- 4
  SUFFIX_CHAR <- "0"
  # Copy for fault tolerance
  dt <- data.table::copy(bmf)
  dt[, ntee_code_raw := as.character(get(input_ntee_col))]
  # Validation
  ntee_nchar_tbl <- table(nchar(dt[, ntee_code_raw]))
  if (any(names(ntee_nchar_tbl) > 4)){
    stop("Some codes have more than 4 characters. Inspect data.")
  } else {
    message("NTEE Code Character Distribution")
    print(ntee_nchar_tbl)
  }
  # Conversion
  dt[, `:=`(
    ntee_code_1 = ifelse(
      ntee_code_raw != "",
      stringr::str_pad(substr(ntee_code_raw, 1, 3), 
                       width = 3, 
                       side = "right",
                       pad = "0"),
      NA_character_
    ),
    ntee_code_2 = ifelse(
      nchar(ntee_code_raw) == TARGET_LENGTH, 
      paste0(substr(ntee_code_raw, 1, 1), 
             substr(ntee_code_raw, 4, 4), 
             SUFFIX_CHAR),
      NA_character_
    )
  )]
  # SCD table of existing NTEE codes
  dt_long <- data.table::melt(
    dt,
    id.vars = c("ein", "ntee_code_raw"),
    measure.vars = c("ntee_code_1", "ntee_code_2"),
    value.name = "ntee_code_validated"
  )
  dt_long <- dt_long[! is.na(ntee_code_validated)]
  dt_long[lookup,
          `:=`(ntee_code_definition = i.ntee_code_definition,
               ntee_code_general_category = i.ntee_code_general_category),
          on = .(ntee_code_validated = ntee_code)]
  dt_long[, effective_year := year]
  # Create NTEEV2 codes
  dt_long[
    ,
    `:=`(ntee_major_group = substr(ntee_code_raw, 1, 1),
         ntee_digits23 = data.table::fcase(
           substr(ntee_code_raw, 2, 3) == "", "99",
           ! is.na(ntee_code_raw), substr(ntee_code_raw, 2, 3),
           default = NA_character_
         ),
         ntee_digits45 = ifelse(
           nchar(ntee_code_raw) == 4,
           paste0(substr(ntee_code_raw, 4, 4), "0"),
           NA_character_
         ))
  ][
    ,
    nteev2_level_two := data.table::fcase(
      as.integer(ntee_digits23) <= 19  & is.na(ntee_digits45), paste0(ntee_major_group, "00"),
      as.integer(ntee_digits23) <= 19 & ! is.na(ntee_digits45), paste0(ntee_major_group, ntee_digits45),
      ntee_major_group != "", paste0(ntee_major_group, ntee_digits23),
      default = "Z99"
    )
  ][
    ,
    `:=`(nteev2_industry_group = data.table::fcase(
      nteev2_level_two %in% c("B40", "B41", "B42", "B43", "B50"), "UNI",
      nteev2_level_two %in% c("E20", "E21", "E22", "E24"), "HOS",
      ntee_major_group == "A", "ART",
      ntee_major_group == "B", "EDU",
      ntee_major_group %in% c("C", "D"), "ENV",
      ntee_major_group %in% c("E", "F", "G", "H"), "HEL",
      ntee_major_group %in% LETTERS[9:16], "HMS", # I–P
      ntee_major_group == "Q", "IFA",
      ntee_major_group %in% c("R","S","T","U","V","W"), "PSB",
      ntee_major_group == "X", "REL",
      ntee_major_group == "Y", "MMB",
      ntee_major_group == "Z", "UNU",
      default = "UNU"
    ),
    nteev2_org_type = data.table::fcase(
      ntee_digits23 == 1 , "AA",
      ntee_digits23 == 2 , "MT",
      ntee_digits23 == 3 , "PA",
      ntee_digits23 == 5 , "RP",
      ntee_digits23 == 11, "MS",
      ntee_digits23 == 12, "MM",
      ntee_digits23 == 19, "NS",
      default = "RG"
    ))
  ][,
    nteev2 := paste0(
      nteev2_industry_group, 
      "-", 
      nteev2_level_two,
      "-",
      nteev2_org_type
    )
  ]
  # return scd table
  return(dt_long)
}

transform_ntee_code <- function(bmf, ntee_dim_table, lookup, input_ntee_col){
  # Make copies for fault tolerance
  dt <- data.table::copy(bmf)
  data.table::setnames(dt, input_ntee_col, "ntee_code_raw")
  dim <- data.table::copy(ntee_dim_table)
  
  # Concatenate ntee codes with 2 codes, first three characters and last character for the larger group
  ntee_summary <- dim[
    ! is.na(ntee_code_definition), 
    .(ntee_code_definition = paste(ntee_code_definition, collapse = ";"),
      ntee_code_general_category = paste(ntee_code_general_category, collapse = ";")),
    by = .(ein)
  ]
  # lookup join ntee definitions and nteev2
  dt[ntee_summary,
     `:=`(ntee_code_definition = i.ntee_code_definition,
          ntee_code_general_category = i.ntee_code_general_category),
     on = .(ein)]
  dt[ntee_dim_table, nteev2 := i.nteev2, on = .(ein)]
  # Assign invalid codes
  dt[, ntee_code_validated := ifelse(is.na(ntee_code_definition) |
                                     ntee_code_definition == "",
                                     "INVALID",
                                     ntee_code_raw)]
  dt[, nteev2 := ifelse(is.na(nteev2) | ntee_code_validated == "INVALID", 
                        "UNU-Z99-RG", 
                        nteev2)]
  # Output validation
  validated_3_char_ntee_codes <- substr(unique(dt[ntee_code_validated != "INVALID", ntee_code_validated]), 1, 3)
  missing_3_char_ntee_codes <- setdiff(validated_3_char_ntee_codes, lookup[, ntee_code])
  message(
    "The following partially valid NTEE codes were found: ",
    paste(missing_3_char_ntee_codes, collapse = ", ")
  )
  unmapped_ntee <- dt[nteev2 == "INVALID", ntee_code_raw] |> unique()
  if (any(substr(unmapped_ntee, 1, 3) %in% lookup[, ntee_code])){
    message("Warning some valid NTEE codes were not successfully mapped to NTEEV2 format:")
    message(paste(unmapped_ntee, collapse = ", "))
  }
  return(dt)
}