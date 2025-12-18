# Functions to transform NTEE Code

transform_ntee_code <- function(bmf,
                                ntee_code_lookup,
                                ntee_major_group_lookup,
                                activity_code_lookup,
                                input_ntee_col,
                                year,
                                path) {
  VALID_NTEE_CODES <- c("INVALID", "UNDEFINED", ntee_code_lookup$ntee_code)
  HELPER_COLUMNS <- c(".len", ".first", ".last", ".int23", ".char23")
  COLS_TO_FIX <- c("naics_code", "ntee_code_major_group", "ntee_code_definition")
  
  dt <- data.table::copy(bmf)
  dt[, ntee_code_raw := as.character(get(input_ntee_col))]
  dt[, ntee_code_raw := toupper(trimws(ntee_code_raw))]
  # Input validation
  .ntee_raw_validation(dt)
  # Internal helper columns
  dt[, `:=`(
    .len = nchar(ntee_code_raw),
    .first = substr(ntee_code_raw, 1, 1),
    .char23 = substr(ntee_code_raw, 2, 3)
  )]
  dt[, `:=`(
    .last = substr(ntee_code_raw, .len, .len),
    .int23 = as.integer(.char23)
  )]
  # Clean raw ntee codes
  dt[, ntee_code_clean := data.table::fcase(
    .len == 0, "UNDEFINED",
    ! grepl("[A-Z]", .first), "INVALID",
    .len == 1, paste0(ntee_code_raw, "00"),
    .len == 2, paste0(ntee_code_raw, "0"),
    .len == 3, ntee_code_raw,
    .len == 4 & grepl("[A-Z]", .last), substr(ntee_code_raw, 1, 3),
    .len == 4 & grepl("[0-9]", .last), paste0(.first, .last, "0"),
    default = "INVALID"
  )]
  # Assign Invalid NTEE Codes to codes not found in lookup
  dt[! ntee_code_clean %chin% VALID_NTEE_CODES, ntee_code_clean := "INVALID"]
  # NTEE activity codes
  dt[, ntee_activity_code := data.table::fcase(
    .len == 4 & grepl("[A-Z]", .last), paste0(.last, "00"),
    .len == 4 & grepl("[0-9]", .last), substr(ntee_code_raw, 2, 3),
    default = "UNDEFINED"
  )]
  # Primary lookup join for activity code definitions
  dt[activity_code_lookup,
     ntee_activity_code_definition :=  i.ntee_activity_code_definition,
     on = .(ntee_activity_code)]
  # Secondary lookup join for activity codes that are secondary NTEE codes
  # Joined this way to avoid creating a shallow copy
  dt[is.na(ntee_activity_code_definition)][
    ntee_code_lookup, 
    ntee_activity_code_definition := i.ntee_code_definition, 
    on = .(ntee_activity_code = ntee_code)]   
  # Major Group lookup
  dt[ntee_major_group_lookup,
     ntee_code_major_group := i.ntee_code_major_group,
     on = .(first = ntee_code_first_character)]
  # NTEE Code definition lookup
  dt[ntee_code_lookup,
     `:=` (naics_code = i.naics_code,
           ntee_code_definition = i.ntee_code_definition),
     on = .(ntee_code_clean = ntee_code)]
  # Final cleanup of transformed columns 
  dt[ntee_code_clean %chin% c("UNDEFINED", "INVALID"), 
     (cols_to_fix) := "UNDEFINED"]
  # NTEEV2 Code
  dt <- .nteev2_code_transform(dt)
  # Remove redundant columns
  dt[, HELPER_COLUMNS := NULL]
  # SCD Table
  scd <- .create_ntee_scd_table(dt, path, year)
  .ntee_output_validation(scd)
  return(dt)
}

.create_ntee_scd_table <- function(dt, path, year) {
  #' @title SCD Table of NTEE Codes
  #' @note
    #' Since this function writes a dataset, we should not include it with the transformation function. This violates the principle of keeping functions pure.
  scd <- dt[, .(
    ein,
    ntee_code_raw,
    ntee_code_clean,
    ntee_code_definition,
    ntee_major_group,
    ntee_activity_code,
    nteev2_code,
    nteev2_subsector,
    nteev2_org_type
  )]
  scd[, effective_year := year]
  data.table::fwrite(scd, path)
  return(scd)
}

.ntee_raw_validation <- function(dt){
  ntee_nchar_tbl <- table(nchar(dt[, ntee_code_raw]))
  if (any(names(ntee_nchar_tbl) > 4)){
    stop("Some codes have more than 4 characters. Inspect data.")
  } else {
    message("NTEE Code Character Distribution")
    print(ntee_nchar_tbl)
  }
  return(NULL)
}

.nteev2_code_transform <- function(dt){
  # NTEEV2 Code
  dt[, nteev2_code := data.table::fcase(
    .len == 3 & .int23 <= 19, paste0(.first, "00"),
    .len == 4 & .int23 <= 19, paste0(.first, .last, "0"),
    default = ntee_code_clean
  )]
  # NTEEV2 Subsector
  dt[, nteev2_subsector := data.table::fcase(
    nteev2_code %in% c("B40", "B41", "B42", "B43", "B50"), "UNI",
    nteev2_code %in% c("E20", "E21", "E22", "E24"), "HOS",
    .first == "A", "ART",
    .first == "B", "EDU",
    .first %chin% c("C", "D"), "ENV",
    .first %chin% c("E", "F", "G", "H"), "HEL",
    .first %chin% LETTERS[9:16], "HMS", # I–P
    .first == "Q", "IFA",
    .first %chin% c("R","S","T","U","V","W"), "PSB",
    .first == "X", "REL",
    .first == "Y", "MMB",
    .first == "Z", "UNU",
    default = "UNDEFINED"
  )]
  # NTEEV2 organization type
  dt[, nteev2_org_type = data.table::fcase(
    .char23 == "01" , "AA",
    .char23 == "02" , "MT",
    .char23 == "03" , "PA",
    .char23 == "05" , "RP",
    .char23 == "11", "MS",
    .char23 == "12", "MM",
    .char23 == "19", "NS",
    default = "RG"
  )]
  # NTEEV2 full code
  dt[, nteev2 := paste0(nteev2_subsector, nteev2_code, nteev2_org_type, sep = "-")]
  return(dt)
}

.ntee_output_validation <- function(scd){
  # Output validation
  total_rows <- nrow(scd)
  undefined_count <- nrow(scd[ntee_code_clean == "UNDEFINED"])
  invalid_count <- nrow(scd[ntee_code_clean == "INVALID"])

  message("--- NTEE Code Mapping Quality Report ---")
  message(sprintf("Total Records: %s", format(total_rows, big.mark=",")))
  message(sprintf("Undefined NTEE codes: %d (%0.2f%%)", 
                  undefined_count, 
                  (undefined_count/total_rows)*100))
  message(sprintf("Invalid NTEE Codes: %d (%0.2f%%)",
                  invalid_count, 
                  (invalid_count/total_rows)*100))
}