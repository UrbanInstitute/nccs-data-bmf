# ============================================================================
# address.R
# Transform STREET, CITY, STATE, ZIP columns to standardized address fields
# ============================================================================

# ============================================================================
# Module Constants
# ============================================================================

#' Street type abbreviations (USPS Publication 28)
#' @noRd
STREET_TYPE_MAP <- data.table::data.table(
  pattern = c(
    "ALLEY", "ANNEX", "ARCADE", "AVENUE", "BAYOU", "BEACH", "BEND", "BLUFF",
    "BOTTOM", "BOULEVARD", "BRANCH", "BRIDGE", "BROOK", "BURG", "BYPASS",
    "CAMP", "CANYON", "CAPE", "CAUSEWAY", "CENTER", "CIRCLE", "CLIFF",
    "CLUB", "COMMON", "CORNER", "CORNERS", "COURSE", "COURT", "COURTS",
    "COVE", "CREEK", "CRESCENT", "CREST", "CROSSING", "CROSSROAD", "CURVE",
    "DALE", "DAM", "DIVIDE", "DRIVE", "ESTATE", "ESTATES", "EXPRESSWAY",
    "EXTENSION", "FALL", "FALLS", "FERRY", "FIELD", "FIELDS", "FLAT",
    "FLATS", "FORD", "FOREST", "FORGE", "FORK", "FORKS", "FORT", "FREEWAY",
    "GARDEN", "GARDENS", "GATEWAY", "GLEN", "GREEN", "GROVE", "HARBOR",
    "HAVEN", "HEIGHTS", "HIGHWAY", "HILL", "HILLS", "HOLLOW", "INLET",
    "ISLAND", "ISLANDS", "ISLE", "JUNCTION", "KEY", "KNOLL", "LAKE",
    "LAKES", "LAND", "LANDING", "LANE", "LIGHT", "LOAF", "LOCK", "LOCKS",
    "LODGE", "LOOP", "MALL", "MANOR", "MEADOW", "MEADOWS", "MEWS", "MILL",
    "MILLS", "MISSION", "MOTORWAY", "MOUNT", "MOUNTAIN", "NECK", "ORCHARD",
    "OVAL", "OVERPASS", "PARK", "PARKWAY", "PASS", "PASSAGE", "PATH",
    "PIKE", "PINE", "PINES", "PLACE", "PLAIN", "PLAINS", "PLAZA", "POINT",
    "POINTS", "PORT", "PRAIRIE", "RADIAL", "RAMP", "RANCH", "RAPID",
    "RAPIDS", "REST", "RIDGE", "RIDGES", "RIVER", "ROAD", "ROADS", "ROUTE",
    "ROW", "RUN", "SHOAL", "SHOALS", "SHORE", "SHORES", "SKYWAY", "SPRING",
    "SPRINGS", "SPUR", "SQUARE", "SQUARES", "STATION", "STRAVENUE",
    "STREAM", "STREET", "STREETS", "SUMMIT", "TERRACE", "THROUGHWAY",
    "TRACE", "TRACK", "TRAFFICWAY", "TRAIL", "TRAILER", "TUNNEL",
    "TURNPIKE", "UNDERPASS", "UNION", "VALLEY", "VIADUCT", "VIEW", "VIEWS",
    "VILLAGE", "VILLE", "VISTA", "WALK", "WALL", "WAY", "WAYS", "WELL",
    "WELLS"
  ),
  abbrev = c(
    "ALY", "ANX", "ARC", "AVE", "BYU", "BCH", "BND", "BLF",
    "BTM", "BLVD", "BR", "BRG", "BRK", "BG", "BYP",
    "CP", "CYN", "CPE", "CSWY", "CTR", "CIR", "CLF",
    "CLB", "CMN", "COR", "CORS", "CRSE", "CT", "CTS",
    "CV", "CRK", "CRES", "CRST", "XING", "XRD", "CURV",
    "DL", "DM", "DV", "DR", "EST", "ESTS", "EXPY",
    "EXT", "FALL", "FLS", "FRY", "FLD", "FLDS", "FLT",
    "FLTS", "FRD", "FRST", "FRG", "FRK", "FRKS", "FT", "FWY",
    "GDN", "GDNS", "GTWY", "GLN", "GRN", "GRV", "HBR",
    "HVN", "HTS", "HWY", "HL", "HLS", "HOLW", "INLT",
    "IS", "ISS", "ISLE", "JCT", "KY", "KNL", "LK",
    "LKS", "LAND", "LNDG", "LN", "LGT", "LF", "LCK", "LCKS",
    "LDG", "LOOP", "MALL", "MNR", "MDW", "MDWS", "MEWS", "ML",
    "MLS", "MSN", "MTWY", "MT", "MTN", "NCK", "ORCH",
    "OVAL", "OPAS", "PARK", "PKWY", "PASS", "PSGE", "PATH",
    "PIKE", "PNE", "PNES", "PL", "PLN", "PLNS", "PLZ", "PT",
    "PTS", "PRT", "PR", "RADL", "RAMP", "RNCH", "RPD",
    "RPDS", "RST", "RDG", "RDGS", "RIV", "RD", "RDS", "RTE",
    "ROW", "RUN", "SHL", "SHLS", "SHR", "SHRS", "SKWY", "SPG",
    "SPGS", "SPUR", "SQ", "SQS", "STA", "STRA",
    "STRM", "ST", "STS", "SMT", "TER", "TRWY",
    "TRCE", "TRAK", "TRFY", "TRL", "TRLR", "TUNL",
    "TPKE", "UPAS", "UN", "VLY", "VIA", "VW", "VWS",
    "VLG", "VL", "VIS", "WALK", "WALL", "WAY", "WAYS", "WL",
    "WLS"
  )
)

#' Directional abbreviations (USPS)
#' @noRd
DIRECTIONAL_MAP <- data.table::data.table(
  pattern = c("NORTH", "SOUTH", "EAST", "WEST",
              "NORTHEAST", "NORTHWEST", "SOUTHEAST", "SOUTHWEST"),
  abbrev = c("N", "S", "E", "W", "NE", "NW", "SE", "SW")
)

#' Unit type abbreviations (USPS)
#' @noRd
UNIT_TYPE_MAP <- data.table::data.table(
  pattern = c("APARTMENT", "BASEMENT", "BUILDING", "DEPARTMENT", "FLOOR",
              "FRONT", "HANGAR", "LOBBY", "LOT", "LOWER", "OFFICE",
              "PENTHOUSE", "PIER", "REAR", "ROOM", "SIDE", "SLIP", "SPACE",
              "STOP", "SUITE", "TRAILER", "UNIT", "UPPER"),
  abbrev = c("APT", "BSMT", "BLDG", "DEPT", "FL",
             "FRNT", "HNGR", "LBBY", "LOT", "LOWR", "OFC",
             "PH", "PIER", "REAR", "RM", "SIDE", "SLIP", "SPC",
             "STOP", "STE", "TRLR", "UNIT", "UPPR")
)

#' US State and Territory abbreviations
#' @noRd
US_STATE_ABBREV <- data.table::data.table(
  name = c(
    "ALABAMA", "ALASKA", "ARIZONA", "ARKANSAS", "CALIFORNIA", "COLORADO",
    "CONNECTICUT", "DELAWARE", "FLORIDA", "GEORGIA", "HAWAII", "IDAHO",
    "ILLINOIS", "INDIANA", "IOWA", "KANSAS", "KENTUCKY", "LOUISIANA",
    "MAINE", "MARYLAND", "MASSACHUSETTS", "MICHIGAN", "MINNESOTA",
    "MISSISSIPPI", "MISSOURI", "MONTANA", "NEBRASKA", "NEVADA",
    "NEW HAMPSHIRE", "NEW JERSEY", "NEW MEXICO", "NEW YORK",
    "NORTH CAROLINA", "NORTH DAKOTA", "OHIO", "OKLAHOMA", "OREGON",
    "PENNSYLVANIA", "RHODE ISLAND", "SOUTH CAROLINA", "SOUTH DAKOTA",
    "TENNESSEE", "TEXAS", "UTAH", "VERMONT", "VIRGINIA", "WASHINGTON",
    "WEST VIRGINIA", "WISCONSIN", "WYOMING", "DISTRICT OF COLUMBIA",
    # Territories
    "AMERICAN SAMOA", "GUAM", "NORTHERN MARIANA ISLANDS", "PUERTO RICO",
    "VIRGIN ISLANDS", "FEDERATED STATES OF MICRONESIA", "MARSHALL ISLANDS",
    "PALAU", "ARMED FORCES AMERICAS", "ARMED FORCES EUROPE",
    "ARMED FORCES PACIFIC"
  ),
  abbrev = c(
    "AL", "AK", "AZ", "AR", "CA", "CO", "CT", "DE", "FL", "GA", "HI", "ID",
    "IL", "IN", "IA", "KS", "KY", "LA", "ME", "MD", "MA", "MI", "MN", "MS",
    "MO", "MT", "NE", "NV", "NH", "NJ", "NM", "NY", "NC", "ND", "OH", "OK",
    "OR", "PA", "RI", "SC", "SD", "TN", "TX", "UT", "VT", "VA", "WA", "WV",
    "WI", "WY", "DC", "AS", "GU", "MP", "PR", "VI", "FM", "MH", "PW", "AA",
    "AE", "AP"
  )
)

#' Valid 2-letter state/territory codes
#' @noRd
VALID_STATE_CODES <- US_STATE_ABBREV$abbrev

#' Output column names for address transformation
#' @noRd
ADDRESS_OUTPUT_COLS <- c(
  # Raw preservation
  "org_addr_street_raw",
  "org_addr_city_raw",
  "org_addr_state_raw",

  "org_addr_zip_raw",
  # Cleaned components
 "org_addr_street",
  "org_addr_city",
  "org_addr_state",
  "org_addr_zip5",
  "org_addr_zip4",
  "org_addr_zip",
  # Full address
  "org_addr_full",
  # Quality flags
  "org_addr_is_missing",
  "org_addr_is_po_box",
  "org_addr_is_rural_route",
  "org_addr_has_special_chars",
  "org_addr_missing_number",
  "org_addr_state_invalid"
)

# ============================================================================
# Internal Helper Functions
# ============================================================================

#' Apply pattern-based abbreviations to a vector
#'
#' @param text_vector Character vector to process
#' @param pattern_map data.table with 'pattern' and 'abbrev' columns
#' @param word_boundary If TRUE, only match whole words
#'
#' @return Character vector with patterns replaced by abbreviations
#' @noRd
.apply_abbreviations <- function(text_vector, pattern_map, word_boundary = TRUE) {
  result <- text_vector

  for (i in seq_len(nrow(pattern_map))) {
    pattern <- pattern_map$pattern[i]
    replacement <- pattern_map$abbrev[i]

    if (word_boundary) {
      # Match whole words only (with word boundaries)
      regex_pattern <- paste0("\\b", pattern, "\\b")
    } else {
      regex_pattern <- pattern
    }

    result <- stringr::str_replace_all(
      result,
      stringr::regex(regex_pattern, ignore_case = TRUE),
      replacement
    )
  }

  return(result)
}

#' Clean and standardize street address
#'
#' @param street_vector Character vector of raw street addresses
#'
#' @return Character vector of USPS-standardized street addresses
#' @noRd
.clean_street <- function(street_vector) {
  result <- street_vector

 # 1. Convert to uppercase
  result <- toupper(result)

  # 2. Remove extra whitespace
  result <- stringr::str_squish(result)

  # 3. Standardize common special character patterns
  # Replace # with space (unit indicator)
  result <- stringr::str_replace_all(result, "#", " ")
  # Remove other problematic characters but keep alphanumeric, space, dash, comma, period
  result <- stringr::str_replace_all(result, "[^A-Z0-9\\s\\-,\\./]", "")

  # 4. Standardize directionals (NORTH -> N, etc.)
  # Do this first to handle "123 NORTH MAIN STREET" -> "123 N MAIN ST"
  result <- .apply_abbreviations(result, DIRECTIONAL_MAP, word_boundary = TRUE)

  # 5. Standardize street types (STREET -> ST, AVENUE -> AVE, etc.)
  result <- .apply_abbreviations(result, STREET_TYPE_MAP, word_boundary = TRUE)

  # 6. Standardize unit types (APARTMENT -> APT, SUITE -> STE, etc.)
  result <- .apply_abbreviations(result, UNIT_TYPE_MAP, word_boundary = TRUE)

  # 7. Final whitespace cleanup
  result <- stringr::str_squish(result)

  # 8. Handle empty strings
  result[result == ""] <- NA_character_

  return(result)
}

#' Clean city name
#'
#' @param city_vector Character vector of raw city names
#'
#' @return Character vector of cleaned city names
#' @noRd
.clean_city <- function(city_vector) {
  result <- city_vector

  # 1. Convert to uppercase
  result <- toupper(result)

  # 2. Remove extra whitespace
  result <- stringr::str_squish(result)

  # 3. Remove trailing state abbreviations (common data entry error)
  # Pattern: ", XX" or " XX" at end where XX is 2 letters
  result <- stringr::str_remove(result, ",?\\s+[A-Z]{2}$")

  # 4. Remove non-alphanumeric except spaces and hyphens
  result <- stringr::str_replace_all(result, "[^A-Z0-9\\s\\-]", "")

  # 5. Final cleanup
  result <- stringr::str_squish(result)

  # 6. Handle empty strings
  result[result == ""] <- NA_character_

  return(result)
}

#' Standardize state to 2-letter abbreviation
#'
#' @param state_vector Character vector of raw state values
#'
#' @return Character vector of 2-letter state abbreviations (NA for invalid)
#' @noRd
.standardize_state <- function(state_vector) {
  result <- toupper(stringr::str_squish(state_vector))

  # Check if already a valid 2-letter code
  is_valid_code <- result %in% VALID_STATE_CODES

  # For those that aren't valid codes, try to match full name
  needs_lookup <- !is_valid_code & !is.na(result)

  if (any(needs_lookup)) {
    # Create lookup vector
    state_lookup <- stats::setNames(US_STATE_ABBREV$abbrev, US_STATE_ABBREV$name)

    # Apply lookup for full names
    result[needs_lookup] <- state_lookup[result[needs_lookup]]
  }

  # Anything that's not a valid code after lookup becomes NA
  result[!result %in% VALID_STATE_CODES & !is.na(result)] <- NA_character_

  return(result)
}

#' Clean and parse ZIP code
#'
#' @param zip_vector Character vector of raw ZIP codes
#'
#' @return data.table with columns: zip5, zip4, zip_full
#' @noRd
.clean_zip <- function(zip_vector) {
  # Convert to character and clean
  raw <- as.character(zip_vector)

  # Remove all non-numeric characters except dash
  clean <- stringr::str_replace_all(raw, "[^0-9\\-]", "")

  # Extract ZIP5 (first 5 digits)
  zip5 <- stringr::str_extract(clean, "^\\d{5}")

  # Extract ZIP4 (digits after dash or positions 6-9)
  # Handle both "12345-6789" and "123456789" formats
  zip4 <- ifelse(
    stringr::str_detect(clean, "\\d{5}-\\d{4}"),
    stringr::str_extract(clean, "(?<=-)\\d{4}"),
    ifelse(
      stringr::str_detect(clean, "^\\d{9}"),
      stringr::str_sub(clean, 6, 9),
      NA_character_
    )
  )

  # Build full ZIP
  zip_full <- ifelse(
    !is.na(zip4),
    paste0(zip5, "-", zip4),
    zip5
  )

  return(data.table::data.table(
    zip5 = zip5,
    zip4 = zip4,
    zip_full = zip_full
  ))
}

#' Detect P.O. Box addresses
#'
#' @param street_vector Character vector of street addresses
#'
#' @return Logical vector (TRUE if P.O. Box detected)
#' @noRd
.detect_po_box <- function(street_vector) {
  # Patterns for P.O. Box variations
  po_box_pattern <- stringr::regex(
    "\\b(P\\.?\\s*O\\.?\\s*BOX|POST\\s*OFFICE\\s*BOX|POB\\s*\\d)\\b",
    ignore_case = TRUE
  )

  return(stringr::str_detect(street_vector, po_box_pattern) %in% TRUE)
}

#' Detect rural route addresses
#'
#' @param street_vector Character vector of street addresses
#'
#' @return Logical vector (TRUE if rural route detected)
#' @noRd
.detect_rural_route <- function(street_vector) {
  # Patterns for rural routes and highway contracts
  rural_pattern <- stringr::regex(
    "\\b(RR\\s*\\d|RURAL\\s*ROUTE|HC\\s*\\d|HIGHWAY\\s*CONTRACT|STAR\\s*ROUTE)\\b",
    ignore_case = TRUE
  )

  return(stringr::str_detect(street_vector, rural_pattern) %in% TRUE)
}

#' Build full concatenated address
#'
#' @param street Character vector of cleaned street addresses
#' @param city Character vector of cleaned city names
#' @param state Character vector of state abbreviations
#' @param zip Character vector of ZIP codes
#'
#' @return Character vector of full addresses
#' @noRd
.build_full_address <- function(street, city, state, zip) {
  # Handle NAs gracefully
  street_part <- ifelse(is.na(street), "", street)
  city_part <- ifelse(is.na(city), "", city)
  state_part <- ifelse(is.na(state), "", state)
  zip_part <- ifelse(is.na(zip), "", zip)

  # Build "STREET, CITY, STATE ZIP" format
  # Handle missing components
  city_state_zip <- paste0(
    city_part,
    ifelse(city_part != "" & state_part != "", ", ", ""),
    state_part,
    ifelse((city_part != "" | state_part != "") & zip_part != "", " ", ""),
    zip_part
  )

  full_addr <- paste0(
    street_part,
    ifelse(street_part != "" & city_state_zip != "", ", ", ""),
    city_state_zip
  )

  # Clean up any double commas or extra spaces
  full_addr <- stringr::str_replace_all(full_addr, ",\\s*,", ",")
  full_addr <- stringr::str_squish(full_addr)

  # Empty strings become NA
  full_addr[full_addr == ""] <- NA_character_

  return(full_addr)
}

# ============================================================================
# Main Transformation Functions
# ============================================================================

#' Transform Address Columns to Standardized Format
#'
#' @description
#' Transforms raw STREET, CITY, STATE, and ZIP columns into USPS-standardized
#' address fields suitable for geocoding. Produces cleaned component fields,
#' a concatenated full address, and detailed quality flags.
#'
#' @param dt data.table containing BMF data with address columns
#' @param street_col character name of street column (default: "STREET")
#' @param city_col character name of city column (default: "CITY")
#' @param state_col character name of state column (default: "STATE")
#' @param zip_col character name of ZIP column (default: "ZIP")
#'
#' @return data.table with new address columns:
#'   \describe{
#'     \item{org_addr_street_raw}{Original street value}
#'     \item{org_addr_city_raw}{Original city value}
#'     \item{org_addr_state_raw}{Original state value}
#'     \item{org_addr_zip_raw}{Original ZIP value}
#'     \item{org_addr_street}{USPS-standardized street}
#'     \item{org_addr_city}{Cleaned city name}
#'     \item{org_addr_state}{2-letter state abbreviation}
#'     \item{org_addr_zip5}{5-digit ZIP code}
#'     \item{org_addr_zip4}{4-digit ZIP extension (if present)}
#'     \item{org_addr_zip}{Full ZIP (5 or 9 digit)}
#'     \item{org_addr_full}{Concatenated full address}
#'     \item{org_addr_is_missing}{TRUE if street is NA/empty}
#'     \item{org_addr_is_po_box}{TRUE if P.O. Box address}
#'     \item{org_addr_is_rural_route}{TRUE if rural route address}
#'     \item{org_addr_has_special_chars}{TRUE if original had special chars}
#'     \item{org_addr_missing_number}{TRUE if street has no digits}
#'     \item{org_addr_state_invalid}{TRUE if state couldn't be standardized}
#'   }
#'
#' @examples
#' \dontrun{
#' # Basic transformation
#' bmf <- transform_address(bmf_raw)
#'
#' # With custom column names
#' bmf <- transform_address(bmf_raw, street_col = "ADDRESS", zip_col = "ZIPCODE")
#' }
#'
#' @export
transform_address <- function(dt,
                              street_col = "STREET",
                              city_col = "CITY",
                              state_col = "STATE",
                              zip_col = "ZIP") {

  # Input validation
  required_cols <- c(street_col, city_col, state_col, zip_col)
  validate_data_table(dt, required_cols, context = "BMF data")

  # Safe copy to avoid side effects
  dt_safe <- data.table::copy(dt)

  # ---- Preserve raw values ----
  dt_safe[, org_addr_street_raw := as.character(get(street_col))]
  dt_safe[, org_addr_city_raw := as.character(get(city_col))]
  dt_safe[, org_addr_state_raw := as.character(get(state_col))]
  dt_safe[, org_addr_zip_raw := as.character(get(zip_col))]

  # ---- Quality flags (based on raw values) ----
  # Detect special characters BEFORE cleaning
  dt_safe[, org_addr_has_special_chars := grepl("[#@&%*]", org_addr_street_raw)]

  # Detect P.O. Box and rural routes from raw
  dt_safe[, org_addr_is_po_box := .detect_po_box(org_addr_street_raw)]
  dt_safe[, org_addr_is_rural_route := .detect_rural_route(org_addr_street_raw)]

  # ---- Clean components ----
  dt_safe[, org_addr_street := .clean_street(org_addr_street_raw)]
  dt_safe[, org_addr_city := .clean_city(org_addr_city_raw)]
  dt_safe[, org_addr_state := .standardize_state(org_addr_state_raw)]

  # ZIP cleaning returns multiple columns
  zip_result <- .clean_zip(dt_safe$org_addr_zip_raw)
  dt_safe[, org_addr_zip5 := zip_result$zip5]
  dt_safe[, org_addr_zip4 := zip_result$zip4]
  dt_safe[, org_addr_zip := zip_result$zip_full]

  # ---- Build full address ----
  dt_safe[, org_addr_full := .build_full_address(
    org_addr_street, org_addr_city, org_addr_state, org_addr_zip
  )]

  # ---- Additional quality flags ----
  # Missing address (street is NA or empty)
  dt_safe[, org_addr_is_missing := is.na(org_addr_street) | org_addr_street == ""]

  # Missing street number (no digits in cleaned street)
  dt_safe[, org_addr_missing_number := !is.na(org_addr_street) &
            !grepl("\\d", org_addr_street)]

  # Invalid state (raw had value but couldn't standardize)
  dt_safe[, org_addr_state_invalid := !is.na(org_addr_state_raw) &
            org_addr_state_raw != "" &
            is.na(org_addr_state)]

  # ---- Quality report ----
  total_rows <- nrow(dt_safe)

  missing_count <- dt_safe[org_addr_is_missing == TRUE, .N]
  po_box_count <- dt_safe[org_addr_is_po_box == TRUE, .N]
  rural_count <- dt_safe[org_addr_is_rural_route == TRUE, .N]
  special_char_count <- dt_safe[org_addr_has_special_chars == TRUE, .N]
  missing_number_count <- dt_safe[org_addr_missing_number == TRUE, .N]
  invalid_state_count <- dt_safe[org_addr_state_invalid == TRUE, .N]

  message(sprintf(
    "Address transformation complete: %s records processed",
    format(total_rows, big.mark = ",")
  ))
  message(sprintf(
    "  - Missing addresses: %s (%.1f%%)",
    format(missing_count, big.mark = ","),
    100 * missing_count / total_rows
  ))
  message(sprintf(
    "  - P.O. Box addresses: %s (%.1f%%)",
    format(po_box_count, big.mark = ","),
    100 * po_box_count / total_rows
  ))
  message(sprintf(
    "  - Rural route addresses: %s (%.1f%%)",
    format(rural_count, big.mark = ","),
    100 * rural_count / total_rows
  ))
  message(sprintf(
    "  - Special characters detected: %s (%.1f%%)",
    format(special_char_count, big.mark = ","),
    100 * special_char_count / total_rows
  ))
  message(sprintf(
    "  - Missing street numbers: %s (%.1f%%)",
    format(missing_number_count, big.mark = ","),
    100 * missing_number_count / total_rows
  ))
  message(sprintf(
    "  - Invalid state codes: %s (%.1f%%)",
    format(invalid_state_count, big.mark = ","),
    100 * invalid_state_count / total_rows
  ))

  return(dt_safe)
}
