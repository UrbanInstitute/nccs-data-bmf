# Add long descriptions and missing NAICS codes to the ntee_code lookup sheet
# (nccs-contracts ADR 0049, BACKLOG Z29).
#
# What it does
#   1. Reads the ntee_code sheet of data/lookup/bmf_code_lookup.xlsx.
#   2. Adds a column ntee_code_description: the paragraph-length description
#      of each code from the Nonprofit Open Data Collective's NTEE table. A
#      copy of that table is saved to data/lookup/nodc_ntee_descriptions.csv
#      so the repo records what was used. Codes the NODC table has no text
#      for are left blank and listed at the end.
#   3. Fills every naics_code that is still UNDEFINED (except Z99, which has
#      no industry) from data/lookup/naics_for_newer_irs_codes.csv, the
#      NAICS 2022 matches reviewed on the website side (nccs PR #101).
#   4. Stamps effective_date on the rows whose NAICS changed and on the new
#      column, then writes the sheet back. Every other sheet is untouched.
#
# Run from the repo root:  Rscript scripts/add_ntee_code_descriptions.R
# Needs network access for the NODC download. Safe to re-run: a second run
# finds nothing to change.

# All functions are namespaced (workspace code convention); nothing is attached.

LOOKUP_PATH        <- "data/lookup/bmf_code_lookup.xlsx"
NAICS_FILL_PATH    <- "data/lookup/naics_for_newer_irs_codes.csv"
NODC_COPY_PATH     <- "data/lookup/nodc_ntee_descriptions.csv"
NODC_URL           <- paste0(
  "https://raw.githubusercontent.com/Nonprofit-Open-Data-Collective/",
  "mission-taxonomies/main/NTEE/all-ntee-original.csv"
)
TODAY_STAMP        <- format(Sys.Date(), "%Y%m%d")
CODES_WITHOUT_NAICS <- c("Z99")

# ---------------------------------------------------------------------------
# 1. Read the current sheet
# ---------------------------------------------------------------------------

ntee_sheet <- openxlsx::read.xlsx(LOOKUP_PATH, sheet = "ntee_code") |>
  tibble::as_tibble() |>
  dplyr::mutate(dplyr::across(dplyr::everything(), as.character))

rows_before <- nrow(ntee_sheet)

# ---------------------------------------------------------------------------
# 2. Long descriptions from the NODC table
# ---------------------------------------------------------------------------

nodc_table <- readr::read_csv(
  NODC_URL,
  col_types = readr::cols(.default = readr::col_character())
)

readr::write_csv(nodc_table, NODC_COPY_PATH)

nodc_descriptions <- nodc_table |>
  dplyr::transmute(
    ntee_code             = toupper(trimws(ntee)),
    ntee_code_description = stringr::str_squish(definition)
  ) |>
  # The NODC table writes the literal word NULL where it has no text (the
  # 24 codes the IRS added in 2021 today).
  dplyr::filter(
    !is.na(ntee_code_description),
    !ntee_code_description %in% c("", "NULL", "NA")
  )

# ---------------------------------------------------------------------------
# 3. NAICS codes for rows still marked UNDEFINED
# ---------------------------------------------------------------------------

naics_fill <- readr::read_csv(
  NAICS_FILL_PATH,
  col_types = readr::cols(.default = readr::col_character())
) |>
  dplyr::select(ntee_code = NTEE_IRS, naics_fill = NAICS)

# ---------------------------------------------------------------------------
# 4. Combine
# ---------------------------------------------------------------------------

updated_sheet <- ntee_sheet |>
  dplyr::select(-dplyr::any_of("ntee_code_description")) |>
  dplyr::left_join(nodc_descriptions, by = "ntee_code") |>
  dplyr::left_join(naics_fill, by = "ntee_code") |>
  dplyr::mutate(
    naics_was_undefined = naics_code == "UNDEFINED" & !ntee_code %in% CODES_WITHOUT_NAICS,
    naics_now_filled    = naics_was_undefined & !is.na(naics_fill),
    naics_code          = dplyr::if_else(naics_now_filled, naics_fill, naics_code),
    effective_date      = dplyr::if_else(naics_now_filled, TODAY_STAMP, effective_date)
  )

still_undefined <- updated_sheet |>
  dplyr::filter(naics_was_undefined, !naics_now_filled) |>
  dplyr::pull(ntee_code)

if (length(still_undefined) > 0) {
  stop(
    "These codes are UNDEFINED and have no row in ", NAICS_FILL_PATH, ": ",
    paste(still_undefined, collapse = ", ")
  )
}

codes_without_description <- updated_sheet |>
  dplyr::filter(is.na(ntee_code_description)) |>
  dplyr::pull(ntee_code)

# Codes with no NODC paragraph keep a blank description rather than stopping
# the run; they are listed at the end so someone can write the text later.

final_sheet <- updated_sheet |>
  dplyr::select(
    ntee_code, naics_code, ntee_code_definition, ntee_code_description, effective_date
  )

stopifnot(nrow(final_sheet) == rows_before)

# ---------------------------------------------------------------------------
# 5. Write the sheet back, leaving every other sheet alone
# ---------------------------------------------------------------------------

workbook <- openxlsx::loadWorkbook(LOOKUP_PATH)
openxlsx::deleteData(
  workbook, sheet = "ntee_code",
  cols = 1:20, rows = 1:(rows_before + 10), gridExpand = TRUE
)
openxlsx::writeData(workbook, sheet = "ntee_code", x = as.data.frame(final_sheet))
openxlsx::saveWorkbook(workbook, LOOKUP_PATH, overwrite = TRUE)

# ---------------------------------------------------------------------------
# 6. Report
# ---------------------------------------------------------------------------

filled_codes <- updated_sheet |>
  dplyr::filter(naics_now_filled) |>
  dplyr::pull(ntee_code)

cat("Rows:", nrow(final_sheet), "\n")
cat("NAICS filled for", length(filled_codes), "codes:", paste(filled_codes, collapse = " "), "\n")
cat("Descriptions present for", sum(!is.na(final_sheet$ntee_code_description)), "of", nrow(final_sheet), "codes\n")
if (length(codes_without_description) > 0) {
  cat("No NODC description (left blank):", paste(codes_without_description, collapse = " "), "\n")
}
cat("Still UNDEFINED by design:", paste(CODES_WITHOUT_NAICS, collapse = " "), "\n")
