# ============================================================================
# unified_bmf_org_columns.R
# Add the derived organization columns to the finished Unified BMF
# (ADR 0058 and ADR 0059)
#
# Terms used in this file, each with one meaning:
#
#   Unified BMF table  The one-row-per-EIN table that build_master_bmf()
#                      leaves in the DuckDB database, named bmf_master.
#   derived columns    The six columns this file adds at the end of the
#                      Unified BMF table. Three value columns, each followed
#                      by its words column.
#   value column       A derived column that holds a short code:
#                      nccs_org_role (O, S, M), nccs_org_legal_type (PC, PF,
#                      O) and nccs_org_field (17 two-letter values). Missing
#                      when unknown.
#   words column       The column that holds the same value in words. Its
#                      name is the value column's name plus "_definition".
#   input columns      The columns of the Unified BMF table the derived
#                      columns are worked out from, plus ein to match rows.
#
# Each rule lives in one place: R/nccs_org_role.R, R/nccs_org_legal_type.R
# and R/nccs_org_field.R. This file reads the input columns into R, calls
# those rules, and writes the derived columns back. Load the three rule
# files before this one.
# ============================================================================

# ============================================================================
# 1. Constants
# ============================================================================

UNIFIED_BMF_TABLE_NAME <- "bmf_master"

UNIFIED_BMF_ORG_INPUT_COLUMNS <- c(
  "ein",
  "ntee_code_raw",
  "subsection_code",
  "foundation_code"
)

# The value columns, in the order they sit in the file, each with its
# allowed values and their words.
UNIFIED_BMF_ORG_VALUE_LABELS <- list(
  nccs_org_role = NCCS_ORG_ROLE_LABELS,
  nccs_org_legal_type = NCCS_ORG_LEGAL_TYPE_LABELS,
  nccs_org_field = NCCS_ORG_FIELD_LABELS
)

# All six derived columns, in the order they sit in the file.
UNIFIED_BMF_ORG_COLUMNS <- c(
  "nccs_org_role",
  "nccs_org_role_definition",
  "nccs_org_legal_type",
  "nccs_org_legal_type_definition",
  "nccs_org_field",
  "nccs_org_field_definition"
)

# Short-lived names used inside the database while the columns are added.
UNIFIED_BMF_ORG_COLUMNS_BY_EIN_NAME <- "nccs_org_columns_by_ein"
UNIFIED_BMF_WITH_ORG_COLUMNS_NAME <- "bmf_master_with_org_columns"

# ============================================================================
# 2. Checks before anything changes
# ============================================================================

#' Stop unless the Unified BMF table can take the derived columns
#'
#' @details
#' Two things must hold: the table has every input column, and it has one
#' row per EIN. Without the second, matching the derived columns back by
#' EIN would repeat rows. Nothing in the database is changed by this
#' function.
#'
#' @param con DBI connection to the DuckDB database that holds the table.
#' @param table_name name of the Unified BMF table.
#'
#' @return the number of rows in the table, as an integer.
#'
#' @keywords internal
check_unified_bmf_ready_for_org_columns <- function(con, table_name) {
  existing_columns <- DBI::dbListFields(con, table_name)

  missing_input_columns <- setdiff(
    UNIFIED_BMF_ORG_INPUT_COLUMNS,
    existing_columns
  )

  if (length(missing_input_columns) > 0) {
    rlang::abort(paste0(
      "The Unified BMF table is missing the column(s) the derived columns are worked out from: ",
      paste(missing_input_columns, collapse = ", ")
    ))
  }

  quoted_table_name <- DBI::dbQuoteIdentifier(con, table_name)

  row_counts <- DBI::dbGetQuery(con, sprintf(
    "SELECT COUNT(*) AS row_count, COUNT(DISTINCT ein) AS ein_count FROM %s",
    quoted_table_name
  ))

  if (row_counts$row_count != row_counts$ein_count) {
    rlang::abort(sprintf(
      "The Unified BMF table must have one row per EIN; it has %s rows and %s EINs.",
      format(row_counts$row_count, big.mark = ","),
      format(row_counts$ein_count, big.mark = ",")
    ))
  }

  row_count <- as.integer(row_counts$row_count)

  return(row_count)
}

# ============================================================================
# 3. Deriving the columns for every EIN
# ============================================================================

#' Derive the role, legal type and field of every organization
#'
#' @details
#' Reads only the input columns into R and applies the three rules:
#' [derive_nccs_org_role()] with its default definition (the one used by
#' the older NCCS files from September 2017 on),
#' [derive_nccs_org_legal_type()] and [derive_nccs_org_field()]. Each value
#' is put in words with [look_up_nccs_org_label()].
#'
#' @param con DBI connection to the DuckDB database that holds the table.
#' @param table_name name of the Unified BMF table.
#'
#' @return tibble with one row per EIN: `ein`, then the six derived columns
#'   in the order of `UNIFIED_BMF_ORG_COLUMNS`.
#'
#' @keywords internal
derive_org_columns_by_ein <- function(con, table_name) {
  quoted_table_name <- DBI::dbQuoteIdentifier(con, table_name)

  quoted_input_columns <- paste(
    DBI::dbQuoteIdentifier(con, UNIFIED_BMF_ORG_INPUT_COLUMNS),
    collapse = ", "
  )

  input_rows <- DBI::dbGetQuery(con, sprintf(
    "SELECT %s FROM %s",
    quoted_input_columns,
    quoted_table_name
  ))

  # The three rules.
  org_role <- derive_nccs_org_role(
    ntee_code = input_rows$ntee_code_raw,
    subsection_code = input_rows$subsection_code,
    foundation_code = input_rows$foundation_code
  )

  org_legal_type <- derive_nccs_org_legal_type(
    subsection_code = input_rows$subsection_code,
    foundation_code = input_rows$foundation_code
  )

  org_field <- derive_nccs_org_field(input_rows$ntee_code_raw)

  # Each value, then the same value in words.
  org_columns_by_ein <- dplyr::tibble(
    ein = input_rows$ein,
    nccs_org_role = org_role,
    nccs_org_role_definition = look_up_nccs_org_label(
      org_role,
      NCCS_ORG_ROLE_LABELS
    ),
    nccs_org_legal_type = org_legal_type,
    nccs_org_legal_type_definition = look_up_nccs_org_label(
      org_legal_type,
      NCCS_ORG_LEGAL_TYPE_LABELS
    ),
    nccs_org_field = org_field,
    nccs_org_field_definition = look_up_nccs_org_label(
      org_field,
      NCCS_ORG_FIELD_LABELS
    )
  )

  return(org_columns_by_ein)
}

# ============================================================================
# 4. The step the build calls
# ============================================================================

#' Write the SQL that copies the Unified BMF table with the derived columns added
#'
#' @details
#' The copy keeps every existing column in its existing order, leaves out
#' derived columns from an earlier run, and puts the six derived columns
#' last. Rows are matched to their derived columns by EIN.
#'
#' @param con DBI connection to the DuckDB database that holds the table.
#' @param table_name name of the Unified BMF table.
#'
#' @return one SQL statement, as a character string.
#'
#' @keywords internal
build_table_with_org_columns_sql <- function(con, table_name) {
  existing_columns <- DBI::dbListFields(con, table_name)

  columns_to_keep <- setdiff(existing_columns, UNIFIED_BMF_ORG_COLUMNS)

  quoted_columns_to_keep <- paste0(
    "unified_bmf.",
    DBI::dbQuoteIdentifier(con, columns_to_keep),
    collapse = ", "
  )

  quoted_derived_columns <- paste0(
    "org_columns.",
    DBI::dbQuoteIdentifier(con, UNIFIED_BMF_ORG_COLUMNS),
    collapse = ", "
  )

  create_table_sql <- sprintf(
    "CREATE TABLE %s AS
     SELECT %s, %s
       FROM %s AS unified_bmf
       LEFT JOIN %s AS org_columns
         ON unified_bmf.ein = org_columns.ein",
    DBI::dbQuoteIdentifier(con, UNIFIED_BMF_WITH_ORG_COLUMNS_NAME),
    quoted_columns_to_keep,
    quoted_derived_columns,
    DBI::dbQuoteIdentifier(con, table_name),
    DBI::dbQuoteIdentifier(con, UNIFIED_BMF_ORG_COLUMNS_BY_EIN_NAME)
  )

  return(create_table_sql)
}

#' Add the derived organization columns to the Unified BMF table
#'
#' @details
#' Replaces the Unified BMF table with a copy that has every existing
#' column in its existing order, followed by the six derived columns:
#' `nccs_org_role` and `nccs_org_role_definition` (ADR 0058), then
#' `nccs_org_legal_type`, `nccs_org_legal_type_definition`,
#' `nccs_org_field` and `nccs_org_field_definition` (ADR 0059). No row is
#' added or dropped and no existing value changes.
#'
#' Safe to run twice: derived columns left by an earlier run are replaced,
#' not duplicated. The replacement happens inside one database transaction,
#' so a failure part-way leaves the table as it was.
#'
#' Stops, changing nothing, if the table lacks an input column or does not
#' have one row per EIN.
#'
#' @param con DBI connection to the DuckDB database that holds the table.
#' @param table_name name of the Unified BMF table. Defaults to the name
#'   `build_master_bmf()` uses.
#'
#' @return the number of organizations in the table, as an integer.
#'
#' @export
add_nccs_org_columns_to_unified_bmf <- function(con,
                                                table_name = UNIFIED_BMF_TABLE_NAME) {
  row_count <- check_unified_bmf_ready_for_org_columns(con, table_name)

  org_columns_by_ein <- derive_org_columns_by_ein(con, table_name)

  quoted_table_name <- DBI::dbQuoteIdentifier(con, table_name)

  quoted_new_table_name <- DBI::dbQuoteIdentifier(con, UNIFIED_BMF_WITH_ORG_COLUMNS_NAME)

  create_table_sql <- build_table_with_org_columns_sql(con, table_name)

  duckdb::duckdb_register(con, UNIFIED_BMF_ORG_COLUMNS_BY_EIN_NAME, org_columns_by_ein)

  on.exit(
    duckdb::duckdb_unregister(con, UNIFIED_BMF_ORG_COLUMNS_BY_EIN_NAME),
    add = TRUE
  )

  DBI::dbWithTransaction(con, {
    DBI::dbExecute(con, create_table_sql)

    DBI::dbExecute(con, sprintf("DROP TABLE %s", quoted_table_name))

    DBI::dbExecute(con, sprintf(
      "ALTER TABLE %s RENAME TO %s",
      quoted_new_table_name,
      quoted_table_name
    ))
  })

  return(row_count)
}

# ============================================================================
# 5. Counts for the validation report
# ============================================================================

#' Count the organizations in the Unified BMF table by one value column
#'
#' @details
#' Groups by the value column and its words column together, so a row
#' whose words disagree with its value shows up as its own line.
#'
#' @param value_column name of one value column, for example
#'   `"nccs_org_role"`.
#' @param con DBI connection to the DuckDB database that holds the table.
#' @param table_name name of the Unified BMF table.
#'
#' @return data frame with four columns: `column` (the value column's
#'   name), `value`, `definition` and `organizations`. Sorted by value,
#'   with the row for a missing value last.
#'
#' @keywords internal
count_unified_bmf_by_one_org_column <- function(value_column, con, table_name) {
  words_column <- paste0(value_column, "_definition")

  organizations_by_value <- DBI::dbGetQuery(con, sprintf(
    "SELECT %s AS value, %s AS definition, COUNT(*) AS organizations
       FROM %s
      GROUP BY 1, 2
      ORDER BY 1 NULLS LAST, 2 NULLS LAST",
    DBI::dbQuoteIdentifier(con, value_column),
    DBI::dbQuoteIdentifier(con, words_column),
    DBI::dbQuoteIdentifier(con, table_name)
  ))

  counts <- data.frame(
    column = rep(value_column, nrow(organizations_by_value)),
    value = as.character(organizations_by_value$value),
    definition = as.character(organizations_by_value$definition),
    organizations = as.integer(organizations_by_value$organizations)
  )

  return(counts)
}

#' Count the organizations in the Unified BMF table by every derived column
#'
#' @details
#' Gives the numbers the validation report shows and checks before anything
#' is published (ADR 0059 Decision 10). Organizations whose value is
#' unknown are counted in their own row, with a missing `value`.
#'
#' Stops if the table does not have all six derived columns.
#'
#' @param con DBI connection to the DuckDB database that holds the table.
#' @param table_name name of the Unified BMF table.
#'
#' @return data frame with one row per value of each value column and four
#'   columns: `column`, `value`, `definition` and `organizations`. The
#'   value columns come in the order they sit in the file.
#'
#' @export
count_unified_bmf_by_org_columns <- function(con,
                                             table_name = UNIFIED_BMF_TABLE_NAME) {
  missing_derived_columns <- setdiff(
    UNIFIED_BMF_ORG_COLUMNS,
    DBI::dbListFields(con, table_name)
  )

  if (length(missing_derived_columns) > 0) {
    rlang::abort(paste0(
      "The Unified BMF table does not have the derived column(s): ",
      paste(missing_derived_columns, collapse = ", ")
    ))
  }

  counts_by_column <- purrr::map(
    names(UNIFIED_BMF_ORG_VALUE_LABELS),
    count_unified_bmf_by_one_org_column,
    con = con,
    table_name = table_name
  )

  organizations_by_value <- purrr::list_rbind(counts_by_column)

  return(organizations_by_value)
}
