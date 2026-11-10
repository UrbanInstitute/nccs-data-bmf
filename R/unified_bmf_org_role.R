# ============================================================================
# unified_bmf_org_role.R
# Add the organization role columns to the finished Unified BMF (ADR 0058)
#
# Terms used in this file, each with one meaning:
#
#   Unified BMF table  The one-row-per-EIN table that build_master_bmf()
#                      leaves in the DuckDB database, named bmf_master.
#   role               What an organization mainly does: operating,
#                      supporting or mutual benefit. Defined, with its rule,
#                      in R/nccs_org_role.R.
#   role columns       The two columns this file adds at the end of the
#                      Unified BMF table: nccs_org_role (O, S, M or missing)
#                      and nccs_org_role_definition (the same in words).
#   input columns      The columns of the Unified BMF table the role is
#                      derived from, plus ein to match rows.
#
# The rule lives only in R/nccs_org_role.R. This file reads the input
# columns into R, calls that rule, and writes the role columns back.
# ============================================================================

# ============================================================================
# 1. Constants
# ============================================================================

UNIFIED_BMF_TABLE_NAME <- "bmf_master"

UNIFIED_BMF_ORG_ROLE_INPUT_COLUMNS <- c(
  "ein",
  "ntee_code_raw",
  "subsection_code",
  "foundation_code"
)

UNIFIED_BMF_ORG_ROLE_COLUMNS <- c(
  "nccs_org_role",
  "nccs_org_role_definition"
)

# Short-lived names used inside the database while the columns are added.
UNIFIED_BMF_ORG_ROLE_BY_EIN_NAME <- "nccs_org_role_by_ein"
UNIFIED_BMF_WITH_ORG_ROLE_NAME <- "bmf_master_with_org_role"

# ============================================================================
# 2. Checks before anything changes
# ============================================================================

#' Stop unless the Unified BMF table can take the role columns
#'
#' @details
#' Two things must hold: the table has every input column, and it has one
#' row per EIN. Without the second, matching the role back by EIN would
#' repeat rows. Nothing in the database is changed by this function.
#'
#' @param con DBI connection to the DuckDB database that holds the table.
#' @param table_name name of the Unified BMF table.
#'
#' @return the number of rows in the table, as an integer.
#'
#' @keywords internal
check_unified_bmf_ready_for_org_role <- function(con, table_name) {
  existing_columns <- DBI::dbListFields(con, table_name)

  missing_input_columns <- setdiff(
    UNIFIED_BMF_ORG_ROLE_INPUT_COLUMNS,
    existing_columns
  )

  if (length(missing_input_columns) > 0) {
    rlang::abort(paste0(
      "The Unified BMF table is missing the column(s) the role is derived from: ",
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
# 3. Deriving the role for every EIN
# ============================================================================

#' Derive the role of every organization in the Unified BMF table
#'
#' @details
#' Reads only the input columns into R and applies
#' [derive_nccs_org_role()] with its default definition (the one used by
#' the older NCCS files from September 2017 on).
#'
#' @param con DBI connection to the DuckDB database that holds the table.
#' @param table_name name of the Unified BMF table.
#'
#' @return tibble with one row per EIN and three columns: `ein`,
#'   `nccs_org_role` and `nccs_org_role_definition`.
#'
#' @keywords internal
derive_org_role_by_ein <- function(con, table_name) {
  quoted_table_name <- DBI::dbQuoteIdentifier(con, table_name)

  input_rows <- DBI::dbGetQuery(con, sprintf(
    "SELECT ein, ntee_code_raw, subsection_code, foundation_code FROM %s",
    quoted_table_name
  ))

  org_role <- derive_nccs_org_role(
    ntee_code = input_rows$ntee_code_raw,
    subsection_code = input_rows$subsection_code,
    foundation_code = input_rows$foundation_code
  )

  org_role_by_ein <- dplyr::tibble(
    ein = input_rows$ein,
    nccs_org_role = org_role,
    nccs_org_role_definition = label_nccs_org_role(org_role)
  )

  return(org_role_by_ein)
}

# ============================================================================
# 4. The step the build calls
# ============================================================================

#' Add the organization role columns to the Unified BMF table
#'
#' @details
#' Replaces the Unified BMF table with a copy that has every existing
#' column in its existing order, followed by `nccs_org_role` and
#' `nccs_org_role_definition` (ADR 0058). No row is added or dropped and no
#' existing value changes.
#'
#' Safe to run twice: role columns left by an earlier run are replaced, not
#' duplicated. The replacement happens inside one database transaction, so
#' a failure part-way leaves the table as it was.
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
add_nccs_org_role_to_unified_bmf <- function(con,
                                             table_name = UNIFIED_BMF_TABLE_NAME) {
  row_count <- check_unified_bmf_ready_for_org_role(con, table_name)

  org_role_by_ein <- derive_org_role_by_ein(con, table_name)

  existing_columns <- DBI::dbListFields(con, table_name)

  columns_to_keep <- setdiff(existing_columns, UNIFIED_BMF_ORG_ROLE_COLUMNS)

  quoted_columns_to_keep <- paste0(
    "unified_bmf.",
    DBI::dbQuoteIdentifier(con, columns_to_keep),
    collapse = ", "
  )

  quoted_table_name <- DBI::dbQuoteIdentifier(con, table_name)

  quoted_new_table_name <- DBI::dbQuoteIdentifier(con, UNIFIED_BMF_WITH_ORG_ROLE_NAME)

  create_table_sql <- sprintf(
    "CREATE TABLE %s AS
     SELECT %s,
            org_role.nccs_org_role,
            org_role.nccs_org_role_definition
       FROM %s AS unified_bmf
       LEFT JOIN %s AS org_role
         ON unified_bmf.ein = org_role.ein",
    quoted_new_table_name,
    quoted_columns_to_keep,
    quoted_table_name,
    UNIFIED_BMF_ORG_ROLE_BY_EIN_NAME
  )

  duckdb::duckdb_register(con, UNIFIED_BMF_ORG_ROLE_BY_EIN_NAME, org_role_by_ein)

  on.exit(
    duckdb::duckdb_unregister(con, UNIFIED_BMF_ORG_ROLE_BY_EIN_NAME),
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
# 5. Counts to read before publishing
# ============================================================================

#' Count the organizations in the Unified BMF table by role
#'
#' @details
#' Gives the numbers to compare with the previous build before anything is
#' published. Organizations whose role is unknown are counted in their own
#' row, with a missing `nccs_org_role`.
#'
#' @param con DBI connection to the DuckDB database that holds the table.
#' @param table_name name of the Unified BMF table.
#'
#' @return data frame with one row per role and three columns:
#'   `nccs_org_role`, `nccs_org_role_definition` and `organizations`.
#'
#' @export
count_unified_bmf_by_org_role <- function(con,
                                          table_name = UNIFIED_BMF_TABLE_NAME) {
  quoted_table_name <- DBI::dbQuoteIdentifier(con, table_name)

  organizations_by_role <- DBI::dbGetQuery(con, sprintf(
    "SELECT nccs_org_role, nccs_org_role_definition, COUNT(*) AS organizations
       FROM %s
      GROUP BY nccs_org_role, nccs_org_role_definition
      ORDER BY nccs_org_role NULLS LAST",
    quoted_table_name
  ))

  organizations_by_role$organizations <- as.integer(organizations_by_role$organizations)

  return(organizations_by_role)
}
