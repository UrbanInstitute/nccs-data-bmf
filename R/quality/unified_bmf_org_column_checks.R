# ============================================================================
# unified_bmf_org_column_checks.R
# The part of the Unified BMF validation report that covers the derived
# organization columns (ADR 0059 Decision 10)
#
# Terms used in this file, each with one meaning:
#
#   value column     One of nccs_org_role, nccs_org_legal_type and
#                    nccs_org_field (see R/unified_bmf_org_columns.R).
#   counts           A table with one row per value of each value column:
#                    column, value, definition (the value in words) and
#                    organizations. Blanks have their own row. Made
#                    by count_unified_bmf_by_org_columns().
#   allowed list     The values a value column may hold, with their words:
#                    UNIFIED_BMF_ORG_VALUE_LABELS. A blank is always
#                    allowed and has no words.
#   previous build   The Unified BMF build published before this one. Its
#                    counts are read from its validation report.
#   change           This build's count minus the previous build's count.
#                    Blank (NA) when the previous build has no counts for
#                    that value column.
#   problem          A row of the counts that must stop the publish.
#
# The check itself works on the counts alone, so it can be tested without a
# database. Load R/unified_bmf_org_columns.R before this file.
# ============================================================================

# ============================================================================
# 1. Constants
# ============================================================================

# The validation report of the published Unified BMF. Read before this
# build's upload replaces it.
PREVIOUS_UNIFIED_BMF_REPORT_URL <-
  "https://nccsdata.s3.amazonaws.com/unified/bmf/bmf_unified_quality_report.json"

UNIFIED_BMF_ORG_COUNT_COLUMNS <- c("column", "value", "definition", "organizations")

PROBLEM_VALUE_NOT_ALLOWED <- "value is not in the allowed list"
PROBLEM_WORDS_DO_NOT_MATCH <- "the value in words does not match the value"
PROBLEM_COLUMN_HAS_NO_COUNTS <- "the column has no counts"

# ============================================================================
# 2. Problems that stop the publish
# ============================================================================

#' Find the rows of the counts that must stop the publish
#'
#' @details
#' Three kinds of problem:
#' \enumerate{
#'   \item The value is not in the allowed list of its value column.
#'   \item The value is allowed, but the words beside it are not the words
#'     the allowed list gives. A blank must have no words.
#'   \item A value column has no row at all in the counts.
#' }
#'
#' @param organizations_by_value the counts of this build.
#'
#' @return data frame with one row per problem: `column`, `value`,
#'   `definition`, `organizations` and `problem` (what is wrong, in words).
#'   No rows when nothing is wrong.
#'
#' @keywords internal
find_unified_bmf_org_column_problems <- function(organizations_by_value) {
  # All allowed values in one table, so each count can be matched to the
  # words its value should have.
  allowed_values <- purrr::imap(
    UNIFIED_BMF_ORG_VALUE_LABELS,
    build_allowed_org_value_rows
  ) |>
    purrr::list_rbind()

  checked_counts <- organizations_by_value |>
    dplyr::left_join(allowed_values, by = c("column", "value")) |>
    dplyr::mutate(
      is_missing_value = is.na(value),
      is_allowed_value = is_missing_value | !is.na(expected_definition),
      has_no_words_and_expects_none = is.na(definition) & is.na(expected_definition),
      has_expected_words =
        has_no_words_and_expects_none |
        dplyr::coalesce(definition == expected_definition, FALSE),
      problem = dplyr::case_when(
        !is_allowed_value ~ PROBLEM_VALUE_NOT_ALLOWED,
        !has_expected_words ~ PROBLEM_WORDS_DO_NOT_MATCH,
        .default = NA_character_
      )
    )

  problems_in_counts <- checked_counts |>
    dplyr::filter(!is.na(problem)) |>
    dplyr::select(dplyr::all_of(c(UNIFIED_BMF_ORG_COUNT_COLUMNS, "problem")))

  # A value column that the counts do not mention at all.
  columns_without_counts <- setdiff(
    names(UNIFIED_BMF_ORG_VALUE_LABELS),
    organizations_by_value$column
  )

  problems_for_absent_columns <- data.frame(
    column = columns_without_counts,
    value = rep(NA_character_, length(columns_without_counts)),
    definition = rep(NA_character_, length(columns_without_counts)),
    organizations = rep(0L, length(columns_without_counts)),
    problem = rep(PROBLEM_COLUMN_HAS_NO_COUNTS, length(columns_without_counts))
  )

  problems <- dplyr::bind_rows(problems_in_counts, problems_for_absent_columns) |>
    as.data.frame()

  return(problems)
}

#' List the allowed values of one value column, with their words
#'
#' @param labels named character vector: names are the allowed values,
#'   values are their words.
#' @param value_column name of the value column the labels belong to.
#'
#' @return data frame with one row per allowed value: `column`, `value`
#'   and `expected_definition`.
#'
#' @keywords internal
build_allowed_org_value_rows <- function(labels, value_column) {
  allowed_rows <- data.frame(
    column = rep(value_column, length(labels)),
    value = names(labels),
    expected_definition = unname(labels)
  )

  return(allowed_rows)
}

# ============================================================================
# 3. Change from the previous build
# ============================================================================

#' Put the previous build's counts beside this build's counts
#'
#' @details
#' A value the previous build had and this build does not have is kept,
#' with a count of 0, so a value that disappears is visible. Where the
#' previous build has no counts for a value column (it was built before
#' the column existed), the previous count and the change are `NA`.
#'
#' @param organizations_by_value the counts of this build.
#' @param previous_organizations_by_value the counts of the previous build,
#'   or `NULL` when there are none.
#'
#' @return data frame with one row per value of each value column:
#'   `column`, `value`, `definition`, `organizations`,
#'   `previous_organizations` and `change`. The value columns come in the
#'   order they sit in the file; within one, values are sorted and the row
#'   for blanks is last.
#'
#' @keywords internal
compare_org_column_counts_with_previous <- function(organizations_by_value,
                                                    previous_organizations_by_value) {
  if (is.null(previous_organizations_by_value)) {
    previous_organizations_by_value <- organizations_by_value[0, ]
  }

  previous_counts <- previous_organizations_by_value |>
    dplyr::select(
      column,
      value,
      previous_definition = definition,
      previous_organizations = organizations
    )

  columns_in_previous_build <- unique(previous_counts$column)

  counts_with_change <- organizations_by_value |>
    dplyr::full_join(previous_counts, by = c("column", "value")) |>
    dplyr::mutate(
      is_column_in_previous_build = column %in% columns_in_previous_build,
      definition = dplyr::coalesce(definition, previous_definition),
      organizations = dplyr::coalesce(organizations, 0L),
      # 0 only where the previous build counted this column and did not
      # have this value. Otherwise the previous count stays blank.
      previous_organizations = dplyr::if_else(
        is_column_in_previous_build,
        dplyr::coalesce(previous_organizations, 0L),
        NA_integer_
      ),
      change = organizations - previous_organizations,
      column_position = match(column, names(UNIFIED_BMF_ORG_VALUE_LABELS))
    ) |>
    dplyr::arrange(column_position, value) |>
    dplyr::select(
      dplyr::all_of(c(UNIFIED_BMF_ORG_COUNT_COLUMNS, "previous_organizations", "change"))
    ) |>
    as.data.frame()

  return(counts_with_change)
}

# ============================================================================
# 4. The check the validation report calls
# ============================================================================

#' Check the derived organization columns of a Unified BMF build
#'
#' @details
#' For `nccs_org_role`, `nccs_org_legal_type` and `nccs_org_field` this
#' gives the count for every value, the count of blanks and the
#' change from the previous build. It fails when a value is outside the
#' allowed list, when a value and its words disagree, or when a value
#' column has no counts (ADR 0059 Decision 10). A failed check stops the
#' publish, because the build stops when the validation report fails.
#'
#' A large change does not fail the check. The change is there for a person
#' to read before publishing.
#'
#' @param organizations_by_value the counts of this build, from
#'   [count_unified_bmf_by_org_columns()].
#' @param previous_organizations_by_value the counts of the previous build,
#'   from [read_previous_unified_bmf_org_column_counts()], or `NULL` when
#'   there are none.
#'
#' @return list with four parts: `passed` (TRUE or FALSE), `counts` (data
#'   frame, see [compare_org_column_counts_with_previous()]), `problems`
#'   (data frame, see [find_unified_bmf_org_column_problems()]) and
#'   `has_previous_counts` (TRUE when a previous build was compared).
#'
#' @export
check_unified_bmf_org_columns <- function(organizations_by_value,
                                          previous_organizations_by_value = NULL) {
  problems <- find_unified_bmf_org_column_problems(organizations_by_value)

  counts <- compare_org_column_counts_with_previous(
    organizations_by_value,
    previous_organizations_by_value
  )

  result <- list(
    passed = nrow(problems) == 0,
    counts = counts,
    problems = problems,
    has_previous_counts = !is.null(previous_organizations_by_value)
  )

  return(result)
}

# ============================================================================
# 5. The summary for the build log
# ============================================================================

#' Print the check of the derived organization columns to the build log
#'
#' @details
#' Prints the counts first, then either the problems or one line saying
#' there are none. The validation report page shows the same tables; this
#' summary is for a person reading the log while the build runs.
#'
#' @param org_column_check the list returned by
#'   [check_unified_bmf_org_columns()].
#'
#' @return `org_column_check`, unchanged and invisibly.
#'
#' @export
print_unified_bmf_org_column_check <- function(org_column_check) {
  problem_count <- nrow(org_column_check$problems)

  has_problems <- problem_count > 0

  # The counts, whether or not the check passed.
  message("Derived organization columns: count for every value, and change from the previous build")

  print(org_column_check$counts)

  # The outcome of the check.
  if (has_problems) {
    message(sprintf(
      "FAILED: %d problem(s) in the derived organization columns:",
      problem_count
    ))

    print(org_column_check$problems)
  }

  if (!has_problems) {
    message("Derived organization columns: every value is allowed and matches its value in words.")
  }

  return(invisible(org_column_check))
}

# ============================================================================
# 6. Reading the previous build's counts
# ============================================================================

#' Read the derived-column counts from the previous build's validation report
#'
#' @details
#' The validation report is a JSON file. A report written before the
#' derived columns existed has no counts, and this function then returns
#' `NULL`. If the report cannot be read at all, the function warns and
#' returns `NULL`: the build goes on, and the report leaves the change
#' blank.
#'
#' @param report_source path or URL of the previous build's validation
#'   report. Defaults to the published Unified BMF report.
#'
#' @return data frame with the columns `column`, `value`, `definition` and
#'   `organizations`, or `NULL` when there are no counts to read.
#'
#' @export
read_previous_unified_bmf_org_column_counts <- function(report_source = PREVIOUS_UNIFIED_BMF_REPORT_URL) {
  previous_report <- tryCatch(
    jsonlite::fromJSON(report_source),
    error = function(read_error) {
      warning(
        "The previous Unified BMF validation report could not be read from ",
        report_source,
        ": ",
        conditionMessage(read_error),
        call. = FALSE
      )

      return(NULL)
    }
  )

  previous_rows <- previous_report$derived_org_columns$counts

  if (is.null(previous_rows)) {
    return(NULL)
  }

  previous_counts <- data.frame(
    column = as.character(previous_rows$column),
    value = as.character(previous_rows$value),
    definition = as.character(previous_rows$definition),
    organizations = as.integer(previous_rows$organizations)
  )

  return(previous_counts)
}
