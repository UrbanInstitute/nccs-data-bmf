# ============================================================================
# build_address_resolved_crosswalk.R
#
# Builds the address-resolved crosswalk: a LONG-FORMAT address log, one row
# per (EIN, address spell), ordered by recency, so consumers get each
# organization's current and prior mailing addresses across EVERY vintage of
# both BMF pipelines. Shape ratified by nccs-contracts ADR 0042 Decision B
# (supersedes the ADR 0041 §4 wide/views sketch); motivating request:
# longitudinal address research the one-row-per-EIN Unified BMF cannot serve.
#
# Design:
#   * Aggregate the RAW address fields (org_addr_{street,city,state,zip}_raw):
#     verbatim source, vintage-invariant, no cleaner dependency.
#   * One row per (EIN, distinct address tuple). `spell_rank` 0 = the most
#     recent address, 1 = the one before it, and so on, so `spell_rank == 0`
#     reproduces the one-row-per-EIN Unified BMF view and higher ranks are the
#     address history. A "spell" here is an address TENURE aggregated over every
#     observation of that tuple, not a contiguity-checked survival spell: an
#     organization that moved away and later returned collapses into one row
#     whose first_vintage/last_vintage spans the gap.
#   * ZIP is normalized to the shared 5-digit base for the spell key, because
#     the two pipelines render ZIPs differently and the same address otherwise
#     splits into two spells on format alone (see normalize_zip5_sql()).
#   * Street coverage begins at the 2009 legacy vintages (ADR 0041); earlier
#     observations carry NULL street with real city/state/zip, kept honestly.
#   * Keyed on EIN2 per the maintainer's spec, with canonical ein and
#     ein_prefixed alongside (ADR 0036).
#   * `spell_id` (ADR 0051) is a stable identifier for each (organization,
#     address) pair, computed from EIN2 and the normalized address fields
#     (R/address_spell_id.R). Unlike spell_rank, which is renumbered when an
#     organization gains an address, it never changes between builds, so the
#     address-geo-resolved crosswalk joins to this table on it.
#
# Requirements: DuckDB + httpfs, AWS creds via credential chain. The address
# projection is fatter than ntee-resolved's single column; on a laptop set
# DUCKDB_MEMORY_LIMIT/DUCKDB_THREADS down and expect the S3 scan to dominate.
#   eval "$(aws configure export-credentials --profile thiya --format env)"
#   Rscript scripts/build_address_resolved_crosswalk.R
# ============================================================================

suppressPackageStartupMessages({
  library(DBI); library(duckdb); library(data.table); library(arrow); library(jsonlite)
})
library(here)
source(here::here("R", "config.R"))                  # BMF_S3_BUCKET
source(here::here("R", "utils", "logging.R"))        # log_info()
source(here::here("R", "ein.R"))                     # ein_to_prefixed/ein_to_ein2 (ADR 0036)
source(here::here("R", "address_spell_id.R"))        # address_spell_id() (ADR 0051)

bucket_name      <- if (exists("BMF_S3_BUCKET")) BMF_S3_BUCKET else "nccsdata"
aws_region       <- Sys.getenv("AWS_DEFAULT_REGION", unset = "us-east-1")
output_directory <- here::here("data", "crosswalks")
output_stem      <- file.path(output_directory, "address_resolved_crosswalk")
if (!dir.exists(output_directory)) dir.create(output_directory, recursive = TRUE)

# Production-scale gates (section 2b) only bind on full runs, so local parquet
# fixtures stay testable. A full build is ~11M rows across ~3.7M EINs.
PRODUCTION_SCALE_ROWS      <- 1e6
CROSS_SOURCE_SHARE_FLOOR   <- 0.01   # see section 2b for why 1% is the floor
PER_STATE_MIN_SPELLS       <- 5e4    # states below this are too small to judge

# Env-overridable so the script can be given a preliminary test against local
# parquet fixtures (point both at file globs) before an expensive full S3 run.
# Only the pipeline's own *_intermediate.parquet per vintage folder (backlog
# Z28): older *_processed.parquet copies may sit beside it and must not be read.
current_pipeline_glob <- Sys.getenv(
  "ADDR_XWALK_CUR_GLOB", sprintf("s3://%s/intermediate/bmf/*/*_intermediate.parquet", bucket_name))
legacy_pipeline_glob  <- Sys.getenv(
  "ADDR_XWALK_LEG_GLOB", sprintf("s3://%s/intermediate/bmf-legacy/*/*_intermediate.parquet", bucket_name))

# ---------------------------------------------------------------------------
# 1. Connect + httpfs + S3 credentials + spill config (same as ntee-resolved)
# ---------------------------------------------------------------------------
duckdb_connection <- DBI::dbConnect(duckdb::duckdb())
on.exit(DBI::dbDisconnect(duckdb_connection, shutdown = TRUE), add = TRUE)
# S3 machinery only when a glob actually targets S3, which lets the script run
# against local parquet fixtures with no AWS credentials in scope.
if (any(startsWith(c(current_pipeline_glob, legacy_pipeline_glob), "s3://"))) {
  DBI::dbExecute(duckdb_connection, "INSTALL httpfs; LOAD httpfs;")
  DBI::dbExecute(duckdb_connection, "INSTALL aws; LOAD aws;")
  DBI::dbExecute(duckdb_connection, sprintf("SET s3_region='%s';", aws_region))
  DBI::dbExecute(duckdb_connection,
                 "CREATE SECRET IF NOT EXISTS s3cred (TYPE S3, PROVIDER credential_chain);")
}
DBI::dbExecute(duckdb_connection, sprintf(
  "SET temp_directory='%s';",
  Sys.getenv("DUCKDB_TEMP_DIR", file.path(tempdir(), "duckdb_spill"))))
DBI::dbExecute(duckdb_connection, "SET preserve_insertion_order=false;")
DBI::dbExecute(duckdb_connection, sprintf(
  "SET memory_limit='%s';", Sys.getenv("DUCKDB_MEMORY_LIMIT", "9GB")))
DBI::dbExecute(duckdb_connection, sprintf(
  "SET threads=%s;", Sys.getenv("DUCKDB_THREADS", "4")))

#' Pick the formatted-EIN string column out of a parquet glob's schema.
#'
#' Both pipelines' intermediate parquets can carry a case-colliding pair of EIN
#' columns (`EIN` numeric-ish, `ein_1` the formatted string DuckDB renamed on
#' collision), so the name is resolved from the schema rather than assumed.
#' Same handling as build_ntee_resolved_crosswalk.R.
resolve_ein_column <- function(connection, parquet_glob) {
  # DESCRIBE -> column_name/column_type frame -> keep VARCHAR ein/ein_1 only
  schema_columns <- DBI::dbGetQuery(connection, sprintf(
    "SELECT column_name, column_type FROM (DESCRIBE SELECT * FROM read_parquet('%s'))",
    parquet_glob))
  ein_candidates <- schema_columns$column_name[
    tolower(schema_columns$column_name) %in% c("ein", "ein_1") &
      grepl("VARCHAR", schema_columns$column_type, ignore.case = TRUE)]
  if (length(ein_candidates) == 0L) {
    stop("Could not find a formatted EIN string column in ", parquet_glob)
  }
  if ("ein_1" %in% ein_candidates) "ein_1" else ein_candidates[[1]]
}

log_info("Resolving EIN column from schemas")
current_ein_column <- resolve_ein_column(duckdb_connection, current_pipeline_glob)
legacy_ein_column  <- resolve_ein_column(duckdb_connection, legacy_pipeline_glob)
log_info(sprintf("  current EIN col = %s | legacy EIN col = %s",
                 current_ein_column, legacy_ein_column))

# ---------------------------------------------------------------------------
# 2. Observation view: one normalized address tuple per (vintage, ein).
#    An observation counts as an address when street OR city is present.
# ---------------------------------------------------------------------------

#' SQL that trims/upper-cases a raw text field and maps empty string to NULL.
normalize_text_sql <- function(column_name) {
  sprintf("nullif(trim(upper(CAST(%s AS VARCHAR))), '')", column_name)
}

#' SQL that reduces either pipeline's raw ZIP rendering to the shared 5-digit base.
#'
#' The two pipelines skew in OPPOSITE directions and both skews break the spell
#' key, so normalization has to handle each:
#'   * current: raw ZIPs carry the ZIP+4 route add-on ("02138-1234"). The
#'     5-digit base is the first five digits; the add-on is not part of address
#'     identity and is not stable across vintages.
#'   * legacy: raw ZIPs went through a numeric round-trip that STRIPPED
#'     LEADING ZEROS ("02138" stored as "2138"). Left-padding restores them.
#'     Without this, every organization in a 0-prefix state (MA, CT, RI, NJ, ME,
#'     NH, PR, VI) format-splits against its own current-pipeline rows: the
#'     2026-07-28 review measured exactly 0.00% cross-source spells in all six
#'     of those states while the national share was 15.8%.
#'
#' Digits-only -> longer than 5 means ZIP+4, keep the first 5 -> otherwise
#' left-pad back to 5. Empty/NULL input stays NULL.
normalize_zip5_sql <- function(column_name) {
  zip_digits <- sprintf("regexp_replace(CAST(%s AS VARCHAR), '[^0-9]', '', 'g')", column_name)
  sprintf("CASE WHEN %s = '' THEN NULL
                WHEN length(%s) > 5 THEN substr(%s, 1, 5)
                ELSE lpad(%s, 5, '0') END",
          zip_digits, zip_digits, zip_digits, zip_digits)
}

#' SQL selecting one normalized observation row per (vintage, EIN) from a list
#' of parquet files (one vintage each).
observation_select_sql <- function(parquet_files, source_label, ein_column) {
  file_list_sql <- paste0("['", paste(parquet_files, collapse = "', '"), "']")
  sprintf("
  SELECT regexp_extract(filename, '(\\d{4}_\\d{2})', 1) AS vintage_ym,
         \"%s\" AS ein,
         '%s'   AS src,
         %s     AS street,
         %s     AS city,
         %s     AS state,
         %s     AS zip5
  FROM read_parquet(%s, filename = true, union_by_name = true)",
          ein_column, source_label,
          normalize_text_sql("org_addr_street_raw"),
          normalize_text_sql("org_addr_city_raw"),
          normalize_text_sql("org_addr_state_raw"),
          normalize_zip5_sql("org_addr_zip_raw"),
          file_list_sql)
}

# ---------------------------------------------------------------------------
# 2a. Summarize a few BMF releases at a time, then combine the summaries.
#
# What is being computed. For each organization and each address: the first
# BMF release (vintage) the address appeared in, the last one, and how many
# different releases showed it.
#
# Why not in one pass. The input is every organization's address in every
# release since 1989, about 250 million rows. To count how many different
# releases showed each address, the database has to keep every (organization,
# address, release) combination in working memory at once. That did not fit:
# it wrote 47 GB of overflow to disk and ran out of space on a laptop
# (2026-09-30).
#
# What is done instead. The release files are processed in groups ("chunks")
# of ADDR_XWALK_FILES_PER_CHUNK files. Each chunk produces a small summary (a
# "partial") with one row per organization and address: first release, last
# release, number of releases. A partial is a few million rows, not 250
# million. The partials are then combined in one final step.
#
# Why combining gives exactly the same answer as one pass:
#   * First and last release: the earliest of the chunks' earliest is the
#     overall earliest, and likewise for the latest.
#   * Number of releases: each file is exactly one release, and a file sits
#     in exactly one chunk, so no release is ever counted in two chunks. The
#     chunk counts can therefore simply be added up. (Adding up counts of
#     distinct things is only safe because of this.)
#   * `source` (current, legacy or both): one chunk cannot know whether an
#     address also appears in the other pipeline, so this is decided in the
#     final step, when every chunk is in view.
#
# Example: an address appears in the January, February and March releases;
# January and February fall in chunk 1, March in chunk 2. Chunk 1 reports
# first = January, last = February, count = 2. Chunk 2 reports first = March,
# last = March, count = 1. Combined: first = January, last = March, count = 3.
# ---------------------------------------------------------------------------

files_per_chunk <- as.integer(Sys.getenv("ADDR_XWALK_FILES_PER_CHUNK", "10"))

# The partials are written inside the DuckDB overflow folder. The folder is
# emptied at the start of every run, because the final step combines every
# partial it finds there: a partial left by an earlier run would be counted
# again.
partials_dir <- file.path(
  Sys.getenv("DUCKDB_TEMP_DIR", file.path(tempdir(), "duckdb_spill")),
  "address_partials"
)
unlink(partials_dir, recursive = TRUE)
dir.create(partials_dir, recursive = TRUE, showWarnings = FALSE)

#' The parquet files a glob pattern matches, in name order.
#'
#' @param connection DuckDB connection (with httpfs loaded when the pattern
#'   points at S3).
#' @param parquet_glob Character: a file pattern, local or s3://.
#' @return Character vector of file paths, sorted by name, one per BMF release.
list_parquet_files <- function(connection, parquet_glob) {

  file_query <- sprintf("SELECT file FROM glob('%s') ORDER BY file", parquet_glob)
  file_table <- DBI::dbGetQuery(connection, file_query)

  return(file_table$file)

}

#' Summarize one chunk of release files and write the summary to disk.
#'
#' For every organization and address seen in the chunk: how many different
#' releases showed it, and the first and last of them. Written to one parquet
#' file per chunk under `partials_dir`.
#'
#' @param parquet_files Character vector: the release files of this chunk
#'   (one release each).
#' @param source_label Character: "current" or "legacy", the pipeline the
#'   files come from.
#' @param ein_column Character: name of the formatted-EIN column in these
#'   files (from resolve_ein_column()).
#' @param chunk_index Integer: the chunk's number within its pipeline, used
#'   in the file name.
#' @return Character: the path of the partial that was written.
aggregate_one_chunk <- function(parquet_files, source_label, ein_column, chunk_index) {

  partial_path <- file.path(partials_dir, sprintf("%s_%03d.parquet", source_label, chunk_index))

  log_info(sprintf("  %s chunk %d: %d vintage file(s)", source_label, chunk_index, length(parquet_files)))

  observation_sql <- observation_select_sql(parquet_files, source_label, ein_column)

  DBI::dbExecute(duckdb_connection, sprintf("
    COPY (
      SELECT ein, src, street, city, state, zip5,
             COUNT(DISTINCT vintage_ym) AS vintage_count,
             MIN(vintage_ym)            AS first_vintage_in_source,
             MAX(vintage_ym)            AS last_vintage_in_source
      FROM (%s)
      WHERE ein IS NOT NULL AND (street IS NOT NULL OR city IS NOT NULL)
      GROUP BY ein, src, street, city, state, zip5
    ) TO '%s' (FORMAT PARQUET, COMPRESSION ZSTD)",
    observation_sql,
    partial_path
  ))

  return(partial_path)

}

#' Summarize every release file of one pipeline, chunk by chunk.
#'
#' @param parquet_glob Character: file pattern matching the pipeline's
#'   release files.
#' @param source_label Character: "current" or "legacy".
#' @param ein_column Character: name of the formatted-EIN column.
#' @return Character vector: the paths of the partials written, one per chunk.
aggregate_source_in_chunks <- function(parquet_glob, source_label, ein_column) {

  parquet_files <- list_parquet_files(duckdb_connection, parquet_glob)

  # Files 1..10 go to chunk 1, 11..20 to chunk 2, and so on.
  chunk_index <- ceiling(seq_along(parquet_files) / files_per_chunk)
  file_chunks <- split(parquet_files, chunk_index)

  log_info(sprintf(
    "%s pipeline: %d vintage files in %d chunk(s)",
    source_label,
    length(parquet_files),
    length(file_chunks)
  ))

  aggregate_chunk_of_this_source <- function(chunk_files, index) {

    return(aggregate_one_chunk(chunk_files, source_label, ein_column, index))

  }

  partial_paths <- purrr::imap_chr(unname(file_chunks), aggregate_chunk_of_this_source)

  return(partial_paths)

}

log_info("Aggregating per (ein, source, address tuple), chunk by chunk")
partial_paths <- c(
  aggregate_source_in_chunks(current_pipeline_glob, "current", current_ein_column),
  aggregate_source_in_chunks(legacy_pipeline_glob,  "legacy",  legacy_ein_column)
)

# Combine the partials (see the explanation at 2a): release counts are added
# up, the earliest first release and the latest last release are kept, and an
# address seen in both pipelines is marked 'both'.
log_info("Combining the chunk partials into spells")
address_spells <- data.table::as.data.table(DBI::dbGetQuery(duckdb_connection, sprintf("
  SELECT ein, street, city, state, zip5,
         SUM(vintage_count)                  AS n_vintages,
         MIN(first_vintage_in_source)        AS first_vintage,
         MAX(last_vintage_in_source)         AS last_vintage,
         CASE WHEN COUNT(DISTINCT src) > 1 THEN 'both' ELSE MIN(src) END AS source
  FROM read_parquet(['%s'])
  GROUP BY ein, street, city, state, zip5",
  paste(partial_paths, collapse = "', '"))))
address_spells[, n_vintages := as.integer(n_vintages)]

log_info(sprintf("Spells: %s rows across %s EINs",
                 format(nrow(address_spells), big.mark = ","),
                 format(data.table::uniqueN(address_spells$ein), big.mark = ",")))

# ---------------------------------------------------------------------------
# 2b. Hard invariants (systematized from the 2026-07-26 zip-format incident and
#     the 2026-07-28 leading-zero finding: a broken cross-pipeline join key
#     produces impossible statistics, so we fail the build on them rather than
#     hoping someone reads the quality JSON). Thresholds bind on full runs only.
# ---------------------------------------------------------------------------

# A normalized ZIP is exactly five digits or absent. Anything else means
# normalize_zip5_sql() did not fire (the 3-4 char legacy values that shipped in
# the first published build were the tell).
malformed_zip5_count <- address_spells[!is.na(zip5) & nchar(zip5) != 5L, .N]
if (malformed_zip5_count > 0L) {
  stop(sprintf(paste0("Invariant violated: %s spells carry a zip5 that is not ",
                      "exactly 5 digits (ZIP normalization broken)."),
               format(malformed_zip5_count, big.mark = ",")))
}

if (nrow(address_spells) > PRODUCTION_SCALE_ROWS) {
  # Legacy vintages end 2022_08 and the current pipeline starts 2023_06. Any
  # organization that lived on both sides of that gap without moving writes the
  # same normalized tuple in both sources, so those rows MUST fold into a
  # 'both' spell. At ~3.7M EINs a near-zero share is mechanically impossible
  # and means the key is format-splitting, not that the data is strange.
  cross_source_share <- address_spells[source == "both", .N] / nrow(address_spells)
  if (cross_source_share < CROSS_SOURCE_SHARE_FLOOR) {
    stop(sprintf(
      paste0("Invariant violated: cross-source ('both') spells are %.3f%% of the ",
             "table, below the %.0f%% floor. See ",
             "docs/reference/address-data-invariants.md."),
      100 * cross_source_share, 100 * CROSS_SOURCE_SHARE_FLOOR))
  }

  # Same test per state, because the national figure hides stratified breakage:
  # the leading-zero defect left six states at exactly 0.00% while the national
  # share sat at a healthy 15.8% and the global gate passed.
  per_state_cross_source <- address_spells[
    !is.na(state), .(spell_count = .N,
                     cross_source_share = sum(source == "both") / .N), by = state]
  states_below_floor <- per_state_cross_source[
    spell_count >= PER_STATE_MIN_SPELLS & cross_source_share < CROSS_SOURCE_SHARE_FLOOR]
  if (nrow(states_below_floor) > 0L) {
    stop(sprintf(
      paste0("Invariant violated: %d state(s) with >= %s spells fall below the ",
             "%.0f%% cross-source floor (%s). A whole state at ~zero means its ",
             "ZIP/street rendering differs between pipelines. See ",
             "docs/reference/address-data-invariants.md."),
      nrow(states_below_floor), format(PER_STATE_MIN_SPELLS, big.mark = ","),
      100 * CROSS_SOURCE_SHARE_FLOOR,
      paste(sprintf("%s %.2f%%", states_below_floor$state,
                    100 * states_below_floor$cross_source_share), collapse = ", ")))
  }
}

# ---------------------------------------------------------------------------
# 3. Rank spells per EIN: 0 = most recent (last_vintage desc, n desc).
#     The full tuple is in the sort key so ordering is deterministic: DuckDB
#     returns rows in an arbitrary order (preserve_insertion_order=false, many
#     threads), and a partial key would let tied rows (street-less legacy
#     spells especially) land in a different order on every rebuild, changing
#     spell_rank and the artifact's sha256 for no data reason (ADR 0014
#     idempotency compares sha256).
# ---------------------------------------------------------------------------
data.table::setorder(address_spells, ein, -last_vintage, -n_vintages,
                     street, city, state, zip5, na.last = TRUE)
address_spells[, spell_rank           := seq_len(.N) - 1L, by = ein]
address_spells[, n_distinct_addresses := .N,               by = ein]

# ADR 0036 renderings; EIN2 leads per the maintainer's key spec.
address_spells[, ein_prefixed := ein_to_prefixed(ein)]
address_spells[, EIN2         := ein_to_ein2(ein)]

# ADR 0051: the stable spell identifier. One organization at one normalized
# address is one spell, so the id must be unique here; a repeat would mean
# the GROUP BY above and the hash disagree about what a distinct address is.
address_spells[, spell_id := address_spell_id(EIN2, street, city, state, zip5)]

duplicate_spell_id_count <- address_spells[, .N, by = spell_id][N > 1L, .N]
if (duplicate_spell_id_count > 0L) {
  stop(sprintf("Invariant violated: %s spell_id values repeat; the identifier must be unique per (EIN, address).",
               format(duplicate_spell_id_count, big.mark = ",")))
}

data.table::setcolorder(address_spells, c("spell_id", "EIN2", "ein", "ein_prefixed", "spell_rank",
  "street", "city", "state", "zip5",
  "first_vintage", "last_vintage", "n_vintages", "source",
  "n_distinct_addresses"))

# ---------------------------------------------------------------------------
# 4. Write parquet + csv + quality JSON
#    (validate via scripts/validate_address_crosswalk.R, then publish via
#     R/publish_address_resolved_crosswalk.R)
# ---------------------------------------------------------------------------
arrow::write_parquet(address_spells, paste0(output_stem, ".parquet"), compression = "zstd")
data.table::fwrite(address_spells, paste0(output_stem, ".csv"))

quality <- list(
  timestamp            = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z"),
  total_spell_rows     = nrow(address_spells),
  distinct_eins        = data.table::uniqueN(address_spells$ein),
  spells_per_ein_mean  = round(nrow(address_spells) /
                               data.table::uniqueN(address_spells$ein), 3),
  spells_per_ein_max   = address_spells[, max(n_distinct_addresses)],
  pct_eins_multi_addr  = round(100 * data.table::uniqueN(
                                 address_spells[n_distinct_addresses > 1L, ein]) /
                               data.table::uniqueN(address_spells$ein), 2),
  street_null_spells   = address_spells[is.na(street), .N],
  source_counts        = as.list(table(address_spells$source)),
  current_spells       = address_spells[spell_rank == 0L, .N]
)
jsonlite::write_json(quality, paste0(output_stem, "_quality.json"),
                     auto_unbox = TRUE, pretty = TRUE)
log_info(sprintf("Wrote %s.{parquet,csv,_quality.json}", output_stem))
log_info(sprintf("Quality: %s spells | %s EINs | %.2f%% multi-address | max spells %d",
                 format(quality$total_spell_rows, big.mark = ","),
                 format(quality$distinct_eins, big.mark = ","),
                 quality$pct_eins_multi_addr, quality$spells_per_ein_max))
