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
current_pipeline_glob <- Sys.getenv(
  "ADDR_XWALK_CUR_GLOB", sprintf("s3://%s/intermediate/bmf/*/*.parquet", bucket_name))
legacy_pipeline_glob  <- Sys.getenv(
  "ADDR_XWALK_LEG_GLOB", sprintf("s3://%s/intermediate/bmf-legacy/*/*.parquet", bucket_name))

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

#' SQL selecting one normalized observation row per (vintage, EIN) from a glob.
observation_select_sql <- function(parquet_glob, source_label, ein_column) {
  sprintf("
  SELECT regexp_extract(filename, '(\\d{4}_\\d{2})', 1) AS vintage_ym,
         \"%s\" AS ein,
         '%s'   AS src,
         %s     AS street,
         %s     AS city,
         %s     AS state,
         %s     AS zip5
  FROM read_parquet('%s', filename = true, union_by_name = true)",
          ein_column, source_label,
          normalize_text_sql("org_addr_street_raw"),
          normalize_text_sql("org_addr_city_raw"),
          normalize_text_sql("org_addr_state_raw"),
          normalize_zip5_sql("org_addr_zip_raw"),
          parquet_glob)
}

log_info("Building observation view over all intermediate parquets")
# Both pipelines' observation selects -> one union view the aggregate reads once
DBI::dbExecute(duckdb_connection, sprintf(
  "CREATE OR REPLACE TEMP VIEW obs AS %s UNION ALL BY NAME %s;",
  observation_select_sql(current_pipeline_glob, "current", current_ein_column),
  observation_select_sql(legacy_pipeline_glob,  "legacy",  legacy_ein_column)))

# Inner GROUP BY: distinct vintages per (ein, source, address tuple).
# Outer GROUP BY: fold the two sources together, tagging tuples seen in both.
# n_vintages counts DISTINCT vintages rather than rows so the column name stays
# true even if a vintage ever lands as several parquet parts or duplicate EINs.
log_info("Aggregating per (ein, address tuple) across sources")
address_spells <- data.table::as.data.table(DBI::dbGetQuery(duckdb_connection, "
  SELECT ein, street, city, state, zip5,
         SUM(vintage_count)                  AS n_vintages,
         MIN(first_vintage_in_source)        AS first_vintage,
         MAX(last_vintage_in_source)         AS last_vintage,
         CASE WHEN COUNT(DISTINCT src) > 1 THEN 'both' ELSE MIN(src) END AS source
  FROM (
    SELECT ein, src, street, city, state, zip5,
           COUNT(DISTINCT vintage_ym) AS vintage_count,
           MIN(vintage_ym)            AS first_vintage_in_source,
           MAX(vintage_ym)            AS last_vintage_in_source
    FROM obs
    WHERE ein IS NOT NULL AND (street IS NOT NULL OR city IS NOT NULL)
    GROUP BY ein, src, street, city, state, zip5
  )
  GROUP BY ein, street, city, state, zip5"))

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

data.table::setcolorder(address_spells, c("EIN2", "ein", "ein_prefixed", "spell_rank",
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
