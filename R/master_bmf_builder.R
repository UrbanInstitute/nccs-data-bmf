# ============================================================================
# master_bmf_builder.R
#
# Builds the Master BMF: one row per EIN, drawn from the most-recent vintage
# in which that EIN appears across both the current monthly BMF pipeline
# (s3://nccsdata/processed/bmf/) and the legacy 501CX-NONPROFIT-PX pipeline
# (s3://nccsdata/processed/bmf-legacy/).
#
# Strategy: DuckDB read_csv_auto with union_by_name reconciles the legacy
# slim per-vintage schema against the current full schema. A window function
# keeps the newest row per EIN and computes first/last vintage markers.
#
# When the same vintage_ym appears in both sources (the 2014–2016 overlap
# window), the current pipeline's row wins because its schema is richer.
# ============================================================================

#' Discover all processed BMF CSVs (current + legacy) in S3
#'
#' Lists every `bmf_YYYY_MM_processed.csv` (current) and
#' `bmf_legacy_YYYY_MM_processed.csv` (legacy) under their respective
#' processed prefixes.
#'
#' @return data.table with columns: bmf_source, vintage_ym, s3_uri
discover_master_inputs <- function(bucket = BMF_S3_BUCKET) {
  log_info("Discovering current BMF processed CSVs...")
  current_objs <- aws.s3::get_bucket(
    bucket = bucket, prefix = BMF_S3_PROCESSED_PREFIX, max = Inf
  )
  current_keys <- vapply(current_objs,
    function(o) if (!is.null(o$Key)) o$Key else NA_character_,
    character(1)
  )
  current_keys <- current_keys[grepl(
    "processed/bmf/\\d{4}_\\d{2}/bmf_\\d{4}_\\d{2}_processed\\.csv$",
    current_keys
  )]

  log_info("Discovering legacy BMF processed CSVs...")
  legacy_objs <- aws.s3::get_bucket(
    bucket = bucket, prefix = BMF_S3_LEGACY_PROCESSED_PREFIX, max = Inf
  )
  legacy_keys <- vapply(legacy_objs,
    function(o) if (!is.null(o$Key)) o$Key else NA_character_,
    character(1)
  )
  legacy_keys <- legacy_keys[grepl(
    "processed/bmf-legacy/\\d{4}_\\d{2}/bmf_legacy_\\d{4}_\\d{2}_processed\\.csv$",
    legacy_keys
  )]

  current_dt <- data.table::data.table(
    bmf_source = "current",
    vintage_ym = stringr::str_extract(current_keys, "(?<=bmf/)\\d{4}_\\d{2}"),
    s3_uri     = sprintf("s3://%s/%s", bucket, current_keys)
  )
  legacy_dt <- data.table::data.table(
    bmf_source = "legacy",
    vintage_ym = stringr::str_extract(legacy_keys, "(?<=bmf-legacy/)\\d{4}_\\d{2}"),
    s3_uri     = sprintf("s3://%s/%s", bucket, legacy_keys)
  )

  inputs <- data.table::rbindlist(list(current_dt, legacy_dt))
  data.table::setorder(inputs, bmf_source, vintage_ym)

  log_info(sprintf("Discovered %d current + %d legacy = %d input files",
                   nrow(current_dt), nrow(legacy_dt), nrow(inputs)))
  inputs
}

#' Connect to DuckDB with sensible defaults for the master build
#'
#' @param db_path Path to the DuckDB file (NULL for in-memory)
#' @param memory_limit DuckDB memory_limit (e.g. "100GB")
#' @param threads NULL = use all cores
#' @param s3_region AWS region for httpfs reads
duckdb_connect_for_master <- function(db_path = NULL,
                                       memory_limit = "12GB",
                                       threads = NULL,
                                       s3_region = "us-east-1") {
  if (!requireNamespace("duckdb", quietly = TRUE)) {
    stop("Package 'duckdb' is required. install.packages('duckdb').")
  }

  # Use the canonical inline pattern: dbConnect(duckdb::duckdb(), ...).
  # Assigning the driver to a local `drv` and returning only the
  # connection lets R GC the driver after the function exits, which
  # invalidates the in-memory database mid-pipeline ("rapi_prepare:
  # Invalid connection"). Inline keeps the driver's lifetime bound to
  # the connection.
  dbdir_arg <- if (is.null(db_path)) ":memory:" else db_path
  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = dbdir_arg)

  DBI::dbExecute(con, sprintf("SET memory_limit = '%s'", memory_limit))
  if (!is.null(threads)) {
    DBI::dbExecute(con, sprintf("SET threads = %d", as.integer(threads)))
  }

  # httpfs lets DuckDB read S3 URIs directly. Auto-detects credentials from
  # the AWS SDK chain (IAM instance role > env vars > ~/.aws/credentials).
  DBI::dbExecute(con, "INSTALL httpfs")
  DBI::dbExecute(con, "LOAD httpfs")
  DBI::dbExecute(con, sprintf("SET s3_region = '%s'", s3_region))

  con
}

#' Build the Master BMF table inside DuckDB
#'
#' Produces a `bmf_master` table:
#'   - one row per EIN
#'   - row contents drawn from the newest vintage in which the EIN appears
#'     (current pipeline wins on ties)
#'   - augmented with bmf_source, bmf_vintage_ym, first_vintage_ym,
#'     last_vintage_ym, first_year_in_bmf, last_year_in_bmf,
#'     bmf_vintages_observed
#'
#' @param con DuckDB connection
#' @param inputs data.table from discover_master_inputs()
#' @return invisibly returns the row count of bmf_master
build_master_bmf <- function(con,
                              inputs,
                              current_glob = "s3://nccsdata/processed/bmf/*/bmf_*_processed.csv",
                              legacy_glob  = "s3://nccsdata/processed/bmf-legacy/*/bmf_legacy_*_processed.csv") {
  has_current <- nrow(inputs[bmf_source == "current"]) > 0
  has_legacy  <- nrow(inputs[bmf_source == "legacy"])  > 0

  if (!has_current && !has_legacy) {
    stop("No input files discovered for master build.")
  }

  log_info(sprintf(
    "Stacking %d current + %d legacy CSVs via DuckDB read_csv_auto (S3 glob)...",
    sum(inputs$bmf_source == "current"),
    sum(inputs$bmf_source == "legacy")
  ))

  # Stage 1: read both sets via S3 glob so DuckDB enumerates keys via
  # ListObjects rather than us passing a 100+-element array literal
  # (the array form triggered "rapi_prepare: Invalid connection" with
  # large file lists). union_by_name reconciles the legacy slim schema
  # against the current full schema; filename=true gives us the source
  # URI to derive vintage_ym from.
  parts <- c()
  if (has_current) {
    parts <- c(parts, sprintf("
      SELECT *,
             regexp_extract(filename, 'bmf_(\\d{4}_\\d{2})_processed', 1) AS vintage_underscore,
             'current' AS bmf_source
        FROM read_csv_auto('%s',
                           union_by_name = true,
                           filename      = true,
                           sample_size   = -1,
                           all_varchar   = false)
    ", current_glob))
  }
  if (has_legacy) {
    parts <- c(parts, sprintf("
      SELECT *,
             regexp_extract(filename, 'bmf_legacy_(\\d{4}_\\d{2})_processed', 1) AS vintage_underscore,
             'legacy' AS bmf_source
        FROM read_csv_auto('%s',
                           union_by_name = true,
                           filename      = true,
                           sample_size   = -1,
                           all_varchar   = false)
    ", legacy_glob))
  }

  stack_sql <- paste0(
    "CREATE OR REPLACE TABLE stacked AS\n",
    paste(parts, collapse = "\nUNION ALL BY NAME\n")
  )

  t0 <- Sys.time()
  DBI::dbExecute(con, stack_sql)
  log_info(sprintf("Stacked rows: %s (%.1f sec)",
                   format(DBI::dbGetQuery(con, "SELECT COUNT(*) AS n FROM stacked")$n,
                          big.mark = ","),
                   as.numeric(Sys.time() - t0, units = "secs")))

  # Drop rows missing EIN before deduping.
  before_n <- DBI::dbGetQuery(con, "SELECT COUNT(*) AS n FROM stacked")$n
  DBI::dbExecute(con, "DELETE FROM stacked WHERE ein IS NULL OR ein = ''")
  after_n  <- DBI::dbGetQuery(con, "SELECT COUNT(*) AS n FROM stacked")$n
  if (before_n != after_n) {
    log_warn(sprintf("Dropped %s rows with missing EIN before dedup",
                     format(before_n - after_n, big.mark = ",")))
  }

  # Stage 2: rank by vintage desc, current-wins-tie. Compute first/last/count.
  # 'current' < 'legacy' alphabetically, so ASC puts current first on ties.
  log_info("Deduping to one row per EIN with first/last vintage markers...")
  t0 <- Sys.time()
  DBI::dbExecute(con, "
    CREATE OR REPLACE TABLE bmf_master AS
    WITH normalized AS (
      SELECT *,
             REPLACE(vintage_underscore, '_', '-') AS bmf_vintage_ym
        FROM stacked
    ),
    ranked AS (
      SELECT *,
             ROW_NUMBER() OVER (
               PARTITION BY ein
               ORDER BY bmf_vintage_ym DESC, bmf_source ASC
             ) AS rn,
             MIN(bmf_vintage_ym) OVER (PARTITION BY ein) AS first_vintage_ym,
             MAX(bmf_vintage_ym) OVER (PARTITION BY ein) AS last_vintage_ym,
             COUNT(*) OVER (PARTITION BY ein)            AS bmf_vintages_observed
        FROM normalized
    )
    SELECT * EXCLUDE (rn, vintage_underscore),
           CAST(SUBSTR(first_vintage_ym, 1, 4) AS INTEGER) AS first_year_in_bmf,
           CAST(SUBSTR(last_vintage_ym,  1, 4) AS INTEGER) AS last_year_in_bmf
      FROM ranked
     WHERE rn = 1
  ")
  log_info(sprintf("Dedup complete (%.1f sec)",
                   as.numeric(Sys.time() - t0, units = "secs")))

  # Drop the staging table to free memory before subsequent queries.
  DBI::dbExecute(con, "DROP TABLE stacked")

  master_n <- DBI::dbGetQuery(con, "SELECT COUNT(*) AS n FROM bmf_master")$n
  log_info(sprintf("Master BMF: %s unique EINs",
                   format(master_n, big.mark = ",")))

  invisible(master_n)
}

#' Write Master BMF outputs (parquet + CSV + dictionary)
#'
#' @param con DuckDB connection (must contain bmf_master table)
#' @param out_dir Local directory to write outputs (default: "data/master")
#' @return Named list of output paths
write_master_outputs <- function(con, out_dir = "data/master") {
  if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

  parquet_path <- file.path(out_dir, "bmf_master.parquet")
  csv_path     <- file.path(out_dir, "bmf_master.csv")
  dict_path    <- file.path(out_dir, "bmf_master_data_dictionary.csv")

  log_info(sprintf("Writing parquet: %s", parquet_path))
  DBI::dbExecute(con, sprintf(
    "COPY bmf_master TO '%s' (FORMAT 'parquet', COMPRESSION 'zstd')",
    parquet_path
  ))

  log_info(sprintf("Writing CSV: %s", csv_path))
  DBI::dbExecute(con, sprintf(
    "COPY bmf_master TO '%s' (FORMAT 'csv', HEADER true)",
    csv_path
  ))

  log_info(sprintf("Writing data dictionary: %s", dict_path))
  master_dt <- data.table::as.data.table(
    DBI::dbGetQuery(con, "SELECT * FROM bmf_master LIMIT 0")
  )
  # Reuse the standard dictionary generator from post_checks.R if available;
  # otherwise emit a minimal name+type listing.
  if (exists("generate_data_dictionary", mode = "function")) {
    sample <- data.table::as.data.table(
      DBI::dbGetQuery(con, "SELECT * FROM bmf_master USING SAMPLE 100000 ROWS")
    )
    dict <- generate_data_dictionary(sample)
  } else {
    dict <- data.table::data.table(
      column = names(master_dt),
      type   = vapply(master_dt, function(x) class(x)[1], character(1))
    )
  }
  data.table::fwrite(dict, dict_path)

  list(parquet = parquet_path, csv = csv_path, dictionary = dict_path)
}
