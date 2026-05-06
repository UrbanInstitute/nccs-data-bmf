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
                                       threads = NULL) {
  if (!requireNamespace("duckdb", quietly = TRUE)) {
    stop("Package 'duckdb' is required. install.packages('duckdb').")
  }

  # Inline pattern: dbConnect(duckdb::duckdb(), ...). Required because
  # assigning the driver to a local variable and returning only the
  # connection lets R GC finalize the driver, which invalidates the
  # in-memory database mid-pipeline.
  dbdir_arg <- if (is.null(db_path)) ":memory:" else db_path
  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = dbdir_arg)

  DBI::dbExecute(con, sprintf("SET memory_limit = '%s'", memory_limit))
  if (!is.null(threads)) {
    DBI::dbExecute(con, sprintf("SET threads = %d", as.integer(threads)))
  }

  # No httpfs/aws extensions needed: the master build reads CSVs from
  # a local staging directory populated by download_master_inputs(),
  # which uses the R aws.s3 package (already proven-working in this
  # project). DuckDB-side S3 auth (httpfs creds, ListObjectsV2 perms)
  # is bypassed entirely.

  con
}

#' Download all discovered master inputs to a local staging directory
#'
#' Uses the AWS CLI's `aws s3 sync` per source prefix (10-way concurrent
#' transfers, built-in incremental sync), which is dramatically faster
#' than per-file `aws.s3::save_object()` calls. Requires the `aws` CLI
#' on PATH (installed by `scripts/setup_ec2.sh`).
#'
#' Output layout (mirrors the S3 directory structure):
#'   <dest_dir>/current/<YYYY_MM>/bmf_<YYYY_MM>_processed.csv
#'   <dest_dir>/legacy/<YYYY_MM>/bmf_legacy_<YYYY_MM>_processed.csv
#'
#' `aws s3 sync` is inherently incremental — files whose local size +
#' mtime already match the S3 object are skipped. A second run with
#' the same source will be near-instant (metadata calls only).
#'
#' To force a fresh download of every file, delete the staging
#' directory before running, e.g.:
#'   unlink("data/master/staging", recursive = TRUE)
#'
#' @param inputs data.table from discover_master_inputs() — used only
#'   for the bmf_source vector
#' @param dest_dir local staging directory (default: "data/master/staging")
#' @param mirror_delete if TRUE, also delete local files not present
#'   in S3 (passes --delete to aws s3 sync). Default FALSE.
#' @return invisibly returns dest_dir
download_master_inputs <- function(inputs,
                                    dest_dir      = "data/master/staging",
                                    mirror_delete = FALSE) {
  if (!nzchar(Sys.which("aws"))) {
    stop("AWS CLI not found on PATH. Install via scripts/setup_ec2.sh.")
  }
  if (!dir.exists(dest_dir)) dir.create(dest_dir, recursive = TRUE)

  prefix_map <- list(
    current = list(
      s3_src = sprintf("s3://%s/%s",
                       BMF_S3_BUCKET, BMF_S3_PROCESSED_PREFIX),
      include = "bmf_*_processed.csv"
    ),
    legacy = list(
      s3_src = sprintf("s3://%s/%s",
                       BMF_S3_BUCKET, BMF_S3_LEGACY_PROCESSED_PREFIX),
      include = "bmf_legacy_*_processed.csv"
    )
  )

  for (src in unique(inputs$bmf_source)) {
    cfg <- prefix_map[[src]]
    if (is.null(cfg)) next
    local_dst <- file.path(dest_dir, src)
    if (!dir.exists(local_dst)) dir.create(local_dst, recursive = TRUE)

    # Use system() with single-quoted globs so neither R's wrapper nor
    # /bin/sh expand the '*' against the current working directory.
    delete_flag <- if (mirror_delete) " --delete" else ""
    cmd <- sprintf(
      "aws s3 sync %s %s --exclude '*' --include '*/%s'%s",
      shQuote(cfg$s3_src),
      shQuote(local_dst),
      cfg$include,
      delete_flag
    )

    log_info(sprintf("aws s3 sync -> %s (%s)",
                     local_dst, cfg$s3_src))
    t0 <- Sys.time()
    rc <- system(cmd)
    if (rc != 0) {
      stop(sprintf("aws s3 sync failed (rc=%d) for %s\nCommand: %s",
                   rc, src, cmd))
    }
    log_info(sprintf("  done %s sync in %.1f sec",
                     src,
                     as.numeric(Sys.time() - t0, units = "secs")))
  }

  invisible(dest_dir)
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
                              staging_dir = "data/master/staging") {
  if (!"bmf_source" %in% names(inputs)) {
    stop("inputs is missing required column 'bmf_source'. ",
         "Run discover_master_inputs() to construct inputs correctly.")
  }
  has_current <- any(inputs$bmf_source == "current")
  has_legacy  <- any(inputs$bmf_source == "legacy")

  if (!has_current && !has_legacy) {
    stop("No input files discovered for master build.")
  }

  current_root <- normalizePath(file.path(staging_dir, "current"),
                                mustWork = FALSE)
  legacy_root  <- normalizePath(file.path(staging_dir, "legacy"),
                                mustWork = FALSE)

  log_info(sprintf(
    "Stacking %d current + %d legacy CSVs from local staging (%s)...",
    sum(inputs$bmf_source == "current"),
    sum(inputs$bmf_source == "legacy"),
    staging_dir
  ))

  # Read CSVs from local staging directory. aws s3 sync preserves the
  # S3 key structure, so files are at:
  #   staging/current/YYYY_MM/bmf_YYYY_MM_processed.csv
  #   staging/legacy/YYYY_MM/bmf_legacy_YYYY_MM_processed.csv
  # We match the underscore form in the regex and convert to dash with
  # REPLACE so bmf_vintage_ym ends up as "YYYY-MM".
  parts <- c()
  if (has_current) {
    parts <- c(parts, sprintf("
      SELECT *,
             REPLACE(regexp_extract(filename, '(\\d{4}_\\d{2})/[^/]+\\.csv$', 1), '_', '-') AS vintage_dash,
             'current' AS bmf_source
        FROM read_csv_auto('%s/*/bmf_*_processed.csv',
                           union_by_name = true,
                           filename      = true,
                           sample_size   = -1,
                           all_varchar   = false)
    ", current_root))
  }
  if (has_legacy) {
    parts <- c(parts, sprintf("
      SELECT *,
             REPLACE(regexp_extract(filename, '(\\d{4}_\\d{2})/[^/]+\\.csv$', 1), '_', '-') AS vintage_dash,
             'legacy' AS bmf_source
        FROM read_csv_auto('%s/*/bmf_legacy_*_processed.csv',
                           union_by_name = true,
                           filename      = true,
                           sample_size   = -1,
                           all_varchar   = false)
    ", legacy_root))
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
    WITH ranked AS (
      SELECT *,
             vintage_dash AS bmf_vintage_ym,
             ROW_NUMBER() OVER (
               PARTITION BY ein
               ORDER BY vintage_dash DESC, bmf_source ASC
             ) AS rn,
             MIN(vintage_dash) OVER (PARTITION BY ein) AS first_vintage_ym,
             MAX(vintage_dash) OVER (PARTITION BY ein) AS last_vintage_ym,
             COUNT(*) OVER (PARTITION BY ein)          AS bmf_vintages_observed
        FROM stacked
    )
    SELECT * EXCLUDE (rn, vintage_dash, filename),
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
