# ============================================================================
# build_address_resolved_crosswalk.R
#
# Builds the address-resolved crosswalk: one row per EIN with its mailing
# address resolved across EVERY vintage (current monthly + legacy), so
# consumers can recover address history for longitudinal work (the motivating
# request: reconstructing organizations' reported addresses by year, which the
# one-row-per-EIN Unified BMF cannot carry). See nccs-contracts ADR 0041 §4;
# design copies the NTEE-resolved pattern (ADR 0034): expose all resolutions,
# no opinionated pick, separate join layer keyed on `ein` (ADR 0016).
#
# Design:
#   * Aggregate the RAW address fields (org_addr_{street,city,state,zip}_raw).
#     Raw is verbatim source and vintage-invariant — independent of which
#     cleaner version processed the vintage — so no reprocessing is needed
#     beyond the ADR 0041 legacy street re-publish this depends on.
#   * Street coverage begins at the 2009 legacy vintages (earlier legacy files
#     never carried streets — ADR 0041). Observations with a NULL street but a
#     real city/state/zip still count as addresses; consumers see the street
#     gap honestly instead of losing the whole early history.
#   * Tuples are normalized only for grouping (upper + trim + empty->NULL);
#     the exposed component values keep that normalized form.
#   * "Expose all, no single pick": addr_current (may be NULL for EINs absent
#     from the current pipeline), addr_most_recent, addr_first, plus the full
#     per-address distribution with vintage spans.
#
# Requirements: DuckDB + httpfs, AWS creds reachable by DuckDB's credential
# chain. Sized for the EC2 batch box (address columns are a much fatter
# projection than ntee_code_raw); locally set DUCKDB_MEMORY_LIMIT/THREADS down.
#   eval "$(aws configure export-credentials --profile thiya --format env)"
#   Rscript scripts/build_address_resolved_crosswalk.R
# ============================================================================

suppressPackageStartupMessages({
  library(DBI); library(duckdb); library(data.table); library(arrow); library(jsonlite)
})
library(here)
source(here::here("R", "config.R"))                 # BMF_S3_BUCKET
source(here::here("R", "utils", "logging.R"))
source(here::here("R", "ein.R"))                     # ein_to_prefixed/ein_to_ein2 (ADR 0036)

BUCKET   <- if (exists("BMF_S3_BUCKET")) BMF_S3_BUCKET else "nccsdata"
REGION   <- Sys.getenv("AWS_DEFAULT_REGION", unset = "us-east-1")
OUT_DIR  <- here::here("data", "crosswalks")
OUT_STEM <- file.path(OUT_DIR, "address_resolved_crosswalk")
if (!dir.exists(OUT_DIR)) dir.create(OUT_DIR, recursive = TRUE)

# Env-overridable so the script can be smoke-tested against local parquet
# fixtures (point both at file globs) before an expensive full S3 run.
CUR_GLOB <- Sys.getenv("ADDR_XWALK_CUR_GLOB",
                       sprintf("s3://%s/intermediate/bmf/*/*.parquet", BUCKET))
LEG_GLOB <- Sys.getenv("ADDR_XWALK_LEG_GLOB",
                       sprintf("s3://%s/intermediate/bmf-legacy/*/*.parquet", BUCKET))

# ---------------------------------------------------------------------------
# 1. Connect + httpfs + S3 credentials + spill config (same as ntee-resolved)
# ---------------------------------------------------------------------------
con <- dbConnect(duckdb::duckdb())
on.exit(dbDisconnect(con, shutdown = TRUE), add = TRUE)
# S3 machinery only when a glob actually targets S3 — lets the script run
# against local parquet fixtures with no AWS credentials in scope.
if (any(startsWith(c(CUR_GLOB, LEG_GLOB), "s3://"))) {
  dbExecute(con, "INSTALL httpfs; LOAD httpfs;")
  dbExecute(con, "INSTALL aws; LOAD aws;")
  dbExecute(con, sprintf("SET s3_region='%s';", REGION))
  dbExecute(con, "CREATE SECRET IF NOT EXISTS s3cred (TYPE S3, PROVIDER credential_chain);")
}
dbExecute(con, sprintf("SET temp_directory='%s';",
                       Sys.getenv("DUCKDB_TEMP_DIR", file.path(tempdir(), "duckdb_spill"))))
dbExecute(con, "SET preserve_insertion_order=false;")
dbExecute(con, sprintf("SET memory_limit='%s';", Sys.getenv("DUCKDB_MEMORY_LIMIT", "9GB")))
dbExecute(con, sprintf("SET threads=%s;",        Sys.getenv("DUCKDB_THREADS", "4")))

# Same case-insensitive EIN column collision handling as ntee-resolved.
resolve_ein_col <- function(con, glob) {
  cols <- dbGetQuery(con, sprintf(
    "SELECT column_name, column_type FROM (DESCRIBE SELECT * FROM read_parquet('%s'))", glob))
  cand <- cols$column_name[tolower(cols$column_name) %in% c("ein", "ein_1") &
                           grepl("VARCHAR", cols$column_type, ignore.case = TRUE)]
  if (length(cand) == 0L) stop("Could not find a formatted EIN string column in ", glob)
  if ("ein_1" %in% cand) "ein_1" else cand[[1]]
}

log_info("Resolving EIN column from schemas")
ein_cur <- resolve_ein_col(con, CUR_GLOB)
ein_leg <- resolve_ein_col(con, LEG_GLOB)
log_info(sprintf("  current EIN col = %s | legacy EIN col = %s", ein_cur, ein_leg))

# ---------------------------------------------------------------------------
# 2. Observation view: one normalized address tuple per (vintage, ein).
#    An observation counts as an address when street OR city is present.
# ---------------------------------------------------------------------------
norm <- function(col) sprintf("nullif(trim(upper(CAST(%s AS VARCHAR))), '')", col)

obs_sql <- function(glob, src, eincol) sprintf("
  SELECT regexp_extract(filename, '(\\d{4}_\\d{2})', 1) AS vintage_ym,
         \"%s\" AS ein,
         '%s'   AS src,
         %s     AS street,
         %s     AS city,
         %s     AS state,
         %s     AS zip
  FROM read_parquet('%s', filename = true, union_by_name = true)",
  eincol, src,
  norm("org_addr_street_raw"), norm("org_addr_city_raw"),
  norm("org_addr_state_raw"),  norm("org_addr_zip_raw"), glob)

log_info("Building observation view over all intermediate parquets")
dbExecute(con, sprintf("CREATE OR REPLACE TEMP VIEW obs AS %s UNION ALL BY NAME %s;",
                       obs_sql(CUR_GLOB, "current", ein_cur),
                       obs_sql(LEG_GLOB, "legacy",  ein_leg)))

log_info("Aggregating per (ein, src, address tuple)")
cnt <- as.data.table(dbGetQuery(con, "
  SELECT ein, src, street, city, state, zip,
         COUNT(*)        AS n,
         MIN(vintage_ym) AS first_vintage,
         MAX(vintage_ym) AS last_vintage
  FROM obs
  WHERE ein IS NOT NULL AND (street IS NOT NULL OR city IS NOT NULL)
  GROUP BY ein, src, street, city, state, zip"))

log_info("Computing latest-current tuple per EIN (may be NULL)")
cur <- as.data.table(dbGetQuery(con, "
  SELECT ein,
         arg_max(street, vintage_ym) AS addr_current_street,
         arg_max(city,   vintage_ym) AS addr_current_city,
         arg_max(state,  vintage_ym) AS addr_current_state,
         arg_max(zip,    vintage_ym) AS addr_current_zip,
         max(vintage_ym)             AS addr_current_vintage
  FROM obs
  WHERE src = 'current' AND ein IS NOT NULL
  GROUP BY ein"))

log_info(sprintf("Observed: %s (ein,src,tuple) rows | %s EINs seen in current",
                 format(nrow(cnt), big.mark = ","), format(nrow(cur), big.mark = ",")))

# ---------------------------------------------------------------------------
# 3. Resolve per EIN — most-recent / first / distribution (no single pick).
# ---------------------------------------------------------------------------
log_info("Resolving per-EIN fields")

addr_cols <- c("street", "city", "state", "zip")

# most-recent: greatest last_vintage (tie -> higher count)
setorder(cnt, ein, -last_vintage, -n)
most_recent <- cnt[, .SD[1L], by = ein,
  .SDcols = c(addr_cols, "last_vintage", "src")]
setnames(most_recent,
  c(addr_cols, "last_vintage", "src"),
  c(paste0("addr_most_recent_", addr_cols), "addr_most_recent_vintage",
    "addr_most_recent_source"))

# first: smallest first_vintage (tie -> higher count)
setorder(cnt, ein, first_vintage, -n)
first_addr <- cnt[, .SD[1L], by = ein,
  .SDcols = c(addr_cols, "first_vintage", "src")]
setnames(first_addr,
  c(addr_cols, "first_vintage", "src"),
  c(paste0("addr_first_", addr_cols), "addr_first_vintage", "addr_first_source"))

# distribution (JSON keyed on the concatenated one-line address) + metadata
cnt[, addr_line := paste(fcoalesce(street, ""), fcoalesce(city, ""),
                         fcoalesce(state, ""), fcoalesce(zip, ""), sep = " | ")]
dist <- cnt[, .(n = sum(n), first = min(first_vintage), last = max(last_vintage)),
            by = .(ein, addr_line)]
dist_json <- dist[, .(
  addr_distribution = toJSON(setNames(
    lapply(seq_len(.N), function(i) list(n = n[i], first = first[i], last = last[i])),
    addr_line), auto_unbox = TRUE),
  n_distinct_addresses = uniqueN(addr_line),
  n_vintages_with_address = sum(n)
), by = ein]

# ---------------------------------------------------------------------------
# 4. Assemble one row per EIN (universe = any EIN with >=1 address observed).
# ---------------------------------------------------------------------------
xwalk <- Reduce(function(a, b) merge(a, b, by = "ein", all = TRUE),
                list(most_recent, first_addr, dist_json, cur))

xwalk[, addr_agreement := fifelse(n_vintages_with_address == 1L, "single",
                          fifelse(n_distinct_addresses == 1L, "unanimous", "mixed"))]

# ADR 0036: additive coercion-safe EIN renderings alongside the canonical ein.
xwalk[, ein_prefixed := ein_to_prefixed(ein)]
xwalk[, EIN2         := ein_to_ein2(ein)]

setcolorder(xwalk, c("ein", "ein_prefixed", "EIN2",
  paste0("addr_current_", addr_cols), "addr_current_vintage",
  paste0("addr_most_recent_", addr_cols), "addr_most_recent_vintage", "addr_most_recent_source",
  paste0("addr_first_", addr_cols), "addr_first_vintage", "addr_first_source",
  "addr_distribution", "n_distinct_addresses", "n_vintages_with_address",
  "addr_agreement"))

log_info(sprintf("Resolved crosswalk: %s EINs", format(nrow(xwalk), big.mark = ",")))

# ---------------------------------------------------------------------------
# 5. Write parquet + csv (publish via R/publish_address_resolved_crosswalk.R)
# ---------------------------------------------------------------------------
arrow::write_parquet(xwalk, paste0(OUT_STEM, ".parquet"), compression = "zstd")
data.table::fwrite(xwalk, paste0(OUT_STEM, ".csv"))
log_info(sprintf("Wrote %s.{parquet,csv}", OUT_STEM))

# Sanity: an EIN observed only in legacy should still resolve (addr_current
# NULL, most-recent from legacy), and post-ADR-0041 its street should be
# non-NULL for 2009+ vintages.
leg_only <- xwalk[is.na(addr_current_vintage)][1L]
if (nrow(leg_only)) log_info(sprintf(
  "legacy-only spot check: ein=%s most_recent=%s (%s, %s) vintage=%s",
  leg_only$ein, leg_only$addr_most_recent_street, leg_only$addr_most_recent_city,
  leg_only$addr_most_recent_state, leg_only$addr_most_recent_vintage))
