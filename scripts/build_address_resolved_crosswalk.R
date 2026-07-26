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
#   * One row per (EIN, distinct address tuple); spell_rank 0 = most recent
#     (greatest last_vintage, ties to higher observation count).
#   * ZIP is normalized to 5 digits for the spell key: current raw ZIPs are
#     ZIP+4 while legacy are 5-digit, and without this the same address
#     splits into two spells on format alone (caught by quality stats on the
#     first full build: zero cross-source 'both' spells).
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
         substr(%s, 1, 5) AS zip5
  FROM read_parquet('%s', filename = true, union_by_name = true)",
  eincol, src,
  norm("org_addr_street_raw"), norm("org_addr_city_raw"),
  norm("org_addr_state_raw"),  norm("org_addr_zip_raw"), glob)

log_info("Building observation view over all intermediate parquets")
dbExecute(con, sprintf("CREATE OR REPLACE TEMP VIEW obs AS %s UNION ALL BY NAME %s;",
                       obs_sql(CUR_GLOB, "current", ein_cur),
                       obs_sql(LEG_GLOB, "legacy",  ein_leg)))

log_info("Aggregating per (ein, address tuple) across sources")
spells <- as.data.table(dbGetQuery(con, "
  SELECT ein, street, city, state, zip5,
         SUM(cnt)                            AS n_vintages,
         MIN(first_v)                        AS first_vintage,
         MAX(last_v)                         AS last_vintage,
         CASE WHEN COUNT(DISTINCT src) > 1 THEN 'both' ELSE MIN(src) END AS source
  FROM (
    SELECT ein, src, street, city, state, zip5,
           COUNT(*) AS cnt, MIN(vintage_ym) AS first_v, MAX(vintage_ym) AS last_v
    FROM obs
    WHERE ein IS NOT NULL AND (street IS NOT NULL OR city IS NOT NULL)
    GROUP BY ein, src, street, city, state, zip5
  )
  GROUP BY ein, street, city, state, zip5"))

log_info(sprintf("Spells: %s rows across %s EINs",
                 format(nrow(spells), big.mark = ","),
                 format(uniqueN(spells$ein), big.mark = ",")))

# ---------------------------------------------------------------------------
# 3. Rank spells per EIN: 0 = most recent (last_vintage desc, n desc).
# ---------------------------------------------------------------------------
setorder(spells, ein, -last_vintage, -n_vintages, street, na.last = TRUE)
spells[, spell_rank := seq_len(.N) - 1L, by = ein]
spells[, n_distinct_addresses := .N, by = ein]

# ADR 0036 renderings; EIN2 leads per the maintainer's key spec.
spells[, ein_prefixed := ein_to_prefixed(ein)]
spells[, EIN2         := ein_to_ein2(ein)]

setcolorder(spells, c("EIN2", "ein", "ein_prefixed", "spell_rank",
  "street", "city", "state", "zip5",
  "first_vintage", "last_vintage", "n_vintages", "source",
  "n_distinct_addresses"))

# ---------------------------------------------------------------------------
# 4. Write parquet + csv + quality JSON
#    (publish via R/publish_address_resolved_crosswalk.R)
# ---------------------------------------------------------------------------
arrow::write_parquet(spells, paste0(OUT_STEM, ".parquet"), compression = "zstd")
data.table::fwrite(spells, paste0(OUT_STEM, ".csv"))

quality <- list(
  timestamp            = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z"),
  total_spell_rows     = nrow(spells),
  distinct_eins        = uniqueN(spells$ein),
  spells_per_ein_mean  = round(nrow(spells) / uniqueN(spells$ein), 3),
  spells_per_ein_max   = spells[, max(n_distinct_addresses)],
  pct_eins_multi_addr  = round(100 * uniqueN(spells[n_distinct_addresses > 1L, ein]) /
                               uniqueN(spells$ein), 2),
  street_null_spells   = spells[is.na(street), .N],
  source_counts        = as.list(table(spells$source)),
  current_spells       = spells[spell_rank == 0L, .N]
)
jsonlite::write_json(quality, paste0(OUT_STEM, "_quality.json"),
                     auto_unbox = TRUE, pretty = TRUE)
log_info(sprintf("Wrote %s.{parquet,csv,_quality.json}", OUT_STEM))
log_info(sprintf("Quality: %s spells | %s EINs | %.2f%% multi-address | max spells %d",
                 format(quality$total_spell_rows, big.mark = ","),
                 format(quality$distinct_eins, big.mark = ","),
                 quality$pct_eins_multi_addr, quality$spells_per_ein_max))
