# ============================================================================
# build_ntee_resolved_crosswalk.R
#
# Builds the NTEE-resolved crosswalk: one row per EIN with its NTEE code
# resolved across EVERY vintage (current monthly + legacy), so consumers can
# recover a usable classification for orgs the IRS later NULLed (e.g. Carnegie
# Mellon 25-0969449: NTEE_CD is empty in current BMF but B43 in legacy). See
# nccs-contracts ADR 0034 and docs/16-ntee-resolved-crosswalk.qmd.
#
# Design (validated 2026-06-17):
#   * Aggregate RAW codes (ntee_code_raw). Raw is the verbatim IRS/NCCS source
#     and is vintage-INVARIANT — independent of which cleaner version processed
#     the vintage — so NO reprocessing of legacy/older vintages is required.
#   * Read only (ein, ntee_code_raw) from the all-columns INTERMEDIATE parquets
#     via DuckDB httpfs. Parquet column projection means we pull a small slice
#     of the ~104 GB, so this runs locally; no EC2 needed.
#   * Clean the (small) set of distinct raw codes through the CURRENT, fixed
#     transform_ntee_code() — reuse, never reimplement — then resolve per EIN.
#   * "Expose all, no single pick" (ADR 0016 consumer-composes): publish
#     current / most-recent / modal / full distribution; the consumer chooses.
#
# Requirements: DuckDB + httpfs, AWS creds reachable by DuckDB's credential
# chain (env AWS_* or a configured profile). Run e.g.:
#   eval "$(aws configure export-credentials --profile thiya --format env)"  # or set AWS_*
#   Rscript scripts/build_ntee_resolved_crosswalk.R
# ============================================================================

suppressPackageStartupMessages({
  library(DBI); library(duckdb); library(data.table); library(arrow); library(jsonlite)
})
library(here)
source(here::here("R", "config.R"))                 # lookup_ls, BMF_S3_BUCKET
source(here::here("R", "utils", "logging.R"))
source(here::here("R", "transform_ntee_code.R"))     # the fixed cleaner (reused)

BUCKET   <- if (exists("BMF_S3_BUCKET")) BMF_S3_BUCKET else "nccsdata"
REGION   <- Sys.getenv("AWS_DEFAULT_REGION", unset = "us-east-1")
OUT_DIR  <- here::here("data", "crosswalks")
OUT_STEM <- file.path(OUT_DIR, "ntee_resolved_crosswalk")
if (!dir.exists(OUT_DIR)) dir.create(OUT_DIR, recursive = TRUE)

# Glob every intermediate parquet. The two pipelines use different filenames
# under intermediate/ (current: *_processed.parquet, legacy: *_intermediate.parquet),
# so glob on *.parquet rather than a fixed stem.
CUR_GLOB <- sprintf("s3://%s/intermediate/bmf/*/*.parquet", BUCKET)
LEG_GLOB <- sprintf("s3://%s/intermediate/bmf-legacy/*/*.parquet", BUCKET)

# ---------------------------------------------------------------------------
# 1. Connect + httpfs + S3 credentials (credential chain: env, then profile)
# ---------------------------------------------------------------------------
con <- dbConnect(duckdb::duckdb())
on.exit(dbDisconnect(con, shutdown = TRUE), add = TRUE)
dbExecute(con, "INSTALL httpfs; LOAD httpfs;")
dbExecute(con, "INSTALL aws; LOAD aws;")   # credential_chain secret needs the aws extension
dbExecute(con, sprintf("SET s3_region='%s';", REGION))
dbExecute(con, "CREATE SECRET IF NOT EXISTS s3cred (TYPE S3, PROVIDER credential_chain);")

# Spill-to-disk + memory tuning. The default in-memory connection has no temp
# directory, so the heavy per-(ein,src,raw) hash aggregate cannot spill and
# OOMs on a RAM-limited box. Configure a temp dir and a memory ceiling below
# total RAM so DuckDB streams to disk instead. All env-overridable.
dbExecute(con, sprintf("SET temp_directory='%s';",
                       Sys.getenv("DUCKDB_TEMP_DIR", file.path(tempdir(), "duckdb_spill"))))
dbExecute(con, "SET preserve_insertion_order=false;")
dbExecute(con, sprintf("SET memory_limit='%s';", Sys.getenv("DUCKDB_MEMORY_LIMIT", "9GB")))
dbExecute(con, sprintf("SET threads=%s;",        Sys.getenv("DUCKDB_THREADS", "4")))

# The formatted EIN (XX-XXXXXXX) string column collides case-insensitively with
# the raw integer 'EIN' in the all-columns parquet; DuckDB surfaces the string
# as 'ein_1'. Resolve the column name defensively from one file's schema.
resolve_ein_col <- function(con, glob) {
  cols <- dbGetQuery(con, sprintf(
    "SELECT column_name, column_type FROM (DESCRIBE SELECT * FROM read_parquet('%s'))", glob))
  # prefer the VARCHAR column literally named ein / the dedup 'ein_1'
  cand <- cols$column_name[tolower(cols$column_name) %in% c("ein", "ein_1") &
                           grepl("VARCHAR", cols$column_type, ignore.case = TRUE)]
  if (length(cand) == 0L) stop("Could not find a formatted EIN string column in ", glob)
  # if both 'ein' and 'ein_1' exist, the dedup artifact 'ein_1' is the formatted one
  if ("ein_1" %in% cand) "ein_1" else cand[[1]]
}

log_info("Resolving EIN column from schemas")
ein_cur <- resolve_ein_col(con, CUR_GLOB)
ein_leg <- resolve_ein_col(con, LEG_GLOB)
log_info(sprintf("  current EIN col = %s | legacy EIN col = %s", ein_cur, ein_leg))

# ---------------------------------------------------------------------------
# 2. Per-(ein, src, raw) counts + first/last vintage, plus the latest-current
#    value per EIN (which may legitimately be NULL). Heavy scan stays in DuckDB.
# ---------------------------------------------------------------------------
# vintage_ym is parsed from the filename (intermediate/.../YYYY_MM/...).
obs_sql <- function(glob, src, eincol) sprintf("
  SELECT regexp_extract(filename, '(\\d{4}_\\d{2})', 1) AS vintage_ym,
         \"%s\"                                          AS ein,
         '%s'                                            AS src,
         nullif(trim(CAST(ntee_code_raw AS VARCHAR)), '') AS ntee_raw
  FROM read_parquet('%s', filename = true, union_by_name = true)", eincol, src, glob)

log_info("Building observation view over all intermediate parquets")
dbExecute(con, sprintf("CREATE OR REPLACE TEMP VIEW obs AS %s UNION ALL BY NAME %s;",
                       obs_sql(CUR_GLOB, "current", ein_cur),
                       obs_sql(LEG_GLOB, "legacy",  ein_leg)))

log_info("Aggregating per (ein, src, raw)")
cnt <- as.data.table(dbGetQuery(con, "
  SELECT ein, src, ntee_raw,
         COUNT(*)        AS n,
         MIN(vintage_ym) AS first_vintage,
         MAX(vintage_ym) AS last_vintage
  FROM obs
  WHERE ntee_raw IS NOT NULL AND ein IS NOT NULL
  GROUP BY ein, src, ntee_raw"))

log_info("Computing latest-current value per EIN (may be NULL)")
cur <- as.data.table(dbGetQuery(con, "
  SELECT ein,
         arg_max(ntee_raw, vintage_ym) AS current_raw,
         max(vintage_ym)               AS current_vintage
  FROM obs
  WHERE src = 'current' AND ein IS NOT NULL
  GROUP BY ein"))

log_info(sprintf("Observed: %s (ein,src,raw) rows | %s EINs seen in current",
                 format(nrow(cnt), big.mark = ","), format(nrow(cur), big.mark = ",")))

# ---------------------------------------------------------------------------
# 3. Clean the DISTINCT (raw, src) codes through the fixed transform_ntee_code.
#    legacy_mode applies the pre-2003 5-char crosswalk; key the map on (raw,src)
#    so a legacy 5-char and an identical-looking current code can't collide.
# ---------------------------------------------------------------------------
distinct_codes <- unique(rbind(
  cnt[, .(ntee_raw, src)],
  cur[!is.na(current_raw), .(ntee_raw = current_raw, src = "current")]
))

clean_codes <- function(codes, legacy) {
  if (nrow(codes) == 0L) return(data.table())
  tmp <- data.table(ein = sprintf("00-%07d", seq_len(nrow(codes))),  # dummy grain
                    NTEE_CD = codes$ntee_raw)
  out <- transform_ntee_code(
    tmp,
    ntee_code_lookup         = lookup_ls$ntee_code,
    ntee_major_group_lookup  = lookup_ls$ntee_code_major_group,
    ntee_common_code_lookup  = lookup_ls$ntee_common_code,
    nteev2_subsector_lookup  = lookup_ls$nteev2_subsector,
    ntee_legacy_5char_lookup = lookup_ls$ntee_legacy_5char,
    input_ntee_col = "NTEE_CD",
    year = as.integer(format(Sys.Date(), "%Y")),
    path = NULL, write_scd = FALSE, legacy_mode = legacy
  )
  data.table(ntee_raw = codes$ntee_raw, src = codes$src,
             clean = out$ntee_code_clean,
             subsector = out$nteev2_subsector,
             nteev2 = out$nteev2)
}

log_info(sprintf("Cleaning %s distinct (raw, src) codes via transform_ntee_code()",
                 format(nrow(distinct_codes), big.mark = ",")))
cmap <- rbind(
  clean_codes(distinct_codes[src == "legacy"],  legacy = TRUE),
  clean_codes(distinct_codes[src == "current"], legacy = FALSE)
)
setkey(cmap, ntee_raw, src)

# attach cleaned forms to the per-(ein,src,raw) counts
cnt <- merge(cnt, cmap, by = c("ntee_raw", "src"), all.x = TRUE)

# ---------------------------------------------------------------------------
# 4. Resolve per EIN (all four views; no single opinionated pick — ADR 0034).
# ---------------------------------------------------------------------------
log_info("Resolving per-EIN fields")

# most-recent non-null: row with the greatest last_vintage (tie -> higher count)
setorder(cnt, ein, -last_vintage, -n)
most_recent <- cnt[, .SD[1L], by = ein,
  .SDcols = c("clean", "subsector", "nteev2", "last_vintage", "src")]
setnames(most_recent,
  c("clean", "subsector", "nteev2", "last_vintage", "src"),
  c("ntee_most_recent", "ntee_most_recent_subsector", "ntee_most_recent_nteev2",
    "ntee_most_recent_vintage", "ntee_most_recent_source"))

# modal: collapse to per-(ein,clean) totals, then greatest count (tie -> recency)
modal_base <- cnt[!is.na(clean), .(mn = sum(n), mlast = max(last_vintage)),
                  by = .(ein, clean, subsector, nteev2)]
setorder(modal_base, ein, -mn, -mlast)
modal <- modal_base[, .SD[1L], by = ein,
  .SDcols = c("clean", "subsector", "nteev2", "mn")]
setnames(modal,
  c("clean", "subsector", "nteev2", "mn"),
  c("ntee_modal", "ntee_modal_subsector", "ntee_modal_nteev2", "ntee_modal_n"))

# distribution (JSON keyed on cleaned NTEE-CC) + metadata
dist <- cnt[!is.na(clean), .(n = sum(n), first = min(first_vintage), last = max(last_vintage)),
            by = .(ein, clean)]
dist_json <- dist[, .(
  ntee_code_distribution = toJSON(setNames(
    lapply(seq_len(.N), function(i) list(n = n[i], first = first[i], last = last[i])),
    clean), auto_unbox = TRUE),
  n_distinct_codes = uniqueN(clean),
  n_vintages_with_ntee = sum(n)
), by = ein]

# current (latest current vintage; may be NULL) -> clean via the (raw,'current') map
cur <- merge(cur, cmap[src == "current", .(current_raw = ntee_raw, clean, subsector, nteev2)],
             by = "current_raw", all.x = TRUE)
setnames(cur, c("clean", "subsector", "nteev2"),
         c("ntee_current", "ntee_current_subsector", "ntee_current_nteev2"))

# ---------------------------------------------------------------------------
# 5. Assemble one row per EIN (universe = any EIN with >=1 observed NTEE).
# ---------------------------------------------------------------------------
xwalk <- Reduce(function(a, b) merge(a, b, by = "ein", all = TRUE),
                list(most_recent, modal, dist_json,
                     cur[, .(ein, ntee_current, ntee_current_subsector,
                             ntee_current_nteev2, current_vintage)]))

# agreement flag: single observation / all-agree / mixed
xwalk[, ntee_agreement := fifelse(n_vintages_with_ntee == 1L, "single",
                          fifelse(n_distinct_codes == 1L, "unanimous", "mixed"))]

# keep only EINs that actually carry an NTEE somewhere
xwalk <- xwalk[!is.na(ntee_most_recent) | !is.na(ntee_current)]
setcolorder(xwalk, c("ein",
  "ntee_current", "ntee_current_subsector", "ntee_current_nteev2", "current_vintage",
  "ntee_most_recent", "ntee_most_recent_subsector", "ntee_most_recent_nteev2",
  "ntee_most_recent_vintage", "ntee_most_recent_source",
  "ntee_modal", "ntee_modal_subsector", "ntee_modal_nteev2", "ntee_modal_n",
  "ntee_code_distribution", "n_distinct_codes", "n_vintages_with_ntee", "ntee_agreement"))

log_info(sprintf("Resolved crosswalk: %s EINs", format(nrow(xwalk), big.mark = ",")))

# ---------------------------------------------------------------------------
# 6. Write parquet + csv (publish separately via R/publish_ntee_resolved_crosswalk.R)
# ---------------------------------------------------------------------------
arrow::write_parquet(xwalk, paste0(OUT_STEM, ".parquet"), compression = "zstd")
data.table::fwrite(xwalk, paste0(OUT_STEM, ".csv"))
log_info(sprintf("Wrote %s.{parquet,csv}", OUT_STEM))

# quick sanity line (the motivating case)
cmu <- xwalk[ein == "25-0969449"]
if (nrow(cmu)) log_info(sprintf("CMU check: current=%s most_recent=%s/%s modal=%s",
  cmu$ntee_current, cmu$ntee_most_recent, cmu$ntee_most_recent_subsector, cmu$ntee_modal))
