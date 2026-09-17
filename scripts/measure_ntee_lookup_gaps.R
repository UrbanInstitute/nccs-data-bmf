# Z18 measurement: how often do the 12 NTEE-CC codes that are missing from the
# ntee_code lookup sheet appear in the raw IRS data, per code and per vintage?
#
# Read only. Reads just the raw NTEE column from every per-vintage intermediate
# parquet on S3 (both pipelines) with DuckDB, so nothing large is downloaded.
# Writes two small CSVs to data/quality/:
#   ntee_lookup_gaps_by_vintage.csv  (vintage x code counts with the vintage's row count)
#   ntee_lookup_gaps_summary.csv     (per code: vintages seen, first/last, max, count in the
#                                     latest published current vintage, zero if absent)
#
# Needs live AWS credentials (aws sso login --profile thiya, then export).

suppressMessages({
  library(DBI)
  library(duckdb)
  library(dplyr)
  library(here)
})
source(here::here("R", "config.R"))

BUCKET <- if (exists("BMF_S3_BUCKET")) BMF_S3_BUCKET else "nccsdata"
REGION <- Sys.getenv("AWS_DEFAULT_REGION", unset = "us-east-1")
OUT_DIR <- here::here("data", "quality")
if (!dir.exists(OUT_DIR)) dir.create(OUT_DIR, recursive = TRUE)

# Pinned in tests/testthat/test-ntee-v2.R; shrink both lists together.
GAP_CODES <- c("B29", "E6A", "F31", "K2A", "K2B", "K2C",
               "L4A", "L4B", "M99", "P76", "P7A", "P83")

CUR_GLOB <- sprintf("s3://%s/intermediate/bmf/*/*.parquet", BUCKET)
LEG_GLOB <- sprintf("s3://%s/intermediate/bmf-legacy/*/*.parquet", BUCKET)

con <- dbConnect(duckdb::duckdb())
on.exit(dbDisconnect(con, shutdown = TRUE), add = TRUE)
dbExecute(con, "INSTALL httpfs; LOAD httpfs;")
dbExecute(con, "INSTALL aws; LOAD aws;")
dbExecute(con, sprintf("SET s3_region='%s';", REGION))
dbExecute(con, "CREATE SECRET IF NOT EXISTS s3cred (TYPE S3, PROVIDER credential_chain);")
dbExecute(con, sprintf("SET threads=%s;", Sys.getenv("DUCKDB_THREADS", "4")))

code_list <- paste(sprintf("'%s'", GAP_CODES), collapse = ", ")

# Two result sets per pipeline: every scanned vintage with its row count, and
# the (vintage, code) counts for the gap codes. The vintage is the folder name
# in the S3 path.
obs_cte <- function(glob) sprintf("
  WITH obs AS (
    SELECT regexp_extract(filename, '/([0-9]{4}_[0-9]{2})/[^/]+$', 1) AS vintage,
           upper(trim(CAST(ntee_code_raw AS VARCHAR)))                 AS code
    FROM read_parquet('%s', filename = true, union_by_name = true)
  )", glob)

totals_sql <- function(glob, src) paste0(obs_cte(glob), sprintf("
  SELECT '%s' AS source, vintage, count(*) AS rows
  FROM obs GROUP BY vintage", src))

counts_sql <- function(glob, src) paste0(obs_cte(glob), sprintf("
  SELECT '%s' AS source, vintage, code, count(*) AS n
  FROM obs WHERE code IN (%s) GROUP BY vintage, code", src, code_list))

message("Counting current-pipeline vintages ...")
cur_totals <- dbGetQuery(con, totals_sql(CUR_GLOB, "current"))
cur_counts <- dbGetQuery(con, counts_sql(CUR_GLOB, "current"))
message("Counting legacy-pipeline vintages ...")
leg_totals <- dbGetQuery(con, totals_sql(LEG_GLOB, "legacy"))
leg_counts <- dbGetQuery(con, counts_sql(LEG_GLOB, "legacy"))

# Every scanned vintage, whether or not it holds any gap code.
vintage_totals <- bind_rows(cur_totals, leg_totals) |>
  mutate(rows = as.integer(rows)) |>
  arrange(source, vintage)

# Only (vintage, code) pairs with at least one observation.
positive_code_counts <- bind_rows(cur_counts, leg_counts) |>
  mutate(n = as.integer(n))

code_counts_with_denominators <- positive_code_counts |>
  left_join(vintage_totals, by = c("source", "vintage")) |>
  mutate(share_pct = round(100 * n / rows, 4)) |>
  arrange(vintage, code)

per_code_history <- code_counts_with_denominators |>
  group_by(code) |>
  summarise(
    n_vintages_present = n_distinct(vintage),
    first_vintage      = min(vintage),
    last_vintage       = max(vintage),
    max_n              = max(n),
    total_obs          = sum(n),
    .groups = "drop"
  )

# The latest published current vintage comes from the full vintage list, so a
# vintage with zero gap codes still counts as latest.
latest_current_vintage <- vintage_totals |>
  filter(source == "current") |>
  slice_max(vintage, n = 1)

latest_current_counts <- tibble(code = GAP_CODES) |>
  left_join(
    code_counts_with_denominators |>
      filter(source == "current", vintage == latest_current_vintage$vintage) |>
      select(code, latest_n = n),
    by = "code"
  ) |>
  mutate(
    latest_vintage   = latest_current_vintage$vintage,
    latest_n         = coalesce(latest_n, 0L),
    latest_share_pct = round(100 * latest_n / latest_current_vintage$rows, 4)
  )

complete_summary <- tibble(code = GAP_CODES) |>
  left_join(per_code_history, by = "code") |>
  left_join(latest_current_counts, by = "code") |>
  mutate(
    n_vintages_present = coalesce(n_vintages_present, 0L),
    total_obs          = coalesce(total_obs, 0L),
    n_vintages_total   = nrow(vintage_totals)
  ) |>
  arrange(desc(latest_n), code)

write.csv(code_counts_with_denominators,
          file.path(OUT_DIR, "ntee_lookup_gaps_by_vintage.csv"), row.names = FALSE)
write.csv(complete_summary,
          file.path(OUT_DIR, "ntee_lookup_gaps_summary.csv"), row.names = FALSE)

cat("\nVintages counted:", nrow(vintage_totals),
    "(current:", sum(vintage_totals$source == "current"),
    ", legacy:", sum(vintage_totals$source == "legacy"), ")\n",
    "Latest current vintage:", latest_current_vintage$vintage, "\n\n")
print(as.data.frame(complete_summary))
