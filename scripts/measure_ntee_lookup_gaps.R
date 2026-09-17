# Z18 measurement: how often do the 12 NTEE-CC codes that are missing from the
# ntee_code lookup sheet appear in the raw IRS data, per code and per vintage?
#
# Read only. Reads just the raw NTEE column from every per-vintage intermediate
# parquet on S3 (both pipelines) with DuckDB, so nothing large is downloaded.
# Writes two small CSVs to data/quality/:
#   ntee_lookup_gaps_by_vintage.csv  (vintage x code counts, plus vintage rows)
#   ntee_lookup_gaps_summary.csv     (per code: vintages seen, first/last, max, latest)
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

# One row per (vintage, raw code) for the gap codes, plus the vintage row count
# so shares can be computed. The vintage is the folder name in the S3 path.
count_sql <- function(glob, src) sprintf("
  WITH obs AS (
    SELECT regexp_extract(filename, '/([0-9]{4}_[0-9]{2})/[^/]+$', 1) AS vintage,
           upper(trim(CAST(ntee_code_raw AS VARCHAR)))                 AS code
    FROM read_parquet('%s', filename = true, union_by_name = true)
  )
  SELECT '%s' AS source, vintage, code, count(*) AS n
  FROM obs
  WHERE code IN (%s)
  GROUP BY source, vintage, code
  UNION ALL
  SELECT '%s' AS source, vintage, '_rows' AS code, count(*) AS n
  FROM obs
  GROUP BY source, vintage
", glob, src, code_list, src)

message("Sweeping current-pipeline vintages ...")
cur <- dbGetQuery(con, count_sql(CUR_GLOB, "current"))
message("Sweeping legacy-pipeline vintages ...")
leg <- dbGetQuery(con, count_sql(LEG_GLOB, "legacy"))

long <- bind_rows(cur, leg) |>
  mutate(n = as.integer(n)) |>
  arrange(source, vintage, code)

by_vintage <- long |>
  filter(code != "_rows") |>
  left_join(long |> filter(code == "_rows") |> select(source, vintage, rows = n),
            by = c("source", "vintage")) |>
  mutate(share_pct = round(100 * n / rows, 4)) |>
  arrange(vintage, code)

vintage_rows <- long |> filter(code == "_rows")
all_vintages <- vintage_rows |> distinct(source, vintage)

summary <- by_vintage |>
  group_by(code) |>
  summarise(
    n_vintages_present = n_distinct(vintage),
    first_vintage      = min(vintage),
    last_vintage       = max(vintage),
    max_n              = max(n),
    total_obs          = sum(n),
    .groups = "drop"
  ) |>
  left_join(
    by_vintage |>
      filter(source == "current") |>
      filter(vintage == max(vintage)) |>
      select(code, latest_vintage = vintage, latest_n = n, latest_share_pct = share_pct),
    by = "code"
  ) |>
  right_join(tibble(code = GAP_CODES), by = "code") |>
  mutate(n_vintages_total = nrow(all_vintages)) |>
  arrange(desc(coalesce(latest_n, 0L)), code)

write.csv(by_vintage, file.path(OUT_DIR, "ntee_lookup_gaps_by_vintage.csv"), row.names = FALSE)
write.csv(summary,    file.path(OUT_DIR, "ntee_lookup_gaps_summary.csv"),    row.names = FALSE)

cat("\nVintages swept:", nrow(all_vintages),
    "(current:", sum(all_vintages$source == "current"),
    ", legacy:", sum(all_vintages$source == "legacy"), ")\n\n")
print(as.data.frame(summary))
