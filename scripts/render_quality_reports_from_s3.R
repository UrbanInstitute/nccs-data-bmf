# Re-render every published quality report HTML from the JSON reports on S3.
#
# Why: the batch box that runs the pipelines is temporary, and the HTML
# reports it renders into docs/quality-reports/ (published via GitHub Pages
# and linked from the NCCS website catalog) are lost with it unless they are
# committed. The JSON report is the durable record: it is published next to
# every data set. This script rebuilds the HTML from those JSONs so the site
# always matches what is on S3.
#
# Usage:
#   Rscript scripts/render_quality_reports_from_s3.R [--jobs 4] [--only unified,geocoded,current,legacy]
# Writes docs/quality-reports/*.html and regenerates index.html. Does not
# upload; publish the unified/geocoded HTML with --upload (S3 write).
suppressMessages({library(jsonlite); library(here)})
source(here("R", "config.R")); source(here("R", "utils", "logging.R"))
args <- commandArgs(trailingOnly = TRUE)
opt <- function(flag, default) { i <- match(flag, args); if (is.na(i)) default else args[i + 1] }
jobs   <- as.integer(opt("--jobs", "4"))
only   <- strsplit(opt("--only", "unified,geocoded,current,legacy"), ",")[[1]]
upload <- "--upload" %in% args
bucket <- BMF_S3_BUCKET
out_dir <- here("docs", "quality-reports"); dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
work <- file.path(tempdir(), "qr_render"); dir.create(work, showWarnings = FALSE)

s3_keys <- function(prefix) {
  objs <- aws.s3::get_bucket(bucket, prefix = prefix, max = Inf)
  vapply(objs, function(o) o$Key, character(1))
}
# Timestamps are ISO text in JSON; the templates format() them as date-times.
fix_ts <- function(r) {
  if (is.character(r$timestamp)) {
    p <- as.POSIXct(r$timestamp, tz = "UTC", format = "%Y-%m-%dT%H:%M:%S")
    if (is.na(p)) p <- as.POSIXct(r$timestamp, tz = "UTC", format = "%Y-%m-%d %H:%M:%S")
    if (!is.na(p)) r$timestamp <- p
  }
  r
}
jobs_tbl <- list()
add_job <- function(key, template, out_name) {
  jobs_tbl[[length(jobs_tbl) + 1]] <<- list(key = key, template = template, out = out_name)
}
if ("unified" %in% only)
  add_job("unified/bmf/bmf_unified_quality_report.json", "master_quality_report_template.qmd", "bmf_unified_quality_report.html")
if ("geocoded" %in% only)
  add_job(paste0(BMF_S3_UNIFIED_GEOCODING_PREFIX, "latest/bmf_unified_geocoded_quality_report.json"),
          "geocoded_quality_report_template.qmd", "bmf_unified_geocoded_quality_report.html")
if ("current" %in% only)
  for (k in grep("/bmf_\\d{4}_\\d{2}_quality_report\\.json$", s3_keys("processed/bmf/"), value = TRUE))
    add_job(k, "quality_report_template.qmd", sub("\\.json$", ".html", basename(k)))
if ("legacy" %in% only)
  for (k in grep("/bmf_legacy_\\d{4}_\\d{2}_quality_report\\.json$", s3_keys("processed/bmf-legacy/"), value = TRUE))
    add_job(k, "quality_report_template.qmd", sub("\\.json$", ".html", basename(k)))
log_info(sprintf("%d reports to render (jobs=%d)", length(jobs_tbl), jobs))

# Stage: one working dir per job with its own template copy and RDS.
job_scripts <- character(0)
for (j in jobs_tbl) {
  d <- file.path(work, sub("\\.html$", "", j$out)); dir.create(d, showWarnings = FALSE)
  tmp_json <- file.path(d, "report.json")
  aws.s3::save_object(j$key, bucket = bucket, file = tmp_json)
  r <- fix_ts(fromJSON(tmp_json, simplifyVector = TRUE))
  saveRDS(r, file.path(d, "report.rds"))
  file.copy(here("R", "quality", j$template), file.path(d, "report.qmd"), overwrite = TRUE)
  # One small shell script per job; xargs runs them N at a time.
  js <- file.path(d, "run.sh")
  writeLines(c("#!/usr/bin/env bash", sprintf("cd '%s' || exit 1", d),
               "quarto render report.qmd --to html -P report_data_path:\"$PWD/report.rds\" > render.log 2>&1 || exit 1",
               sprintf("cp report.html '%s'", file.path(out_dir, j$out))), js)
  Sys.chmod(js, "755"); job_scripts <- c(job_scripts, js)
}
writeLines(job_scripts, file.path(work, "jobs.txt"))
system2("xargs", c("-P", jobs, "-n", "1", "bash"), stdin = file.path(work, "jobs.txt"))
rendered <- vapply(jobs_tbl, function(j) file.exists(file.path(out_dir, j$out)) &&
                     file.mtime(file.path(out_dir, j$out)) > Sys.time() - 3600, logical(1))
log_info(sprintf("Rendered %d / %d", sum(rendered), length(jobs_tbl)))
if (any(!rendered)) {
  for (j in jobs_tbl[!rendered]) log_warn(sprintf("FAILED %s (see %s/render.log)", j$out, file.path(work, sub("\\.html$", "", j$out))))
}
source(here("R", "utils", "render_quality_report_index.R")); render_quality_report_index(out_dir)
if (upload) {
  for (j in jobs_tbl) if (j$template != "quality_report_template.qmd" && rendered[match(j$out, vapply(jobs_tbl, `[[`, "", "out"))]) {
    key <- sub("\\.json$", ".html", j$key)
    log_info(sprintf("Uploading %s -> s3://%s/%s: %s", j$out, bucket, key,
                     isTRUE(upload_to_s3(file.path(out_dir, j$out), key))))
  }
}
if (any(!rendered)) quit(status = 1)
