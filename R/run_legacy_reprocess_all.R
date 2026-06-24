# ============================================================================
# run_legacy_reprocess_all.R
#
# Reprocess every legacy NCCS 501CX-NONPROFIT-PX vintage listed in
# s3://nccsdata/legacy/bmf/ through the current fixed pipeline. Each vintage
# runs in its own Rscript subprocess so memory is released between runs.
#
# Idempotency / resume:
#   - Reads processed/bmf-legacy/YYYY_MM/_manifest.json from S3.
#   - Skips vintages whose remote manifest already records the current git SHA
#     (unless LEGACY_REPROCESS_FORCE <- TRUE).
#   - Persists a local summary CSV so interrupted runs can resume and still
#     report row counts / Z99 share for already-completed vintages.
#
# Usage:
#   source("R/run_legacy_reprocess_all.R")
#
# Optional flags before sourcing:
#   LEGACY_REPROCESS_FORCE         <- TRUE
#   LEGACY_REPROCESS_DRY_RUN       <- TRUE
#   LEGACY_REPROCESS_ORDER         <- "newest-first"
#   LEGACY_REPROCESS_SKIP_VINTAGES <- c("2017-09")
#   LEGACY_REPROCESS_STOP_ON_ERROR <- TRUE
# ============================================================================

if (!exists("LEGACY_REPROCESS_FORCE"))         LEGACY_REPROCESS_FORCE <- FALSE
if (!exists("LEGACY_REPROCESS_DRY_RUN"))       LEGACY_REPROCESS_DRY_RUN <- FALSE
if (!exists("LEGACY_REPROCESS_ORDER"))         LEGACY_REPROCESS_ORDER <- "oldest-first"
if (!exists("LEGACY_REPROCESS_SKIP_VINTAGES")) LEGACY_REPROCESS_SKIP_VINTAGES <- character(0)
if (!exists("LEGACY_REPROCESS_STOP_ON_ERROR")) LEGACY_REPROCESS_STOP_ON_ERROR <- FALSE

suppressPackageStartupMessages({
  library(data.table)
})

source(here::here("R", "config.R"))
source(here::here("R", "manifest.R"))
source(here::here("R", "utils", "logging.R"))

LEGACY_REPROCESS_LOG_DIR <- here::here("logs", "legacy_reprocess")
LEGACY_REPROCESS_SUMMARY_PATH <- file.path(LEGACY_REPROCESS_LOG_DIR, "summary.csv")

legacy_reprocess_processed_csv_path <- function(vintage_tag) {
  here::here("data", "processed",
             sprintf("bmf_legacy_%s_processed.csv", vintage_tag))
}

legacy_reprocess_manifest_key <- function(vintage_tag) {
  sprintf("%s%s/_manifest.json", BMF_S3_LEGACY_PROCESSED_PREFIX, vintage_tag)
}

legacy_reprocess_processed_file_key <- function(vintage_tag) {
  sprintf("bmf_legacy_%s_processed.csv", vintage_tag)
}

legacy_reprocess_row_count_from_manifest <- function(remote_manifest, vintage_tag) {
  if (is.null(remote_manifest) || is.null(remote_manifest$files)) {
    return(NA_integer_)
  }
  entry <- remote_manifest$files[[legacy_reprocess_processed_file_key(vintage_tag)]]
  if (is.null(entry) || is.null(entry$row_count)) return(NA_integer_)
  as.integer(entry$row_count)
}

legacy_reprocess_read_metrics <- function(csv_path) {
  if (!file.exists(csv_path)) {
    return(list(row_count = NA_integer_, z99_share = NA_real_))
  }

  dt <- tryCatch(
    data.table::fread(csv_path, select = "nteev2_code"),
    error = function(e) NULL
  )
  if (is.null(dt)) {
    return(list(row_count = NA_integer_, z99_share = NA_real_))
  }

  row_count <- nrow(dt)
  z99_share <- if (row_count == 0L || !"nteev2_code" %in% names(dt)) {
    NA_real_
  } else {
    mean(dt$nteev2_code == "Z99", na.rm = TRUE)
  }

  list(row_count = as.integer(row_count), z99_share = z99_share)
}

legacy_reprocess_load_summary <- function(path) {
  if (!file.exists(path)) {
    return(data.table(
      vintage = character(),
      status = character(),
      git_sha = character(),
      row_count = integer(),
      z99_share = numeric(),
      started_at = character(),
      finished_at = character(),
      elapsed_seconds = numeric(),
      log_path = character()
    ))
  }

  data.table::fread(path, colClasses = list(character = c(
    "vintage", "status", "git_sha", "started_at", "finished_at", "log_path"
  )))
}

legacy_reprocess_save_summary <- function(summary_dt, path) {
  data.table::setorder(summary_dt, vintage)
  data.table::fwrite(summary_dt, path)
}

legacy_reprocess_upsert_summary <- function(summary_dt, row) {
  row_dt <- as.data.table(row)
  summary_dt <- summary_dt[vintage != row$vintage]
  data.table::rbindlist(list(summary_dt, row_dt), fill = TRUE)
}

legacy_reprocess_cached_metrics <- function(summary_dt, vintage_tag, target_sha) {
  hit <- summary_dt[vintage == vintage_tag & git_sha == target_sha][.N]
  if (hit == 0L) {
    return(list(row_count = NA_integer_, z99_share = NA_real_))
  }
  row <- summary_dt[vintage == vintage_tag & git_sha == target_sha][1L]
  list(row_count = as.integer(row$row_count), z99_share = as.numeric(row$z99_share))
}

legacy_reprocess_run_vintage <- function(year, month, code_sha, dry_run = FALSE) {
  vintage_dash <- sprintf("%04d-%02d", as.integer(year), as.integer(month))
  vintage_tag  <- gsub("-", "_", vintage_dash, fixed = TRUE)
  log_path     <- file.path(LEGACY_REPROCESS_LOG_DIR,
                            sprintf("bmf_legacy_%s.log", vintage_tag))
  started_at   <- format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")

  cat(sprintf("[%s] %s code_sha=%s\n", started_at, vintage_dash, code_sha),
      file = log_path)

  if (dry_run) {
    return(list(
      status = "dry_run",
      row_count = NA_integer_,
      z99_share = NA_real_,
      started_at = started_at,
      finished_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
      elapsed_seconds = 0,
      log_path = log_path
    ))
  }

  expr <- sprintf(
    "LEGACY_BMF_YEAR <- %d; LEGACY_BMF_MONTH <- %d; source(%s)",
    as.integer(year),
    as.integer(month),
    shQuote("R/run_legacy_pipeline.R")
  )

  t0 <- Sys.time()
  stdout_path <- tempfile(pattern = sprintf("legacy_%s_stdout_", vintage_tag))
  stderr_path <- tempfile(pattern = sprintf("legacy_%s_stderr_", vintage_tag))
  on.exit(unlink(c(stdout_path, stderr_path), force = TRUE), add = TRUE)
  rc <- system2(
    command = file.path(R.home("bin"), "Rscript"),
    args = c("--vanilla", "-e", expr),
    stdout = stdout_path,
    stderr = stderr_path
  )
  file.append(log_path, c(stdout_path, stderr_path))
  elapsed_seconds <- round(as.numeric(Sys.time() - t0, units = "secs"), 1)
  finished_at <- format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")

  if (!identical(rc, 0L)) {
    return(list(
      status = sprintf("failed_rc%d", rc),
      row_count = NA_integer_,
      z99_share = NA_real_,
      started_at = started_at,
      finished_at = finished_at,
      elapsed_seconds = elapsed_seconds,
      log_path = log_path
    ))
  }

  metrics <- legacy_reprocess_read_metrics(
    legacy_reprocess_processed_csv_path(vintage_tag)
  )

  list(
    status = "ok",
    row_count = metrics$row_count,
    z99_share = metrics$z99_share,
    started_at = started_at,
    finished_at = finished_at,
    elapsed_seconds = elapsed_seconds,
    log_path = log_path
  )
}

dir.create(here::here("data", "raw", "legacy"), recursive = TRUE, showWarnings = FALSE)
dir.create(here::here("data", "intermediate"), recursive = TRUE, showWarnings = FALSE)
dir.create(here::here("data", "processed"), recursive = TRUE, showWarnings = FALSE)
dir.create(here::here("data", "quality"), recursive = TRUE, showWarnings = FALSE)
dir.create(here::here("data", "checkpoints"), recursive = TRUE, showWarnings = FALSE)
dir.create(LEGACY_REPROCESS_LOG_DIR, recursive = TRUE, showWarnings = FALSE)

log_phase_start("LEGACY REPROCESS ALL")
code_sha <- git_short_sha()
if (is.na(code_sha) || !nzchar(code_sha)) {
  stop("Could not determine git SHA for idempotent legacy reprocess.")
}
log_info(sprintf("Current code SHA: %s", code_sha))

summary_dt <- legacy_reprocess_load_summary(LEGACY_REPROCESS_SUMMARY_PATH)
vintages <- list_available_legacy_bmf_files()
if (length(vintages) == 0L) {
  stop("No legacy vintages found in S3.")
}

if (identical(LEGACY_REPROCESS_ORDER, "oldest-first")) {
  vintages <- rev(vintages)
} else if (!identical(LEGACY_REPROCESS_ORDER, "newest-first")) {
  stop("LEGACY_REPROCESS_ORDER must be 'oldest-first' or 'newest-first'.")
}

if (length(LEGACY_REPROCESS_SKIP_VINTAGES) > 0L) {
  vintages <- setdiff(vintages, LEGACY_REPROCESS_SKIP_VINTAGES)
}

log_info(sprintf("Queued %d legacy vintages (%s).",
                 length(vintages), LEGACY_REPROCESS_ORDER))

for (ym in vintages) {
  year <- sub("-.*$", "", ym)
  month <- sub("^.*-", "", ym)
  vintage_tag <- gsub("-", "_", ym, fixed = TRUE)

  remote_manifest <- read_existing_manifest(
    legacy_reprocess_manifest_key(vintage_tag),
    bucket = BMF_S3_BUCKET
  )
  remote_sha <- if (is.null(remote_manifest) || is.null(remote_manifest$git_sha)) {
    NA_character_
  } else {
    as.character(remote_manifest$git_sha)
  }

  if (!LEGACY_REPROCESS_FORCE && !is.na(remote_sha) && identical(remote_sha, code_sha)) {
    cached <- legacy_reprocess_cached_metrics(summary_dt, vintage_tag, code_sha)
    local_metrics <- legacy_reprocess_read_metrics(
      legacy_reprocess_processed_csv_path(vintage_tag)
    )
    log_path <- file.path(LEGACY_REPROCESS_LOG_DIR,
                          sprintf("bmf_legacy_%s.log", vintage_tag))
    row_count <- if (!is.na(local_metrics$row_count)) {
      local_metrics$row_count
    } else if (!is.na(cached$row_count)) {
      cached$row_count
    } else {
      legacy_reprocess_row_count_from_manifest(remote_manifest, vintage_tag)
    }
    z99_share <- if (!is.na(local_metrics$z99_share)) {
      local_metrics$z99_share
    } else {
      cached$z99_share
    }

    log_info(sprintf("SKIP %s (already processed at git_sha=%s)", ym, code_sha))
    cat(sprintf("[%s] SKIP %s code_sha=%s (remote manifest matches current SHA)\n",
                format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
                ym,
                code_sha),
        file = log_path)
    summary_dt <- legacy_reprocess_upsert_summary(summary_dt, list(
      vintage = vintage_tag,
      status = "skipped_sha",
      git_sha = code_sha,
      row_count = row_count,
      z99_share = z99_share,
      started_at = NA_character_,
      finished_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
      elapsed_seconds = 0,
      log_path = log_path
    ))
    legacy_reprocess_save_summary(summary_dt, LEGACY_REPROCESS_SUMMARY_PATH)
    next
  }

  log_info(sprintf("RUN  %s", ym))
  result <- legacy_reprocess_run_vintage(
    year = year,
    month = month,
    code_sha = code_sha,
    dry_run = LEGACY_REPROCESS_DRY_RUN
  )

  summary_dt <- legacy_reprocess_upsert_summary(summary_dt, list(
    vintage = vintage_tag,
    status = result$status,
    git_sha = code_sha,
    row_count = result$row_count,
    z99_share = result$z99_share,
    started_at = result$started_at,
    finished_at = result$finished_at,
    elapsed_seconds = result$elapsed_seconds,
    log_path = result$log_path
  ))
  legacy_reprocess_save_summary(summary_dt, LEGACY_REPROCESS_SUMMARY_PATH)

  if (LEGACY_REPROCESS_STOP_ON_ERROR &&
      startsWith(result$status, "failed")) {
    stop(sprintf("Stopping on failure for %s (%s). See %s",
                 ym, result$status, result$log_path))
  }
}

log_phase_start("LEGACY REPROCESS SUMMARY")
summary_view <- copy(summary_dt)
summary_view[, z99_share_pct := round(100 * z99_share, 2)]
data.table::setorder(summary_view, vintage)
print(summary_view[, .(vintage, status, row_count, z99_share_pct, git_sha)])
log_info(sprintf("Summary saved: %s", LEGACY_REPROCESS_SUMMARY_PATH))
