# ============================================================================
# master_geocoding_delta.R
#
# Delta variant of the master geocoding export (Z8 on the contracts
# backlog, first hand-run during the Z1 rebuild 2026-07-29): instead of
# resubmitting all ~3.5M unique addresses to the geocoder service each
# cycle, carry forward results for every address already present in the
# published geocoded Unified BMF and submit ONLY addresses new to this
# build.
#
# Implements the bulk-run etiquette in docs/reference/geocoder-service.md
# (mandatory, maintainer directive 2026-07-25):
#   rule 1  batched submission (GEOCODER_BATCH_SIZE)
#   rule 2  at most MAX_IN_FLIGHT batches in flight; next submits only
#           as prior ones complete (retrieve drives the window)
#   rule 3  geocode_ledger.tsv records EVERY transition (submitted_at,
#           output_seen_at, downloaded_at, status) and is synced to
#           s3://<bucket>/<unified geocoding prefix>runs/{run_id}/
#           so progress survives machine loss
#   rule 4  restarted runs resume from the ledger; submitted-but-pending
#           stems are never resubmitted
#   rule 6  raw service outputs are archived (server-side copy) to the
#           run-stamped runs/{run_id}/ prefix before local staging, so
#           re-merges are free and a later export can safely clear the
#           flat local staging dir
#
# Outputs mirror prepare_master_geocoder_batches() exactly, so
# merge_master_geocoded_results() runs unchanged afterwards:
#   <geocoding_dir>/input/bmf_master_geocoder_addr_lookup.parquet
#   <geocoding_dir>/input/bmf_master_geocoder_manifest.json
#   <geocoding_dir>/input/bmf_master_geocoder_batch_NN.csv       (delta only)
#   <geocoding_dir>/output/bmf_master_geocoder_batch_00_geocoded.csv
#       (carryover: prior results, raw geocoder column names, keyed to the
#        NEW representative EIN per address)
#   <geocoding_dir>/output/bmf_master_geocoder_batch_NN_geocoded.csv
#       (delta results, written by retrieve_master_geocoder_delta())
#
# Carryover semantics: an address counts as "already attempted" if it
# appears in the published geocoded artifact at all -- including addresses
# the geocoder failed to match (their geo_* stay NA). Failures are NOT
# resubmitted; a failed match last month fails this month too, and
# re-tries ride the occasional full re-export instead.
# ============================================================================

MAX_IN_FLIGHT <- 3L  # bulk-run etiquette rule 2

delta_runs_prefix <- function(run_id) {
  paste0(BMF_S3_UNIFIED_GEOCODING_PREFIX, "runs/", run_id, "/")
}

# Write the ledger locally AND sync it to the run-stamped S3 prefix
# (rule 3: the ledger must survive the machine). A LATEST_RUN pointer at
# the runs/ root names the current run so a clean checkout can find it.
delta_ledger_write <- function(led, geocoding_dir, run_id) {
  ledger_path <- file.path(geocoding_dir, "geocode_ledger.tsv")
  data.table::fwrite(led, ledger_path, sep = "\t", quote = FALSE)
  ledger_uploaded <- upload_to_s3(
    ledger_path, paste0(delta_runs_prefix(run_id), "geocode_ledger.tsv"))
  latest_run_path <- file.path(geocoding_dir, "LATEST_RUN")
  writeLines(run_id, latest_run_path)
  pointer_uploaded <- upload_to_s3(
    latest_run_path, paste0(BMF_S3_UNIFIED_GEOCODING_PREFIX,
                            "runs/LATEST_RUN"))
  # REQUIRED, fail closed: every cross-machine safety check discovers the
  # current run through the mirrored ledger + LATEST_RUN pointer. A run
  # whose state cannot be mirrored, or whose pointer is stale, is
  # invisible to those checks -- another machine could then export fresh
  # and resubmit this run's in-flight stems. Better to halt here.
  if (!isTRUE(ledger_uploaded) || !isTRUE(pointer_uploaded)) {
    stop(sprintf(paste0(
      "Ledger/pointer S3 sync failed for run %s (ledger ok: %s, ",
      "pointer ok: %s). Local ledger %s is current; fix S3 access and ",
      "re-run -- do NOT start a fresh export."),
      run_id, isTRUE(ledger_uploaded), isTRUE(pointer_uploaded),
      ledger_path))
  }
  invisible(ledger_path)
}

# Read the ledger for a run. The mirrored S3 copy is AUTHORITATIVE: when
# the run is known, it is always fetched fresh (a same-run local copy can
# still be stale if another machine advanced the run -- acting on it could
# resubmit in-flight stems). Fails closed when the run is known but the
# mirror cannot be fetched. The local file alone is trusted only in the
# legacy no-run_id path where no mirror is expected to exist.
delta_ledger_read <- function(geocoding_dir, run_id = NULL) {
  local_ledger_path <- file.path(geocoding_dir, "geocode_ledger.tsv")
  if (is.null(run_id)) run_id <- delta_latest_run_id()
  if (!is.null(run_id)) {
    remote_tmp <- tempfile(fileext = ".tsv")
    rc <- system2("aws", c("s3", "cp",
                           paste0("s3://", BMF_S3_BUCKET, "/",
                                  delta_runs_prefix(run_id),
                                  "geocode_ledger.tsv"),
                           remote_tmp, "--only-show-errors"))
    if (rc == 0L && file.exists(remote_tmp)) {
      file.copy(remote_tmp, local_ledger_path, overwrite = TRUE)
    } else if (file.exists(local_ledger_path)) {
      led <- data.table::fread(local_ledger_path, sep = "\t",
                               colClasses = "character")
      if (!all(startsWith(led$batch_id, run_id))) {
        stop(sprintf(paste0(
          "Local ledger does not belong to run %s and the mirrored ",
          "copy could not be fetched; refusing to guess."), run_id))
      }
      stop(sprintf(paste0(
        "Mirrored ledger for run %s could not be fetched; refusing to ",
        "act on possibly-stale local state (another machine may have ",
        "advanced this run). Fix S3 access and retry."), run_id))
    }
  }
  stopifnot(file.exists(local_ledger_path))
  data.table::fread(local_ledger_path, sep = "\t", colClasses = "character")
}

# Resolve the most recent run_id from the S3 pointer (NULL if none).
delta_latest_run_id <- function() {
  out <- suppressWarnings(system2(
    "aws", c("s3", "cp",
             paste0("s3://", BMF_S3_BUCKET, "/",
                    BMF_S3_UNIFIED_GEOCODING_PREFIX, "runs/LATEST_RUN"),
             "-"), stdout = TRUE, stderr = FALSE))
  if (length(out) && nzchar(out[1])) out[1] else NULL
}

# Submit staged batches until MAX_IN_FLIGHT are in flight (rules 2 + 4).
# Ledger statuses: staged -> submitted -> retrieved (or failed-*).
delta_submit_window <- function(geocoding_dir, run_id) {
  led <- delta_ledger_read(geocoding_dir, run_id)
  input_dir <- file.path(geocoding_dir, "input")
  in_flight <- sum(led$status == "submitted")
  for (i in which(led$status == "staged")) {
    if (in_flight >= MAX_IN_FLIGHT) break
    stem <- led$service_stem[i]
    fn   <- led$batch_file[i]
    # Clean-checkout resume: restore staged inputs from the run mirror.
    for (miss in c(fn, paste0(stem, ".json"))) {
      lp <- file.path(input_dir, miss)
      if (!file.exists(lp)) {
        rc <- system2("aws", c("s3", "cp",
                               paste0("s3://", BMF_S3_BUCKET, "/",
                                      delta_runs_prefix(run_id), "staged/",
                                      miss),
                               lp, "--only-show-errors"))
        if (rc != 0L || !file.exists(lp)) {
          stop(sprintf(
            "Staged input %s missing locally and at the run mirror", miss))
        }
      }
    }
    # Form JSON first: the CSV's ObjectCreated event is the trigger,
    # so the form must already be in place when the worker reads it.
    form_uploaded <- upload_to_s3(
      file.path(input_dir, paste0(stem, ".json")),
      paste0("data/form-data/", stem, ".json"),
      bucket = "geocoding-codestar-prod")
    csv_uploaded <- upload_to_s3(
      file.path(input_dir, fn),
      paste0("data/input-data/", stem, ".csv"),
      bucket = "geocoding-codestar-prod")
    if (!isTRUE(csv_uploaded) || !isTRUE(form_uploaded)) {
      stop(sprintf("Service submission failed for %s", stem))
    }
    led$submitted_at[i] <- format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z")
    led$status[i] <- "submitted"
    in_flight <- in_flight + 1L
    log_info(sprintf("Submitted %s as %s (%s addresses; %d in flight)",
                     fn, stem, led$n_addresses[i], in_flight))
  }
  delta_ledger_write(led, geocoding_dir, run_id)
  invisible(led)
}


#' Prepare a delta geocoding export against the published geocoded artifact
#'
#' Stages ALL delta batches + form JSONs locally and records them in the
#' ledger as `staged`; with `submit = TRUE` it then opens the submission
#' window (at most MAX_IN_FLIGHT in flight). Remaining batches are
#' submitted by retrieve_master_geocoder_delta() as earlier ones complete.
#'
#' @param master_path        Path to the freshly rebuilt bmf_unified.parquet.
#' @param geocoding_dir      Working dir (same contract as the full export).
#' @param prior_geocoded_uri S3 URI of the published geocoded parquet to
#'                           carry results forward from (default: latest/).
#' @param batch_size         Addresses per delta batch.
#' @param urbanid            Stem prefix for service submission filenames.
#' @param email              Notification email for the service form JSON.
#' @param submit             If TRUE, submit up to MAX_IN_FLIGHT batches now.
#' @return Invisibly: list(run_id, n_unique, n_carryover, n_delta, stems).
#' @export
prepare_master_geocoder_delta <- function(
    master_path        = here::here("data", "master", "bmf_unified.parquet"),
    geocoding_dir      = here::here("data", "geocoding", "master"),
    prior_geocoded_uri = paste0("s3://", BMF_S3_BUCKET, "/",
                                BMF_S3_UNIFIED_GEOCODING_PREFIX,
                                "latest/bmf_unified_geocoded.parquet"),
    batch_size         = GEOCODER_BATCH_SIZE,
    urbanid            = "thiya",
    email              = "TPoongundranar@urban.org",
    submit             = FALSE
  ) {

  stopifnot(file.exists(master_path))
  run_id <- sprintf("delta_%s_%s", format(Sys.time(), "%Y_%m_%d_%H%M%S"), urbanid)

  input_dir  <- file.path(geocoding_dir, "input")
  output_dir <- file.path(geocoding_dir, "output")
  for (d in c(input_dir, output_dir)) {
    if (!dir.exists(d)) dir.create(d, recursive = TRUE)
  }

  # Refuse to start a new run while a prior one has stems in flight
  # (rule 4: resume, don't restart; a fresh export would orphan them).
  # Refuse to start while ANY prior state -- the local workspace ledger OR
  # the mirrored ledger of the run named by LATEST_RUN -- has pending
  # stems. Both are checked: a stale workspace must not hide in-flight
  # work submitted from another machine, and vice versa.
  old_ledger <- file.path(geocoding_dir, "geocode_ledger.tsv")
  check_pending <- function(path, label) {
    if (!file.exists(path)) return(invisible(NULL))
    led <- data.table::fread(path, sep = "\t", colClasses = "character")
    pending <- led[led$status %in% c("staged", "submitted"), ]
    if (nrow(pending) > 0L) {
      stop(sprintf(paste0(
        "%s has %d pending stem(s) (%s). Resume with ",
        "retrieve_master_geocoder_delta(), or mark them failed-* ",
        "before re-exporting."),
        label, nrow(pending),
        paste(pending$service_stem, collapse = ", ")))
    }
    invisible(NULL)
  }
  check_pending(old_ledger, sprintf("Local ledger %s", old_ledger))
  prior_run <- delta_latest_run_id()
  if (!is.null(prior_run)) {
    remote_tmp <- tempfile(fileext = ".tsv")
    rc <- system2("aws", c("s3", "cp",
                           paste0("s3://", BMF_S3_BUCKET, "/",
                                  delta_runs_prefix(prior_run),
                                  "geocode_ledger.tsv"),
                           remote_tmp, "--only-show-errors"))
    # Fail CLOSED: if a prior run exists but its state can't be verified,
    # refuse to export rather than risk double-loading the shared queue.
    if (rc != 0L || !file.exists(remote_tmp)) {
      stop(sprintf(paste0(
        "LATEST_RUN names run %s but its mirrored ledger could not be ",
        "fetched; cannot verify no stems are in flight. Fix S3 access ",
        "and retry."), prior_run))
    }
    check_pending(remote_tmp,
                  sprintf("Mirrored ledger of run %s", prior_run))
  }

  # The merge step glob-reads every *_geocoded.csv in output/, and batch
  # filenames are stable across runs -- leftovers from a prior run would be
  # silently folded into this one. Raw per-run service outputs are already
  # retained at runs/{run_id}/ on S3 (rule 6), so clearing the flat local
  # staging dir loses nothing.
  stale <- list.files(output_dir, pattern = "_geocoded\\.csv$",
                      full.names = TRUE)
  if (length(stale)) {
    unlink(stale)
    log_info(sprintf("Cleared %d stale geocoded file(s) from %s",
                     length(stale), output_dir))
  }

  # ---- geocodable universe + dedup: byte-identical to the full export ----
  log_info(sprintf("Reading master BMF: %s", master_path))
  bmf <- arrow::read_parquet(master_path) |> data.table::as.data.table()
  bmf[, org_addr_full := trimws(as.character(org_addr_full))]
  geocodable <- bmf[!is.na(org_addr_full) & nchar(org_addr_full) > 0]
  log_info(sprintf("Geocodable rows: %s of %s",
                   format(nrow(geocodable), big.mark = ","),
                   format(nrow(bmf), big.mark = ",")))

  data.table::setorder(geocodable, ein)
  unique_addr <- geocodable[, .(ein = ein[1L]),
                            by = .(f_address = org_addr_full)]
  data.table::setorder(unique_addr, ein)
  unique_addr <- unique_addr[, .(ein, f_address)]
  log_info(sprintf("Unique addresses this build: %s",
                   format(nrow(unique_addr), big.mark = ",")))

  # ---- address-lookup manifest (full universe; merge step needs it) ------
  addr_lookup <- geocodable[, .(ein, org_addr_full)]
  data.table::setnames(addr_lookup, "ein", "ein_all")
  addr_lookup <- merge(addr_lookup, unique_addr,
                       by.x = "org_addr_full", by.y = "f_address",
                       all.x = TRUE)
  data.table::setnames(addr_lookup, "ein", "representative_ein")
  addr_lookup_path <- file.path(input_dir,
                                "bmf_master_geocoder_addr_lookup.parquet")
  arrow::write_parquet(addr_lookup, addr_lookup_path)
  log_info(sprintf("Address-lookup manifest: %s (%s rows)",
                   addr_lookup_path,
                   format(nrow(addr_lookup), big.mark = ",")))

  # ---- carryover: prior geocoded results, one row per address ------------
  log_info(sprintf("Reading prior geocoded artifact: %s", prior_geocoded_uri))
  prior_local <- file.path(geocoding_dir, "prior_geocoded.parquet")
  if (startsWith(prior_geocoded_uri, "s3://")) {
    rc <- system2("aws", c("s3", "cp", prior_geocoded_uri, prior_local),
                  stdout = FALSE)
    if (rc != 0L || !file.exists(prior_local)) {
      stop(sprintf("Failed to download prior geocoded artifact: %s",
                   prior_geocoded_uri))
    }
  } else {
    prior_local <- prior_geocoded_uri
  }
  geo_new_names <- unname(GEOCODER_COLUMN_MAP)
  prior_cols <- intersect(c("org_addr_full", geo_new_names),
                          arrow::open_dataset(prior_local)$schema$names)
  prior <- arrow::read_parquet(prior_local, col_select = prior_cols) |>
    data.table::as.data.table()
  prior[, org_addr_full := trimws(as.character(org_addr_full))]
  prior <- prior[!is.na(org_addr_full) & nchar(org_addr_full) > 0]
  if ("geo_lat" %in% names(prior)) {
    data.table::setorder(prior, org_addr_full, -geo_lat, na.last = TRUE)
  }
  prior <- prior[!duplicated(org_addr_full)]
  log_info(sprintf("Prior attempted addresses: %s",
                   format(nrow(prior), big.mark = ",")))

  carryover <- merge(unique_addr, prior,
                     by.x = "f_address", by.y = "org_addr_full")
  delta <- unique_addr[!f_address %in% carryover$f_address]
  log_info(sprintf("Carryover: %s | Delta (new addresses to submit): %s",
                   format(nrow(carryover), big.mark = ","),
                   format(nrow(delta), big.mark = ",")))

  # Carryover file uses the RAW geocoder column names so the merge step's
  # GEOCODER_COLUMN_MAP rename applies uniformly to it and to fresh
  # service outputs alike. Keyed to the NEW representative EIN.
  present <- geo_new_names[geo_new_names %in% names(carryover)]
  raw_names <- names(GEOCODER_COLUMN_MAP)[match(present, GEOCODER_COLUMN_MAP)]
  data.table::setnames(carryover, present, raw_names)
  data.table::setcolorder(carryover, c("ein", "f_address", raw_names))
  carryover_path <- file.path(output_dir,
                              "bmf_master_geocoder_batch_00_geocoded.csv")
  data.table::fwrite(carryover, carryover_path, quote = TRUE)
  log_info(sprintf("Carryover written: %s", carryover_path))

  # ---- stage delta batches + forms; ledger; windowed submission ----------
  stems <- character(0)
  batch_details <- list()
  if (nrow(delta) > 0L) {
    n_batches <- ceiling(nrow(delta) / batch_size)
    idx <- rep(seq_len(n_batches), each = batch_size,
               length.out = nrow(delta))
    batches <- split(delta, idx)
    ts0 <- as.integer(Sys.time())
    for (i in seq_len(n_batches)) {
      fn <- sprintf("bmf_master_geocoder_batch_%02d.csv", i)
      data.table::fwrite(batches[[i]], file.path(input_dir, fn), quote = TRUE)
      stem <- sprintf("%s-%d-public", urbanid, ts0 + i - 1L)
      stems <- c(stems, stem)
      batch_details[[i]] <- list(
        batch_number = i, filename = fn,
        expected_output_filename =
          sprintf("bmf_master_geocoder_batch_%02d_geocoded.csv", i),
        service_stem = stem, row_count = nrow(batches[[i]]),
        first_ein = batches[[i]]$ein[1L],
        last_ein  = batches[[i]]$ein[nrow(batches[[i]])]
      )
      # Full form schema, matching what the web form emits: ALL keys must
      # be present (empty/null where inapplicable). A submission missing
      # the IRB/Y-drive keys wedges the Windows worker silently
      # (diagnosed 2026-08-11 by diffing against a known-good form JSON).
      form <- list(email = email, pii = "No", has_faddress = "on",
                   has_address = "on", pii_project_code = "",
                   is_human_subject = "No", is_irb_approved = NA,
                   has_irb_intake = NA, y_center = "", y_location = "",
                   pii_email = "",
                   filename = paste0(stem, ".csv"),
                   original_filename = fn)
      # NA -> null keeps the keys present (list(NULL) would drop them)
      jsonlite::write_json(form, file.path(input_dir, paste0(stem, ".json")),
                           auto_unbox = TRUE, na = "null")
      # Mirror the staged inputs to the run prefix so a clean-checkout
      # resume can submit batches that never left this machine. REQUIRED:
      # submit-on-resume hard-depends on this mirror, so a failed mirror
      # write must fail the export, not surface later during recovery.
      batch_mirrored <- upload_to_s3(file.path(input_dir, fn),
                   paste0(delta_runs_prefix(run_id), "staged/", fn))
      form_mirrored <- upload_to_s3(file.path(input_dir, paste0(stem, ".json")),
                   paste0(delta_runs_prefix(run_id), "staged/", stem, ".json"))
      if (!isTRUE(batch_mirrored) || !isTRUE(form_mirrored)) {
        stop(sprintf("Staged-input mirror failed for batch %02d (%s)", i, stem))
      }
    }

    led <- data.table::rbindlist(lapply(batch_details, function(b) {
      data.table::data.table(
        batch_id = sprintf("%s_%02d", run_id, b$batch_number),
        service_stem = b$service_stem,
        batch_file = b$filename,
        output_file = b$expected_output_filename,
        n_addresses = as.character(b$row_count),
        submitted_at = "", output_seen_at = "", downloaded_at = "",
        status = "staged")
    }))
  # ---- manifest (same shape as the full export, plus delta fields) -------
  manifest <- list(
    pipeline           = "master",
    mode               = "delta",
    run_id             = run_id,
    created_at         = format(Sys.time(), "%Y-%m-%dT%H:%M:%S"),
    status             = if (submit && length(stems)) "submitting" else "staged",
    master_source      = master_path,
    prior_geocoded_uri = prior_geocoded_uri,
    total_records      = nrow(bmf),
    geocodable_records = nrow(geocodable),
    unique_addresses   = nrow(unique_addr),
    carryover_addresses = nrow(carryover),
    delta_addresses    = nrow(delta),
    batch_size         = batch_size,
    num_batches        = length(stems),
    max_in_flight      = MAX_IN_FLIGHT,
    batches            = batch_details
  )
  manifest_path <- file.path(input_dir, "bmf_master_geocoder_manifest.json")
  jsonlite::write_json(manifest, manifest_path, pretty = TRUE,
                       auto_unbox = TRUE)
  if (length(stems)) {
    # REQUIRED: clean-checkout retrieve reconstructs the run from this
    # mirrored manifest; the run must not become discoverable (via the
    # ledger sync + LATEST_RUN pointer below) without it.
    manifest_mirrored <- upload_to_s3(
      manifest_path, paste0(delta_runs_prefix(run_id),
                            "bmf_master_geocoder_manifest.json"))
    if (!isTRUE(manifest_mirrored)) stop("Run-manifest mirror upload failed.")
  }
  log_info(sprintf("Manifest saved: %s", manifest_path))

    delta_ledger_write(led, geocoding_dir, run_id)
    if (submit) {
      delta_submit_window(geocoding_dir, run_id)
    } else {
      log_info(sprintf(paste0("DRY RUN: %d batch(es) staged; ",
        "MASTER_GEOCODING_MODE='retrieve' submits (windowed) and polls."),
        n_batches))
    }
  } else {
    log_info("No new addresses -- nothing to submit; merge can run now.")
    # Zero-delta runs still get a manifest: retrieve is unnecessary but the
    # run (and the merge that follows) stays documented and mode-checked.
    manifest <- list(
      pipeline = "master", mode = "delta", run_id = run_id,
      created_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%S"),
      status = "complete-no-delta",
      master_source = master_path,
      prior_geocoded_uri = prior_geocoded_uri,
      total_records = nrow(bmf), geocodable_records = nrow(geocodable),
      unique_addresses = nrow(unique_addr),
      carryover_addresses = nrow(carryover), delta_addresses = 0L,
      batch_size = batch_size, num_batches = 0L,
      max_in_flight = MAX_IN_FLIGHT, batches = list())
    manifest_path <- file.path(input_dir,
                               "bmf_master_geocoder_manifest.json")
    jsonlite::write_json(manifest, manifest_path, pretty = TRUE,
                         auto_unbox = TRUE)
    log_info(sprintf("Manifest saved: %s", manifest_path))
  }


  invisible(list(run_id = run_id, n_unique = nrow(unique_addr),
                 n_carryover = nrow(carryover), n_delta = nrow(delta),
                 stems = stems, manifest = manifest))
}


#' Retrieve delta results; archive raw outputs; keep the window full
#'
#' Polls data/output-data/ for submitted stems. On completion: records
#' output_seen_at, archives the raw service output (server-side copy) to
#' the run-stamped runs/{run_id}/ prefix (rule 6), downloads it to
#' <geocoding_dir>/output/ under the batch filename the merge step
#' expects, records downloaded_at + status (rule 3, synced), and submits
#' the next staged batch so MAX_IN_FLIGHT stays full (rule 2).
#'
#' @param geocoding_dir Same dir passed to prepare_master_geocoder_delta().
#' @param wait          If TRUE, poll until every stem is retrieved;
#'                      if FALSE, one pass over what's ready.
#' @param poll_seconds  Interval between polls.
#' @return Invisibly: character vector of downloaded output paths.
#' @export
retrieve_master_geocoder_delta <- function(
    geocoding_dir = here::here("data", "geocoding", "master"),
    wait          = TRUE,
    poll_seconds  = 300
  ) {

  input_dir  <- file.path(geocoding_dir, "input")
  output_dir <- file.path(geocoding_dir, "output")
  manifest_path <- file.path(input_dir, "bmf_master_geocoder_manifest.json")
  if (!file.exists(manifest_path)) {
    prior_run <- delta_latest_run_id()
    if (is.null(prior_run)) {
      stop("No local manifest and no LATEST_RUN pointer; nothing to resume.")
    }
    dir.create(input_dir, recursive = TRUE, showWarnings = FALSE)
    rc <- system2("aws", c("s3", "cp",
                           paste0("s3://", BMF_S3_BUCKET, "/",
                                  delta_runs_prefix(prior_run),
                                  "bmf_master_geocoder_manifest.json"),
                           manifest_path, "--only-show-errors"))
    if (rc != 0L || !file.exists(manifest_path)) {
      stop(sprintf(
        "Could not fetch mirrored manifest for run %s; cannot resume.",
        prior_run))
    }
  }
  manifest <- jsonlite::read_json(manifest_path)
  stopifnot(identical(manifest$mode, "delta"))
  run_id <- manifest$run_id

  # A staged-only ledger (prepare ran with submit = FALSE) has nothing to
  # poll yet: open the initial submission window here so the documented
  # delta -> retrieve mode sequence works without ad hoc calls.
  led0 <- delta_ledger_read(geocoding_dir, run_id)
  if (!any(led0$status == "submitted") && any(led0$status == "staged")) {
    log_info("Staged-only ledger: opening the initial submission window.")
    delta_submit_window(geocoding_dir, run_id)
  }

  got <- character(0)
  repeat {
    led <- delta_ledger_read(geocoding_dir, run_id)
    changed <- FALSE
    for (i in which(led$status == "submitted")) {
      stem <- led$service_stem[i]
      key  <- paste0("data/output-data/", stem, ".csv")
      ok <- system2("aws", c("s3api", "head-object",
                             "--bucket", "geocoding-codestar-prod",
                             "--key", key),
                    stdout = FALSE, stderr = FALSE) == 0L
      if (!ok) next
      led$output_seen_at[i] <- format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z")
      # Rule 6: archive the raw service output to the run-stamped prefix
      # BEFORE local staging; re-merges are then free and a later export
      # can clear the flat local dir without losing anything.
      archive_uri <- paste0("s3://", BMF_S3_BUCKET, "/",
                            delta_runs_prefix(run_id), stem, ".csv")
      archive_rc <- system2("aws", c("s3", "cp",
                               paste0("s3://geocoding-codestar-prod/", key),
                               archive_uri, "--only-show-errors"))
      if (archive_rc != 0L) stop(sprintf("Run archive failed: %s", archive_uri))
      dest <- file.path(output_dir, led$output_file[i])
      download_rc <- system2("aws", c("s3", "cp",
                               paste0("s3://geocoding-codestar-prod/", key),
                               dest, "--only-show-errors"))
      if (download_rc != 0L) stop(sprintf("Download failed: %s", key))
      led$downloaded_at[i] <- format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z")
      led$status[i] <- "retrieved"
      got <- c(got, dest)
      changed <- TRUE
      log_info(sprintf("Retrieved %s -> %s (archived at %s)",
                       stem, dest, archive_uri))
    }
    if (changed) {
      delta_ledger_write(led, geocoding_dir, run_id)
      # Rule 2: keep the window full as batches complete.
      led <- delta_submit_window(geocoding_dir, run_id)
    }
    if (!any(led$status %in% c("staged", "submitted"))) break
    if (!wait) {
      log_info(sprintf("%d stem(s) still pending; run again later.",
                       sum(led$status %in% c("staged", "submitted"))))
      break
    }
    log_info(sprintf("Waiting on %d stem(s); next poll in %ds",
                     sum(led$status == "submitted"), poll_seconds))
    Sys.sleep(poll_seconds)
  }

  invisible(got)
}
