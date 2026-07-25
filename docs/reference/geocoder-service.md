# Urban's automated geocoder service (how BMF geocoding actually runs)

Documented 2026-07-24 from `UI-Research/techforms-geocoding` (infrastructure
repo, private) so this repo stops describing the geocoder as a "manual"
round-trip. **The geocoder needs no manual activation.** It is an
S3-event-driven service: putting a correctly named CSV in the right prefix is
the entire submission step.

## The system

- **Frontend** (interactive use): https://tech-tools.urban.org/geocoding/
  (repo `UI-Research/tech-aws-forms`).
- **Infrastructure**: `UI-Research/techforms-geocoding` (SAM/Codestar:
  Lambdas, S3 events, buckets).
- **Engine**: a pre-built Windows EC2 instance (`geocoding-service-prod`,
  us-east-1) with an ArcGIS StreetMap Pro license (scripts in
  `UI-Research/geocoding-arcpy-scripts`). It is **started automatically** by
  a Lambda when work arrives and **shuts itself down** when the queue is
  drained. Never start/stop it by hand; never spin up a replacement.
- **Usage dashboard**: `UI-Research/geocoder-usage-visualizer`.

## Event chain (non-confidential path, the one BMF uses)

1. Two objects land in the **`geocoding-codestar-prod`** bucket
   (staging twin: `geocoding-codestar-stg`):
   - `data/input-data/{urbanid}-{unixtimestamp}-public.csv` — the batch.
     Must contain a column **`f_address`** with the address in single-line
     form (`"123 Main St, Denver, CO 80202"`). All other columns ride
     through untouched.
   - `data/form-data/{same-stem}.json` — submission metadata. Required
     fields: `email`, `pii` ("Yes"/"No"), `has_faddress` ("on"),
     `has_address` ("on"), `is_human_subject` ("Yes"/"No"), `filename`
     (system name), `original_filename`. The IRB/Y-Drive fields may be
     empty/null when `pii` and `is_human_subject` are both "No" (always
     the case for BMF: public IRS data).
2. `s3:ObjectCreated` on `data/input-data/*.csv` triggers the
   `geocoding-spinup-ec2-codestar` Lambda, which starts the engine instance
   if it isn't already running (waits out a shutdown-in-progress first).
3. On startup a Windows scheduled task drains the queue **FIFO** and, per
   job, writes:
   - `data/output-data/{same-stem}.csv` — input columns + appended geocode
     columns.
   - `data/log-data/{same-stem}.json` — runtime, instance size, and
     red/yellow/green match counts.
   Then the instance shuts itself down.
4. `s3:ObjectCreated` on `data/output-data/*.csv` triggers the notifier
   Lambda, which emails the `email` from the form JSON a presigned download
   link plus an accuracy summary.

There is also a confidential path (`data/input-confidential-data/...` with a
DataSync hop to the Y drive); BMF never uses it.

## What this means for this repo

- `R/run_geocoding.R` / `R/run_master_geocoding.R` export batches and merge
  results; the middle of the sandwich is fully scriptable: write batch CSVs
  (+ form JSONs) to `s3://geocoding-codestar-prod/data/input-data/`, poll
  `data/output-data/` for the same stems, download, merge. No human step.
- Turnaround is queue-dependent (FIFO, one engine instance, shared
  Urban-wide); there is no documented SLA. Poll rather than assume.
- Batches must add the single-line `f_address` column at export time.
- Anything that changes the geocoder itself is owned by the Urban tech team
  via `techforms-geocoding` / `geocoding-arcpy-scripts`; this repo is a
  client only.
