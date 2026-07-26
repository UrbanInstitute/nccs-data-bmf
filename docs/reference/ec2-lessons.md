# EC2 batch lessons (learned the hard way, 2026-07 ADR 0041 campaign)

Field notes from the legacy street-recovery batch: what actually broke,
why, and the rules that now prevent it. Complements `setup_ec2.sh` and
`docs/10-ec2-batch-processing.qmd`.

## Provisioning

- **Profile one unit of work before fanning out.** The runbook claimed
  "several minutes per vintage"; reality on an m6i.8xlarge was 8m29s
  wall and 6.85 GB peak RSS (measure with `/usr/bin/time -v`). Size
  JOBS from measurements: 10 workers on 128 GB ran the 54-vintage batch
  in ~65 minutes with zero failures.
- **Disk: the runbook's per-vintage estimate compounds.** 55 vintages of
  raw + checkpoints + outputs needed several hundred GB; 500 GB gp3 was
  comfortable, 100 GB would have filled mid-batch.
- Instance profile: one minimal role (S3 RW on the one bucket +
  `AmazonSSMManagedInstanceCore`), created by the maintainer, not the
  session. SSM only; no SSH keys; no security-group changes.

## Software environment

- **Ubuntu 22.04 stock R is 4.1 and current CRAN arrow/duckdb need
  >= 4.2.** Add the CRAN apt repo before installing r-base.
- **Never source-compile R packages on a batch box.** r2u serves every
  CRAN package as an apt binary (seconds, dependencies handled). Posit
  P3M binaries lag brand-new R releases (none for R 4.6 in 2026-07).
- **Minimal install sets only**: the nine packages the legacy pipeline
  loads; master-rebuild extras behind `INSTALL_MASTER_DEPS=1`.
- **`aws.s3` (R) cannot see instance-role credentials without
  `aws.ec2metadata` installed.** The CLI works either way, which makes
  this failure look like a bucket-policy problem. Symptom: R gets
  AccessDenied while `aws s3 ls` succeeds.
- **A partially upgraded R leaves ABI landmines.** Packages compiled
  under the old R in `/usr/local/lib/R/site-library` shadow good ones
  and fail with `undefined symbol: SETLENGTH`. Wipe that library after
  any R major upgrade and reinstall from binaries.

## Running long work over SSM

- **`send-command` kills its process group when the command exits.**
  Detach real work: `setsid nohup <cmd> > log 2>&1 < /dev/null &`, then
  poll the log. End every wrapped job with `echo SENTINEL_EXIT=$?` and
  grep for the sentinel; never infer completion from process checks.
- **`pgrep -f <name>` matches the poller's own command line.** A
  monitor "watching" a process that never started reported RUNNING for
  45 minutes. Prefer sentinel files.
- **R's `download.file` default timeout is 60 s**: any multi-hundred-MB
  fetch dies. Set `options(timeout = 3600)` around big downloads.
- **exists()-guard every control flag** a script assigns (the
  `ENABLE_S3_UPLOAD <- TRUE` clobber let a local validation run attempt
  production writes; only missing credentials stopped it).
- Local `aws sso login` sessions expire in hours; anything that must
  survive the night runs on the instance role, not the laptop.

## Data hygiene

- Publish gates fail loudly: check `upload_to_s3()` return values
  (a 403 once logged as "Uploaded ... _manifest.json").
- `as.integer(file.size())` overflows at 2 GiB and writes `bytes: "NA"`
  into manifests: use `as.numeric` (fixed in `R/manifest.R`).
- Validate re-publishes against RAW sources: the bucket has no object
  versioning, so priors are gone the moment you overwrite
  (`scripts/validate_legacy_republish.R` is the standing gate).
