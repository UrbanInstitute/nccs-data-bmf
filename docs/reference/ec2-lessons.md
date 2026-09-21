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
- **R needs an extra package to use the instance's own AWS credentials,
  and the version on CRAN is too old.** Background: an EC2 instance gets its
  AWS credentials from a small web service that runs on the instance itself
  (the "instance metadata service"). The AWS command-line tool reads it
  directly. The R package we use for S3, `aws.s3`, does not; it relies on a
  helper package, `aws.ec2metadata`, to do that reading. Since 2019 AWS has
  offered a safer version of that service (called IMDSv2) that requires a
  short-lived token with each request, and new instances in our account are
  launched with the older, token-less version switched off. The
  `aws.ec2metadata` release on CRAN (0.2.0, from 2019) predates this and
  never sends the token, so on such an instance it gets nothing back, R
  runs with no credentials at all, and every private S3 call is refused
  with HTTP 403 ("forbidden") while the command-line tool on the same
  machine works. That is exactly what happened on the 2026-09-14 ADR 0048
  reprocess box. The author fixed the package on GitHub (0.2.2, token
  support switched on by the environment variable `USE_IMDS_TOKEN=TRUE`)
  but has not published that fix to CRAN, so `apt`/CRAN cannot give us the
  working version. `setup_ec2.sh` therefore installs the GitHub version and
  sets the variable in `Renviron.site`. One more wrinkle: our runner scripts
  start R with `Rscript --vanilla`, which ignores `Renviron.site`, so each
  runner also has to `export USE_IMDS_TOKEN=TRUE` itself. We are conforming
  to AWS's security default, not the other way round: switching the
  instance back to the token-less service would work too, but it weakens
  the box and the account policy is to keep the safer setting.
- **The batch instance's permissions cover only the `nccsdata` bucket.**
  The role attached to our batch instances (`nccs-bmf-batch`) can read and
  write `s3://nccsdata` and nothing else. The geocoder service lives in a
  different bucket (`geocoding-codestar-prod`): submitting a batch means
  writing a form file and a CSV there, and collecting results means
  checking for and reading a file there. So the geocoding "retrieve" step
  cannot run on the instance. On 2026-09-15 the batch was submitted from
  the maintainer's laptop instead, and the run ledger was updated by hand.
  Fix options: give the role permission to write and read that one bucket,
  or accept that this step always runs from a laptop.
- **Publishing the lookup tables on their own needs `R/manifest.R`
  loaded first.** The lookup publisher calls a helper from `R/manifest.R`
  (`manifest_input_repo()`, which records a repo file's path and checksum
  in the provenance manifest). The master pipeline loads that file before
  calling the publisher, so the dependency was invisible until a one-off
  publish script called the publisher directly and failed with "could not
  find function". `R/publish_lookups.R` now loads `R/manifest.R` itself if
  it has not been loaded.
- **The Quarto HTML render of the Unified BMF quality report fails on the
  box.** The pipeline reports it as a warning and carries on, so the JSON
  report is fine but the HTML copy on `unified/bmf/` is the 2026-08-11
  one. Not yet diagnosed: tracked in https://github.com/UrbanInstitute/nccs-data-bmf/issues/48.
- **Upgrading R in place can leave broken packages behind.** R packages
  that contain compiled code are built against one specific R version. If
  R is upgraded but such packages are left in place (they live in
  `/usr/local/lib/R/site-library`), R still finds and loads the old copies
  first, and they fail with a low-level error such as
  `undefined symbol: SETLENGTH` because the internals they were compiled
  against no longer exist. After any R major upgrade, delete that folder
  and reinstall the packages from the r2u binaries, which are built for
  the installed R.

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

## The expensive one: a transform that quietly emptied a column

The campaign's costliest defect was not a crash. `.clean_zip()` extracted
`^\d{5}`, legacy ZIPs had lost their leading zeros upstream (`02138` stored
as `2138`), and the extraction returned NA. All 55 vintages published with
no ZIP for 100% of legacy rows in the nine 0-prefix states, and ~124k
organizations were geocoded on street, city and state alone. Nobody noticed
for four days. Three lessons, in the order they would have caught it:

- **Post-transformation checks were advisory and nothing read them.**
  `generate_quality_report()` sets `report$passed`, and neither pipeline
  ever looked at it: `STRICT_QUALITY_GATES` was wired only to the
  *pre*-checks. A transform could destroy a column and the run would still
  write, upload and report success, 55 times in a row. A metric nobody gates
  on is a comment. `assert_zip_integrity()` in `R/quality/post_checks.R` now
  halts the run before Phases 10-11 write.
- **A destroyed column is invisible in a completeness percentage.** Losing
  every ZIP in nine states moved national ZIP completeness by a few points,
  which reads as ordinary data messiness. The detectable signal is not
  "how full is this column" but "did populated input become NA output", so
  compare against the input rather than against a threshold.
- **An aggregate gate cannot see a stratified failure.** The address-log
  build had a cross-source match floor precisely to catch broken join keys.
  It passed at 14.32% national while MA, CT, RI, NJ, ME and NH each sat at
  exactly 0.00%. Any national threshold over a country-wide dataset needs a
  per-stratum twin, and the stratum here (state) was the obvious one.

The general form: when a kind of failure is found, leave behind a fast
detector, and make sure something actually fails when it fires.
