# Runbook — NTEE-fix rebuild (provision EC2 from WSL + 2026-06 vintage + master refresh)

Prepared 2026-06-16. Resumes the ADR 0032 NTEE-cleaner fix. Code is already
merged to `main` (PR #23, commit f99a04e). This rebuild publishes the corrected
`nteev2_subsector`/`nteev2` by processing the new 2026-06 vintage and refreshing
the master. Overwrites `latest` in place. Legacy reprocess deferred.

**Instance profile (resolved 2026-06-17):** use `ec2-s3-fullaccess` — it carries
the AWS-managed `AmazonS3FullAccess` policy, so the instance gets R/W to
`s3://nccsdata` with no fallback creds. Already wired into Part A below. (The
short-lived `thiya` SSO credentials fallback remains documented in case the
profile is ever unavailable.)

---

## Part A — Provision the instance (run in WSL on the laptop)

```bash
# --- variables -------------------------------------------------------------
export AWS_PROFILE=thiya
export REGION=$(aws configure get region --profile thiya)   # or hardcode e.g. us-east-1
export ITYPE=m6i.2xlarge          # 8 vCPU / 32 GB
export KEY=ntee-rebuild-key
export SGNAME=ntee-rebuild-sg
export INSTANCE_PROFILE=ec2-s3-fullaccess  # verified 2026-06-17: carries AWS-managed AmazonS3FullAccess (R/W to s3://nccsdata)
```

```bash
# --- 1. Ubuntu 22.04 AMI (region-correct) -----------------------------------
export AMI=$(aws ec2 describe-images --owners 099720109477 \
  --filters "Name=name,Values=ubuntu/images/hvm-ssd/ubuntu-jammy-22.04-amd64-server-*" \
            "Name=state,Values=available" \
  --query 'sort_by(Images,&CreationDate)[-1].ImageId' --output text --region $REGION)
echo "AMI=$AMI"
```

```bash
# --- 2. Key pair ------------------------------------------------------------
aws ec2 create-key-pair --key-name $KEY --query KeyMaterial --output text \
  --region $REGION > ~/.ssh/$KEY.pem && chmod 400 ~/.ssh/$KEY.pem
```

```bash
# --- 3. Security group, SSH from YOUR ip only -------------------------------
export VPC=$(aws ec2 describe-vpcs --filters Name=isDefault,Values=true \
  --query 'Vpcs[0].VpcId' --output text --region $REGION)
export SG=$(aws ec2 create-security-group --group-name $SGNAME \
  --description "NTEE rebuild SSH" --vpc-id $VPC --query GroupId --output text --region $REGION)
export MYIP=$(curl -s https://checkip.amazonaws.com)
aws ec2 authorize-security-group-ingress --group-id $SG \
  --protocol tcp --port 22 --cidr ${MYIP}/32 --region $REGION
```

```bash
# --- 4. Launch (300 GB gp3 root) --------------------------------------------
# Sized for the MASTER build: it stages EVERY processed CSV locally before the
# DuckDB union — ~104 GB as of 2026-06 (legacy ~54 GB + current ~50 GB) and
# growing as vintages accumulate. Add OS/packages, the per-month outputs, and
# DuckDB temp during the dedup over ~100M+ rows: 300 GB is the safe floor.
# (A volume can be grown online later via `aws ec2 modify-volume` + growpart +
#  resize2fs, but provisioning it right avoids a mid-build ENOSPC.)
export IID=$(aws ec2 run-instances --image-id $AMI --instance-type $ITYPE \
  --key-name $KEY --security-group-ids $SG \
  --iam-instance-profile Name=$INSTANCE_PROFILE \
  --block-device-mappings '[{"DeviceName":"/dev/sda1","Ebs":{"VolumeSize":300,"VolumeType":"gp3"}}]' \
  --tag-specifications 'ResourceType=instance,Tags=[{Key=Name,Value=ntee-rebuild}]' \
  --query 'Instances[0].InstanceId' --output text --region $REGION)
aws ec2 wait instance-running --instance-ids $IID --region $REGION
export DNS=$(aws ec2 describe-instances --instance-ids $IID \
  --query 'Reservations[0].Instances[0].PublicDnsName' --output text --region $REGION)
echo "ssh -i ~/.ssh/$KEY.pem ubuntu@$DNS"
```

```bash
# --- 5. Connect -------------------------------------------------------------
ssh -i ~/.ssh/$KEY.pem ubuntu@$DNS
```

> **Credentials fallback (no instance profile):** drop `--iam-instance-profile`
> in step 4; after SSH run `aws configure` (or paste
> `aws configure export-credentials --profile thiya --format env`). SSO creds
> are short-lived — prefer the instance profile.

## Part B — On the instance: run the rebuild

```bash
# 1. code + deps
sudo apt-get update -qq && sudo apt-get install -y -qq git
git clone https://github.com/UrbanInstitute/nccs-data-bmf.git && cd nccs-data-bmf
git log --oneline -1            # expect f99a04e fix(ntee): ... (#23)
bash scripts/setup_ec2.sh

# 1b. Surface instance-profile creds to the R aws.s3 package.
#     The CLI speaks IMDSv2 and works, but aws.s3 only speaks IMDSv1, so it
#     can't read the role creds via the metadata service and S3 returns 403.
#     Export the CLI's resolved creds as env vars (aws.s3 reads those). Valid
#     ~6h; re-run this line before the master/geocode steps if they 403.
#     MUST be run in the SAME shell (and inside tmux, if used) as the pipeline.
eval "$(aws configure export-credentials --format env)"
aws sts get-caller-identity            # sanity: prints the assumed role

# 2. sanity
aws s3 ls s3://nccsdata/processed/bmf/ | tail -3      # up to 2026_05, no 2026_06 yet
Rscript scripts/check_ntee_university_coverage.R      # must print PASS

# 3. process the new 2026-06 vintage (the fix runs here)
# A fresh clone ships only data/{dictionaries,s3-readme}; the pipeline's output
# dirs are gitignored and must exist before the run, or Phase 1 fails on fwrite.
mkdir -p logs data/raw data/intermediate data/processed data/quality data/checkpoints
Rscript -e 'BMF_YEAR<-2026; BMF_MONTH<-6; source("R/run_pipeline.R")' 2>&1 | tee logs/rebuild_2026_06.log

# 4. VERIFY processed output before the master
Rscript -e 'library(data.table); dt<-fread("data/processed/bmf_2026_06_processed.csv", select="nteev2_subsector"); print(dt[,.N,by=nteev2_subsector][order(-N)])'
#   expect UNI~3,589  HOS~5,621  UNU~602,905   (UNI in hundreds -> STOP, wrong code)

# 5. master refresh (overwrites latest in place)
bash scripts/run_master.sh

# 6. verify master
Rscript -e 'library(arrow);library(data.table); m<-as.data.table(read_parquet("data/master/bmf_master.parquet")); cat("rows",nrow(m),"uniqueEIN",uniqueN(m$ein),"\n"); print(m[,.N,by=nteev2_subsector][order(-N)])'
#   rows == uniqueEIN; UNI now ~3,600+

# 7. reach consumers (geocoded master) — re-attach cached geo (NO geocoder calls).
#    The merge reads LOCAL data/geocoding/master/{input,output}/, empty on a
#    fresh box. Pull the cached addr-lookup manifest + the 3 raw geocoder
#    outputs from S3 first. (The raw outputs were staged to S3 on 2026-06-17;
#    geocoding/bmf-master/output/ used to be empty — that gap is now closed.)
mkdir -p data/geocoding/master/input data/geocoding/master/output data/geocoding/master/merged
aws s3 cp s3://nccsdata/geocoding/bmf-master/input/bmf_master_geocoder_addr_lookup.parquet \
          data/geocoding/master/input/
aws s3 cp s3://nccsdata/geocoding/bmf-master/output/ data/geocoding/master/output/ \
          --recursive --exclude "*" --include "bmf_master_geocoder_batch_*_geocoded.csv"

# Run MERGE. NOTE: the orchestrator variable is MASTER_GEOCODING_MODE (NOT
# GEOCODING_MODE). A wrong name silently defaults to "export", which would
# regenerate batches from the new master and overwrite input/ on S3 — do not.
Rscript -e 'MASTER_GEOCODING_MODE<-"merge"; source("R/run_master_geocoding.R")'

# verify geo coverage on the republished master
Rscript -e 'library(arrow);library(data.table); g<-as.data.table(read_parquet("data/geocoding/master/merged/bmf_master_geocoded.parquet")); cat("rows",nrow(g),"with lat/lon",sum(g$geo_is_geocoded,na.rm=TRUE),"\n")'
#   ~1.83M unique addresses expand to the geocoded EIN set; new 2026-06 EINs are NA (expected)
```

> Scope (by design): fixes currently-active EINs (2026-06 wins per-EIN);
> legacy-only EINs keep old values until the deferred legacy reprocess.
> New 2026-06 EINs have null geo until a normal export→geocoder→merge cycle.

UPenn spot-check: `Rscript -e 'library(data.table); dt<-fread("data/processed/bmf_2026_06_processed.csv"); print(dt[EIN=="23-1352685",.(EIN,ntee_code_clean,nteev2_subsector,nteev2)])'` → expect `UNI` / `UNI-B43-RG`.

## Part C — Tear down (run in WSL — stops billing)

```bash
aws ec2 terminate-instances --instance-ids $IID --region $REGION
aws ec2 wait instance-terminated --instance-ids $IID --region $REGION
aws ec2 delete-security-group --group-id $SG --region $REGION
aws ec2 delete-key-pair --key-name $KEY --region $REGION && rm -f ~/.ssh/$KEY.pem
```

## Open items / context

- Contracts: ADR 0032 reconcile = PR #36 (open); ADR 0033 (deprecation-window
  override) being drafted on `adr-0033-deprecation-window-policy`. Merge both
  so the contract record matches the republish.
- Decisions locked: overwrite `latest` in place (no versioned subdir);
  INVALID/UNDEFINED → UNU adopted; current-vintage + master refresh only.
- Local leftover: `data/raw/bmf/2026-06-BMF.csv` (390 MB, gitignored) on the
  laptop — delete to reclaim space.
