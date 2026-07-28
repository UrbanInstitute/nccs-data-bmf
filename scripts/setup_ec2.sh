#!/usr/bin/env bash
# ============================================================================
# setup_ec2.sh
#
# One-shot bootstrap for a fresh Ubuntu 22.04 EC2 instance to run the
# nccs-data-bmf legacy pipeline batch (scripts/run_all_legacy.sh).
#
# Requires an amd64 (x86_64) instance on Ubuntu jammy or noble: R packages
# come from r2u, which publishes for those only. The script checks and exits
# early otherwise. Graviton/arm64 needs a different package strategy.
#
# Installs:
#   - System libraries needed by R packages (curl/ssl/xml2/font stack, cmake)
#   - R + R development headers
#   - Quarto CLI (for quality-report HTML rendering)
#   - AWS CLI v2 (for IAM-role verification and SKIP_EXISTING checks)
#   - All R packages required by the pipeline
#
# AWS credentials are NOT configured here. Either:
#   - attach an IAM role to the instance (preferred), or
#   - run `aws configure` / set AWS_* env vars after this script.
#
# Usage (on the EC2 box, from anywhere):
#   curl -sSL https://raw.githubusercontent.com/UrbanInstitute/nccs-data-bmf/main/scripts/setup_ec2.sh | bash
# or, after cloning:
#   bash scripts/setup_ec2.sh
# ============================================================================
set -euo pipefail

QUARTO_VERSION="${QUARTO_VERSION:-1.6.40}"

log() { printf '\n=== %s ===\n' "$*"; }

log "Updating apt"
sudo apt-get update -y

# r2u publishes amd64 binaries for jammy and noble only. On any other
# architecture or release the r-cran-* installs below either 404 or, worse,
# resolve to Ubuntu's own r-cran-* builds, which are compiled against the
# distro's R (4.1 on jammy) and fail to load under the current R this script
# installs. Fail here with an explanation rather than there with an internals
# error.
host_arch="$(dpkg --print-architecture)"
host_codename="$(lsb_release -cs)"
if [[ "${host_arch}" != "amd64" ]] || [[ ! "${host_codename}" =~ ^(jammy|noble)$ ]]; then
  echo "ERROR: this bootstrap needs amd64 on Ubuntu jammy or noble (r2u's" >&2
  echo "       coverage). Found ${host_arch} on ${host_codename}. Use an" >&2
  echo "       x86_64 instance type, or install R packages from source." >&2
  exit 1
fi

log "Adding the CRAN apt repo (Ubuntu's stock R is too old: current CRAN
arrow/duckdb require R >= 4.2, while jammy ships 4.1, which rotted the
bootstrap on 2026-07-24; see issue #29 batch notes)"
wget -qO- https://cloud.r-project.org/bin/linux/ubuntu/marutter_pubkey.asc \
  | sudo tee /etc/apt/trusted.gpg.d/cran_ubuntu_key.asc >/dev/null
echo "deb https://cloud.r-project.org/bin/linux/ubuntu ${host_codename}-cran40/" \
  | sudo tee /etc/apt/sources.list.d/cran.list >/dev/null

log "Adding the r2u apt repo (prebuilt binaries for every CRAN package:
seconds per package, no source compiles, system deps resolved by apt)"
wget -qO- https://eddelbuettel.github.io/r2u/assets/dirk_eddelbuettel_key.asc \
  | sudo tee /etc/apt/trusted.gpg.d/cranapt_key.asc >/dev/null
echo "deb [arch=amd64] https://r2u.stat.illinois.edu/ubuntu ${host_codename} main" \
  | sudo tee /etc/apt/sources.list.d/cranapt.list >/dev/null

# Pinning is step four of the upstream r2u setup and is not optional: both
# Ubuntu universe and r2u ship r-cran-* packages, and without a preference apt
# picks on version number alone, with no notion of which repo is built against
# the R we just installed. Priority 700 puts r2u ahead of the distro.
sudo tee /etc/apt/preferences.d/99cranapt >/dev/null <<'PIN'
Package: *
Pin: release o=CRAN-Apt Project
Pin: release l=CRAN-Apt Packages
Pin-Priority: 700
PIN

sudo apt-get update -qq

log "Installing system libraries and R"
sudo DEBIAN_FRONTEND=noninteractive apt-get install -y \
  r-base r-base-dev git pandoc cmake \
  libcurl4-openssl-dev libssl-dev libxml2-dev \
  libfontconfig1-dev libharfbuzz-dev libfribidi-dev \
  libpng-dev libtiff5-dev libjpeg-dev libfreetype6-dev \
  libgit2-dev libuv1-dev unzip curl ca-certificates

log "Installing AWS CLI v2 (if not already present)"
if ! command -v aws >/dev/null 2>&1; then
  tmpdir="$(mktemp -d)"
  curl -sSL "https://awscli.amazonaws.com/awscli-exe-linux-x86_64.zip" -o "$tmpdir/awscliv2.zip"
  unzip -q "$tmpdir/awscliv2.zip" -d "$tmpdir"
  sudo "$tmpdir/aws/install" --update
  rm -rf "$tmpdir"
else
  echo "aws CLI already installed: $(aws --version)"
fi

log "Installing Quarto v${QUARTO_VERSION}"
if ! command -v quarto >/dev/null 2>&1 || \
   [[ "$(quarto --version 2>/dev/null)" != "${QUARTO_VERSION}" ]]; then
  tmpdeb="$(mktemp --suffix=.deb)"
  curl -sSL "https://github.com/quarto-dev/quarto-cli/releases/download/v${QUARTO_VERSION}/quarto-${QUARTO_VERSION}-linux-amd64.deb" -o "$tmpdeb"
  sudo dpkg -i "$tmpdeb" || sudo apt-get install -fy
  rm -f "$tmpdeb"
else
  echo "quarto ${QUARTO_VERSION} already installed"
fi

log "Installing R packages (r2u binaries; minimal set by default)"
# Lightweight by default: only what the legacy batch pipeline loads.
# Master-rebuild extras (duckdb/DBI/dplyr) and quarto (optional HTML report
# rendering) install only when requested:
#   INSTALL_MASTER_DEPS=1 bash scripts/setup_ec2.sh
# digest is called directly by R/manifest.R (sha256 for every ADR 0014
# manifest the legacy pipeline writes). It currently arrives as an aws.s3
# dependency, which is luck rather than design, so it is named explicitly.
BATCH_PKGS=(r-cran-data.table r-cran-arrow r-cran-aws.s3 r-cran-openxlsx \
            r-cran-here r-cran-purrr r-cran-stringr r-cran-lubridate \
            r-cran-jsonlite r-cran-digest)
MASTER_PKGS=(r-cran-duckdb r-cran-dbi r-cran-dplyr r-cran-quarto)
sudo DEBIAN_FRONTEND=noninteractive apt-get install -y "${BATCH_PKGS[@]}"
if [[ "${INSTALL_MASTER_DEPS:-0}" == "1" ]]; then
  sudo DEBIAN_FRONTEND=noninteractive apt-get install -y "${MASTER_PKGS[@]}"
fi

log "Verifying R packages load"
Rscript --vanilla -e '
  pkgs <- c("data.table","arrow","aws.s3","openxlsx","here",
            "purrr","stringr","lubridate","jsonlite","digest")
  if (nzchar(Sys.getenv("INSTALL_MASTER_DEPS")) &&
      Sys.getenv("INSTALL_MASTER_DEPS") == "1") {
    pkgs <- c(pkgs, "duckdb","DBI","dplyr","quarto")
  }
  ok <- vapply(pkgs, requireNamespace, logical(1), quietly = TRUE)
  if (!all(ok)) stop("Failed to load: ", paste(pkgs[!ok], collapse = ", "))
  cat("All required R packages installed and loadable.\n")
'

log "Verifying AWS access"
if aws sts get-caller-identity >/dev/null 2>&1; then
  identity="$(aws sts get-caller-identity --query Arn --output text)"
  echo "AWS identity: $identity"
  if aws s3 ls s3://nccsdata/legacy/bmf/ >/dev/null 2>&1; then
    echo "S3 read access to s3://nccsdata/legacy/bmf/ OK"
  else
    echo "WARNING: cannot list s3://nccsdata/legacy/bmf/, check IAM permissions" >&2
  fi
else
  cat >&2 <<'EOF'
WARNING: no AWS credentials detected.
Configure one of:
  - Attach an IAM role to this EC2 instance (preferred), or
  - Run: aws configure
  - Or: export AWS_ACCESS_KEY_ID / AWS_SECRET_ACCESS_KEY / AWS_DEFAULT_REGION
EOF
fi

log "Setup complete"
echo "Next:"
echo "  cd <repo> && bash scripts/run_all_legacy.sh"
