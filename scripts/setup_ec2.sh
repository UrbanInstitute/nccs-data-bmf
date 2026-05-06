#!/usr/bin/env bash
# ============================================================================
# setup_ec2.sh
#
# One-shot bootstrap for a fresh Ubuntu 22.04 EC2 instance to run the
# nccs-data-bmf legacy pipeline batch (scripts/run_all_legacy.sh).
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

log "Installing system libraries and R"
sudo DEBIAN_FRONTEND=noninteractive apt-get install -y \
  r-base r-base-dev git pandoc cmake \
  libcurl4-openssl-dev libssl-dev libxml2-dev \
  libfontconfig1-dev libharfbuzz-dev libfribidi-dev \
  libpng-dev libtiff5-dev libjpeg-dev libfreetype6-dev \
  libgit2-dev unzip curl ca-certificates

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

log "Installing R packages"
Rscript --vanilla -e '
  pkgs <- c("data.table","arrow","aws.s3","openxlsx","here",
            "purrr","stringr","lubridate","jsonlite","quarto",
            "duckdb","DBI")
  to_install <- setdiff(pkgs, rownames(installed.packages()))
  if (length(to_install) > 0) {
    install.packages(to_install, repos = "https://cloud.r-project.org",
                     Ncpus = max(1, parallel::detectCores() - 1))
  }
  ok <- vapply(pkgs, requireNamespace, logical(1), quietly = TRUE)
  if (!all(ok)) {
    stop("Failed to load: ", paste(pkgs[!ok], collapse = ", "))
  }
  cat("All R packages installed and loadable.\n")
'

log "Verifying AWS access"
if aws sts get-caller-identity >/dev/null 2>&1; then
  identity="$(aws sts get-caller-identity --query Arn --output text)"
  echo "AWS identity: $identity"
  if aws s3 ls s3://nccsdata/legacy/bmf/ >/dev/null 2>&1; then
    echo "S3 read access to s3://nccsdata/legacy/bmf/ OK"
  else
    echo "WARNING: cannot list s3://nccsdata/legacy/bmf/ — check IAM permissions" >&2
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
