# ============================================================================
# run_master_geocoding.R
#
# Kept only so that older commands still work. The Unified BMF geocoding
# runner was renamed R/run_unified_geocoding.R when the ADR 0037 rename
# (Master BMF -> Unified BMF) reached the geocoding code. Runbooks and shell
# histories written before then say:
#
#   MASTER_GEOCODING_MODE <- "merge"; source("R/run_master_geocoding.R")
#
# This file passes such a command on to the new runner, with a warning. The
# new runner also honours the old MASTER_GEOCODING_MODE flag. Use
# R/run_unified_geocoding.R and UNIFIED_GEOCODING_MODE in anything new. This
# file can be deleted once no runbook in use refers to it.
# ============================================================================

warning("R/run_master_geocoding.R is now R/run_unified_geocoding.R; passing this run on to the new file.")

source(here::here("R", "run_unified_geocoding.R"))
