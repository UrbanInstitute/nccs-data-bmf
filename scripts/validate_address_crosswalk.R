# ============================================================================
# validate_address_crosswalk.R
#
# Standing invariant suite for the address-resolved crosswalk (see
# docs/reference/address-data-invariants.md). Run after every build,
# before every publish:
#   Rscript scripts/validate_address_crosswalk.R
# Env: ADDR_XWALK_PARQUET, MASTER_PARQUET override the default paths.
# Exits nonzero on any violation.
# ============================================================================

suppressPackageStartupMessages({library(arrow); library(data.table)})
library(here)
source(here::here("R", "utils", "logging.R"))
source(here::here("R", "ein.R"))

xp <- Sys.getenv("ADDR_XWALK_PARQUET",
                 here::here("data", "crosswalks", "address_resolved_crosswalk.parquet"))
mp <- Sys.getenv("MASTER_PARQUET", "")

x <- setDT(read_parquet(xp))
fail <- character(0)
chk <- function(ok, msg) if (!isTRUE(ok)) fail <<- c(fail, msg) else log_info(paste("PASS:", msg))

chk(all(nchar(x$zip5) <= 5, na.rm = TRUE), "zip5 length <= 5")
chk(x[, max(spell_rank) + 1L == .N, by = ein][, all(V1)], "spell ranks contiguous per EIN")
chk(anyDuplicated(x[, .(ein, street, city, state, zip5)]) == 0, "no duplicate spell tuples per EIN")
chk(x[, all(EIN2 == ein_to_ein2(ein))], "EIN2 consistent with canonical ein")
chk(x[, all(ein_prefixed == ein_to_prefixed(ein))], "ein_prefixed consistent with canonical ein")
if (nrow(x) > 1e6) {
  both_share <- x[source == "both", .N] / nrow(x)
  chk(both_share >= 0.01,
      sprintf("cross-source spell share %.2f%% >= 1%% floor", 100 * both_share))
}

# Sample-match spell-0 against the master BMF raw values (current-source orgs)
if (nzchar(mp) && file.exists(mp)) {
  m <- setDT(read_parquet(mp, col_select = c(
    "EIN2", "org_addr_street_raw", "org_addr_city_raw", "org_addr_zip_raw", "bmf_source")))
  set.seed(42)
  samp <- sample(m[bmf_source == "current" & !is.na(org_addr_street_raw), EIN2],
                 min(1000L, m[bmf_source == "current", .N]))
  j <- merge(x[spell_rank == 0L & EIN2 %in% samp],
             m[EIN2 %in% samp], by = "EIN2")
  match_ok <- j[, mean(
    toupper(trimws(org_addr_street_raw)) == street &
    toupper(trimws(org_addr_city_raw))   == city &
    substr(org_addr_zip_raw, 1, 5)       == zip5, na.rm = TRUE)]
  chk(match_ok > 0.99,
      sprintf("spell-0 sample match vs master raw: %.2f%% (n=%d)", 100 * match_ok, nrow(j)))
} else {
  log_info("SKIP: master parquet not provided (set MASTER_PARQUET for the sample-match check)")
}

if (length(fail)) {
  for (f in fail) log_info(paste("FAIL:", f))
  stop(sprintf("Address crosswalk validation: %d failure(s)", length(fail)))
}
log_info("Address crosswalk validation: all checks passed")
