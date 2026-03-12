# ============================================================================
# geocoding_checks.R
# Quality validation for geocoded BMF data
# ============================================================================

# ============================================================================
# Constants
# ============================================================================

# geo_addr_type precision tiers
GEO_PRECISION_HIGH <- c("Subaddress", "PointAddress", "StreetAddress", "StreetInt")
GEO_PRECISION_MEDIUM <- c("StreetName", "StreetAddressExt", "DistanceMarker", "PostalExt")
GEO_PRECISION_LOW <- c("POI", "Locality", "PostalLoc", "Postal")

# Expected geocoder output columns (before renaming)
GEOCODER_EXPECTED_COLUMNS <- c(
  "Latitude", "Longitude", "Match_addr", "Addr_type", "Score",
  "Status", "Region", "RegionAbbr", "Subregion", "MetroArea",
  "City", "Nbrhd", "Postal", "PostalExt", "Country",
  "Match_type", "Distance"
)

# Column rename mapping: geocoder name -> our name
GEOCODER_COLUMN_MAP <- c(
  "Latitude"   = "geo_lat",
  "Longitude"  = "geo_lon",
  "Match_addr" = "geo_match_addr",
  "Addr_type"  = "geo_addr_type",
  "Score"      = "geo_score",
  "Status"     = "geo_status",
  "Region"     = "geo_state",
  "RegionAbbr" = "geo_state_abbr",
  "Subregion"  = "geo_county",
  "MetroArea"  = "geo_metro_area",
  "City"       = "geo_city",
  "Nbrhd"      = "geo_neighborhood",
  "Postal"     = "geo_postal",
  "PostalExt"  = "geo_postal_ext",
  "Country"    = "geo_country",
  "Match_type" = "geo_match_type",
  "Distance"   = "geo_distance"
)

#' Derive geo_precision from geo_addr_type
#'
#' @param addr_type Character vector of geocoder address types
#' @return Character vector of "high", "medium", "low", or NA
#'
#' @export
derive_geo_precision <- function(addr_type) {
  data.table::fcase(
    addr_type %in% GEO_PRECISION_HIGH,   "high",
    addr_type %in% GEO_PRECISION_MEDIUM, "medium",
    addr_type %in% GEO_PRECISION_LOW,    "low",
    default = NA_character_
  )
}

# ============================================================================
# Quality Report
# ============================================================================

#' Generate geocoding quality report
#'
#' @description
#' Produces a quality report for the merged geocoded BMF, covering
#' geocoding rates, precision, score distributions, and per-state breakdowns.
#'
#' @param dt data.table of merged geocoded BMF
#' @param manifest list manifest from geocoding export
#'
#' @return list quality report
#'
#' @export
generate_geocoding_quality_report <- function(dt, manifest = NULL) {

  total_rows <- nrow(dt)

  report <- list(
    timestamp = format(Sys.time(), "%Y-%m-%dT%H:%M:%S"),
    total_records = total_rows,
    geocoding_summary = list(),
    status_distribution = list(),
    precision_distribution = list(),
    score_statistics = list(),
    state_geocoding_rates = list(),
    address_quality_breakdown = list(),
    batch_comparison = list()
  )

  # -------------------------------------------------------------------------
  # Overall geocoding rate
  # -------------------------------------------------------------------------
  geocoded_count <- sum(dt$geo_is_geocoded == TRUE, na.rm = TRUE)
  not_geocoded_count <- total_rows - geocoded_count

  report$geocoding_summary <- list(
    geocoded_count = geocoded_count,
    geocoded_pct = round(100 * geocoded_count / total_rows, 2),
    not_geocoded_count = not_geocoded_count,
    not_geocoded_pct = round(100 * not_geocoded_count / total_rows, 2)
  )

  # -------------------------------------------------------------------------
  # Status distribution (M/T/U)
  # -------------------------------------------------------------------------
  if ("geo_status" %in% names(dt)) {
    status_counts <- dt[!is.na(geo_status), .N, by = geo_status]
    status_total <- sum(status_counts$N)
    report$status_distribution <- lapply(seq_len(nrow(status_counts)), function(i) {
      list(
        status = status_counts$geo_status[i],
        count = status_counts$N[i],
        pct = round(100 * status_counts$N[i] / status_total, 2)
      )
    })
  }

  # -------------------------------------------------------------------------
  # Precision distribution (high/medium/low)
  # -------------------------------------------------------------------------
  if ("geo_precision" %in% names(dt)) {
    precision_counts <- dt[!is.na(geo_precision), .N, by = geo_precision]
    precision_total <- sum(precision_counts$N)
    report$precision_distribution <- lapply(seq_len(nrow(precision_counts)), function(i) {
      list(
        precision = precision_counts$geo_precision[i],
        count = precision_counts$N[i],
        pct = round(100 * precision_counts$N[i] / precision_total, 2)
      )
    })
  }

  # -------------------------------------------------------------------------
  # Score statistics
  # -------------------------------------------------------------------------
  if ("geo_score" %in% names(dt)) {
    scores <- dt[!is.na(geo_score), geo_score]
    if (length(scores) > 0) {
      report$score_statistics <- list(
        mean = round(mean(scores), 2),
        median = round(stats::median(scores), 2),
        p25 = round(stats::quantile(scores, 0.25), 2),
        p75 = round(stats::quantile(scores, 0.75), 2),
        p90 = round(stats::quantile(scores, 0.90), 2),
        min = min(scores),
        max = max(scores)
      )
    }
  }

  # -------------------------------------------------------------------------
  # Geocoding rate by state
  # -------------------------------------------------------------------------
  if ("org_addr_state" %in% names(dt)) {
    state_rates <- dt[
      !is.na(org_addr_state) & org_addr_state != "",
      .(
        total = .N,
        geocoded = sum(geo_is_geocoded == TRUE, na.rm = TRUE)
      ),
      by = org_addr_state
    ]
    state_rates[, geocoded_pct := round(100 * geocoded / total, 2)]
    data.table::setorder(state_rates, geocoded_pct)

    # Flag states with low geocoding rates (< 80%)
    low_rate_states <- state_rates[geocoded_pct < 80]
    report$state_geocoding_rates <- list(
      by_state = lapply(seq_len(nrow(state_rates)), function(i) {
        as.list(state_rates[i])
      }),
      low_rate_count = nrow(low_rate_states),
      low_rate_states = if (nrow(low_rate_states) > 0) {
        low_rate_states$org_addr_state
      } else {
        character(0)
      }
    )
  }

  # -------------------------------------------------------------------------
  # Address quality flag breakdown
  # -------------------------------------------------------------------------
  address_flags <- c("org_addr_is_missing", "org_addr_is_po_box", "org_addr_is_rural_route")
  flag_stats <- list()

  for (flag in address_flags) {
    if (flag %in% names(dt)) {
      flagged <- dt[get(flag) == TRUE]
      flag_stats[[flag]] <- list(
        total = nrow(flagged),
        pct_of_total = round(100 * nrow(flagged) / total_rows, 2),
        geocoded = sum(flagged$geo_is_geocoded == TRUE, na.rm = TRUE),
        geocoded_pct = if (nrow(flagged) > 0) {
          round(100 * sum(flagged$geo_is_geocoded == TRUE, na.rm = TRUE) / nrow(flagged), 2)
        } else {
          NA_real_
        }
      )
    }
  }
  report$address_quality_breakdown <- flag_stats

  # -------------------------------------------------------------------------
  # Batch comparison (if manifest provided)
  # -------------------------------------------------------------------------
  if (!is.null(manifest) && !is.null(manifest$batches)) {
    report$batch_comparison <- list(
      num_batches = manifest$num_batches,
      total_exported = manifest$geocodable_records,
      total_excluded = manifest$excluded_records
    )
  }

  return(report)
}

#' Save geocoding quality report as JSON
#'
#' @param report list quality report from generate_geocoding_quality_report()
#' @param output_path character path to save JSON report
#'
#' @export
save_geocoding_quality_report <- function(report, output_path) {
  dir.create(dirname(output_path), showWarnings = FALSE, recursive = TRUE)
  jsonlite::write_json(report, output_path, pretty = TRUE, auto_unbox = TRUE)
  message(sprintf("Geocoding quality report saved: %s", output_path))
}
