# Run pipeline for all available BMF files in S3
library(here)

# Source config to get list_available_bmf_files()
source(here::here("R", "config.R"))

# Get all available BMF files
available <- list_available_bmf_files()
message(sprintf("Found %d BMF files to process", length(available)))

# Process each file
for (ym in available) {
  message(sprintf("\n========== Processing %s ==========\n", ym))

  # Clean environment from previous run to ensure fresh state
  # Remove BMF_YEAR/BMF_MONTH so they get properly set
  if (exists("BMF_YEAR")) rm(BMF_YEAR)
  if (exists("BMF_MONTH")) rm(BMF_MONTH)

  # Remove other pipeline variables
  pipeline_vars <- c("bmf", "bmf_raw", "bmf_processed", "PROCESSING_YEAR",
                     "PROCESSING_MONTH", "pre_check_results", "quality_report")
  for (v in pipeline_vars) {
    if (exists(v)) rm(list = v)
  }
  gc()

  # Set year and month for this run
  BMF_YEAR <<- as.integer(substr(ym, 1, 4))
  BMF_MONTH <<- as.integer(substr(ym, 6, 7))

  # Run pipeline
  tryCatch({
    source(here::here("R", "run_pipeline.R"))
    message(sprintf("SUCCESS: %s completed", ym))
  }, error = function(e) {
    message(sprintf("ERROR processing %s: %s", ym, e$message))
  })
}

message("\n========== Batch processing complete ==========")
