# ============================================================================
# S3 Configuration
# ============================================================================

BMF_S3_BUCKET <- "nccsdata"
BMF_S3_PREFIX <- "raw/bmf/"
BMF_S3_INTERMEDIATE_PREFIX <- "intermediate/bmf/"
BMF_S3_PROCESSED_PREFIX <- "processed/bmf/"

# ============================================================================
# S3 Download Functions
# ============================================================================

#' List available BMF files in S3
#'
#' @param bucket S3 bucket name (default: "nccsdata")
#' @param prefix S3 prefix (default: "raw/bmf/")
#'
#' @return Character vector of available year-month values (sorted descending)
#'
#' @examples
#' \dontrun{
#' list_available_bmf_files()
#' # [1] "2025-01" "2024-12" "2024-11" ...
#' }
#'
#' @export
list_available_bmf_files <- function(bucket = BMF_S3_BUCKET,
                                      prefix = BMF_S3_PREFIX) {

  available_files <- aws.s3::get_bucket_df(bucket = bucket, prefix = prefix)
  bmf_files <- available_files[grepl("\\d{4}-\\d{2}-BMF\\.csv$", available_files$Key), ]

  if (nrow(bmf_files) == 0) {
    message("No BMF files found in s3://", bucket, "/", prefix)
    return(character(0))
  }

  year_months <- stringr::str_extract(bmf_files$Key, "\\d{4}-\\d{2}")
  return(sort(year_months, decreasing = TRUE))
}

#' Download BMF from S3
#'
#' @description
#' Downloads a BMF CSV file from S3 and returns it as a data.table.
#' By default, downloads the most recent file. Optionally specify
#' year and month to download a specific file.
#'
#' @param bucket S3 bucket name (default: "nccsdata")
#' @param prefix S3 prefix (default: "raw/bmf/")
#' @param year Optional year (YYYY). If NULL, uses most recent file.
#' @param month Optional month (MM). If NULL, uses most recent file.
#'
#' @return data.table of BMF data with attribute "source_year_month"
#'
#' @examples
#' \dontrun{
#' # Download most recent BMF
#' bmf <- download_bmf_from_s3()
#'
#' # Download specific month
#' bmf <- download_bmf_from_s3(year = 2024, month = 12)
#'
#' # Get the source year-month
#' attr(bmf, "source_year_month")
#' }
#'
#' @export
download_bmf_from_s3 <- function(bucket = BMF_S3_BUCKET,
                                  prefix = BMF_S3_PREFIX,
                                  year = NULL,
                                  month = NULL) {

 # List available BMF files in bucket
  available_files <- aws.s3::get_bucket(bucket = bucket, prefix = prefix) |>
    data.table::rbindlist(fill = TRUE)

  # Filter to BMF CSV files matching pattern YYYY-MM-BMF.csv
  bmf_files <- available_files[grepl("\\d{4}-\\d{2}-BMF\\.csv$", available_files$Key), ]

  if (nrow(bmf_files) == 0) {
    stop("No BMF files found in s3://", bucket, "/", prefix)
  }

  # Extract year-month from filenames for sorting
  bmf_files$year_month <- stringr::str_extract(bmf_files$Key, "\\d{4}-\\d{2}")
  bmf_files <- bmf_files[order(bmf_files$year_month, decreasing = TRUE), ]

  # Determine which file to download
  if (!is.null(year) && !is.null(month)) {
    # User specified year and month
    target_ym <- sprintf("%04d-%02d", as.integer(year), as.integer(month))
    target_key <- sprintf("%s%s-BMF.csv", prefix, target_ym)

    if (!target_key %in% bmf_files$Key) {
      available_ym <- paste(bmf_files$year_month, collapse = ", ")
      stop(sprintf(
        "BMF file for %s not found in S3.\nAvailable: %s",
        target_ym, available_ym
      ))
    }

    s3_key <- target_key
    message(sprintf("Downloading specified BMF file: %s", basename(s3_key)))
  } else {
    # Default to most recent file
    s3_key <- bmf_files$Key[1]
    message(sprintf("Using most recent BMF file: %s", basename(s3_key)))
  }

  # Download and read
  message(sprintf("Downloading from s3://%s/%s", bucket, s3_key))

  temp_file <- tempfile(fileext = ".csv")
  on.exit(unlink(temp_file), add = TRUE)

  aws.s3::save_object(
    object = s3_key,
    bucket = bucket,
    file = temp_file
  )

  bmf_raw <- data.table::fread(temp_file)

  # Extract year-month for downstream use
  attr(bmf_raw, "source_year_month") <- stringr::str_extract(s3_key, "\\d{4}-\\d{2}")

  message(sprintf("Downloaded %s rows from BMF", format(nrow(bmf_raw), big.mark = ",")))

  return(bmf_raw)
}

# ============================================================================
# S3 Upload Functions
# ============================================================================

#' Upload a file to S3
#'
#' @param local_file Path to the local file to upload
#' @param s3_key S3 object key (path within bucket)
#' @param bucket S3 bucket name (default: "nccsdata")
#'
#' @return TRUE if upload succeeded, FALSE otherwise
#'
#' @examples
#' \dontrun{
#' upload_to_s3("data/processed/bmf.parquet", "intermediate/bmf/2025_01/bmf.parquet")
#' }
#'
#' @export
upload_to_s3 <- function(local_file, s3_key, bucket = BMF_S3_BUCKET) {
  if (!file.exists(local_file)) {
    message(sprintf("ERROR: Local file not found: %s", local_file))
    return(FALSE)
  }

  tryCatch({
    aws.s3::put_object(
      file = local_file,
      object = s3_key,
      bucket = bucket
    )
    message(sprintf("Uploaded to s3://%s/%s", bucket, s3_key))
    return(TRUE)
  }, error = function(e) {
    message(sprintf("ERROR uploading to S3: %s", e$message))
    return(FALSE)
  })
}

#' Upload processed BMF results to S3
#'
#' @description
#' Uploads the processed BMF parquet file and quality report JSON to S3.
#' Files are uploaded to intermediate/bmf/YYYY_MM/ directory.
#'
#' @param parquet_path Path to the processed BMF parquet file
#' @param quality_report_path Path to the quality report JSON file
#' @param year Processing year (YYYY)
#' @param month Processing month (MM)
#' @param bucket S3 bucket name (default: "nccsdata")
#'
#' @return Named list with upload status for each file
#'
#' @examples
#' \dontrun{
#' upload_bmf_results(
#'   parquet_path = "data/processed/bmf_2025_01_processed.parquet",
#'   quality_report_path = "data/quality/bmf_2025_01_quality_report.json",
#'   year = "2025",
#'   month = "01"
#' )
#' }
#'
#' @export
upload_bmf_results <- function(parquet_path,
                                quality_report_path,
                                year,
                                month,
                                bucket = BMF_S3_BUCKET) {

  # Construct S3 directory path: intermediate/bmf/YYYY_MM/
  s3_dir <- sprintf("%s%s_%s/", BMF_S3_INTERMEDIATE_PREFIX, year, month)

  # Construct S3 keys for each file
  parquet_s3_key <- sprintf("%sbmf_%s_%s_processed.parquet", s3_dir, year, month)
  quality_s3_key <- sprintf("%sbmf_%s_%s_quality_report.json", s3_dir, year, month)

  message(sprintf("Uploading BMF results to s3://%s/%s", bucket, s3_dir))

  # Upload parquet file
  parquet_success <- upload_to_s3(parquet_path, parquet_s3_key, bucket)

  # Upload quality report
  quality_success <- upload_to_s3(quality_report_path, quality_s3_key, bucket)

  # Return status
  results <- list(
    parquet = parquet_success,
    quality_report = quality_success
  )

  if (all(unlist(results))) {
    message("All files uploaded successfully")
  } else {
    message("WARNING: Some uploads failed")
  }

  return(results)
}

#' Upload processed BMF results to S3 (processed folder)
#'
#' @description
#' Uploads the processed BMF parquet file (without raw columns), quality report,
#' and data dictionary to S3. Files are uploaded to processed/bmf/YYYY_MM/ directory.
#'
#' @param parquet_path Path to the processed BMF parquet file
#' @param quality_report_path Path to the quality report JSON file
#' @param dictionary_path Path to the data dictionary CSV file
#' @param year Processing year (YYYY)
#' @param month Processing month (MM)
#' @param bucket S3 bucket name (default: "nccsdata")
#'
#' @return Named list with upload status for each file
#'
#' @export
upload_processed_bmf <- function(csv_path,
                                  quality_report_path,
                                  dictionary_path,
                                  year,
                                  month,
                                  bucket = BMF_S3_BUCKET) {

  # Construct S3 directory path: processed/bmf/YYYY_MM/
  s3_dir <- sprintf("%s%s_%s/", BMF_S3_PROCESSED_PREFIX, year, month)

  # Construct S3 keys for each file
  csv_s3_key <- sprintf("%sbmf_%s_%s_processed.csv", s3_dir, year, month)
  quality_s3_key <- sprintf("%sbmf_%s_%s_quality_report.json", s3_dir, year, month)
  dictionary_s3_key <- sprintf("%sbmf_%s_%s_data_dictionary.csv", s3_dir, year, month)

  message(sprintf("Uploading processed BMF to s3://%s/%s", bucket, s3_dir))

  # Upload CSV file
  csv_success <- upload_to_s3(csv_path, csv_s3_key, bucket)

  # Upload quality report
  quality_success <- upload_to_s3(quality_report_path, quality_s3_key, bucket)

  # Upload data dictionary
  dictionary_success <- upload_to_s3(dictionary_path, dictionary_s3_key, bucket)

  # Return status
  results <- list(
    csv = csv_success,
    quality_report = quality_success,
    data_dictionary = dictionary_success
  )

  if (all(unlist(results))) {
    message("Processed files uploaded successfully")
  } else {
    message("WARNING: Some uploads failed")
  }

  return(results)
}

# ============================================================================
# Data Dictionary Generation
# ============================================================================

#' Generate data dictionary for processed BMF
#'
#' @description
#' Creates a data dictionary by combining static column descriptions
#' with dynamically computed statistics from the data.
#'
#' @param dt data.table of processed BMF data
#' @param descriptions_path Path to static column descriptions CSV
#'
#' @return data.table with column_name, description, type, null_count, null_pct, unique_count
#'
#' @export
generate_data_dictionary <- function(dt,
                                      descriptions_path = here::here("data/dictionaries/column_descriptions.csv")) {

  # Get column names from data
  col_names <- names(dt)

  # Calculate dynamic stats for each column
  stats <- data.table::data.table(
    column_name = col_names,
    type = sapply(dt, function(x) class(x)[1]),
    null_count = sapply(dt, function(x) sum(is.na(x))),
    unique_count = sapply(dt, function(x) data.table::uniqueN(x, na.rm = TRUE))
  )
  stats[, null_pct := round(null_count / nrow(dt) * 100, 2)]

  # Load static descriptions if file exists
  if (file.exists(descriptions_path)) {
    descriptions <- data.table::fread(descriptions_path)
    # Merge descriptions with stats (left join to keep all columns)
    dictionary <- merge(stats, descriptions, by = "column_name", all.x = TRUE)
  } else {
    message("WARNING: Column descriptions file not found: ", descriptions_path)
    dictionary <- stats
    dictionary[, description := NA_character_]
  }

  # Reorder columns
  data.table::setcolorder(dictionary, c("column_name", "description", "type", "null_count", "null_pct", "unique_count"))

  return(dictionary)
}

# ============================================================================
# Lookup Table Paths
# ============================================================================

# Paths for lookup files used by transformation modules
activity_code_lookup_path <- "data/lookup/activity_code_lookup.csv"

# ============================================================================
# Lookup Tables (from Excel workbook)
# ============================================================================

lookup_path <- "data/lookup/bmf_code_lookup.xlsx"
lookup_ls <- openxlsx::getSheetNames(lookup_path) |>
  purrr::set_names() |>
  purrr::map(~ {
    df <- openxlsx::read.xlsx(xlsxFile = lookup_path, sheet = .x)
    data.table::setDT(df)
  })

# ============================================================================
# Validation Constants
# ============================================================================

# Expected minimum row count for BMF (for validation)
BMF_2025_EXPECTED_ROWS <- 1800000
