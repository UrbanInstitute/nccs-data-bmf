# FABLE: FORMATED TABLES 

fable <- function( x, p=FALSE, dig=3, as.factor=FALSE ){
  
  nm.x <- gsub( ".\\$", "", deparse(substitute(x)) )
  
  if( as.factor )
  { x <- factor(x) }
  
  t <- table( x, useNA="ifany" ) 
  
  # COUNTS OF THINGS 
  if( p == FALSE )
  {
    t <- as.data.frame(t)
    names(t) <- c( nm.x, "Freq" )
  
    if( class(x) != "factor" )
    { t <- dplyr::arrange( t, desc(Freq) ) }
    
    t$Freq <- format( t$Freq, big.mark="," )
    k <- knitr::kable( t, align=c("r","r") )
  }
  
  # PROPORTIONS OF THINGS 
  if( p == TRUE )
  {
    t <- prop.table(t) 
    t <- as.data.frame(t)
    
    if( class(x) != "factor" )
    { t <- dplyr::arrange( t, desc(Freq) ) }
    
    names(t) <- c( nm.x, "Freq" )
    t$Freq <- round( t$Freq, dig ) |> format( nsmall = dig )
    k <- knitr::kable( t, align=c("r","r") )
  } 
  
  print(k)
  invisible(t)
}

# fable( d$NCCS_LEVEL_3 )
# fable( d$NCCS_LEVEL_3, p=TRUE )      # as proportions
# fable( d$NCCS_LEVEL_3, as.factor=T ) # orders by label, not count

#' @title Function to get the contents of an S3 Bucket
#' @param bucket_name character scalar. Name of S3 Bucket
#' @param bucket_folder character scalar. Folder to get contents from
#' @param bucket_url character scalar. Base url of S3 Bucket
get_s3_bucket_contents <- function( bucket_name, bucket_folder, bucket_url ){
  
  s3 <- paws::s3()
  obj <- s3$list_objects( Bucket = bucket_name )
  keys <- unlist( purrr::map( obj$Contents, purrr::pluck, "Key" ) )
  keys <- keys[ grepl( bucket_folder, keys ) ]
  
  expr <- sprintf( "(?<=%s).*", bucket_folder )
  filenames <- stringr::str_extract( keys,  expr )
  filenames <- filenames[ nchar( filenames ) > 1 ]
  
  s3_urls <- paste0( bucket_url,
                     bucket_folder,
                     filenames )
  
  s3_ls <- as.list( s3_urls )
  names( s3_ls ) <- filenames
  
  return( s3_ls )
  
}

#' @title Function to download raw data to a destination folder
#' @description This function downloads .dat, .csv or .xlsx data to a destination folder and logs errors
#' @param url_ls list. List of URLs (character)
#' @param destfolder character scalar. String indicating destination folder.
#' @param logger logging object. Logger to log failed downloads
#' @return message indicating that download is complete.

download_raw_data <- function(url_ls, destfolder, logger) {
  purrr::map2(
    .x = unlist(url_ls),
    .y = names(url_ls),
    .f = function(x, y) {
      tryCatch({
        message(sprintf("Downloading file: %s", y))
        if (grepl("dat", y)) {
          df <- readr::read_table(x)
        }
        else if (grepl("csv", y)) {
          df <- data.table::fread(x)
        }
        else if (grepl("xlsx", y)) {
          df <- rio::import(x)
        }
        
      }, warning = function(w) {
        log4r::warn(logger, message = w)
        
      }, error = function(e) {
        log4r::error(logger, message = sprintf("Failed to download file: %s from %s", y, x))
        log4r::error(logger, message = e)
        
      }, finally = {
        message("Moving to Next File")
        
      })
      
      file_root <- gsub("\\..*", "", y)
      destfile <- paste0(destfolder, file_root, ".csv")
      
      rio::export(df, destfile)
      
    },
    .progress = "Download Progress"
  )
  
  return(message("Download Complete"))
  
}

#' @title Run harmonization workflow for Files
#' 
#' @description This function takes URLs to unharmonized files organized by calendar year 
#' and performs variable harmonization based on new variable names in the crosswalk file. 
#' It either harmonizes all files or only harmonizes those that are not present in the 
#' destination bucket. It then saves files according to tax year as defined by 
#' the user, taking the first four characters as the YYYY tax year. 
#' 
#' @param raw_paths paths to raw data.
#' @param logger log4r object. Logs error messages for debugging
#' @param xwalk_df data.frame. Variable crosswalk
#' @param ds character scalar. Data series ("CORE", "SOI" or "BMF")
#' @param scope character scalar. Form scope to harmonize
#' @param tax_year_column character scalar. Name of column containing Tax Year
#' @param destfolder character scalar. Destination folder for harmonized files
#' 
#' @returns List of purrr::safe outputs include error messages

run_harmonization <- function( raw_paths, 
                               logger, 
                               xwalk_df,
                               ds,
                               scope,
                               tax_year_column,
                               destfolder){
  # Create destination folder if it does not exist
  if( !dir.exists( destfolder ) ){
    dir.create( destfolder, recursive = TRUE )
  }
  # Run safely to log errors separately
  safe_harmonize <- purrr::safely(.f = harmonize_data)
  harmonize_rs <-  purrr::imap(raw_paths,
                               safe_harmonize,
                               crosswalk_df = xwalk_df,
                               ds = ds,
                               tax_year_column = tax_year_column,
                               scope = scope,
                               destfolder = destfolder,
                               logger = logger,
                               .progress = "Variable Harmonization")
  msg_body <- "Harmonization Complete"
  log4r::debug(logger, message = msg_body)
  return(harmonize_rs)
}

#' @title Use xwalk to harmonize a Legacy data set
#' 
#' @description This function renames variables in a legacy data set using a 
#' crosswalk file and filters out unmapped columns from the legacy data set.
#' 
#' @param df_path character scalar. Path to raw data.frame.
#' @param filename character scalar. Name of file for raw data.frame.
#' @param crosswalk_df data.frame object. data.frame of crosswalk file
#' @param logger log4r object. Logs error messages for debugging
#' @param ds character scalar. Data series ("CORE", "SOI" or "BMF")
#' @param scope character scalar. Form scope to harmonize
#' @param tax_year_column character scalar. Name of column containing Tax Year
#' @param destfolder character scalar. Destination folder for harmonized files
#' 
#' @returns list of purrr::safely outputs for harmonizing each file

harmonize_data <- function(df_path, 
                           filename, 
                           crosswalk_df,
                           logger,
                           ds,
                           tax_year_column,
                           destfolder,
                           scope){
  dat <- data.table::fread(df_path)
  data.table::setnames(dat, new = toupper(names(dat)))
  # Rename variables based on crosswalk
  dat <- rename_legacy_vars(dat, crosswalk_df)
  # Extract year and month BMF was released from file name
  year <- stringr::str_extract(filename, "\\d{4}")
  month <- stringr::str_extract(filename, "(?<=-)\\d{2}(?=-)")
  # Create ORG_FISCAL_YEAR Column
  dat[, ORG_FISCAL_YEAR := year]
  # Create ORG_FISCAL_MONTH Column
  dat[, ORG_FISCAL_MONTH := month]
  # Create TAX_YEAR Column from tax_year_column (separate function)
  dat <- create_tax_year_col(dat_notaxyear = dat, 
                             df_path = df_path,
                             file_yr = year,
                             tax_year_column = tax_year_column,
                             logger = logger)
  # Derive EIN2 Column
  dat[, EIN2 := derive_ein2(EIN), by = 1:nrow(dat)]
  # Split data set by tax years, named list mapping tax year to chunk
  dat_chunk_ls <- dplyr::lst(!!year := dat)
  # Save each chunk to the correct file by tax year
  rs <-  purrr::imap(.x = dat_chunk_ls,
                     .f = purrr::safely(save_tax_year),
                     hrmnz_files_ls = get_files(folder_name = destfolder,
                                                scope = "PX" ),
                     raw_name = filename,
                     ds = ds,
                     scope = scope,
                     destfolder = destfolder,
                     logger = logger)
  msg <- sprintf("Harmonization of %s complete", filename)
  log4r::debug(logger, message = msg)
  return(rs)
}

#' @title Use xwalk to rename variables
#' 
#' @description This function identifies legacy variables in the crosswalk that are 
#' both present in the input legacy data set and have been assigned new names. It then
#' subsets the crosswalk accordingly.
#' 
#' @param dat_to_rename data.table object. Data.table of legacy core file.
#' @param xwalk_df data.frame object. data.frame of crosswalk file
#' 
#' @returns data.table object with renamed legacy variables

rename_legacy_vars <- function(dat_to_rename,
                               xwalk_df) {
  old_names <- unique(intersect(xwalk_df$old_variable_name, names(dat_to_rename)))
  new_names <- xwalk_df$new_variable_name[xwalk_df$old_variable_name %in% old_names]
  dat_to_rename <- dat_to_rename[, ..old_names]
  data.table::setnames(dat_to_rename,
                       old_names,
                       new_names)
  return(dat_to_rename)
}

#' @title Function to create a tax year column
#' 
#' @description
#' This function checks for the presence of the tax year column specified in 
#' harmonize data and creates a TAX_YEAR column containing the Tax Year of the
#' record in YYYY format. If the column is not present, it logs it with a
#' warning message.
#' 
#' @param dat_notaxyear data.table object. Data.table without TAX_YEAR column
#' @param df_path character scalar. Path to raw data.frame of legacy file
#' @param tax_year_column character scalar. Name of column containing Tax Year
#' @param logger log4r object. Logs error messages for debugging
#' @param file_yr character scalar. Scalar containing year found in file name.
#'
#' @return data.table object with the TAX_YEAR Column.
create_tax_year_col <- function(dat_notaxyear, 
                                df_path, 
                                tax_year_column,
                                logger,
                                file_yr){
  if (tax_year_column %in% names(dat_notaxyear)){
    dat_notaxyear[, 
                  TAX_YEAR := lapply(.SD, assign_tax_year, file_yr = file_yr), 
                  .SDcols = tax_year_column]
  } else {
    dat_notaxyear[, TAX_YEAR := file_yr]
    msg_body <- sprintf("No Tax Year Column Found in %s", df_path)
    log4r::warn(logger, message = msg_body)
  }
  return(dat_notaxyear)
}

#' @title This function assigns a tax year to TAX_YEAR column. It takes the
#' first 4 characters of values in the tax_year_column defined in 
#' create_tax_year_col. If this value is absent, it replaces it with the year
#' in the file name.
#' 
#' @param tax_periods. Character vector. Vector containing tax period.
#' @param file_yr character scalar. Year found on file name.
#' 
#' @return character vector. Vector of parsed tax years.
assign_tax_year <- function(tax_periods, file_yr){
  tax_years = substr(tax_periods, 1, 4)
  tax_years[is.na(tax_years)] <- file_yr
  return(tax_years)
}

#' @title Function to save data.table chunks split by tax year to an output 
#' folder
#' @description 
#' Creates a new file if a file for that tax year does not exist. 
#' If it does, opens existing file and rbinds both data.table objects.
#' 
#' @param tax_year character scalar. Tax Year mapped to chunk during split.
#' @param hrmnz_files_ls named list. List mapping harmonized file names to
#' their file paths in data/harmonized/
#' @param raw_name character scalar. Name of legacy file chunk is from.
#' @param ds character scalar. Data series ("CORE", "SOI" or "BMF")
#' @param scope character scalar. Form scope to harmonize
#' @param destfolder character scalar. Destination folder for harmonized files
#' @param logger log4r object. Logs error messages for debugging
#' @param overwrite boolean. Indicate whether to create new files or add
#' to existing files.
#' 
#' @return named list mapping tax year to raw file name.
save_tax_year <- function(dat_chunk, 
                          tax_year, 
                          hrmnz_files_ls, 
                          raw_name,
                          ds,
                          scope,
                          destfolder,
                          logger,
                          overwrite){
  # Check if data set exists
  destfile_ls = hrmnz_files_ls[grepl(tax_year, hrmnz_files_ls)]
  if (length(destfile_ls) == 0){
    file_name <- sprintf( "%s-%s-%s-HRMN.csv", ds, tax_year, scope )
    destfile <- paste0( destfolder, file_name ) 
  } else {
    destfile = destfile_ls[[1]]
    existing_dat <- data.table::fread(destfile)
    dat_chunk <- data.table::rbindlist(list(dat_chunk, existing_dat),
                                       fill = TRUE)
    dat_chunk <- unique(dat_chunk) # Avoid binding duplicate rows
  }
  # Export data.frame
  rio::export(dat_chunk, destfile)
  rs_ls <- list(tax_year = raw_name)
  gc()
  return(rs_ls)
}

#' @title Append suffixes to repeat column names
#' 
#' @description This function takes checks for duplicate elements in the 
#' character vector and appends a suffix to make them unique
#' 
#' @param input_vector character vector.
#' 
#' @returns same vector with suffixes appended to previously duplicated elements

make_unique_names <- function( input_vector ) {
  
  counts <- table( input_vector )
  duplicate_indices <- which( counts[ input_vector ] > 1 )
  
  for ( index in duplicate_indices ) {
    
    duplicate_elements <- input_vector == input_vector[ index ]
    if ( sum( duplicate_elements ) > 1 ) {
      
      indices_to_rename <- which( duplicate_elements )
      
      for ( i in 2:length( indices_to_rename ) ) {
        
        input_vector[ indices_to_rename[ i ]] <- paste( input_vector[index], 
                                                        i, 
                                                        sep = "_" )
      }
    }
  }
  
  return( input_vector )
}

#' @title Function for postprocessing data harmonized by tax year
#' 
#' @description This function creates 3 new columns:
#'  1. RTRN_ID: unique return IDs "EIN_{EIN}_TAXYEAR" 
#'  2. DUP_RTRN_X: Boolean column indicating if the the return shares the same EIN
#'  as another return. This means that an organization refiled their tex return.
#'  The function also deletes redundant columns: (V1)
#'  3. TAX_YEAR. Column indicating Tax Year.
#'  
#' @param inpath character scalar. Path to harmonized file.
#' @param dest_folder output folder
#' 
#' @return data.table object with post processed data for reupload to S3.
process_harmonized_data <- function( inpath, dest_folder ){
  
  if ( ! file.exists( dest_folder ) ){ dir.create( dest_folder ) }
  
  dat <- dat
  tax_yeaa.table::fread( inpath, colClasses = list( character = c( "F9_00_ORG_EIN" ) ) )
  r <- dat[[ "TAX_YEAR" ]]
  
  make_unique <- function( ein, year ){
    id <- as.character( ein )
    id <- make.unique( id, sep = "." )
    id <- paste( "EIN", id, year, sep = "_" )
    
    return( id )
    
  }
  
  duplicated <- function( ein ){
    
    rs <- ifelse( grepl( "\\.", ein ), 1, 0 )
    
    return( rs )
    
  }
  
  dat[ , TAX_YEAR := as.character( TAX_YEAR ) ]
  dat[ , RTRN_ID := make_unique( F9_00_ORG_EIN, year=tax_year ) ]
  dat[ , DUP_RTRN_X := duplicated( RTRN_ID ) ]
  dat[ , .SD, .SDcols = colSums(is.na(dat)) == 0 ]
  dat[, F990_ORG_EIN := ifelse(nchar(F990_ORG_EIN) == 8,
                               paste0("0", F990_ORG_EIN),
                               F990_ORG_EIN)]
  
  outpath <- gsub( "harmonized", "processed", inpath )
  
  data.table::fwrite( dat, outpath )
  
  return( sprintf( "Finished processing:\n %s", outpath ))
  
}

#' @title Upload a harmonized data set to aws S3
#' 
#' @description This function takes the url link to a legacy data set hosted on
#' aws s3 and uploads its harmonized counterpart to a new folder in the same bucket
#' 
#' @param file_path character scalar. Path to processed file.
#' @param file_name character scalar. Name of harmonized file
#' @param s3_folder Name of s3 folder to upload data set to
#' 
#' @returns Message indicating that upload is complete

upload_to_s3 <- function( file_path, file_name, s3_folder ){
  
  s3 <- paws::s3()
  
  bucket_name = "nccsdata"
  key_name = paste0( s3_folder, file_name )
  
  message( "Harmonizing ", file_name )
  
  s3$put_object(
    Body = file_path,
    Bucket = bucket_name,
    Key = key_name
  )
  
  return(message("S3 Upload Complete"))
  
}

#' @title Upload a harmonized data set to aws S3
#' 
#' @description This function takes the url link to a legacy data set hosted on
#' aws s3 and uploads its harmonized counterpart to a new folder in the same bucket
#' 
#' @param file_name character scalar. Scope of form that is the prefix for 
#' resultant file name
#' @param dataset data.frame object. data.frame of harmonized legacy data set
#' @param key_root character scalar. Root folder for harmonized files
#' 
#' @returns Message indicating that upload is complete

upload_to_s3 <- function( file_name, dataset, key_root ){
  
  s3 <- paws::s3()
  
  bucket_name = "nccsdata"
  file_name <- paste0(file_name, "-HRMN.csv")
  file_path <- file_name
  write.csv(dataset, file_path)
  key_name = paste0( key_root, file_name )
  
  
  s3$put_object(
    Body = file_path,
    Bucket = bucket_name,
    Key = key_name
  )
  
  file.remove(file_path)
  
  return(message("S3 Upload Complete"))
  
}

#' @title This function formats the EIN column to make it 9 digits by adding 0s
#' at the start
#' @param ein character scalar. Organization EIN
#' @return character scalar. Reformatted EIN

format_ein <- function(ein) {
  if (is.na(ein)){
    ein <- "000000000"
    return(ein)
  } else {
    ein_len <- nchar(ein)
    if (ein_len == 9){
      return(ein)
    } else {
      diff = 9 - ein_len
      diff = rep("0", diff)
      diff = paste0(diff, collapse = "")
      ein <- paste0(diff, ein, collapse = "")
      return(ein)
    }
  }
}

#' @title This function derives EIN2 from EIN column. EIN-XX-XXXXXXX
#' @param character scalar. Formatted 9-digit EIN
#' @returns character scalar. EIN2.
derive_ein2 <- function(ein){
  ein2 <- format_ein(ein)
  ein2 <- paste0("EIN-", substr(ein2, 1, 2), "-", substr(ein2, 3, 9))
  return(ein2)
}
