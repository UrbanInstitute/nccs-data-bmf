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
