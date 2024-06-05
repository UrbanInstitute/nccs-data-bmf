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