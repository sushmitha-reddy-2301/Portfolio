updateARDnames <- function(ard, addnames=TRUE, removenames=TRUE, verbose=TRUE) {
  functionName <- as.list(sys.call())[[1]]
  
  v <- validateARDdataset(ard)
  
  if(addnames) {
    missingnames <- v$namesmissing
    if(length(missingnames)>0) { 
      if(verbose) {
        cat(glue::glue("{functionName}: adding"), "\n")
        print(missingnames)
      }
      for(i in missingnames) {
        ard <- ard %>%
          dplyr::mutate(!!i := NA)
      }
    }
  }
  
  if(removenames) {
    unexpectednames <- v$namesunexpected
    if(length(unexpectednames)>0) { 
      if(verbose) {
        cat(glue::glue("{functionName}: removing"), "\n")
        print(unexpectednames)
      }
      for(i in unexpectednames) {
        ard <- ard %>%
          dplyr::mutate(!!i := NULL)
      }
    }
  }
  
  return(ard)
}
