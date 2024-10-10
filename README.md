updateARDnames <- function(ard, addnames=TRUE, removenames=TRUE, verbose=TRUE) {
  functionName <- as.list(sys.call())[[1]]
  
  # sushmitha comments: Validate the ARD dataset
  v <- validateARDdataset(ard)
  
  # sushmitha comments: Check if names should be added
  if(addnames) {
    missingnames <- v$namesmissing
    # sushmitha comments: If there are missing names, add them
    if(length(missingnames) > 0) { 
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
  
  # sushmitha comments: Check if names should be removed
  if(removenames) {
    unexpectednames <- v$namesunexpected
    # sushmitha comments: If there are unexpected names, remove them
    if(length(unexpectednames) > 0) { 
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
  
  # sushmitha comments: Return the updated ARD dataset
  return(ard)
}


#'Sushmitha Comment

#' Function Purpose: 
#' The updateARDnames function updates an ARD dataset by adding or removing column names 
#' based on validation results.
#' 
#' If specified, it checks for missing names in the dataset and adds them as new columns 
#' initialized to NA. It also checks for unexpected names and removes those columns if 
#' specified. 
#' The function returns the updated ARD dataset after making the necessary adjustments.
