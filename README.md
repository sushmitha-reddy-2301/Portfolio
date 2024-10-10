#sushmitha Comment:

#' validateARDdataset
#'
#' This function checks if the variables in an ARD dataset match a list of standardized ARD variables.
#' It validates the ARD dataset by comparing its variable names against a reference set of ARD variables
#' based on the selected ARD type (standard, additional, optional, or all).
#'
#' Function Parameters:
#' 
#' - ard: The ARD dataset you want to validate.
#' - ardtype: Specifies the type of ARD variables to check. Options are:
#'   - "all": Check all variables.
#'   - "standard": Check standard variables (green).
#'   - "additional": Check additional variables (yellow).
#'   - "optional": Check optional variables (orange).
#' - verbose: A TRUE/FALSE flag (default is TRUE). If set to TRUE, the function generates 
#'            informational messages during execution to show the matching, missing, and unexpected variable names.



#' validateARDdataset 
#'
#' function returns a list of standardized ARD variables
#' to use in checking ARD or PKDEF file variables
#'
#' @param ard - ard dataset
#' @param ardtype - all, standard (green), additional (yellow), optional (orange)
#' @param verbose - TRUE/FALSE=DEFAULT, optionally generate informational messages during execution
#' 
#' @keywords nca opennca
#'
#' @importFrom glue glue
#' 
#' @export
#' 
#' @examples
#' \dontrun{
#' library(NCATools)
#' valresult <- validateARDdataset(ard, ardtype="standard")
#' valresult <- validateARDdataset(ard, ardtype="additional")
#' valresult <- validateARDdataset(ard, ardtype="optional")
#' valresult <- validateARDdataset(ard, ardtype="all")
#' valresult <- validateARDdataset(ard)
#' valresult <- validateARDdataset(ard, verbose=TRUE)
#' }
#'
#' @author
#' \itemize{
#'  \item \strong{Thomas Tensfeldt}
#'  \item website: \url{www.pfizer.com}
#'  \item email: \url{thomas.g.tensfeldt@pfizer.com}
#' }
validateARDdataset <- function(ard, ardtype="all", verbose=TRUE) {
    
    # sushmitha comment: Capture the name of the function, used for error and info messages
    functionName <- as.list(sys.call())[[1]]

    # sushmitha comment: If the ARD dataset is not provided, issue a warning message
    if(missing(ard)) {
        warning(glue::glue("{functionName}: provide an ard dataset to validate!"))
    }

    # sushmitha comment: Initialize an empty list to store results
    result <- list()

    # sushmitha comment: Store the selected ARD type (all/standard/additional/optional)
    result[["ardtype"]] <- ardtype

    # sushmitha comment: Retrieve ARD variable list based on the chosen ARD type
    ardvlist <- ardVariables(ardtype=ardtype)
    nardvlist <- unname(unlist(ardvlist %>% select(variable)))

    # sushmitha comment: Check for matching names between the ARD dataset and ARD variable list
    namesmatching <- intersect(names(ard), nardvlist)
    result[["namesmatching"]] <- namesmatching

    # sushmitha comment: If verbose is TRUE, print the matching variable names
    if(verbose) {
        cat(glue::glue("{functionName}: { ardtype } variable names matching"), "\n")
        print(namesmatching)
    }
    
    # sushmitha comment: Identify missing variable names expected in ARD but not found in the dataset
    namesmissing <- setdiff(nardvlist, names(ard))
    result[["namesmissing"]] <- namesmissing

    # sushmitha comment: If verbose is TRUE, print the missing variable names
    if(verbose) {
        cat(glue::glue("{functionName}: { ardtype } variable names missing"), "\n")
        print(namesmissing)
    }

    # sushmitha comment: Identify unexpected variable names present in the ARD dataset but not in the ARD definition
    namesunexpected <- setdiff(names(ard), nardvlist)
    result[["namesunexpected"]] <- namesunexpected

    # sushmitha comment: If verbose is TRUE, print the unexpected variable names
    if(verbose) {
        cat(glue::glue("{functionName}: { ardtype } variable names appearing in ARD dataset but not in ARD definition"), "\n")
        print(namesunexpected)
    }

    # sushmitha comment: Return the result containing matching, missing, and unexpected variable names
    return(result)
}
