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
    functionName <- as.list(sys.call())[[1]]

    if(missing(ard)) {
        warning(glue::glue("{functionName}: provide an ard dataset to validate!"))
    }

    result <- list()

    result[["ardtype"]] <- ardtype
    
    ardvlist <- ardVariables(ardtype=ardtype)
    nardvlist <- unname(unlist(ardvlist %>% select(variable)))

    ## check names in ard that match ardvlist
    namesmatching <- intersect(names(ard), nardvlist)
    result[["namesmatching"]] <- namesmatching
    if(verbose) {
        cat(glue::glue("{functionName}: { ardtype } variable names matching"), "\n")
        print(namesmatching)
    }
    
    ## names missing
    k <- nardvlist %in% names(ard)
    namesmissing <- setdiff(nardvlist, names(ard))
    result[["namesmissing"]] <- namesmissing
    if(verbose) {
        cat(glue::glue("{functionName}: { ardtype } variable names missing"), "\n")
        print(namesmissing)
    }
    
    ## names unexpected
    namesunexpected <- setdiff(names(ard), nardvlist)
    result[["namesunexpected"]] <- namesunexpected
    if(verbose) {
        cat(glue::glue("{functionName}: { ardtype } variable names appearing in ARD dataset but not in ARD definition"), "\n")
        print(namesunexpected)
    }
    
    return(result)
}
