#' mapAnalytesToTreatments 
#'
#' function converts the Analyte to Treatment map list object to an
#'  equivalent tibble to facilitate lookups required in processing treatment
#'  information
#'
#' @param atlist - mapAnalytesToTreatments list mapping unique "Analyte Name"/PKTERM
#' #                 entries from pkdef list objects to unique ectrt_001 values
#' #                 typically stored in eclist[[protocol]][[ecdomain]]$utrt as an
#' #                 output from calls to getStandardDosingData() but derived from
#' #                 ectrt_001
#' @return a tibble
#' @keywords nca opennca
#' @importFrom dplyr tibble add_row
#' @export
#' @examples
#' \dontrun{
#' library(NCATools)
#' mapAnalyteToTreatment <- list()
#' # this block used to test missing analyte name from pkdef in treatment map 
#' if(protocol=="C1071002" | protocol=="C1071003") {
#'     mapAnalyteToTreatment[["FREE PF-06863135"]] <- "PF-06863135"
#'     mapAnalyteToTreatment[["PF-06863135"]] <- "PF-06863135"
#' }
#' print(mapAnalyteToTreatment)
#' $`FREE PF-06863135`
#' [1] "PF-06863135"
#' 
#' $`PF-06863135`
#' [1] "PF-06863135"
#' 
#' # Convert Analyte-Treatment map List to Tibble
#' analyteTreatmentMap <- mapAnalytesToTreatments(atlist=mapAnalyteToTreatment)
#' print(analyteTreatmentMap)
#'  A tibble: 2 x 2
#'   pkdeftrt         ectrt      
#'   <chr>            <chr>      
#' 1 FREE PF-06863135 PF-06863135
#' 2 PF-06863135      PF-06863135
#' }
#'
#' @author
#' \itemize{
#'  \item \strong{Thomas Tensfeldt}
#'  \item website: \url{www.pfizer.com}
#'  \item email: \url{thomas.g.tensfeldt@pfizer.com}
#' }
mapAnalytesToTreatments <- function(atlist) {
  functionName <- as.list(sys.call())[[1]]
  
  # sushmitha comments: Check if the input list of analytes to treatments is not empty
  if(length(atlist) > 0) {
    # sushmitha comments: Loop through each item in the atlist
    for(i in 1:length(atlist)) {
      pkdeftrt <- names(atlist)[i]  # sushmitha comments: Get the name of the treatment
      ectrt <- atlist[[i]]           # sushmitha comments: Get the corresponding analyte
      
      # sushmitha comments: Create the tibble for the first treatment
      if(i == 1) {
        analyteTreatmentMap <- dplyr::tibble(pkdeftrt = pkdeftrt, ectrt = ectrt)
      } else {
        # sushmitha comments: Add a new row for subsequent treatments
        analyteTreatmentMap <- analyteTreatmentMap %>%
          dplyr::add_row(pkdeftrt = pkdeftrt, ectrt = ectrt)
      }
    }
  } else {
    # sushmitha comments: If the list is empty, initialize an empty tibble
    analyteTreatmentMap <- dplyr::tibble()
  }
  
  # sushmitha comments: Return the final tibble mapping treatments to analytes
  return(analyteTreatmentMap)
}

#' Sushmitha Comment

#' Function Purpose: 
#' The mapAnalytesToTreatments function creates a mapping of treatments (pkdeftrt) 
#' to their corresponding analytes (ectrt) from a given list.
#' 
#' It checks if the input list is empty. If not, it iterates over the list to build 
#' a tibble that maps each treatment to its analyte. 
#' The resulting tibble is returned, either populated with the mappings or empty 
#' if the input list was empty.
