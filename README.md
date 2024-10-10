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
  
  if(length(atlist)>0) {
    for(i in 1:length(atlist)) {
      pkdeftrt <- names(atlist)[i]
      ectrt <- atlist[[i]]
      if(i==1) {
        analyteTreatmentMap <- dplyr::tibble(pkdeftrt=pkdeftrt, ectrt=ectrt)
      } else {
        analyteTreatmentMap <- analyteTreatmentMap %>%
          dplyr::add_row(pkdeftrt=pkdeftrt, ectrt=ectrt)
      }
    }
  } else {
    ##        analyteTreatmentMap <- dplyr::tibble(pkdeftrt=NA, ectrt=NA)
    analyteTreatmentMap <- dplyr::tibble()
  }
  
  return(analyteTreatmentMap)
}
