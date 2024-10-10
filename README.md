#'Sushmitha Comment

#' This function processes demography (dm) and vitals (vs) datasets to create a 
#' merged dataset containing key demographic and vital sign variables. The function 
#' allows optional normalization of units for weight (WT) and height (HT).



#' createDemogMergeDataset
#'
#' In SRDM demog (dm) inputfiles, locate basic ARD demography variables,
#' in SRDM vitals (vs) inputfiles, locate HT/WT based vitals and
#' generate a demog subset for merging with the PKDef File or ARD.
#'
#' The ARD demog variables include
#'  HT,HTUNI,HTRAW,WT,WTUNI,WTRAW,RACEOTH,RACES,SEX,RACIALD,ETHNIC,SMOKSTA
#'
#' createDemogMergeDataset will optionally (by default) normalize units for
#'  WT, WTRAW, HT, HTUNI
#'
#' @param data - inputdata data source including dm and vs domains
#' @param normunits - TRUE(default)/FALSE - if TRUE normalize WT to KG from LB and HT to CM from IN
#'
#' @importFrom dplyr mutate rename select left_join filter
#' @importFrom glue glue
#' @importFrom tidyr unite
#'
#' @keywords nca opennca
#'
#' @export
#'
#' @examples
#' \dontrun{
#' library(NCATools)
#'
#' # Add NTPD numeric nominal time post dose (NTPD) column
#' #  to each of the pkdeflist[[i]]$data tibbles in pkdeflist
#' pkdeflist <- addNTPD(pkdeflist)
#' }
#'
#' @author
#' \itemize{
#'  \item \strong{Thomas Tensfeldt}
#'  \item website: \url{www.pfizer.com}
#'  \item email: \url{thomas.g.tensfeldt@pfizer.com}
#' }

#Sushmitha Comment: createDemogMergeDataset combines demographic (dm) and vital sign (vs) datasets, extracting key variables and normalizing units when specified.
createDemogMergeDataset <- function(data, ...) {
  functionName <- as.list(sys.call())[[1]]
  
  # sushmitha comments: Check if input data is missing
  if(missing(data)) {
    msg <- glue::glue("{ functionName }: input dataset missing")
    warning(msg)
  }
  
  # sushmitha comments: Retrieve optional arguments, setting normunits to TRUE by default
  dotargs <- list(...)
  normunits <- ifelse("normunits" %in% names(dotargs), dotargs$normunits, TRUE)
  
  # sushmitha comments: Process demography dataset
  if(length(getDomainFiles(data, "dm")) > 0) {
    dmflist <- getDomainFiles(data, "dm")
    k <- grepl("sm_dm[0-9]+?$", dmflist, ignore.case = TRUE, perl = TRUE)
    dmfile <- dmflist[k]
    
    # sushmitha comments: Identify relevant dm dataset if multiple are present
    if(length(dmfile) > 1) {
      nmatch <- sapply(dmfile, function(i) any(grepl("^(AGE|ETHNIC|RACE|SEX|BRTHDAT).*?$", names(data[[i]]$data), ignore.case = TRUE, perl = TRUE)))
      dmfile <- dmfile[nmatch]
    }
    
    # sushmitha comments: Select the relevant dm dataset
    dm <- data[[dmfile]]$data
    dmlist <- c()
    
    # sushmitha comments: Extract and rename key demographic variables
    k <- grepl("SUBJECTNUMBER", names(dm), ignore.case = TRUE, perl = TRUE)
    if(!any(k)) {
      msg <- glue::glue('{ functionName }: no SUBJECTNUMBER present in demography dataset. Demography will be skipped')
      warning(msg)
    } else {
      if(!"SUBJID" %in% names(dm)) {
        dm <- dm %>% dplyr::rename(SUBJID = names(dm)[k])
      }
      dmlist <- c(dmlist, "SUBJID")
      
      # sushmitha comments: Extract SEX
      k <- grepl("SEX_[0-9]*?_C", names(dm), ignore.case = TRUE, perl = TRUE)
      if(any(k)) {
        dm <- dm %>% dplyr::rename(SEX = names(dm)[k])
        dmlist <- c(dmlist, "SEX")
      }
      
      # sushmitha comments: Extract AGEDERU, AGEDER, ETHNIC, and RACIALD similarly...
      # ...
      
      # sushmitha comments: Collapse race information into a single string
      k <- grepl("race_[0-9]*?_cli[0-9]*?$", names(dm), ignore.case = TRUE, perl = TRUE)
      if(any(k)) {
        dm <- dm %>% tidyr::unite("RACES", names(dm)[k], sep="", na.rm=TRUE)
        dmlist <- c(dmlist, "RACES")
      }
      
      # sushmitha comments: Select relevant columns
      dm <- dm %>% select(all_of(dmlist))
    }
  }
  
  # sushmitha comments: Process vitals dataset
  vslist <- c()
  if(length(getDomainFiles(data, "vs")) > 0) {
    vs <- data[[getDomainFiles(data, "vs")]]$data
    k <- grepl("SUBJECTNUMBER", names(vs), ignore.case = TRUE, perl = TRUE)
    if(!any(k)) {
      msg <- glue::glue('{ functionName }: no SUBJECTNUMBER present in vitals dataset. Vitals will be skipped')
      warning(msg)
    } else {
      if(!"SUBJID" %in% names(vs)) {
        vs <- vs %>% dplyr::rename(SUBJID = names(vs)[k])
      }
      vslist <- c(vslist, "SUBJID")
      
      # sushmitha comments: Handle visit names and extract relevant vital sign data
      # ...
      
      # sushmitha comments: Normalize weight and height if applicable
      if(normunits) {
        # sushmitha comments: Normalize WT and HTRAW to appropriate units
        # ...
      }
      
      # sushmitha comments: Select relevant columns from vitals
      vs <- vs %>% dplyr::select(all_of(vslist))
    }
  }
  
  # sushmitha comments: Ensure SUBJID is character in both datasets
  if("SUBJID" %in% names(dm)) {
    dm <- dm %>% dplyr::mutate(SUBJID = as.character(SUBJID))
  }
  
  # sushmitha comments: Merge the two datasets based on SUBJID
  if(exists("dm") && exists("vs") && length(vslist) > 1) {
    dm <- dm %>% dplyr::left_join(vs, by = "SUBJID")
  }
  
  # sushmitha comments: Return the merged dataset or NULL if no data
  return(ifelse(exists("dm"), dm, NULL))
}
