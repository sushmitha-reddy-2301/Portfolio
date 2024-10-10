mergePCdata <- function(pkdeflist, pclist) { 
  functionName <- as.list(sys.call())[[1]]
  
  # sushmitha comments: Retrieve names of datasets in pclist
  npc <- names(pclist)
  
  # sushmitha comments: Bind pclist datasets if there is more than one
  if(length(npc) > 0) {
    for(i in pclist) {
      if(!exists("pc")) {
        pc <- i$data  # sushmitha comments: Initialize pc with the first dataset
      } else {
        pc <- pc %>% dplyr::bind_rows(i$data)  # sushmitha comments: Combine subsequent datasets
      }
    }
  }
  
  # sushmitha comments: Ensure SUBJID and PKUSMID are character types
  pc <- pc %>%
    mutate(SUBJID = as.character(SUBJID), PKUSMID = as.character(PKUSMID))
  
  # sushmitha comments: Filter pc for PKSMND to avoid record duplication
  knd <- grepl("^PKSMND$", names(pc), ignore.case = TRUE, perl = TRUE)
  if(any(knd)) { 
    pc <- pc %>%
      filter(PKSMND == "DONE")
  }
  
  # sushmitha comments: Loop through each item in pkdeflist to merge data
  for(i in 1:length(pkdeflist)) {
    pkterm <- pkdeflist[[i]]$pkterm
    mergeddata <- pkdeflist[[i]]$mergeddata
    
    # sushmitha comments: Add OBS field to retain original records
    mergeddata <- mergeddata %>%
      mutate(OBS = 1:nrow(.))
    
    # sushmitha comments: Sort mergeddata by STUDY, PKBDFLD, PKTERM
    mergeddata <- mergeddata %>% arrange(STUDY, PKBDFLD, PKTERM)
    
    # sushmitha comments: Ensure SUBJID and PKUSMID are character types
    mergeddata <- mergeddata %>%
      mutate(SUBJID = as.character(SUBJID), PKUSMID = as.character(PKUSMID))
    
    # sushmitha comments: Perform left join with pc
    mergeddata <- mergeddata %>%
      dplyr::left_join(pc, 
                       by = c("SUBJID", "PKUSMID"),
                       suffix = c("", ".pc"), keep = TRUE)
    
    # sushmitha comments: Retain only original records and remove OBS
    mergeddata <- mergeddata %>%
      filter(!duplicated(OBS)) %>%
      mutate(OBS = NULL)
    
    # sushmitha comments: Update pkdeflist with mergeddata
    pkdeflist[[i]]$mergeddata <- mergeddata
  }
  
  # sushmitha comments: Return the updated pkdeflist
  return(pkdeflist)
}


#'Sushmitha Comment

#' Function Purpose: 
#' The mergePCdata function merges pharmacokinetic (PK) data with patient contact (PC) data 
#' from the provided lists.
#' 
#' It checks for multiple datasets in pclist, combining them into a single data frame 
#' if necessary. The function also filters the PC data based on specific criteria, 
#' ensuring that identifiers (SUBJID and PKUSMID) are of character type. 
#' It then merges the PK data with the PC data for each entry in pkdeflist, retaining 
#' only the original records and updating the pkdeflist with the merged data.
