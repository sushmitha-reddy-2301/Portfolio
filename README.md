mergePCdata <- function(pkdeflist, pclist) { 
  functionName <- as.list(sys.call())[[1]]
  
  npc <- names(pclist)
  ## bind pclist datasets if more than one
  if(length(npc)>0) {
    for(i in pclist) {
      if(!exists("pc")) {
        pc <- i$data
      } else {
        pc <- pc %>% dplyr::bind_rows(i$data)
      }
    }
  }
  
  pc <- pc %>%
    mutate(SUBJID=as.character(SUBJID), PKUSMID=as.character(PKUSMID))
  ## 2023-02-13/TGT/ added PKSMND filter to pc to address record replication/extra records issues
  knd <- grepl("^PKSMND$", names(pc), ignore.case=TRUE, perl=TRUE)
  if(any(knd)) { 
    pc <- pc %>%
      filter(PKSMND=="DONE")
  }
  
  for(i in 1:length(pkdeflist)) {
    pkterm <- pkdeflist[[i]]$pkterm
    mergeddata <- pkdeflist[[i]]$mergeddata
    ## add OBS field to ensure retention of only the original records
    mergeddata <- mergeddata %>%
      mutate(OBS=1:nrow(.))
    
    ## sort by STUDY, PKBDFLD, PKTERM
    mergeddata <- mergeddata %>% arrange(STUDY, PKBDFLD, PKTERM)
    ## ensure that SUBJID, PKUSMID from pkdeflist are character
    mergeddata <- mergeddata %>% mutate(SUBJID=as.character(SUBJID), PKUSMID=as.character(PKUSMID))
    ## ensure that SUBJID, PKUSMID from pc are character
    
    ## merge
    mergeddata <- mergeddata %>%
      dplyr::left_join(pc, 
                       by=c("SUBJID", "PKUSMID"),
                       suffix = c("", ".pc"), keep=TRUE)
    
    ## retain only the original records and then remove OBS
    mergeddata <- mergeddata %>%
      filter(!duplicated(OBS)) %>%
      mutate(OBS=NULL)
    
    pkdeflist[[i]]$mergeddata <- mergeddata
  }
  
  return(pkdeflist)
}
