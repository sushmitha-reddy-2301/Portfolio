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
createDemogMergeDataset <- function(data, ...) {
  functionName <- as.list(sys.call())[[1]]
  
  if(missing(data)) {
    msg <- glue::glue("{ functionName }: input dataset missing")
    warning(msg)
  }
  
  ## retrieve optional arguments
  dotargs <- list(...)
  if(!"normunits" %in% names(dotargs)) {
    normunits <- TRUE
  } else {
    normunits <- dotargs$normunits
  }
  
  if(length(getDomainFiles(data, "dm"))>0) {
    ### Assume a single dm dataset
    dmflist <- getDomainFiles(data, "dm")
    k <- grepl("sm_dm[0-9]+?$", dmflist, ignore.case=TRUE, perl=TRUE)
    dmfile <- dmflist[k]
    ### if there is more than one "dm" dataset try to identify the one with matching
    ### column names
    if(length(dmfile)>1) {
      nmatch <- c()
      for(i in dmfile) {
        x <- names(data[[i]]$data)
        r <- "^(AGE|ETHNIC|RACE|SEX|BRTHDAT).*?$"
        k <- grepl(r, x, ignore.case=TRUE, perl=TRUE)
        if(any(k)) {
          nmatch <- c(nmatch, TRUE)
        } else {
          nmatch <- c(nmatch, FALSE)
        }
      }
      dmfile <- dmfile[nmatch]
    }
    data[[dmfile]]$data %>% as.data.frame %>% head(3)
    dm <- data[[dmfile]]$data
    dmlist <- c()
    
    ### SUBJID
    k <- grepl("SUBJECTNUMBER", names(dm), ignore.case=TRUE, perl=TRUE)
    if(!any(k)) {
      msg <- glue::glue('{ functionName }: no SUBJECTNUMBER present in demography dataset. Demography will be skipped')
      warning(msg)
    } else {
      if(!"SUBJID" %in% names(dm)) {
        dm <- dm %>% dplyr::rename(SUBJID=names(dm)[k])
      }
      dmlist <- c(dmlist, "SUBJID")
      
      ### SEX
      k <- grepl("SEX_[0-9]*?_C", names(dm), ignore.case=TRUE, perl=TRUE)
      if(any(k)) {
        dm <- dm %>% dplyr::rename(SEX=names(dm)[k])
        dmlist <- c(dmlist, "SEX")
      }
      
      ### AGEDERU
      k <- grepl("ageu_[0-9]*?$", names(dm), ignore.case=TRUE, perl=TRUE)
      if(any(k)) {
        dm <- dm %>% dplyr::rename(AGEDERU=names(dm)[k])
        dmlist <- c(dmlist, "AGEDERU")
      }
      
      ### AGEDER
      k <- grepl("age_[0-9]*?$", names(dm), ignore.case=TRUE, perl=TRUE)
      if(any(k)) {
        dm <- dm %>% dplyr::rename(AGEDER=names(dm)[k])
        dmlist <- c(dmlist, "AGEDER")
      }
      
      ### ETHNIC
      k <- grepl("ethnic_[0-9]*?_c", names(dm), ignore.case=TRUE, perl=TRUE)
      if(any(k)) {
        dm <- dm %>% dplyr::rename(ETHNIC=names(dm)[k])
        dmlist <- c(dmlist, "ETHNIC")
      }
      
      ### RACES - collapse all race_[0-9]*_cli[0-9] values across all matching columns in single string
      ### if missing(RACES) then RACES = 'UNKNOWN';
      ### if RACES = 'NOT REPORTED' then call missing(RACES);
      k <- grepl("race_[0-9]*?_cli[0-9]*?$", names(dm), ignore.case=TRUE, perl=TRUE)
      if(any(k)) {
        dm <- dm %>% tidyr::unite("RACES", names(dm)[k], sep="", na.rm=TRUE)
        dmlist <- c(dmlist, "RACES")
      }
      
      ### RACIALD
      k <- grepl("suppdm_raciald_[0-9]*?$", names(dm), ignore.case=TRUE, perl=TRUE)
      if(any(k)) {
        dm <- dm %>% dplyr::rename(RACIALD=names(dm)[k])
        dmlist <- c(dmlist, "RACIALD")
      }
      
      dm <- dm %>% select(all_of(dmlist))
    }
  }
  
  ## vitals (vs) dataset to obtain WT, HT
  ## 2022-12-12/TGT/ note that the vs dataset doesn't have harmonized units
  ##                 this should be handled in this module but isn't at this moment
  vslist <- c() ## if vslist is empty - no vs data was identified
  if(length(getDomainFiles(data, "vs"))>0) {
    ## Assume a single vs dataset
    vs <- data[[getDomainFiles(data, "vs")]]$data
    
    ## SUBJID
    k <- grepl("SUBJECTNUMBER", names(vs), ignore.case=TRUE, perl=TRUE)
    if(!any(k)) {
      msg <- glue::glue('{ functionName }: no SUBJECTNUMBER present in vitals dataset. Vitals will be skipped')
      warning(msg)
    } else {
      if(!"SUBJID" %in% names(vs)) {
        vs <- vs %>% dplyr::rename(SUBJID=names(vs)[k])
        ## ensure SUBJID is character
        vs <- vs %>% dplyr::mutate(SUBJID=as.character(SUBJID))
      }
      vslist <- c(vslist, "SUBJID")
      
      ## determine VISITs
      ##  Prioritize BASELINE visit as source for vitals
      ##  then SCREENING visit if BASELINE not available
      ##  if no BASELINE or SCREENING visit, then vitals
      ##  for subject will be missing
      k.visitrefname <- grepl("^VISITREFNAME$", names(vs), ignore.case=TRUE, perl=TRUE)
      ## if VISITREFNAME not present, try VISITNM
      if(!any(k.visitrefname)) {
        k.visitrefname <- grepl("^VISITNAM$", names(vs), ignore.case=TRUE, perl=TRUE)
      }
      if(any(k.visitrefname)>0) {
        visitrefname <- names(vs)[k.visitrefname]
        uvisitrefnames <- unlist(vs %>% dplyr::select(all_of(visitrefname)) %>% distinct)
        vsvisit <- NULL
        
        ## baseline visit
        kb <- grepl("^BAS", uvisitrefnames, ignore.case=TRUE, perl=TRUE)
        if(any(kb)) {
          vsvisit <- uvisitrefnames[kb]
        }
        ## screening visit
        ks <- FALSE # initialize
        if(!any(kb)) {
          ks <- grepl("^SCR", uvisitrefnames, ignore.case=TRUE, perl=TRUE)
          if(any(ks)) {
            vsvisit <- uvisitrefnames[ks]
          }
        }
        ## <<<<<<<<<<< >>>>>>>>>>>>
        ## need accommodation for when there's no baseline nor screening - need to jump out and not add vitals
        if(any(kb, ks)) {
          ## select records matching the selected visit
          vs <- vs %>% dplyr::filter(.data[[visitrefname]]==!!vsvisit)
          
          ## <<<<<<<<<< >>>>>>>>>>
          ## should have check and mitigation for SUBJIDs with more than one selected
          ##  vs record here....
          ## <<<<<<<<<< >>>>>>>>>>
          
          ## WTRAW, WT
          k <- grepl("vsorres_weight_[0-9]*?$|weight$", names(vs), ignore.case=TRUE, perl=TRUE)
          if(any(k)) {
            vs <- vs %>% dplyr::rename(WTRAW=names(vs)[k])
            vs <- vs %>% dplyr::mutate(WT=WTRAW)
            vslist <- c(vslist, "WTRAW", "WT")
          }
          
          ## WTUNI, WTU
          k <- grepl("vsorresu_weight_[0-9]*?$|weightu$", names(vs), ignore.case=TRUE, perl=TRUE)
          if(any(k)) {
            vs <- vs %>% dplyr::rename(WTUNI=names(vs)[k])
            vs <- vs %>% dplyr::mutate(WTU=WTUNI)
            vslist <- c(vslist, "WTUNI", "WTU")
            if(normunits) {
              ## upcase UNIT values
              vs <- vs %>% dplyr::mutate(WTUNI=casefold(WTUNI, upper=TRUE),
                                         WTU=casefold(WTU, upper=TRUE))
            }
          }
          
          ## normalize WT, WTRAW to KG from LB if WTUNI/WTU in LB
          if(normunits) {
            ## conversion of LBs to KGs - multiple by 0.45359
            wt_lb_to_kg <- 0.45359
            k <- grepl("WTUNI$", names(vs), ignore.case=TRUE, perl=TRUE)
            if(any(k)) {
              wtunit <- names(vs)[k]
              
              j <- grepl("WT$", names(vs), ignore.case=TRUE, perl=TRUE)
              if(any(j)) {
                vs <- vs %>%
                  dplyr::mutate(WT=ifelse(WTUNI=="LB", WT*wt_lb_to_kg, WT))  %>%
                  dplyr::mutate(WTU=ifelse(WTU=="LB", "KG", WTU))
                # dplyr::mutate(WTRAW=ifelse(WTUNI=="LB", WTRAW*wt_lb_to_kg, WTRAW)) %>%
                # dplyr::mutate(WTUNI=ifelse(WTUNI=="LB", "KG", WTUNI)) %>%
                
              }
            }
          }
          
          ## HTRAW, HT
          k <- grepl("vsorres_height_[0-9]*?$|height$", names(vs), ignore.case=TRUE, perl=TRUE)
          if(any(k)) {
            vs <- vs %>% dplyr::rename(HTRAW=names(vs)[k])
            vs <- vs %>% dplyr::mutate(HT=HTRAW)
            vslist <- c(vslist, "HTRAW", "HT")
          }
          
          ## HTUNI
          k <- grepl("vsorresu_height_[0-9]*?$|heightu$", names(vs), ignore.case=TRUE, perl=TRUE)
          if(any(k)) {
            vs <- vs %>% dplyr::rename(HTUNI=names(vs)[k])
            vslist <- c(vslist, "HTUNI")
            if(normunits) {
              ## upcase UNIT values
              vs <- vs %>%
                dplyr::mutate(HTUNI=casefold(HTUNI, upper=TRUE))
            }
          }
          
          ## normalize HT, HTRAW to CM from IN if WTUNI in IN
          if(normunits) {
            ## conversion of LBs to KGs - multiple by 0.45359
            ht_in_to_cm <- 2.54
            k <- grepl("HTUNI$", names(vs), ignore.case=TRUE, perl=TRUE)
            if(any(k)) {
              htunit <- names(vs)[k]
              
              j <- grepl("HT$", names(vs), ignore.case=TRUE, perl=TRUE)
              if(any(j)) {
                vs <- vs %>%
                  dplyr::mutate(HT=ifelse(HTUNI=="IN", HT*ht_in_to_cm, HT))
                # %>%
                #     dplyr::mutate(HTRAW=ifelse(HTUNI=="IN", HTRAW*ht_in_to_cm, HTRAW)) %>%
                #     dplyr::mutate(HTUNI=ifelse(HTUNI=="IN", "CM", HTUNI))
              }
            }
          }
          
          vs <- vs %>% dplyr::select(all_of(vslist))
        }
      } else {
        ## for the moment just stop, should be able to check whether
        ##  the vs dataset can still be used
        ##  might want to simply return here without data instead of stop
        warning("Missing visit information for vitals (vs) dataset")
      }
    }
  }
  
  ## ensure SUBJID is character
  if("SUBJID" %in% names(dm)) {
    dm <- dm %>% dplyr::mutate(SUBJID=as.character(SUBJID))
  }
  
  ## merge vitals (vs) records into demog (dm) result
  if(exists("dm") & exists("vs") & length(vslist)>1) {
    dm <- dm %>% dplyr::left_join(vs, by=c("SUBJID"))
  }
  
  if(exists("dm")) {
    return(dm)
  } else {
    return()
  }
  
}
