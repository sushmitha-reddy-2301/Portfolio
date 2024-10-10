mergeECsm_ec001_4 <- function(protocol, ecdomain, pkdef, ard, timecutoff, verbose=TRUE, ec001risk=FALSE) {
  functionName <- as.list(sys.call())[[1]]
  
  pkdeflist <- ard$pkdeflist
  pkterms <- pkdef$pkterms
  mergeddata <- pkdef$mergeddata
  ecdata <- ard$ec[[ecdomain]]$data
  
  ## Add OBS field to ensure can retain only original records from PKDEF file
  print("before dose merge dim(mergedata)")
  print(dim(mergeddata))
  mergeddata <- mergeddata %>%
    mutate(OBS=1:nrow(.))
  k <- table(mergeddata$OBS)
  k <- k[k>1]
  print(k)
  
  ## timecutoff = adjustable parameter in seconds, default=10 min, - surface in main createARDdataset call
  if(missing(timecutoff)) {
    timecutoff = 600
  }
  
  if(verbose) {
    cat(glue::glue("{functionName}: dim(mergeddata) 1: {paste(dim(mergeddata), collapse=' ')}"), "\n")
  }
  
  if(verbose & "atmap" %in% names(ard)) {
    if(nrow(ard$atmap)>0) {
      analyteTreatmentMap <- ard$atmap %>%
        filter(pkdeftrt %in% pkterms) %>% print()
    }
  } else { } ## create a dummy map here? or get it in the function that generates it????
  
  ## ------------------------------------------------------------------
  ## assign the ectrt_001 as per the analyteTreatmentMap
  ##  this will then allow dose mapping via ectrt variable
  if(nrow(analyteTreatmentMap)>0) {
    analyteTreatmentMap %>% dplyr::rename(PKTERM=pkdeftrt) %>% print()
    mergeddata <- mergeddata %>%
      left_join(analyteTreatmentMap %>%
                  dplyr::rename(PKTERM=pkdeftrt),
                by=c("PKTERM" = "PKTERM"),
                keep=FALSE)
  }
  
  ## need to provide a warning if no matching pkterms in pkdeftrt of AnalyteTreatmentMap
  ##  as well as skip merging of sm_ec001_4 content
  if(exists("analyteTreatmentMap")) {
    if(nrow(analyteTreatmentMap)==0) {
      msg <- glue::glue("{functionName}: no matching pkterms in AnalyteTreatmentMap!")
      cat(msg, "\n")
    }
  }
  
  if(verbose) { print(glue::glue("{functionName}: protocol: {protocol} ecdomain: {ecdomain}:")) }
  
  ## get subject ids from ard/pkdef mergeddata
  sidpk <- unname(unlist(mergeddata %>%
                           select(SUBJID) %>%
                           distinct()))
  if(verbose) {
    cat(glue::glue("{functionName}: sidpk (SUBJIDs in PKDEF):"), "\n")
    print(sort(sidpk))
  }
  
  ## get subject ids from each table
  sidec <- unname(unlist(ecdata %>%
                           select(matches("subjectnumberstr")) %>%
                           distinct()))
  if(verbose) {
    cat(glue::glue("{functionName}: sidec: "), "\n")
    print(sort(sidec))
  }
  
  ## only select subject ids matching in both datasets - i.e. subjects with PK Data
  sid <- sidec[sidec %in% sidpk]
  if(verbose) {
    cat(glue::glue("{functionName}: sid matching between sidec and sidpk: "), sort(unlist(sid)), "\n")
  }
  
  ## JP 05/2023: In case of multiple treatments, the LOCF work later will not work for
  ## each treatment-analyte pair. So if there is a map, use that to limit ectrt_001.
  ## This can also be grouped into the other conditional block earlier in this function
  if(nrow(analyteTreatmentMap)>0) {
    ectrts <- analyteTreatmentMap %>%
      pull(ectrt) %>%
      unique()
  } else {
    ectrts <- ecdata %>%
      pull(ectrt_001) %>%
      unique()
  }
  
  
  while ((ecdomain=="sm_ec001_4" | (grepl("sm_ec001",ecdomain) & ec001risk)) & nrow(analyteTreatmentMap)>0) {
    
    
    ## arrange/sort by subjectnumberstr, ectrt_001, visitrefname, ecstdtc_001_dts
    ecdata <- ecdata %>%
      arrange(subjectnumberstr, ectrt_001, ecstdtc_001_dts, visitrefname) %>%
      filter(ectrt_001 %in% ectrts)
    
    if (nrow(ecdata)==0) break
    ecdata <- ecdata %>%
      # issue with mixed ec001 with ec001_4. This may cause an error if the filter makes to df empty
      mutate(recordID=1:nrow(.)) ## setup up a records merge key
    
    ## note the following method won't work for more than "duplicate" doses that occur within "timecutoff"
    ## JP 5/2023: disabling this for now, as it causes error in C1071003 test case
    k <- grepl("^suppec_ecdosadj_001$", names(ecdata), ignore.case=TRUE, perl=TRUE)
    if(any(k) & FALSE) {
      ecdata <- ecdata %>%
        #filter(str_to_lower(suppec_ecdosadj_001)=="no") ## remove records where dose/volume was adjusted for adverse events, this doesn't take that into account at this time
        filter(grepl("^n", suppec_ecdosadj_001, ignore.case=TRUE)) ## remove records where dose/volume was adjusted for adverse events, this doesn't take that into account at this time
    }
    
    ## if dose is 0 & it is doseadj, it can be ignored
    ## this also can be removed if not consistently done by programmers
    k <- grepl("^suppec_ecdosadj_001$", names(ecdata), ignore.case=TRUE, perl=TRUE)
    j <- grepl("(ecdose|suppec_ecvlat)_[0-9]+$", names(ecdata), ignore.case=TRUE, perl=TRUE)
    if(any(k) & any(j)) {
      
      dose_cols <- names(ecdata)[j]
      dose_col <- dose_cols[1]
      if (length(dose_cols)==2) {
        # select the numeric one
        dose_col <- dose_cols[c(
          is.numeric(ecdata[[dose_cols[1]]]),
          is.numeric(ecdata[[dose_cols[2]]])
        )] %>%
          .[1]
      }
      if (verbose) {
        print(dose_col)
      }
      
      ecdata <- ecdata %>%
        ## remove records where dose/volume was adjusted and 0 was given
        # So must be no or if yes then the dose is greater than 0
        filter(grepl("^n", suppec_ecdosadj_001, ignore.case=TRUE) |
                 (grepl("^y", suppec_ecdosadj_001, ignore.case=TRUE) & !is.na(get(dose_col)) & get(dose_col) > 0))
    }
    
    ## check for replicate records within timecutoff
    ecdata <- ecdata %>%
      group_by(subjectnumberstr) %>%
      arrange(subjectnumberstr, lubridate::ymd_hms(ecstdtc_001_dts)) %>%
      mutate(dts=as.POSIXlt(ecstdtc_001_dts)) %>%
      mutate(diffdts=abs(lag(dts)-dts)) %>% ## determine the difference in time between adjacent records
      mutate(withincutoff=diffdts<=timecutoff) %>% ## determine if the difference is within the timecutoff value
      mutate(withincutoff=ifelse(is.na(withincutoff), FALSE, withincutoff)) %>% ## ditto
      mutate(leadtest=lead(withincutoff) | withincutoff) ## set flag to determine which records are potentially impacted by multiple doses within the timecutoff
    
    if(verbose) {
      cat(glue::glue("{functionName}: dim(ecdata): {paste(dim(ecdata), collapse=' ')}"), "\n")
      ecdata %>% as.data.frame() %>% head() %>% print(na.print="NA")
      
      cat(glue::glue("{functionName}: isLOGSvisitrefname =  { isLOGSvisitrefname(ecdata) } "), "\n")
    }
    
    ## if isLOGSvisitrefname == TRUE then check to see if sm_ec001_5 is available and merge
    ## data from there?
    
    ## select subsect ec data from subjects with PK data
    ##  sort by SUBJID and VISIT
    ## 2022-12-30/TGT/ if have ecdomain dataset, ACTTRT is mapped from ectrt_001
    ##  sm_ec001_4 doesn't appear to have equivalents for ACTTRTC, ACTTRTS
    ##  in the current edition of this code, won't attempt to
    ##  merge final randomization data since a definative source
    ##  for it has yet be be identified
    ecdata <- ecdata %>%
      filter(subjectnumberstr %in% sid) %>%
      mutate(ACTTRT=ectrt_001, ACTTRTC=NA, ACTTRTS=NA) %>%
      arrange(subjectnumberstr, ectrt_001, visitrefname)
    
    ## remove records with missing ecstdtc_001
    rex <- "^ecstdtc_001_dts$"
    k <- grepl(rex, names(ecdata), ignore.case=TRUE, perl=TRUE)
    if(any(k>0)) {
      ecdata <- ecdata %>%
        filter(!is.na(ecstdtc_001_dts))
    }
    
    ## filter treatment data to those treatments that appear in both
    ##  the pklist mergeddata dataset and the ecdomain...$ecdata dataset
    ##  use analyteTreatmentMap to map ectrt_001 (ectrt) to matching
    ##  pkdeftrt treatments
    
    if(verbose) {
      cat(glue::glue('{functionName} analyteTreatmentMap:'), '\n')
      print(analyteTreatmentMap)
    }
    
    if(("ectrt" %in% names(analyteTreatmentMap))) { ## check if analyteTreatmentMap has ectrt defined
      if(analyteTreatmentMap$ectrt %in% analyteTreatmentMap$pkdeftrt) { ## check if a value in ectrt
        ecdata <- ecdata %>%
          filter(ectrt_001 %in% (analyteTreatmentMap %>% select(ectrt)))
      }
    }
    
    if(verbose) {
      cat(glue::glue("{functionName}: dim(ecdata): {paste(dim(ecdata), collapse=' ')}"), "\n")
      ecdata %>% as.data.frame() %>% head(5) %>% print()
    }
    
    replicates <- ecdata
    ## identify records with that have duplicate records
    replicates <- replicates %>%
      group_by(subjectnumberstr) %>%
      filter(leadtest==TRUE) %>% ## filter only records impacted
      mutate(priorrecord=ifelse(diffdts<=timecutoff, lag(recordID), recordID)) ## carry recordID from related records
    ## if first record is replicated, have to adjust priorrecord
    replicates <- replicates %>%
      mutate(priorrecord=ifelse(is.na(priorrecord), recordID, priorrecord))
    
    if(verbose) {
      ylist <- c("recordID", "priorrecord", "subjectnumberstr", "visitrefname", "visitindex", "ecstdtc_001_dts",
                 "suppec_ecvlat_001", "suppec_ecvlatu_001", "ecloc_001", "eclat_001", "ecstdtc_001_dts", "ecendtc_001_dts",
                 "ecdose_001", "ecdosu_001", "suppec_ecdosadj_001", "cumulativedose", "cumulativevolume")
      cat(glue::glue("{functionName}: dim(replicates): {paste(dim(replicates), collapse=' ')}"), "\n")
      replicates %>% select(any_of(ylist)) %>% as.data.frame() %>% head(10) %>% print(na.print="NA")
    }
    
    ## if nrow(replicates)>0 there are records that share subjectnumberstr, visitrefname, and ecstdtc_001_dts within timecutoff
    ##  meaning there are replicate dosing records that should be aggregated to a single dose prior to merging with
    ##  pk records
    ## The approach below retains the earliest record and thus the earliest timestamp but accumulates the
    ##  doses (ecdose_001) or volumes (suppec_ecvlat_001) into a summed value that is used as an aggregate dose amount or
    ##  volume
    if(nrow(replicates)>0) {
      ## records to remove from EC
      recordIDsToRemove <- replicates %>%
        select(recordID, priorrecord) %>%
        filter(recordID != priorrecord) %>%
        ungroup() %>%
        select(recordID)
      
      ## recordIDsToRemove are the "latter" dose/volume records from a datetime stamp perspective
      recordIDsToRemove <- unname(unlist(recordIDsToRemove))
      
      ## remove "replicated" dose records from ecdata
      retainrecords <- setdiff(ecdata$recordID, recordIDsToRemove)
      ecdata <- ecdata %>% filter(recordID %in% retainrecords)
      
      ## if ecdose_001 else if suppec_ecvlat_001 ? - need to summarize one or the other or both
      rex <- "(ecdose|suppec_ecvlat)_[0-9]+$"
      k <- grepl(rex, names(ecdata), ignore.case=TRUE, perl=TRUE)
      if(any(k)) {
        ysum <- replicates %>%
          ungroup() %>%
          group_by(subjectnumberstr, priorrecord) %>% ## regroup by recordID
          select(matches(rex)) %>%
          summarize_at(vars(-group_cols()), sum, na.rm=TRUE)
      }
      
      ## remove "replicated" dose records
      retainrecords <- setdiff(replicates$recordID, recordIDsToRemove)
      replicates <- replicates %>% filter(recordID %in% retainrecords)
      
      ## remove summed variables
      rex <- "(ecdose|suppec_ecvlat)_[0-9]+$"
      replicates <- replicates %>% select(!matches(rex))
      
      ## merge replicates with aggregated dose/volumes back into ecdata
      k <- grepl(rex, names(ysum), ignore.case=TRUE, perl=TRUE)
      updatecols <- names(ysum)[k]
      for(i in ysum$priorrecord) {
        ecdata[ecdata$recordID %in% i, updatecols] <- ysum[ysum$priorrecord %in% i, updatecols]
      }
      
      ## remove recordID
      ecdata <- ecdata %>%
        mutate(recordID=NULL,
               diffdts=NULL,
               withincutoff=NULL,
               leadtest=NULL)
    }
    
    ## "^dts$|^ACTTRT$|^dosedt$|^suppec_ecdosadj_001_c$|^(suppec)*(_)*ec(stdtc|frq|freq|prfrq|dosfrm|dose|dosu|doseu|route|vlat|vlatu)+?_[0-9]{3}$"
    rex <- "^dts$|^dosedt$|^(ectrt|ecdose|ecdosu|ecroute|ecdosfrm|formrefname|formidx|suppec_ecprfrq|suppec_ecvlat|suppec_ecvlatu|suppec_ecdosadj)+?_[0-9]+?$|^(ecstdtc_[0-9]+?_dts)$|^(ecendtc_[0-9]+?_dts)$"
    k <- grepl(rex, names(ecdata), ignore.case=TRUE, perl=TRUE)
    dlist <- c("subjectnumberstr", "visitrefname", "ectrt_001", names(ecdata)[k])
    ecdata <- ecdata %>%
      select(any_of(dlist))
    
    ## create mergeddata subset for merging with dosing/exposure information
    if (!"PERIOD" %in% names(mergeddata)) {
      mergeddata <- mergeddata %>%
        mutate(PERIOD=1)
    }
    a <- mergeddata %>%
      mutate(dts=pkdttm) %>%
      mutate(dts=as.POSIXct(dts)) %>%
      select(SUBJID, VISITPC, PKTERM, pkdttm, dts, PKPTMR, PERIOD)
    
    kdts <- grepl("^ecstdtc_[0-9]+?_dts$", names(ecdata), ignore.case=TRUE, perl=TRUE)
    necstdtc <- names(ecdata)[kdts]
    b <- ecdata %>%
      ungroup() %>%
      mutate(SUBJID=as.character(subjectnumberstr)) %>%
      mutate(VISITPC=visitrefname) %>%
      mutate(dts=!!!necstdtc) %>%
      mutate(dsdttm = dts) %>%
      arrange(SUBJID, dts) %>%
      select(SUBJID, VISITPC, dts, dsdttm, matches(rex))
    
    dose <- a %>%
      bind_rows(b, .id="src") %>%
      arrange(SUBJID, dts) %>%
      # Add calendar day
      mutate(CALDAY=lubridate::date(dts))
    
    
    k <- grepl(rex, names(dose), ignore.case=TRUE, perl=TRUE)
    kc <- names(dose)[k]
    
    ## sort dose by SUBJID and datetimestamp
    # dose <- dose %>% arrange(SUBJID, dts)
    
    na.class <- function(x, cv=NULL) { # NA with matching class
      px <- rep(NA, length(x))
      if (is.null(cv)) class(px) <- class(x)
      else class(px) <- class(cv)
      px
    }
    na.locf <- function(x, nocb = FALSE) { # https://stackoverflow.com/a/41752185 (instead of zoo)
      if (nocb) x <- rev(x)
      v <- !is.na(x)
      # match class of x for leading NA
      leadNA <- na.class(NA, x)
      out <- c(leadNA, x[v])[cumsum(v)+1]
      if (nocb) return(rev(out))
      out
    }
    
    # Fill PERIOD (fill NA between 2 vals)
    # https://stackoverflow.com/a/48843154
    dose <- dose %>%
      group_by(SUBJID) %>%
      mutate(prev_val = (PERIOD), next_val = (PERIOD)) %>%
      fill(prev_val, .direction = "down") %>%
      fill(next_val, .direction = "up") %>%
      mutate(PERIOD = ifelse(prev_val == next_val, prev_val, PERIOD )) %>%
      select(-prev_val, -next_val) %>%
      ungroup()
    
    for(i in kc) {
      
      # LOCF where PKPTMR is > 0, NOCB when PKPTMR is 0
      # Unless PK is on same visit day
      dose <- dose %>%
        # Apply logic to match same calendar day
        group_by(SUBJID,CALDAY) %>%
        mutate(!!i := case_when(
          is.na(PKPTMR) ~ get(i),
          PKPTMR==0 ~ na.locf(get(i), nocb = TRUE),
          TRUE ~ na.locf(get(i))
        )) %>%
        # LOCF anything else on same day
        mutate(!!i := na.locf(get(i))) %>%
        ungroup() %>%
        # For PK on the same visit as dose, do that
        group_by(SUBJID,VISITPC) %>%
        mutate(!!i := case_when(
          is.na(PKPTMR) ~ get(i),
          PKPTMR==0 ~ na.locf(get(i), nocb = TRUE),
          TRUE ~ na.locf(get(i))
        )) %>%
        # This may result in NAs still remaining for the visit day, which
        # seems to result from doseadjustment vists. In which case, dose
        # is "midnight" and PKPTMR 0 is not pre-dose trough.
        # For these, do another LOCF
        mutate(!!i := na.locf(get(i))) %>%
        ungroup() %>%
        # Do same for study period
        group_by(SUBJID,PERIOD) %>%
        mutate(!!i := case_when(
          is.na(PERIOD) ~ get(i),
          is.na(PKPTMR) ~ get(i),
          PKPTMR==0 ~ na.locf(get(i), nocb = TRUE),
          TRUE ~ na.locf(get(i))
        )) %>%
        mutate(!!i := case_when(
          is.na(PERIOD) ~ get(i),
          TRUE ~ na.locf(get(i))
        )) %>%
        ungroup() %>%
        # Fill any remaining NAs
        group_by(SUBJID) %>%
        mutate(!!i := na.locf(get(i))) %>%
        # mutate(!!i := case_when(
        #     is.na(PKPTMR) ~ get(i),
        #     PKPTMR==0 ~ na.locf(get(i), nocb = TRUE),
        #     TRUE ~ na.locf(get(i))
        # )) %>%
        ungroup()
    }
    
    
    # for(i in kc) {
    #     dose <- dose %>%
    #         group_by(SUBJID) %>%
    #         fill(!!!i, .direction='down') %>%
    #         ungroup()
    # }
    # for(i in kc) {
    #     dose <- dose %>%
    #         group_by(SUBJID) %>%
    #         fill(!!!i, .direction='up') %>%
    #         ungroup()
    # }
    
    ## subset to PK observation records (src==1) from pkdef
    ##  and remove source (src) field
    dose <- dose %>%
      filter(src==1) %>%
      mutate(src=NULL)
    
    nme <- names(dose)
    
    ## start/end datetime stamp variables
    rex <- "(?<start>st)*(?<end>en)*dtc_[0-9]*?"
    k <- grepl(rex, nme, ignore.case=TRUE, perl=TRUE)
    if(any(k)) { nme <- nme[k] }
    if(verbose) { cat(glue::glue("{functionName}: nme: { nme }"), "\n") }
    
    ## for these data, should total amounts by subject by visitrefname or day of ecstdtc and use
    ## that cut down dataset using the time based upon the (first dt or last dt or the average)?
    ##  Note: assumption here is that units are the same across doses
    kd <- grepl("^(suppec)*(_)*ec(dose)+?_[0-9]{3}$", names(dose), ignore.case=TRUE, perl=TRUE)
    nd <- names(dose)[kd]
    if(verbose) { cat(glue::glue("{functionName}: nd: { nd }"), "\n") }
    
    kv <- grepl("^(suppec)*(_)*ec(vlat)+?_[0-9]{3}$", names(dose), ignore.case=TRUE, perl=TRUE)
    nv <- names(dose)[kv]
    if(verbose) { cat(glue::glue("{functionName}: nv: { nv }"), "\n") }
    
    dose <- dose %>%
      mutate(dosedt = ecstdtc_001_dts)
    
    ## add dosedate from dosedt
    kdt <- grepl("dosedt", names(dose), ignore.case=TRUE, perl=TRUE)
    if(any(kdt)) {
      dose <- dose %>%
        mutate(dosedate=lubridate::as_date(dosedt))
    }
    
    ## rename columns
    ## rename volume column to PKSMVL
    kv  <- grepl("^(suppec)*(_)*ec(?<volume>vlat)+?_[0-9]{3}$", names(dose), ignore.case=TRUE, perl=TRUE)
    if(any(kv)) {
      dose <- dose %>%
        dplyr::rename(PKSMVL=names(dose)[kv])
    }
    
    ## rename volume unit column to PKSMVLU
    kvu  <- grepl("^(suppec)*(_)*ec(?<volumeu>vlatu)+?_[0-9]{3}$", names(dose), ignore.case=TRUE, perl=TRUE)
    if(any(kvu)) {
      dose <- dose %>%
        dplyr::rename(PKSMVLU=names(dose)[kvu])
    }
    ## rename dose unit column to DOSEUNI
    kdu  <- grepl("^(suppec)*(_)*ec(?<doseu>dosu)+?_[0-9]{3}$", names(dose), ignore.case=TRUE, perl=TRUE)
    if(any(kdu)) {
      dose <- dose %>%
        dplyr::rename(DOSEUNI=names(dose)[kdu])
    }
    ## rename dose column to DOSE
    kd  <- grepl("^(suppec)*(_)*ec(?<dose>dose)+?_[0-9]{3}$", names(dose), ignore.case=TRUE, perl=TRUE)
    if(any(kd)) {
      dose <- dose %>%
        dplyr::rename(DOSE=names(dose)[kd])
    }
    ## rename route column to ROUTE
    krt  <- grepl("^(suppec)*(_)*ec(?<route>route)+?_[0-9]{3}$", names(dose), ignore.case=TRUE, perl=TRUE)
    if(any(krt)) {
      dose <- dose %>%
        dplyr::rename(ROUTE=names(dose)[krt])
    }
    ## rename frequency column to frequency
    ##   note that frequency currently not part of ARD specification but will be useful for NONMEM ARD
    kfr  <- grepl("^(suppec)*(_)*ec(?<frequency>(prfrq|frq))+?_[0-9]{3}$", names(dose), ignore.case=TRUE, perl=TRUE)
    if(any(kfr)) {
      dose <- dose %>%
        dplyr::rename(frequency=names(dose)[kfr])
    }
    ## rename dosfrm column to DOSFRM
    ##   note that dosfrm currently not part of ARD specification but will be useful for NONMEM ARD
    kdf  <- grepl("^(suppec)*(_)*ec(?<doseform>dosfrm)+?_[0-9]{3}$", names(dose), ignore.case=TRUE, perl=TRUE)
    if(any(kdf)) {
      dose <- dose %>%
        dplyr::rename(DOSFRM=names(dose)[kdf])
    }
    ## rename ecstdtc column to start dosestartdt
    kds  <- grepl("^(suppec)*(_)*ec(?<dosetimestart>stdtc)+?_[0-9]{3}(_dts)?$", names(dose), ignore.case=TRUE, perl=TRUE)
    if(any(kds)) {
      dose <- dose %>%
        dplyr::rename(dosestartdt=names(dose)[kds])
    }
    ## rename ecendtc column to end doseenddt
    kde  <- grepl("^(suppec)*(_)*ec(?<dosetimeend>endtc)+?_[0-9]{3}(_dts)?$", names(dose), ignore.case=TRUE, perl=TRUE)
    if(any(kde)) {
      dose <- dose %>%
        dplyr::rename(doseenddt=names(dose)[kde])
    }
    
    ## add an original_record value to mergeddata
    mergeddata <- mergeddata %>%
      arrange(SUBJID, pkdttm) %>%
      mutate(originalrecord=1:nrow(.))
    
    ## merge mergeddata with dose
    mergeddata <- mergeddata %>%
      mutate(SUBJID = as.character(SUBJID)) %>%
      mutate(mergeddosedt = strptime(PKADT, format="%Y%m%d")) %>%
      left_join(dose %>%
                  mutate(SUBJID=as.character(SUBJID)),
                by=c("SUBJID" = "SUBJID",
                     "pkdttm" = "pkdttm",
                     "PKTERM" = "PKTERM",
                     "ectrt" = "ectrt_001"),
                suffix=c("", paste0(".", ecdomain)),
                keep=FALSE)
    print("after dose merge dim(mergedata)")
    print(dim(mergeddata))
    k <- table(mergeddata$OBS)
    k <- k[k>1]
    print(k)
    mergeddata <- mergeddata %>%
      filter(!duplicated(OBS))
    mergeddata %>%
      filter(OBS %in% names(k)) %>%
      as.data.frame() %>% print
    
    ## remove records that didn't originally existing in pkdef
    mergeddata <- mergeddata %>%
      filter(!is.na(originalrecord))
    
    ## remove originalrecord
    mergeddata <- mergeddata %>%
      mutate(originalrecord=NULL)
    
    if(verbose) {
      cat(glue::glue("{functionName}: dim(mergeddata) 6: {paste(dim(mergeddata), collapse=' ')}"), "\n")
    }
    
    ## update ard with results
    ard$ards[[pkdef$file]]$pkdeffile <- pkdef$file
    ard$ards[[pkdef$file]]$pkterm <- pkdef$pkterm
    ard$ards[[pkdef$file]]$ard <- mergeddata
    
    if(verbose) {
      cat(glue::glue("{functionName}: names in pkdef file"), "\n")
      print(names(ard$ards[[pkdef$file]]))
    }
    
    if(verbose) {
      cat(glue::glue("{functionName}: ard: (before return)"), "\n")
      ard$ards[[pkdef$file]]$ard %>% as.data.frame() %>% head(5) %>% print()
    }
    
    if(verbose) {
      print(glue::glue("{functionName}: names(ard):"))
      print(names(ard))
      print(glue::glue("{functionName}: names(ard$ards[[pkdef$file]]):"))
      print(names(ard$ards[[pkdef$file]]))
      print(glue::glue("{functionName}: names(ard$ards[[pkdef$file]]$ard):"))
      print(names(ard$ards[[pkdef$file]]$ard))
    }
    
    break
    
  } ## end of "sm_ec001_4" processing
  
  ## return updated ard
  return(ard)
}
