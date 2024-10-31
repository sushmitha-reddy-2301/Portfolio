output$Total_Abnormalities <- renderValueBox({
  req(uploadedData$THdata(), uploadedData$data1())
  
  # Join data to get LLOQ and ULOQ values
  FLT <- uploadedData$THdata()
  
  # Convert PCORRES to numeric, handling "<" values by assuming 0.3 when PCORRES == "<0.400"
  FLT <- FLT %>%
    mutate(PCORRES = as.numeric(ifelse(grepl("^<", PCORRES), 0.3, as.numeric(PCORRES))))
  
  # Convert ULOQ and LLOQ to numeric (removing units)
  normal_values_numeric <- uploadedData$data1() %>%
    mutate(ULOQ = as.numeric(gsub("[^0-9.]", "", ULOQ)),  # Extract numeric part
           LLOQ = as.numeric(gsub("[^0-9.]", "", LLOQ)))
  
  # Join and filter to get abnormal values (Adjusted join and filter logic)
  joined_data <- FLT %>%
    left_join(normal_values_numeric, by = c("STUDYID", "TREATXT", "VISIT")) %>%
    filter(PCORRES < LLOQ | PCORRES > ULOQ)  # Filtering to get only abnormal values
  
  total_abnormalities <- nrow(joined_data)  # Get the count of abnormalities
  
  valueBox(
    elevation = 3,
    value = total_abnormalities,
    subtitle = "Total Abnormalities in Study",
    color = "danger",
    icon = icon("exclamation-triangle"),
    href = NULL
  )
})
