library(shiny)
library(bs4Dash)
library(DT)      # For DataTable rendering
library(waiter)  # For Waiter spinner functionality
library(readxl)  # For reading Excel files
library(ggplot2) # For visualizations
library(plotly)  # For interactive plots
library(dplyr)   # For data manipulation

# UI Function for Data Insights
Data_Insights_UI <- function(id){
  ns <- NS(id)
  
  tabPanel(
    "SDAA Dashboard",
    value = "Tab2",
    
    fluidRow(
      valueBoxOutput(ns("STUDYID")),
      valueBoxOutput(ns("Treatment_Description")),
      valueBoxOutput(ns("Biological_Matrix")),
      valueBoxOutput(ns("Category")),
      valueBoxOutput(ns("Bioanalytical_Method"))
    ),
    
    fluidRow(
      box(
        title = "Unique Subjects Donut Chart",
        closable = FALSE, 
        width = 12,
        status = "info", 
        solidHeader = FALSE, 
        collapsible = TRUE,
        plotlyOutput(ns("Unique_Subjects_Donut"))
      )
    ),
    
    fluidRow(
      box(
        title = "Abnormal Values Donut Chart",
        closable = FALSE, 
        width = 12,
        status = "danger", 
        solidHeader = FALSE, 
        collapsible = TRUE,
        plotlyOutput(ns("Abnormal_Values_Donut"))
      )
    )
  )
}

# Server Function for Data Insights
Data_Insights_server <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    
    ns <- session$ns
    
    output$STUDYID <- renderValueBox({
      Fdata <- data() 
      valueBox(
        elevation = 4,
        value = unique(Fdata$STUDYID),
        subtitle = "Study Identifier",
        color = "primary",
        icon = icon("flask"),
        href = "#"
      )
    })
    
    output$Treatment_Description <- renderValueBox({
      Fdata <- data() 
      valueBox(
        elevation = 4,
        value = unique(Fdata$TREATXT),
        subtitle = "Treatment Description",
        color = "info",
        icon = icon("pills")
      )
    })
    
    output$Biological_Matrix <- renderValueBox({
      Fdata <- data() 
      valueBox(
        elevation = 4,
        value = unique(Fdata$PCSPEC),
        subtitle = "Biological Matrix",
        color = "success",
        icon = icon("vial")
      )
    })
    
    output$Category <- renderValueBox({
      Fdata <- data() 
      valueBox(
        elevation = 4,
        value = unique(Fdata$PCCAT),
        subtitle = "Category",
        color = "warning",
        icon = icon("tags")
      )
    })
    
    output$Bioanalytical_Method <- renderValueBox({
      Fdata <- data() 
      valueBox(
        elevation = 4,
        value = unique(Fdata$PCMETHOD),
        subtitle = "Bioanalytical Method",
        color = "primary",
        icon = icon("microscope")
      )
    })
    
    output$Unique_Subjects_Donut <- renderPlotly({
      Fdata <- data()  # Fetch the reactive data
      if (!is.null(Fdata)) {
        unique_subjects_count <- Fdata %>% 
          group_by(SUBJID) %>% 
          summarise(count = n())
        
        plot_ly(
          labels = ~unique_subjects_count$SUBJID,
          values = ~unique_subjects_count$count,
          type = 'pie',
          hole = 0.6,
          textinfo = 'label+percent'
        ) %>%
          layout(title = 'Unique Number of Subjects',
                 showlegend = TRUE)
      }
    })
    
    output$Abnormal_Values_Donut <- renderPlotly({
      SDAA_data <- data()$uploadedData1  # First File Input for SDAA Data
      ClinPharma_data <- data()$uploadedData2  # Second File Input for Clin Pharma Lead Normal Values
      
      if (!is.null(SDAA_data) && !is.null(ClinPharma_data)) {
        abnormal_data <- SDAA_data %>% 
          filter((PCORRES > ClinPharma_data$ULOQ | PCORRES < ClinPharma_data$LLOQ) | 
                   (PCORRESU != ClinPharma_data$ULOQU & PCORRESU != ClinPharma_data$LLOQU))
        
        abnormal_count <- nrow(abnormal_data)
        normal_count <- nrow(SDAA_data) - abnormal_count
        
        plot_ly(
          labels = c("Normal", "Abnormal"),
          values = c(normal_count, abnormal_count),
          type = 'pie',
          hole = 0.6,
          textinfo = 'label+percent'
        ) %>%
          layout(title = 'Abnormal Values Count',
                 showlegend = TRUE)
      }
    })
  })
}

# Module to call the Data Insights UI and server functions
Data_Insights_module <- function(id, data) {
  Data_Insights_UI(id)
  Data_Insights_server(id, data)
}


