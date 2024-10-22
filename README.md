library(shiny)
library(bs4Dash)
library(DT)      # For DataTable rendering
library(waiter)  # For Waiter spinner functionality
library(readxl)  # For reading Excel files

# UI Function
Data_Load_UI <- function(id) {
  ns <- NS(id)
  
  tabPanel(
    "Load Data",
    value = "Tab1",
    fluidRow(
      # First File Input for SDAA Data
      column(3, 
             fileInput(ns("files1"), 
                       "Upload SDAA Excel File", 
                       accept = c(".csv", ".xlsx"))),
      # Second File Input for Clin Pharma Lead Normal Values
      column(3, 
             fileInput(ns("files2"), 
                       "Upload Clin Pharma Lead Normal Values", 
                       accept = c(".csv", ".xlsx"))),
      # Third File Input for Additional Data
      column(3, 
             fileInput(ns("files3"), 
                       "Upload Treatment Codes File", 
                       accept = c(".csv", ".xlsx")))
    ),
    fluidRow(
      box(
        title = "SDAA Data",
        closable = FALSE,
        width = 12,
        status = "warning",
        solidHeader = FALSE,
        collapsible = TRUE,
        fluidRow(
          column(12,
                 align = "center",
                 DT::dataTableOutput(ns("dtout1")) %>% 
                   withSpinner(color = "#0095FF")
          )
        )
      )
    ),
    fluidRow(
      box(
        title = "Clin Pharma Lead Normal Values",
        closable = FALSE,
        width = 12,
        status = "info",
        solidHeader = FALSE,
        collapsible = TRUE,
        fluidRow(
          column(12,
                 align = "center",
                 DT::dataTableOutput(ns("dtout2")) %>% 
                   withSpinner(color = "#FF5733")
          )
        )
      )
    ),
    fluidRow(
      box(
        title = "Treatment Codes Data",
        closable = FALSE,
        width = 12,
        status = "primary",
        solidHeader = FALSE,
        collapsible = TRUE,
        fluidRow(
          column(12,
                 align = "center",
                 DT::dataTableOutput(ns("dtout3")) %>% 
                   withSpinner(color = "#28a745")
          )
        )
      )
    )
  )
}

# Server Function
Data_Load_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- NS(id)
    
    # Reactive values to store uploaded data
    uploadedData1 <- reactiveVal(NULL)
    uploadedData2 <- reactiveVal(NULL)
    uploadedData3 <- reactiveVal(NULL)
    
    # Waiter object for loading screen
    w <- Waiter$new()
    
    # Observer for SDAA Excel File Upload
    observeEvent(input$files1, {
      if (is.null(input$files1)) {
        sendSweetAlert(session = session, title = "Error", text = "No Records Present", type = "error")
      } else {
        w$show()
        withProgress(message = 'Uploading File 1', value = 0, {
          for (i in 1:10) {
            Sys.sleep(0.1)
            incProgress(1/10, detail = paste("Progress:", i * 10, "%"))
          }
        })
        
        data <- if (tools::file_ext(input$files1$name) == "csv") {
          read.csv(input$files1$datapath)
        } else {
          readxl::read_excel(input$files1$datapath)
        }
        uploadedData1(data)
        
        output$dtout1 <- DT::renderDataTable({
          datatable(
            uploadedData1(), 
            options = list(
              dom = 't',
              scroller = TRUE,
              scrollX = TRUE, 
              "pageLength" = 100
            ),
            rownames = FALSE
          )
        })
        
        w$hide()
      }
    })
    
    # Observer for Clin Pharma Lead Normal Values Upload
    observeEvent(input$files2, {
      if (is.null(input$files2)) {
        sendSweetAlert(session = session, title = "Error", text = "No Records Present", type = "error")
      } else {
        w$show()
        withProgress(message = 'Uploading File 2', value = 0, {
          for (i in 1:10) {
            Sys.sleep(0.1)
            incProgress(1/10, detail = paste("Progress:", i * 10, "%"))
          }
        })
        
        data <- if (tools::file_ext(input$files2$name) == "csv") {
          read.csv(input$files2$datapath)
        } else {
          readxl::read_excel(input$files2$datapath)
        }
        uploadedData2(data)
        
        output$dtout2 <- DT::renderDataTable({
          datatable(
            uploadedData2(), 
            options = list(
              dom = 't',
              scroller = TRUE,
              scrollX = TRUE, 
              "pageLength" = 100
            ),
            rownames = FALSE
          )
        })
        
        w$hide()
      }
    })
    
    # Observer for Treatment Codes File Upload
    observeEvent(input$files3, {
      if (is.null(input$files3)) {
        sendSweetAlert(session = session, title = "Error", text = "No Records Present", type = "error")
      } else {
        w$show()
        withProgress(message = 'Uploading File 3', value = 0, {
          for (i in 1:10) {
            Sys.sleep(0.1)
            incProgress(1/10, detail = paste("Progress:", i * 10, "%"))
          }
        })
        
        data <- if (tools::file_ext(input$files3$name) == "csv") {
          read.csv(input$files3$datapath)
        } else {
          readxl::read_excel(input$files3$datapath)
        }
        uploadedData3(data)
        
        output$dtout3 <- DT::renderDataTable({
          datatable(
            uploadedData3(), 
            options = list(
              dom = 't',
              scroller = TRUE,
              scrollX = TRUE, 
              "pageLength" = 100
            ),
            rownames = FALSE
          )
        })
        
        w$hide()
      }
    })
    
    # Return the reactive values as a list
    return(list(uploadedData1 = uploadedData1, uploadedData2 = uploadedData2, uploadedData3 = uploadedData3))
  })
}

# Module to call the server function
data_load_module <- function(id) {
  moduleServer(id, function(input, output, session) {
    Data_Load_server(id)
  })
}
