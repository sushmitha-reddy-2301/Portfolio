# Load necessary libraries
library(shiny)
library(ggplot2)
library(plotly)
library(viridis)
library(DT)
library(readxl)
library(dplyr)

# Load the dataset
data <- read_excel("~/DataApp.xlsx")

# Convert concentration to numeric dynamically, handling "<" values
data$PCORRES_numeric <- sapply(data$PCORRES, function(x) {
  if (grepl("^<", x)) {
    # Extract the numeric value after "<" and convert it to a numeric type
    limit_value <- as.numeric(sub("<", "", x))
    # Calculate a value dynamically (e.g., 75% of the limit value, or any strategy)
    return(limit_value * 0.75)
  } else {
    return(as.numeric(x))
  }
})

# Convert timepoint to numeric for sorting purposes
data$time_numeric <- as.numeric(sub(" H", "", data$PCTPT))

# Order timepoint dynamically and create a factor with ordered levels
data$PCTPT_factor <- factor(data$PCTPT, levels = unique(data$PCTPT[order(data$time_numeric)]))

# Create a unique identifier for each VISIT and SUBJID combination
data$visit_id <- paste(data$SUBJID, data$VISIT, sep = "_")

# UI for the Shiny app
ui <- fluidPage(
  titlePanel("Interactive Graph with Data Table"),
  
  sidebarLayout(
    sidebarPanel(
      # Place RESET ALL button at the top of the filter panel
      actionButton("reset_filters", "RESET ALL", style = "margin-bottom: 20px;"),
      
      # Filter Panel with search box and scrollable checkbox group for SUBJID, VISIT, PCTPT
      h4("Filters"),
      textInput("subjid_search", "Search SUBJECT ID:", value = ""),
      tags$div(
        style = "height: 200px; overflow-y: scroll;",
        checkboxGroupInput("subjid_filter", "Select SUBJECT ID:", choices = unique(data$SUBJID), selected = unique(data$SUBJID))
      ),
      tags$div(
        style = "height: 200px; overflow-y: scroll;",
        checkboxGroupInput("visit_filter", "Select VISIT:", choices = unique(data$VISIT), selected = unique(data$VISIT))
      ),
      tags$div(
        style = "height: 200px; overflow-y: scroll;",
        checkboxGroupInput("pctpt_filter", "Select PCTPT:", choices = unique(data$PCTPT), selected = unique(data$PCTPT))
      )
    ),
    
    mainPanel(
      fluidRow(
        column(
          width = 12,
          plotlyOutput("interactive_plot", height = "500px")
        )
      ),
      fluidRow(
        column(
          width = 12,
          DTOutput("data_table")
        )
      )
    )
  )
)

# Server logic for the Shiny app
server <- function(input, output, session) {
  
  # Reactive expression to filter SUBJID based on the search input
  filtered_subjid <- reactive({
    if (input$subjid_search == "") {
      unique(data$SUBJID)
    } else {
      unique(data$SUBJID[grepl(input$subjid_search, data$SUBJID, ignore.case = TRUE)])
    }
  })
  
  # Update checkboxGroupInput for SUBJID based on the search input
  observe({
    updateCheckboxGroupInput(
      session,
      "subjid_filter",
      choices = filtered_subjid(),
      selected = input$subjid_filter[input$subjid_filter %in% filtered_subjid()] # Keep selected only those that are in the filtered list
    )
  })
  
  # Observe the RESET button click and reset all filters to their default values
  observeEvent(input$reset_filters, {
    updateTextInput(session, "subjid_search", value = "")
    updateCheckboxGroupInput(session, "subjid_filter", choices = unique(data$SUBJID), selected = unique(data$SUBJID))
    updateCheckboxGroupInput(session, "visit_filter", choices = unique(data$VISIT), selected = unique(data$VISIT))
    updateCheckboxGroupInput(session, "pctpt_filter", choices = unique(data$PCTPT), selected = unique(data$PCTPT))
  })
  
  # Filtered dataset based on checkbox input
  filtered_data <- reactive({
    data %>%
      filter(
        SUBJID %in% input$subjid_filter,
        VISIT %in% input$visit_filter,
        PCTPT %in% input$pctpt_filter
      )
  })
  
  # Create the ggplot object based on filtered data
  output$interactive_plot <- renderPlotly({
    filtered <- filtered_data()
    
    if (nrow(filtered) == 0) {
      return(NULL)
    }
    
    ggplot_obj <- ggplot(filtered, aes(x = PCTPT_factor, y = PCORRES_numeric, color = as.factor(SUBJID), group = visit_id, linetype = VISIT)) +
      geom_line(linewidth = 1) +  # Updated for compatibility with ggplot2 v3.4.0
      geom_point(size = 3) +
      scale_y_log10() +  # Log scale for y-axis
      scale_color_viridis(discrete = TRUE) +  # Use a different color palette for SUBJID (discrete scale)
      labs(
        x = "Time (Hours)",
        y = "Concentration (ng/mL)",
        title = "Concentration vs Time by Subject and Visit",
        color = "Subject ID",
        linetype = "Visit"
      ) +
      theme_bw() +
      theme(
        plot.title = element_text(hjust = 0.5)
      )
    
    # Convert ggplot object to plotly and register the click event
    p <- ggplotly(ggplot_obj, source = "select") %>%
      layout(dragmode = "select")
    event_register(p, "plotly_click")  # Register plotly click event
    
    p
  })
  
  # Initially render an empty data table
  output$data_table <- renderDT({
    datatable(
      data.frame(Message = "Please select a point on the plot to see details."),
      options = list(pageLength = 5, scrollX = TRUE, scrollY = "300px"),
      style = "bootstrap"
    )
  })
  
  # Observe the click event on the plotly plot
  observeEvent(event_data("plotly_click", source = "select"), {
    selected_data <- event_data("plotly_click", source = "select")
    
    if (!is.null(selected_data)) {
      # Debug: Print the selected data to the console
      print("Selected Data:")
      print(selected_data)
      
      # Retrieve the actual label value from PCTPT_factor levels using the numeric index
      levels_list <- levels(data$PCTPT_factor)
      selected_x_value <- levels_list[selected_data$x]
      
      # Convert the selected y value back from log10 to the original scale
      selected_y_value <- 10^as.numeric(selected_data$y)
      
      # Log values being compared for further debugging
      print("Selected x value (from factor levels):")
      print(selected_x_value)
      print("Selected y value (in original scale):")
      print(selected_y_value)
      
      # Use a tolerance for floating-point comparison
      tolerance <- 1e-2  # Setting a small tolerance
      selected_rows <- filtered_data() %>%
        filter(
          abs(PCORRES_numeric - selected_y_value) < tolerance &
            as.character(PCTPT_factor) == selected_x_value
        )
      
      # Select only the desired columns to display in the data table
      selected_rows_to_display <- selected_rows %>%
        select(-PCORRES_numeric, -time_numeric, -PCTPT_factor, -visit_id)
      
      # Log filtered data to the console
      print("Filtered Data to Display:")
      print(selected_rows_to_display)
      
      # Render the selected row without the unwanted columns, with scrolling enabled
      if (nrow(selected_rows_to_display) > 0) {
        output$data_table <- renderDT({
          datatable(
            selected_rows_to_display,
            options = list(pageLength = 5, scrollX = TRUE, scrollY = "300px"),
            style = "bootstrap"
          )
        })
      } else {
        # Render a message indicating no match was found
        output$data_table <- renderDT({
          datatable(
            data.frame(Message = "No matching record found."),
            options = list(pageLength = 5, scrollX = TRUE, scrollY = "300px"),
            style = "bootstrap"
          )
        })
      }
    }
  })
}

# Run the Shiny app
shinyApp(ui, server)
