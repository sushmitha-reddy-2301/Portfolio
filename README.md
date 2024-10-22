server <- function(input, output, session) {
  
  # Stop the app when the session ends
  session$onSessionEnded(function() {
    stopApp()
  })
  
  # Render the user profile
  output$user <- renderUser({
    dashboardUser(
      name = "SDAA Dashboard",
      image = "https://s28.q4cdn.com/781576035/files/images/PFE-ONC-Lockup_KO.png",
      subtitle = "Author - Sushmitha"
    )
  })
  
  # Call the Data Load Module and store the reactive data
  uploadedData <- data_load_module("upload_module")

  
  # Observe the reactive value to ensure it is working correctly
  observe({
    data <- uploadedData$uploadedData1()
  # Properly access the reactive value
    if (!is.null(data)) {
      print("Reactive Data:")
      print(head(data, 10))
    } else {
      print("No data uploaded yet.")
    }
  })
  
  # Pass the reactive data to the Data Insights modules
  Data_Insights_module("insights_module", uploadedData$uploadedData1)
  Data_Insights_module_2("insights_module_2", uploadedData$uploadedData1,uploadedData$uploadedData2 )
  Data_Insights_module_3("insights_module_3", uploadedData$uploadedData1)
  Data_Insights_module_4("insights_module_4", uploadedData$uploadedData1)
  Data_Insights_module_5("insights_module_5", uploadedData$uploadedData1)
}
