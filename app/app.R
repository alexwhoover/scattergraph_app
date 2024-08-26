# Load necessary libraries
library(shiny)

# Source the UI and server components
source("server.R")
source("ui.R")

# Run the application
shinyApp(ui = ui, server = server)
