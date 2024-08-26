# Load necessary libraries
library(shiny)
library(plotly)

# Define UI for application
ui <- fluidPage(
  
  # Application title
  titlePanel("Interactive Scatter Plot Generator"),
  
  # Sidebar layout with input and output definitions
  sidebarLayout(
    sidebarPanel(
      width = 3,
      # Input: Data CSV File
      fileInput("data_file", "Upload CSV File:",
                multiple = FALSE,
                accept = c("text/csv",
                           "text/comma-separated-values,text/plain",
                           ".csv")),
      
      # Input: Header Names
      numericInput("n_skip", "# Rows to Skip:", value = 2),
      textInput("t_header", "Timestamp Column Name:", value = "yyyy/MM/dd hh:mm:ss"),
      textInput("d_header", "Depth Column Name:", value = "DFINAL (mm)"),
      textInput("v_header", "Velocity Column Name:", value = "VFINAL (m/s)"),
      
      # Input: Plot Title
      textInput("plot_title", "Plot Title:", value = ""),
      
      # Input: Date Range
      dateInput("start_date", "Start Date:", value = as.Date("2020-01-01")),
      dateInput("end_date", "End Date:", value = as.Date("2024-06-26")),
      
      # Output: Table to display min/max dates
      tableOutput("file_contents"),
      
      # Input: Velocity Value Range
      # sliderInput("v_min", "Minimum Velocity (m/s):", min = 0, max = 10, value = 0, step = 0.1),
      # sliderInput("v_max", "Maximum Velocity (m/s):", min = 0, max = 10, value = 10, step = 0.1),
      numericInput("v_min", "Minimum Velocity (m/s)", value = 0),
      numericInput("v_max", "Maximum Velocity (m/s)", value = 100),
      
      # Input: Depth Value Range
      # sliderInput("d_min", "Minimum Depth (m):", min = 0, max = 2, value = 0, step = 0.05),
      # sliderInput("d_max", "Maximum Depth (m):", min = 0, max = 2, value = 2, step = 0.05),
      numericInput("d_min", "Minimum Depth (mm)", value = 0),
      numericInput("d_max", "Maximum Depth (mm)", value = 10000),
      
      # Input: Manning's N
      numericInput("n", "Manning's N: ", value = 0.012),
      
      # Input: Pipe Diameter
      numericInput("D", "Pipe Diameter (mm): ", value = 600),
      
      # Input: Pipe Slope
      numericInput("S", "Pipe Slope (m/m): ", value = 0.01),
      
      # Input: Dead Dog
      numericInput("d_dog", "Dead Dog (mm): ", value = 0),
      
      # Button: Render Plot
      actionButton(inputId = "render_plot", label = "Render Plot"),
      p('\n'),
      
      # Button: Render Data Gaps Table
      actionButton(inputId = "render_gaps_table", label = "Show Data Gaps"),
      p('\n'),
      
      # Data Download
      downloadButton("downloadData", "Download Filtered Data"),
      
      # Data Download
      downloadButton("downloadCurves", "Download Calculated Curves")
    ),
    
    # Main panel for displaying outputs
    mainPanel(
      # Output: Plot
      plotlyOutput("scatterplot", height = "800px"),
      
      # Table to display stats
      tableOutput("coefList"),
      
      # Consecutive Data Periods
      tableOutput("data_table"),
      
      # Corresponding Max Storms
      tableOutput("rain_table")
    )
  )
)