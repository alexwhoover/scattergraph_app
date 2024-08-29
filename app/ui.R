# Load necessary libraries
library(shiny)
library(shinyjs)
library(plotly)

# Define UI for application
ui <- fluidPage(
  useShinyjs(),
  
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
      selectInput("t_header", "Timestamp Column Name:", choices = NULL),
      selectInput("d_header", "Depth Column Name:", choices = NULL),
      selectInput("v_header", "Velocity Column Name:", choices = NULL),
      
      # Button: Lock-in Data
      actionButton(inputId = "lock_in", label = "Confirm Data"),
      p('\n'),
      
      # Input: Date Range
      dateInput("start_date", "Start Date:"),
      dateInput("end_date", "End Date:"),
      
      # Input: Plot Title
      textInput("plot_title", "Plot Title:", value = ""),
      
      # Input: Velocity Value Range
      numericInput("v_min", "Minimum Velocity (m/s)", value = 0),
      numericInput("v_max", "Maximum Velocity (m/s)", value = 100),
      
      # Input: Depth Value Range
      numericInput("d_min", "Minimum Depth (mm)", value = 0),
      numericInput("d_max", "Maximum Depth (mm)", value = 10000),
      
      # Input: Manning's N
      numericInput("n", "Manning's N: ", value = 0.012),
      
      # Input: Pipe Diameter
      numericInput("D", "Pipe Diameter (mm): ", value = 600),
      
      # Input: Pipe Slope
      numericInput("S", "Pipe Slope (%): ", value = 1.0),
      
      # Input: Dead Dog
      numericInput("d_dog", "Dead Dog (mm): ", value = 0),
      
      # Button: Render Plot
      actionButton(inputId = "render_plot", label = "Render Plot")
    ),
    
    # Main panel for displaying outputs
    mainPanel(
      tabsetPanel(
        tabPanel("Data Preview", h2("Raw Data Preview"), fluidRow(tableOutput("data_preview")), h2("Formatted Data Preview"), fluidRow(tableOutput("data_preview_formatted")), h2("Summary Statistics"), fluidRow(tableOutput("summary_stats")), h2("Data Gaps"), fluidRow(tableOutput("data_gaps"))),
        tabPanel("Plot", plotlyOutput("scatterplot", height = "800px"), tableOutput("coefList"))

      )
    )
  )
)