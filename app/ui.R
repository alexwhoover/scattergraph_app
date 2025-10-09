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
      selectInput("q_header", "Flow Column Name (If Generating Report)", choices = NULL),
      
      # Button: Lock-in Data
      actionButton(inputId = "lock_in", label = "Confirm Data"),
      p('\n'),
      
      # Input: Date Range
      dateInput("start_date", "Start Date:"),
      dateInput("end_date", "End Date:"),
      
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
      fluidRow(
        column(width = 4, actionButton(inputId = "render_plot", label = "Render Plot"))
      ),
      
      p('\n'),
      
      fluidRow(
        column(width = 4, downloadButton("save_report", "Generate Summary Excel"))
      ),
      
      p('\n'),
      
      fluidRow(
        column(width = 4, downloadButton("save_scattergraph", "Generate Scattergraph Excel"))
      )
      
    ),
    
    # Main panel for displaying outputs
    mainPanel(
      tabsetPanel(
        tabPanel("Instructions", 
                 h3("Step 1:"),
                 p("Upload a CSV containing sewer flow data."),
                 p("The input csv file must include data for datetime, depth and velocity."), 
                 p("Datetime must be in the format YYYY-MM-DD HH:MM:SS, depth must be in millimeters, and velocity must be in meters per second."),
                 h3("Step 2:"),
                 p("Select the column names in the CSV which correspond to depth, velocity, and flow."),
                 h3("Step 3:"),
                 p("Click \'Confirm Data\'"),
                 h3("Step 4:"),
                 p("Navigate to the \'Data Preview\' tab and verify data has uploaded correctly."),
                 h3("Step 5:"),
                 p("Fill out the all further plotting parameters."),
                 h3("Step 6:"),
                 p("Click \'Render Plot\' to calculate regression curves and plot scattergraph."),
                 p("Navigate to the \'Plot\' and \'Weekly Plot\' tabs to view plots."),
                 h3("Optional:"),
                 p("Generate legacy format Excel spreadsheets with uploaded data for archiving purposes.")
              ),
        tabPanel("Data Preview", h2("Raw Data Preview"), fluidRow(tableOutput("data_preview")), h2("Formatted Data Preview"), fluidRow(tableOutput("data_preview_formatted")), h2("Summary Statistics"), fluidRow(tableOutput("summary_stats")), h2("Data Gaps"), fluidRow(tableOutput("data_gaps"))),
        tabPanel("Plot", br(), br(), plotlyOutput("scatterplot", height = "800px"), tableOutput("coefList")),
        tabPanel("Weekly Plot", br(), br(), plotlyOutput("weekly_scatterplot", height = "800px"))

      )
    )
  )
)
