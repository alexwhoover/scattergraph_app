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
      fluidRow(
        column(width = 4, actionButton(inputId = "render_plot", label = "Render Plot")),
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
                 h2("Data Format"), 
                 p("The input csv file must include data for datetime, depth and velocity."), 
                 p("\n"), 
                 p("Datetime must be in the format YYYY-MM-DD HH:MM:SS, depth must be in millimeters, and velocity must be in meters per second."),
                 p("\n"),
                 h2("Current Error Log"),
                 p("1. Negative velocities are filtered out in the main plot."),
                 p("2. Data points are filtered if they exceed the calculated maximum capacity of the pipe. Surcharge with flow relief could theoretically go past this, seen with STC_403734_US1."),
                 h2("Suggestions for Improvement"),
                 p("1. Pipe height line has not yet been added."),
                 p("2. Weekly scattergraph should include Manning's Curves for reference."),
                 p("3. Weekly scattergraph should show filtered points in gray."),
                 p("4. Should add option to download data directly from FlowWorks or import via CSV."),
                 p("5. Add more date format options to CSV."),
                 p("6. Add more unit options to CSV."),
                 p("7. Choose which curve iso-Q lines are based on. Allow variation of percentages.")
              ),
        tabPanel("Data Preview", h2("Raw Data Preview"), fluidRow(tableOutput("data_preview")), h2("Formatted Data Preview"), fluidRow(tableOutput("data_preview_formatted")), h2("Summary Statistics"), fluidRow(tableOutput("summary_stats")), h2("Data Gaps"), fluidRow(tableOutput("data_gaps"))),
        tabPanel("Plot", plotlyOutput("scatterplot", height = "800px"), tableOutput("coefList")),
        tabPanel("Weekly Plot", plotlyOutput("weekly_scatterplot", height = "800px"))

      )
    )
  )
)
