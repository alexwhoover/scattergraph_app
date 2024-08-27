# Load necessary libraries
library(shiny)
library(plotly)
library(tidyverse)
library(lubridate)
library(zoo)

# Load mathematical functions for calculating partial flow in circular pipe
source("functions.R")

options(shiny.maxRequestSize=30*1024^2)

# Define server logic ####
server <- function(input, output) {
  
  # Get data from user input ----
  data <- reactive({
    
    # Check that file exists
    req(input$data_file)
    
    # Rename to in_file
    in_file <- input$data_file
    
    # Try to read csv. If error, print notification.
    tryCatch({
      df <- read_csv(in_file$datapath, skip = input$n_skip) %>%
        select('datetime' = input$t_header, 'd' = input$d_header, 'v' = input$v_header) %>%
        
        # Convert depth to mm to make calculations easier. It is converted back during final plotting.
        mutate(d = d / 1000)
      
      # Print message to console for debugging purposes
      showNotification("Data successfully read")

      # Return the filtered dataframe
      df
    }, error = function(e) {
      # Error message if failed to read csv file.
      showNotification("Error reading the CSV file. Please ensure it is properly formatted.", type = "error")
      NULL
    })
  })
  #######################################
  
  # Display min and max dates in imported CSV ----
  # Do not filter data
  output$file_contents <- renderTable({
    # Check that data exists
    req(data())
    
    # Import data
    df <- data()
    
    tryCatch({
      df <- df %>%
        arrange(datetime) %>% # Sort by datetime
        filter(row_number() %in% c(1, n())) %>% # Keep first and last row of dataframe
        mutate(datetime = format(as.Date(datetime, origin = 1900-01-01), "%Y-%m-%d")) %>% # Convert datetime to only date
        select("Min / Max Dates" = "datetime") # Rename column header
      
      df
    }, error = function(e) {
      showNotification("Error printing max and min dates.", type = "error")
      NULL
    })
  })
  #######################################
  
  # Get Plot Title
  plot_title <- eventReactive(input$render_plot, {
    input$plot_title
  })
  
  # Coefficient Calculation ----
  # Reactive function that returns a vector of coefficients and R^2 values
  # in format v_coef = list(C_DM, R2_DM, C_LC, R2_LC, C_SS, R2_SS)
  # Filter data
  v_coef <- eventReactive(input$render_plot, {
    req(data())
    df <- data() %>%
      filter(datetime >= input$start_date & datetime < input$end_date) %>%
      filter(v >= input$v_min & v <= input$v_max) %>% # Filter by velocity range
      filter(d >= input$d_min/1000 & d <= input$d_max/1000) # Filter by depth range
    
    # Design Method ====
    # Calculate coefficient based on user input values
    C_DM <- (1/input$n)*input$S^(0.5)
    
    # Compute stats for R^2 calculation
    stats_DM = df %>% select(v, d) %>%
      filter(d <= input$D/1000) %>%
      mutate(
        theta = calculate_theta(d, input$D/1000),
        A = calculate_area(theta, input$D/1000),
        P = calculate_P(theta, input$D/1000),
        R = A/P,
      ) %>%
      compute_stats(.) %>%
      compute_error(., C_DM)
    
    # Calculate R^2
    R2_DM = calculate_R2(stats_DM)
    
    # Lanfear-Coll Method ====
    
    # Compute stats for linear regression with 0 intercept
    stats_LC = df %>% select(v, d) %>%
      filter(d <= input$D/1000) %>%
      mutate(
        theta = calculate_theta(d, input$D/1000),
        A = calculate_area(theta, input$D/1000),
        P = calculate_P(theta, input$D/1000),
        R = A/P,
      ) %>%
      compute_stats(.)
    
    # Linear regression to calculate C
    C_LC = sum(stats_LC$xy)/sum(stats_LC$x2)
    
    # Compute error stats for R^2 calculation
    stats_LC <- stats_LC %>%
      compute_error(., C_LC)
    
    # Calculate R^2
    R2_LC = calculate_R2(stats_LC)
    
    # Stevens-Schutzack Method ====
    
    # Compute stats for linear regression with 0 intercept
    stats_SS = df %>% select(v, d) %>%
      filter(d <= input$D/1000) %>%
      mutate(
        d_e = ifelse(d - input$d_dog/1000 < 0, 0, d - input$d_dog/1000),
        theta = calculate_theta(d, input$D/1000),
        theta_e = calculate_theta(d_e, input$D/1000),
        A_e = calculate_area(theta_e, input$D/1000),
        P = calculate_P(theta, input$D/1000),
        R = A_e/P,
      ) %>%
      compute_stats(.)
    
    # Linear regression to calculate C
    C_SS = sum(stats_SS$xy)/sum(stats_SS$x2)
    
    # Compute error stats for R^2 calculation
    stats_SS <- stats_SS %>%
      compute_error(., C_SS)
    
    # Compute R^2
    R2_SS = calculate_R2(stats_SS)
    
    # Return calculated coefficients and R^2 values in named list
    list("C_DM" = C_DM, "R2_DM" = R2_DM,
         "C_LC" = C_LC, "R2_LC" = R2_LC,
         "C_SS" = C_SS, "R2_SS" = R2_SS
         )
    
  })
  ##################################################
  
  
  # Wide Dataframe Calculation ----
  df_curves <- eventReactive(input$render_plot, {
    
    # Calculate coefficients and R^2 values
    v_coef <- v_coef()
    
    # Output v_coef to table
    output$coefList <- renderTable({
      req(data())
      
      v_coef
    })
    
    # Calculate predicted velocities from 3 methods
    df_curves <- data_frame(
      d_percent = seq(from = 0.001, to = 1, by = 0.001),
      d = input$D/1000 * d_percent,
      d_e = ifelse(d - input$d_dog/1000 < 0, 0, d - input$d_dog/1000),
      theta = calculate_theta(d, input$D/1000),
      theta_e = calculate_theta(d_e, input$D/1000),
      A = calculate_area(theta, input$D/1000),
      A_e = calculate_area(theta_e, input$D/1000),
      P = calculate_P(theta, input$D/1000),
      R = A/P,
      R_e = A_e/P,
      v_DM = v_coef$C_DM * R ^ (2/3),
      v_LC = v_coef$C_LC * R ^ (2/3),
      v_SS = v_coef$C_SS * R_e ^ (2/3)
    ) %>%
      
      # Iso-Froude Lines
      mutate(
        B = input$D/1000 * sin(theta/2),
        d_h = A/B,
        v_07 = 0.7*sqrt(9.81*d_h),
        v_1 = 1*sqrt(9.81*d_h),
        v_15 = 1.5*sqrt(9.81*d_h)
      ) %>%
      
      # Iso-Q Lines
      mutate(
        Q = v_SS*A_e,
        iso_2 = max(Q) * 0.02 / A_e,
        iso_5 = max(Q) * 0.05 / A_e,
        iso_10 = max(Q) * 0.1 / A_e,
        iso_15 = max(Q) * 0.15 / A_e,
        iso_20 = max(Q) * 0.2 / A_e,
        iso_25 = max(Q) * 0.25 / A_e
      )
    
    # Return plotting dataframe
    df_curves
  })
  
  # Conversion from Wide Dataframe to Long Dataframe ----
  df_curves_long <- reactive({
    req(df_curves())
    df_curves <- df_curves()
    
    Q_max = max(df_curves$Q)
    
    # Define the order for plot legend
    legend_order <- c("v_DM", "v_LC", "v_SS", "iso_2", "iso_5", "iso_10", "iso_15", "iso_20", "iso_25", "v_07", "v_1", "v_15")
    legend_labels <- c("Design Method", "Lanfear-Coll Method", "Stevens-Schutzback Method",
                       paste0('2% iso-Q = ', as.character(round(Q_max * 0.02, 3)), ' cms'),
                       paste0('5% iso-Q = ', as.character(round(Q_max * 0.05, 3)), ' cms'),
                       paste0('10% iso-Q = ', as.character(round(Q_max * 0.1, 3)), ' cms'),
                       paste0('15% iso-Q = ', as.character(round(Q_max * 0.15, 3)), ' cms'),
                       paste0('20% iso-Q = ', as.character(round(Q_max * 0.20, 3)), ' cms'),
                       paste0('25% iso-Q = ', as.character(round(Q_max * 0.25, 3)), ' cms'),
                       "Fr = 0.7",
                       "Fr = 1.0",
                       "Fr = 1.5"
    )

    # Convert dataframe to format compatible with ggplot
    df_curves_long <- df_curves %>%
      select(d, all_of(legend_order)) %>%
      pivot_longer(cols = all_of(legend_order), names_to = "Legend", values_to = "v") %>%
      mutate(Legend = factor(Legend, levels = legend_order, labels = legend_labels)) %>%
      filter(v != Inf, v > 0) %>%
      mutate(d_mm = d * 1000) %>%
      select(v, d, d_mm, Legend) %>%
      arrange(Legend, d)
    
    # Return dataframe with columns: v, d, d_mm, Legend
    df_curves_long
  })
  
  # Reformat data for plotting ----
  data_formatted <- eventReactive(input$render_plot, {
    req(data())
    data_raw <- data() %>%
      filter(datetime >= input$start_date & datetime < input$end_date)
    
    data_raw %>%
      mutate(
        d_mm = d * 1000,
        Legend = ifelse(
          v >= input$v_min & v <= input$v_max & d_mm >= input$d_min & d_mm <= input$d_max,
          "Observed",
          "Discarded"
        )
      )
    
  })
  
  
  # Plotting ----
  output$scatterplot <- renderPlotly({
    df_curves_long <- df_curves_long()
    df_data_filtered <- data_formatted()
    
    # Calculate x bound for plot visual
    max_x <- df_curves_long %>%
      filter(Legend %in% c("Design Method", "Lanfear-Coll Method", "Stevens-Schutzback Method")) %>%
      summarize(max_x = max(v, na.rm = TRUE)) %>%
      pull()
    
    # Plot
    ggplotly(
      # Create ggplot object
      ggplot() +
        geom_path(data = df_curves_long, aes(x = v, y = d_mm, color = Legend)) + # Add all curves
        geom_point(data = df_data_filtered[df_data_filtered$Legend == "Observed", ], aes(x = v, y = d_mm, label = datetime), color = "green4", size = 0.5) +
        geom_point(data = df_data_filtered[df_data_filtered$Legend == "Discarded", ], aes(x = v, y = d_mm, label = datetime), color = "gray", size = 0.5) + 
        xlim(0, max_x) +
        labs(x = "Velocity (m/s)", y = "Depth (mm)", title = plot_title()) +
        theme_minimal(),
      
      dynamicTicks = "y"
    )
  })

  # Summarize Consecutive Data in Output Table "data_table" ----
  summary_table <- eventReactive(input$render_gaps_table, {
    req(data())
    df <- data()
    summary_table <- get_consecutive_periods(df)
    summary_table
  })
  
  output$data_table <- renderTable({
    summary_table()
  })

}