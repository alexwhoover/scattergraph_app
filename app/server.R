# Load necessary libraries
library(shiny)
library(plotly)
library(tidyverse)
library(openxlsx)

# Load mathematical functions for calculating partial flow in circular pipe
source("../R/functions.R")

options(shiny.maxRequestSize=30*1024^2)

server <- function(input, output, session) {
  lapply(c("start_date", "end_date", "plot_title", "v_min", "v_max", "d_min", "d_max", "n", "D", "S", "d_dog", "render_plot", "save_report", "save_scattergraph"), disable)
  
  # Import Data ####
  data <- reactiveValues(raw = NULL, formatted = NULL, formatted_q = NULL)
  
  # Reactive Dependencies: data_file and n_skip
  observeEvent({
    input$data_file
    input$n_skip
  }, {
    if(!is.null(input$data_file)) {
      tryCatch({
        # Read uploaded csv file    
        data$raw <- read_csv(input$data_file$datapath, skip = input$n_skip)
        
        # Update dropdowns with column headers from csv file
        updateSelectInput(session, "t_header", choices = names(data$raw))
        updateSelectInput(session, "d_header", choices = names(data$raw))
        updateSelectInput(session, "v_header", choices = names(data$raw))
        updateSelectInput(session, "q_header", choices = c("Not Applicable", names(data$raw)))
        
        # Preview Data
        output$data_preview <- renderTable(head(data$raw))
        output$summary_stats <- renderTable(
          data$raw %>%
            summary() %>%
            as.data.frame() %>%
            separate(Freq, c("Stat", "Value"), sep = ":") %>%
            filter(!is.na(Stat)) %>%
            select("Column" = "Var2", "Stat", "Value") %>%
            pivot_wider(names_from = Stat, values_from = Value)
        )
        
        showNotification("Data successfully read")
      }, 
      
      error = function(e) {
        showNotification("Error reading the CSV file.", type = "error")
        data$raw <- NULL
      })
    }
  })
  ############################################################
  
  # Format Data ####
  
  # Reactive dependencies: t_header, d_header, v_header
  observeEvent({
    input$t_header
    input$d_header
    input$v_header
    input$q_header
  }, {
    req(data$raw)
    
    tryCatch({
      data$formatted <- data$raw %>%
        select('datetime' = input$t_header, 'd' = input$d_header, 'v' = input$v_header) %>%
        mutate(
          datetime = as.POSIXct(datetime, tz = "UTC"),
          d = d / 1000
        )
      
      if(!is.null(input$q_header) & input$q_header != "Not Applicable") {
        print("yes")
        data$formatted_q <- data$raw %>%
          select('datetime' = input$t_header, 'd' = input$d_header, 'v' = input$v_header, 'q' = input$q_header) %>%
          mutate(
            datetime = as.POSIXct(datetime, tz = "UTC")
          )
        
        print(data$formatted_q)
      }
      
      
      output$data_preview_formatted <- renderTable(head(data$formatted %>% mutate(datetime = format(datetime, format = "%Y-%m-%d %H:%M"), d = d * 1000)))
      print("data formatted")
    }, error = function(e) {
      print("data not formatted")
    })
    
  })
  
  # Reactive dependencies: lock_in
  observeEvent(input$lock_in, {
    lapply(c("n_skip", "t_header", "d_header", "v_header", "q_header"), disable)
    lapply(c("start_date", "end_date", "plot_title", "v_min", "v_max", "d_min", "d_max", "n", "D", "S", "d_dog", "render_plot"), enable)
    
    min_date <- data$formatted %>%
      summarize(min_date = min(datetime, na.rm = TRUE)) %>%
      pull()
    
    max_date <- data$formatted %>%
      summarize(max_date = max(datetime, na.rm = TRUE)) %>%
      pull()
    
    data_gaps <- get_consecutive_periods(data$formatted)
    output$data_gaps <- renderTable(data_gaps)
    
    updateDateInput(session, "start_date", value = as.Date(min_date))
    updateDateInput(session, "end_date", value = as.Date(max_date))
    
  })
  
  observeEvent(input$render_plot, {
    enable("save_report")
    enable("save_scattergraph")
  })
  
  #########################################################
  
  # Calculations ####
  
  # Coefficient Calculation ----
  # Reactive function that returns a vector of coefficients and R^2 values
  # in format v_coef = list(C_DM, R2_DM, C_LC, R2_LC, C_SS, R2_SS)
  # Filter data
  v_coef <- eventReactive(input$render_plot, {
    req(data$formatted)
    df <- data$formatted %>%
      filter(datetime >= input$start_date & datetime < input$end_date) %>%
      filter(v >= input$v_min & v <= input$v_max) %>% # Filter by velocity range
      filter(d >= input$d_min/1000 & d <= input$d_max/1000) # Filter by depth range
    
    # Design Method ====
    # Calculate coefficient based on user input values
    C_DM <- (1/input$n)*(input$S/100)^(0.5)
    
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
  
  # Wide Dataframe Calculation ----
  df_curves <- eventReactive(input$render_plot, {
    
    # Calculate coefficients and R^2 values
    v_coef <- v_coef()
    
    # Output v_coef to table
    output$coefList <- renderTable({
      req(data$formatted)
      
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
  
  ########################################################
  
  # Plotting ####
  
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
  
  # Reformat data for plotting
  data_formatted <- eventReactive(input$render_plot, {
    req(data$formatted)
    data_raw <- data$formatted %>%
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
  
  # Reformat data for weekly plotting
  # data_formatted_weekly <- eventReactive(input$render_plot, {
  #   req(data$formatted)
  #   data_raw <- data$formatted %>%
  #     filter(datetime >= input$start_date & input$end_date) %>%
  #     mutate(grouped_time = floor_date(datetime, "week"))
  #   
  #   data_raw %>%
  #     mutate(
  #       d_mm = d * 1000,
  #       Legend = ifelse(
  #         v >= input$v_min & v <= input$v_max & d_mm >= input$d_min & d_mm <= input$d_max,
  #         "Observed",
  #         "Discarded"
  #       )
  #     )
  # })
  
  # Send plot to output
  
  output$scatterplot <- renderPlotly({
    df_curves_long <- df_curves_long()
    df_data_filtered <- data_formatted()
    
    # Calculate x bound for plot visual
    max_x_curves <- df_curves_long %>%
      filter(Legend %in% c("Design Method", "Lanfear-Coll Method", "Stevens-Schutzback Method")) %>%
      summarize(max_x = max(v, na.rm = TRUE)) %>%
      pull()
    
    max_x_data <- df_data_filtered %>%
      summarize(max_v = max(v, na.rm = TRUE)) %>%
      pull()
    
    max_x = max(c(max_x_curves, max_x_data), na.rm = TRUE) * 1.2
    
    # Plot
    ggplotly(
      # Create ggplot object
      ggplot() +
        geom_path(data = df_curves_long, aes(x = v, y = d_mm, color = Legend)) + # Add all curves
        geom_point(data = df_data_filtered[df_data_filtered$Legend == "Observed", ], aes(x = v, y = d_mm, label = datetime), color = "green4", size = 0.5) +
        geom_point(data = df_data_filtered[df_data_filtered$Legend == "Discarded", ], aes(x = v, y = d_mm, label = datetime), color = "gray", size = 0.5) + 
        xlim(0, max_x) +
        labs(x = "Velocity (m/s)", y = "Depth (mm)", title = input$plot_title) +
        theme_minimal(),
      
      dynamicTicks = "y"
    )
  })
  
  output$weekly_scatterplot <- renderPlotly({
    df_curves_long <- df_curves_long()
    df_data_filtered <- data_formatted() %>%
      mutate(grouped_time = floor_date(datetime, "week"))
    
    # Calculate x bound for plot visual
    max_x <- df_curves_long %>%
      filter(Legend %in% c("Design Method", "Lanfear-Coll Method", "Stevens-Schutzback Method")) %>%
      summarize(max_x = max(v, na.rm = TRUE)) %>%
      pull()
    
    # Plot
    df_data_filtered %>%
      plot_ly(x = ~v, y = ~d_mm, frame = ~grouped_time, type = "scatter", mode = "markers", name = "Scatter") %>%
      layout(
        title = input$plot_title,
        xaxis = list(title = "Velocity (m/s)"),
        yaxis = list(title = "Depth (mm)")
      ) %>%
      animation_opts(transition = 0, frame = 1000, redraw = TRUE)
  })
  
  ########################################################
  
  # Report Generation ####
  output$save_report <- downloadHandler(
    filename = function () {
      paste0("exported_report_", format(Sys.time(), format = "%Y%m%d_%H%M"), ".xlsx")
    },
    content = function(file) {
      # Open workbook template
      tryCatch({
        wb <- loadWorkbook("../data/excel_templates/summary_template.xlsx")
        print("Template loaded")
      }, error = function(e) {
        print("Error loading template")
      })
      
  
      # Paste formatted data into "Data" sheet
      data <- data$formatted_q %>%
        select("Timestamp" = datetime, "Depth (mm)" = d, "Velocity (m/s)" = v, "Flowrate (L/s)" = q)

      writeDataTable(wb, sheet = "Data", x = data, startCol = 1, startRow = 1, tableName = "data")

      curves <- df_curves() %>%
        mutate(d_mm = d*1000) %>%
        select(d_percent, d, d_mm, everything())
      writeDataTable(wb, sheet = "Scattergraph_Data", x = curves, startCol = 1, startRow = 1, tableName = "curves")

      # Write information to "Summary" sheet
      writeData(wb, sheet = "Summary", x = as.character(Sys.Date()), startCol = 2, startRow = 3)
      writeData(wb, sheet = "Summary", x = input$D, startCol = 4, startRow = 1)
      writeData(wb, sheet = "Summary", x = input$S, startCol = 4, startRow = 2)
      writeData(wb, sheet = "Summary", x = as.character(input$start_date), startCol = 2, startRow = 4)
      writeData(wb, sheet = "Summary", x = as.character(input$end_date), startCol = 2, startRow = 5)
  
      saveWorkbook(wb, file, overwrite = TRUE)
  })
  
  output$save_scattergraph <- downloadHandler(
    filename = function () {
      paste0("exported_scattergraph_", format(Sys.time(), format = "%Y%m%d_%H%M"), ".xlsx")
    },
    content = function(file) {
      # Open workbook template
      tryCatch({
        wb <- loadWorkbook("../data/excel_templates/scattergraph_template.xlsx")
        print("Template loaded")
      }, error = function(e) {
        print("Error loading template")
      })
      
      
      # Paste formatted data into "Data Input" sheet
      data <- data$formatted_q %>%
        select("Timestamp" = datetime, "Depth (mm)" = d, "Velocity (m/s)" = v)
      
      writeData(wb, sheet = "Data Input", x = data, startCol = 7, startRow = 3, colNames = FALSE)
      
      
      # Write information to "Data Input" sheet
      writeData(wb, sheet = "Data Input", x = input$D / 1000, startCol = 3, startRow = 11)
      writeData(wb, sheet = "Data Input", x = input$d_dog / 1000, startCol = 3, startRow = 12)
      writeData(wb, sheet = "Data Input", x = input$S, startCol = 2, startRow = 17)
      
      saveWorkbook(wb, file, overwrite = TRUE)
    })
  
  

}