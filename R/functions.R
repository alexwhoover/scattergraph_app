library(tidyverse)
library(testthat)

# COMMON FUNCTIONS ####

# Function for theta calculation
# Inputs: d = water depth in meters, D = pipe diameter in meters
# Output: the angle, theta, in radians
calculate_theta <- function(d, D) {
  if (any(is.na(d) | is.na(D))) {
    warning("Water depth (d) or pipe diameter (D) is NA. Returning NA.")
    NA
  } else if (any(d < 0 | D < 0)) {
    warning("Water depth (d) and pipe diameter (D) must be positive numbers. Returning NA.")
    NA
  } else if (any(d > D)) {
    warning("Water depth (d) must less than or equal to pipe diameter (D). Returning NA.")
    NA
  } else {
    2 * acos(1 - 2 * d / D)
  }
}

# Function for flow area calculation
# Inputs: theta = the angle in radians between a circular pipe center and the water surface chord, D = pipe diameter in meters
# Output: the wetted area in meters squared
calculate_area <- function(theta, D) {
  (D^2/8) * (theta - sin(theta))
}

# Function for wetted perimeter calculation
# Inputs: theta = the angle in radians between a circular pipe center and the water surface chord, D = pipe diameter in meters
# Outputs: the wetted perimeter in meters
calculate_P <- function(theta, D) {
  D * theta / 2
}

# Function for calculating R^2 value
# Inputs: A dataframe with columns SSE and SYY
# Output: An R^2 value
calculate_R2 <- function(df) {
  1 - sum(df$SSE) / sum(df$SYY)
}

# Function for computing necessary variables for linear regression with 0 intercept
# Inputs: A dataframe with columns R (hydraulic radius (m)), v (velocity (m/s))
# Output: The input dataframe with columns added for x, y, x * y, and x^2. This dataframe is then used to calculate the C coefficient.
compute_regression_vars <- function(df) {
  df %>%
    mutate(
      x = R ^ (2/3),
      y = v,
      xy = x * y,
      x2 = x^2
    )
}

# Function for computing error statistics
# Inputs: A dataframe from compute_stats() and a coefficient C
# Output: The input dataframe with columns added for v_pred, SSE, and SYY
compute_error <- function(df, C) {
  v_avg <- mean(df$v)
  df %>%
    mutate(
      v_pred = C * R^(2/3),
      SSE = (v_pred - v)^2,
      SYY = (v - v_avg)^2
    )
}

filter_formatted_data <- function(formatted_data, start_date, end_date, v_min, v_max, d_min, d_max, D) {
  formatted_data %>%
    filter(datetime >= start_date & datetime < end_date) %>%
    filter(v >= v_min & v <= v_max) %>%
    filter(d >= d_min/1000 & d <= d_max/1000 & d <= D/1000)
}

compute_hydraulics <- function(filtered_df, D) {
  filtered_df %>%
    mutate(
      theta = calculate_theta(d, D/1000),
      A = calculate_area(theta, D/1000),
      P = calculate_P(theta, D/1000),
      R = A/P,
    )
}

compute_hydraulics_SS <- function(filtered_df, D, d_dog) {
  filtered_df %>%
    mutate(
      d_e = ifelse(d - d_dog/1000 < 0, 0, d - d_dog/1000),
      theta = calculate_theta(d, D/1000),
      theta_e = calculate_theta(d_e, D/1000),
      A_e = calculate_area(theta_e, D/1000),
      P = calculate_P(theta, D/1000),
      R = A_e/P,
    )
}

# compute_regression_method <- function(filtered_df, D, method, d_dog = NULL, n = NULL, S = NULL) {
#   if (method == "DM") {
#     C <- (1/n)*(S/100)^(0.5)
#     filtered_df <- filtered_df %>%
#       compute_hydraulics(D) %>%
#       compute_regression_vars()
#   }
#   else if (method == "LC") {
#     filtered_df <- filtered_df %>%
#       compute_hydraulics(D) %>%
#       compute_regression_vars()
#     C = sum(filtered_df$xy)/sum(filtered_df$x2)
# 
#   }
#   else if (method == "SS") {
#     filtered_df <- filtered_df %>%
#       compute_hydraulics_SS(D, d_dog) %>%
#       compute_regression_vars()
#     C = sum(filtered_df$xy)/sum(filtered_df$x2)
# 
#   }
#   else {
#     stop("Invalid method for regression fitting.")
#   }
#   filtered_df <- filtered_df %>%
#     compute_error(C)
#   R2 = calculate_R2(filtered_df)
#   
#   list(C = C, R2 = R2)
# }

compute_regression_method <- function(filtered_df, D, method, d_dog = NULL, n = NULL, S = NULL) {
  
  # Common preprocessing
  if (method %in% c("DM", "LC")) {
    filtered_df <- filtered_df %>%
      compute_hydraulics(D) %>%
      compute_regression_vars()
  } else if (method == "SS") {
    filtered_df <- filtered_df %>%
      compute_hydraulics_SS(D, d_dog) %>%
      compute_regression_vars()
  } else {
    stop("Invalid method for regression fitting.")
  }
  
  # Coefficient calculation
  if (method == "DM") {
    if (is.null(n) || is.null(S))
      stop("n and S must be provided for the 'DM' method.")
    C <- (1/n) * (S/100)^(0.5)
  } else {
    C <- sum(filtered_df$xy) / sum(filtered_df$x2)
  }
  
  # Compute errors and R²
  filtered_df <- filtered_df %>%
    compute_error(C)
  R2 <- calculate_R2(filtered_df)
  
  list(C = C, R2 = R2)
}

# Function to identify consecutive periods. A period of consecutive data is defined as consecutive data with no data gaps greater than 2 days. Once a data gap of at least 2 days is found, a new period is started.
# Inputs: A timeseries dataframe with a column labelled "datetime"
# Output: A dataframe with columns period_id, start, end, and count.
get_consecutive_periods <- function(df) {
  if ("datetime" %in% names(df) &
      inherits(df$datetime, "POSIXct") &
      length(df$datetime > 0)) {
    df_export <- df %>%
      # Arrange data by datetime
      arrange(datetime) %>%
      
      # Calculate the interval between consecutive datetime entries
      mutate(interval = difftime(datetime, lag(datetime, default = first(datetime)), units = "mins"),
             
             # Identify new periods where the interval is greater than 2 days
             new_period = ifelse(interval > 2 * 1440, 1, 0),
             
             # Create a unique period ID by cumulatively summing the new period flags
             period_id = cumsum(new_period) + 1) %>%
      
      # Group data by period ID and summarize each period
      group_by(period_id) %>%
      summarise(start = datetime %>%
                  min() %>%
                  format("%Y-%m-%d %H:%M"),   # Start time of the period
                end = datetime %>%
                  max() %>%
                  format("%Y-%m-%d %H:%M"),     # End time of the period
                count = n(),             # Number of timestamps in the period
                .groups = 'drop')
    
    return(df_export)
  } else {
    return(NULL)
  }
  
  
}

