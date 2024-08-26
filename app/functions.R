library(tidyverse)

# Common Functions ####

# Function for theta calculation
# Inputs: d = water depth in meters, D = pipe diameter in meters
# Output: the angle, theta, in radians
calculate_theta <- function(d, D) {
  tryCatch({
    2 * acos(1 - 2 * d / D)
  },
  warning = function(cond) {
    print("Theta could not be calculated due to depth above pipe diameter")
    0
  })
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
compute_stats <- function(df) {
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

# Function to identify consecutive periods. A period of consecutive data is defined as consecutive data with no data gaps greater than 2 days. Once a data gap of at least 2 days is found, a new period is started.
# Inputs: A timeseries dataframe with a column labelled "datetime"
# Output: A dataframe with columns period_id, start, end, and count.
get_consecutive_periods <- function(df) {
  df <- df %>%
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
    summarise(start = as.character(min(datetime)),   # Start time of the period
              end = as.character(max(datetime)),     # End time of the period
              count = n(),             # Number of timestamps in the period
              .groups = 'drop') %>%
    
    # Arrange the summary table by start time
    arrange(start)
  
  return(df)
}

# Function to determine the timestamp and magnitude of maximum storm events for a range of durations.
# Inputs: 
# - df_rain: a dataframe with 5-minute rainfall data with columns names "datetime", "r_5min", "r_10min", ... "r_24hr". 
# -- The dataframe must have no missing 5-min intervals to correctly calculate rolling averages.
# - start_date: POSIXct datetime object signifying the start of the period of interest
# - end_date: POSIXct datetime object signifying the end of the period of interest
# Output: A dataframe summarizing timestamp and magnitude of maximum storm events.
get_max_rainfall <- function(df_rain, start_date, end_date) {
  if (start_date > end_date) {
    print("Start date is greater than end date.")
    return(NULL)
  } else {
    
    # Filter df_rain with start and end date. Convert datetime to character for lookup.
    df_rain <- df_rain %>%
      filter(datetime >= start_date & datetime < end_date) %>%
      mutate(datetime = as.character(datetime))
    
    # Calculate maximum storm events and corresponding timestamps
    summary_table <- df_rain %>%
      summarise(
        timestamp_5min = datetime[which.max(r_5min)],
        timestamp_10min = datetime[which.max(r_10min)],
        timestamp_15min = datetime[which.max(r_15min)],
        timestamp_30min = datetime[which.max(r_30min)],
        timestamp_1hr = datetime[which.max(r_1hr)],
        timestamp_2hr = datetime[which.max(r_2hr)],
        timestamp_6hr = datetime[which.max(r_6hr)],
        timestamp_12hr = datetime[which.max(r_12hr)],
        timestamp_24hr = datetime[which.max(r_24hr)],
        
        amount_5min = max(r_5min, na.rm = FALSE),
        amount_10min = max(r_10min, na.rm = FALSE),
        amount_15min = max(r_15min, na.rm = FALSE),
        amount_30min = max(r_30min, na.rm = FALSE),
        amount_1hr = max(r_1hr, na.rm = FALSE),
        amount_2hr = max(r_2hr, na.rm = FALSE),
        amount_6hr = max(r_6hr, na.rm = FALSE),
        amount_12hr = max(r_12hr, na.rm = FALSE),
        amount_24hr = max(r_24hr, na.rm = FALSE)
      )
    
    # Reformat table for visual display
    summary_table_formatted <- data.frame(
      "Duration" = c("Storm End Time", "Rainfall Amount (mm)", "Rainfall Intensity (mm/hr)"),
      "5min" = c(summary_table$timestamp_5min, summary_table$amount_5min, round(summary_table$amount_5min * 60/5, 2)),
      "10min" = c(summary_table$timestamp_10min, summary_table$amount_10min, round(summary_table$amount_10min * 60/10, 2)),
      "15min" = c(summary_table$timestamp_15min, summary_table$amount_15min, round(summary_table$amount_15min * 60/15, 2)),
      "30min" = c(summary_table$timestamp_30min, summary_table$amount_30min, round(summary_table$amount_30min * 60/30, 2)),
      "1hr" = c(summary_table$timestamp_1hr, summary_table$amount_1hr, round(summary_table$amount_1hr, 2)),
      "2hr" = c(summary_table$timestamp_2hr, summary_table$amount_2hr, round(summary_table$amount_2hr / 2, 2)),
      "6hr" = c(summary_table$timestamp_6hr, summary_table$amount_6hr, round(summary_table$amount_6hr / 6, 2)),
      "12hr" = c(summary_table$timestamp_12hr, summary_table$amount_12hr, round(summary_table$amount_12hr / 12, 2)),
      "24hr" = c(summary_table$timestamp_24hr, summary_table$amount_24hr, round(summary_table$amount_24hr / 24, 2))
    )
    
    return(summary_table_formatted)
  }
}