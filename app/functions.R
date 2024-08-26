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