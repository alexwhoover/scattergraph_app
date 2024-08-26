# Define test data frames
df1 <- tibble(datetime = as.POSIXct(c("2024-01-01 00:00:00", 
                                      "2024-01-02 00:00:00", 
                                      "2024-01-03 00:00:00", 
                                      "2024-01-10 00:00:00", 
                                      "2024-01-11 00:00:00"), tz = "UTC"))

df2 <- tibble(datetime = as.POSIXct(c("2024-02-01 12:00:00", 
                                      "2024-02-02 12:00:00", 
                                      "2024-02-04 12:00:00", 
                                      "2024-02-06 12:00:00", 
                                      "2024-02-10 12:00:00",
                                      "2024-02-12 12:05:00"), tz = "UTC"))

df_empty <- tibble(datetime = as.POSIXct(character(), tz = "UTC"))

# Unit tests for get_consecutive_periods
test_that("get_consecutive_periods works with a regular dataframe", {
  result <- get_consecutive_periods(df1)
  
  expect_equal(nrow(result), 2)
  expect_equal(result$period_id, c(1, 2))
  expect_equal(result$start, c("2024-01-01 00:00", "2024-01-10 00:00"))
  expect_equal(result$end, c("2024-01-03 00:00", "2024-01-11 00:00"))
  expect_equal(result$count, c(3, 2))
})

test_that("get_consecutive_periods handles another regular dataframe", {
  result <- get_consecutive_periods(df2)
  
  expect_equal(nrow(result), 3)
  expect_equal(result$period_id, c(1, 2, 3))
  expect_equal(result$start, c("2024-02-01 12:00", "2024-02-10 12:00", "2024-02-12 12:05"))
  expect_equal(result$end, c("2024-02-06 12:00", "2024-02-10 12:00", "2024-02-12 12:05"))
  expect_equal(result$count, c(4, 1, 1))
})

test_that("get_consecutive_periods handles empty dataframe", {
  result <- get_consecutive_periods(df_empty)
  expect_null(result)
})

test_that("get_consecutive_periods handles a single row dataframe", {
  df_single <- tibble(datetime = as.POSIXct("2024-04-01 00:00:00", tz = "UTC"))
  result <- get_consecutive_periods(df_single)
  
  expect_equal(nrow(result), 1)
  expect_equal(result$period_id, 1)
  expect_equal(result$start, "2024-04-01 00:00")
  expect_equal(result$end, "2024-04-01 00:00")
  expect_equal(result$count, 1)
})

test_that("get_consecutive_periods handles identical datetimes", {
  df_identical <- tibble(datetime = as.POSIXct(c("2024-05-01 00:00:00", 
                                                 "2024-05-01 00:00:00", 
                                                 "2024-05-01 00:00:00"), tz = "UTC"))
  result <- get_consecutive_periods(df_identical)
  
  expect_equal(nrow(result), 1)
  expect_equal(result$period_id, 1)
  expect_equal(result$start, "2024-05-01 00:00")
  expect_equal(result$end, "2024-05-01 00:00")
  expect_equal(result$count, 3)
})