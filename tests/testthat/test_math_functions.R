library(testthat)
source("../../R/functions.R")

# Test tolerance for floating-point comparisons
TOLERANCE <- 1e-5

test_that("Theta calculates correctly", {
    expect_equal(calculate_theta(0.04345, 0.3), 1.56164931, tolerance = TOLERANCE)
    expect_equal(calculate_theta(0.04372, 0.3), 1.566757347, tolerance = TOLERANCE)
    
    # Test NA cases
    expect_warning(result <- calculate_theta(NA, 0.3))
    expect_true(is.na(result))
    
    # Test negative values
    expect_warning(result <- calculate_theta(-0.1, 0.3))
    expect_true(is.na(result))
    
    # Test depth greater than diameter
    expect_warning(result <- calculate_theta(0.5, 0.3))
    expect_true(is.na(result))
})
