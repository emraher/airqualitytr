# Create sample test data
create_test_data <- function() {
  tibble::tibble(
    time = seq(as.POSIXct("2024-01-01 00:00"), by = "hour", length.out = 100),
    parameter = rep(c("PM10", "SO2"), each = 50),
    value = c(
      # PM10 values: mostly valid with some issues
      rnorm(45, mean = 50, sd = 20), # Normal values
      -5, # Negative value
      3000, # Exceeds limit
      NA, # Missing
      200, # Outlier
      rnorm(1, mean = 50, sd = 20),
      # SO2 values: mostly valid
      rnorm(48, mean = 30, sd = 10),
      NA, # Missing
      -2 # Negative
    )
  )
}

test_that("check_invalid_values identifies negative values", {
  test_data <- create_test_data()
  result <- check_invalid_values(test_data)

  expect_true("is_negative" %in% names(result))
  expect_true(any(result$is_negative, na.rm = TRUE))
  expect_true(any(result$quality_flag == "negative_value"))
})

test_that("check_invalid_values identifies values exceeding limits", {
  test_data <- create_test_data()
  result <- check_invalid_values(test_data)

  expect_true("exceeds_limit" %in% names(result))
  expect_true(any(result$exceeds_limit, na.rm = TRUE))
  expect_true(any(result$quality_flag == "exceeds_plausible_limit"))
})

test_that("check_invalid_values identifies outliers", {
  test_data <- create_test_data()
  result <- check_invalid_values(test_data, outlier_threshold = 3)

  expect_true("is_outlier" %in% names(result))
  expect_true("is_valid" %in% names(result))
  expect_true("quality_flag" %in% names(result))
})

test_that("check_invalid_values handles custom parameter limits", {
  test_data <- create_test_data()
  custom_limits <- list(PM10 = 100, SO2 = 50)
  result <- check_invalid_values(test_data, parameter_limits = custom_limits)

  expect_s3_class(result, "tbl_df")
  expect_true(any(result$exceeds_limit, na.rm = TRUE))
})

test_that("check_invalid_values validates inputs", {
  expect_error(check_invalid_values("not a data frame"), "must be a data frame")
  expect_error(
    check_invalid_values(tibble::tibble(x = 1)),
    "Missing required columns"
  )
})

test_that("assess_completeness calculates correct metrics", {
  test_data <- create_test_data()
  result <- assess_completeness(test_data, by_parameter = TRUE)

  expect_s3_class(result, "tbl_df")
  expect_true("parameter" %in% names(result))
  expect_true("total_records" %in% names(result))
  expect_true("valid_records" %in% names(result))
  expect_true("completeness_pct" %in% names(result))
  expect_true("coverage_pct" %in% names(result))
  expect_equal(nrow(result), 2) # Two parameters
})

test_that("assess_completeness handles empty data", {
  empty_data <- tibble::tibble(
    time = as.POSIXct(character(0)),
    parameter = character(0),
    value = numeric(0)
  )
  result <- assess_completeness(empty_data)

  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 0)
})

test_that("assess_completeness validates inputs", {
  expect_error(assess_completeness("not a data frame"), "must be a data frame")
  expect_error(
    assess_completeness(tibble::tibble(x = 1)),
    "Missing required columns"
  )
})

test_that("identify_gaps finds time gaps correctly", {
  # Create data with known gaps
  gap_data <- tibble::tibble(
    time = c(
      as.POSIXct("2024-01-01 00:00"),
      as.POSIXct("2024-01-01 01:00"),
      as.POSIXct("2024-01-01 06:00"), # 5 hour gap
      as.POSIXct("2024-01-01 07:00")
    ),
    parameter = "PM10",
    value = c(10, 20, 30, 40)
  )

  result <- identify_gaps(gap_data, min_gap_hours = 3)

  expect_s3_class(result, "tbl_df")
  expect_true("gap_start" %in% names(result))
  expect_true("gap_end" %in% names(result))
  expect_true("gap_hours" %in% names(result))
  expect_gt(nrow(result), 0)
  expect_true(any(result$gap_hours >= 3))
})

test_that("identify_gaps handles data without gaps", {
  no_gap_data <- tibble::tibble(
    time = seq(as.POSIXct("2024-01-01 00:00"), by = "hour", length.out = 24),
    parameter = "PM10",
    value = rnorm(24, 50, 10)
  )

  result <- identify_gaps(no_gap_data, min_gap_hours = 3)

  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 0)
})

test_that("identify_gaps validates inputs", {
  expect_error(identify_gaps("not a data frame"), "must be a data frame")
  expect_error(
    identify_gaps(tibble::tibble(x = 1)),
    "Missing required columns"
  )
})

test_that("quality_report generates comprehensive report", {
  test_data <- create_test_data()
  report <- quality_report(test_data)

  expect_type(report, "list")
  expect_s3_class(report, "air_quality_report")
  expect_named(report, c("summary", "completeness", "invalid_values", "gaps", "flagged_data"))

  # Check summary structure
  expect_s3_class(report$summary, "tbl_df")
  expect_true("total_records" %in% names(report$summary))
  expect_true("valid_records" %in% names(report$summary))
  expect_true("validity_pct" %in% names(report$summary))

  # Check completeness
  expect_s3_class(report$completeness, "tbl_df")

  # Check invalid values summary
  expect_s3_class(report$invalid_values, "tbl_df")

  # Check gaps
  expect_s3_class(report$gaps, "tbl_df")

  # Check flagged data
  expect_s3_class(report$flagged_data, "tbl_df")
})

test_that("quality_report print method works", {
  test_data <- create_test_data()
  report <- quality_report(test_data)

  expect_output(print(report), "Air Quality Data Report")
  expect_output(print(report), "Overall Summary")
  expect_output(print(report), "Total records")
})

test_that("quality_report validates inputs", {
  expect_error(quality_report("not a data frame"), "must be a data frame")
})
