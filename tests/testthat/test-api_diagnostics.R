test_that("test_api_connection runs without errors", {
  # Use mocked API if on CI/CRAN, otherwise use real API
  if (use_mocked_api() && fixtures_available()) {
    httptest::with_mock_dir("m", {
      result <- test_api_connection(timeout = 5, verbose = FALSE)

      expect_type(result, "list")
      expect_s3_class(result, "api_diagnostic")
      expect_named(result, c("overall_status", "tests", "recommendations"))
      expect_true(result$overall_status %in% c("success", "partial", "failure"))
    })
  } else {
    skip_on_cran()
    skip_on_ci()
    skip_if_offline()

    result <- test_api_connection(timeout = 15, verbose = FALSE)

    expect_type(result, "list")
    expect_s3_class(result, "api_diagnostic")
    expect_named(result, c("overall_status", "tests", "recommendations"))
    expect_true(result$overall_status %in% c("success", "partial", "failure"))
  }
})

test_that("test_api_connection tests structure is correct", {
  # Use mocked API if on CI/CRAN, otherwise use real API
  if (use_mocked_api() && fixtures_available()) {
    httptest::with_mock_dir("m", {
      result <- test_api_connection(verbose = FALSE)

      expect_s3_class(result$tests, "tbl_df")
      expect_true(all(c("test_name", "description", "status", "response_time", "error_message") %in% names(result$tests)))
      expect_gt(nrow(result$tests), 0)
    })
  } else {
    skip_on_cran()
    skip_on_ci()
    skip_if_offline()

    result <- test_api_connection(verbose = FALSE)

    expect_s3_class(result$tests, "tbl_df")
    expect_true(all(c("test_name", "description", "status", "response_time", "error_message") %in% names(result$tests)))
    expect_gt(nrow(result$tests), 0)
  }
})

test_that("test_api_connection provides recommendations", {
  # Use mocked API if on CI/CRAN, otherwise use real API
  if (use_mocked_api() && fixtures_available()) {
    httptest::with_mock_dir("m", {
      result <- test_api_connection(verbose = FALSE)

      expect_type(result$recommendations, "character")
      expect_gt(length(result$recommendations), 0)
    })
  } else {
    skip_on_cran()
    skip_on_ci()
    skip_if_offline()

    result <- test_api_connection(verbose = FALSE)

    expect_type(result$recommendations, "character")
    expect_gt(length(result$recommendations), 0)
  }
})

test_that("test_api_connection verbose mode produces output", {
  # Skip on CI/CRAN since verbose output is less relevant for automated tests
  skip_on_cran()
  skip_on_ci()
  skip_if_offline()

  expect_output(test_api_connection(verbose = TRUE), "Testing API Connection")
})

test_that("api_diagnostic print method works", {
  # Use mocked API if on CI/CRAN, otherwise use real API
  if (use_mocked_api() && fixtures_available()) {
    httptest::with_mock_dir("m", {
      result <- test_api_connection(verbose = FALSE)

      expect_output(print(result), "API Connection Diagnostic")
      expect_output(print(result), "Overall Status")
      expect_output(print(result), "Recommendations")
    })
  } else {
    skip_on_cran()
    skip_on_ci()
    skip_if_offline()

    result <- test_api_connection(verbose = FALSE)

    expect_output(print(result), "API Connection Diagnostic")
    expect_output(print(result), "Overall Status")
    expect_output(print(result), "Recommendations")
  }
})

test_that("get_api_status returns status information", {
  # Use mocked API if on CI/CRAN, otherwise use real API
  if (use_mocked_api() && fixtures_available()) {
    httptest::with_mock_dir("m", {
      status <- get_api_status()

      expect_type(status, "list")
      expect_named(status, c("available", "response_time_avg", "tests_passed", "tests_total", "timestamp"))
      expect_type(status$available, "logical")
      expect_type(status$response_time_avg, "double")
      expect_type(status$tests_passed, "integer")
      expect_type(status$tests_total, "integer")
      expect_s3_class(status$timestamp, "POSIXct")
    })
  } else {
    skip_on_cran()
    skip_on_ci()
    skip_if_offline()

    status <- get_api_status()

    expect_type(status, "list")
    expect_named(status, c("available", "response_time_avg", "tests_passed", "tests_total", "timestamp"))
    expect_type(status$available, "logical")
    expect_type(status$response_time_avg, "double")
    expect_type(status$tests_passed, "integer")
    expect_type(status$tests_total, "integer")
    expect_s3_class(status$timestamp, "POSIXct")
  }
})

test_that("get_api_status runs quickly", {
  # Use mocked API if on CI/CRAN, otherwise use real API
  if (use_mocked_api() && fixtures_available()) {
    httptest::with_mock_dir("m", {
      start_time <- Sys.time()
      status <- get_api_status()
      elapsed <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))

      expect_lt(elapsed, 30) # Should complete within 30 seconds
    })
  } else {
    skip_on_cran()
    skip_on_ci()
    skip_if_offline()

    start_time <- Sys.time()
    status <- get_api_status()
    elapsed <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))

    expect_lt(elapsed, 30) # Should complete within 30 seconds
  }
})

test_that("format_api_error generates informative messages", {
  # Network error
  msg <- airqualitytr:::format_api_error("network")
  expect_true(grepl("Network connection failed", msg))
  expect_true(grepl("Check your internet connection", msg))

  # Timeout error
  msg <- airqualitytr:::format_api_error("timeout")
  expect_true(grepl("timed out", msg))

  # HTTP error with status
  msg <- airqualitytr:::format_api_error("http", http_status = 404)
  expect_true(grepl("404", msg))
  expect_true(grepl("Not found", msg))

  msg <- airqualitytr:::format_api_error("http", http_status = 500)
  expect_true(grepl("500", msg))
  expect_true(grepl("Internal server error", msg))
})

test_that("format_api_error handles different error types", {
  # Network error
  msg_network <- airqualitytr:::format_api_error("network")
  expect_type(msg_network, "character")
  expect_length(msg_network, 1)
  expect_gt(nchar(msg_network), 0)

  # Timeout error
  msg_timeout <- airqualitytr:::format_api_error("timeout")
  expect_type(msg_timeout, "character")
  expect_length(msg_timeout, 1)
  expect_gt(nchar(msg_timeout), 0)

  # HTTP error
  msg_http <- airqualitytr:::format_api_error("http", http_status = 500)
  expect_type(msg_http, "character")
  expect_length(msg_http, 1)
  expect_gt(nchar(msg_http), 0)

  # Parse error
  msg_parse <- airqualitytr:::format_api_error("parse")
  expect_type(msg_parse, "character")
  expect_length(msg_parse, 1)
  expect_gt(nchar(msg_parse), 0)

  # Empty error
  msg_empty <- airqualitytr:::format_api_error("empty")
  expect_type(msg_empty, "character")
  expect_length(msg_empty, 1)
  expect_gt(nchar(msg_empty), 0)
})
