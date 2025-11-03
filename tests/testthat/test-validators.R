test_that("validate_station_id accepts valid UUID", {
  valid_uuid <- "468478b7-ace5-4bd3-b89a-a9c1c2e53080"
  expect_silent(airqualitytr:::validate_station_id(valid_uuid))
})

test_that("validate_station_id rejects invalid formats", {
  expect_error(airqualitytr:::validate_station_id("not-a-uuid"), "Invalid station_id format")
  expect_error(airqualitytr:::validate_station_id(""), "cannot be an empty string")
  expect_error(airqualitytr:::validate_station_id(NULL), "required and cannot be NULL")
  expect_error(airqualitytr:::validate_station_id(c("a", "b")), "single character string")
})

test_that("validate_parameters accepts valid parameters", {
  expect_silent(airqualitytr:::validate_parameters("PM10"))
  expect_silent(airqualitytr:::validate_parameters(c("PM10", "SO2", "NO2")))
})

test_that("validate_parameters rejects invalid parameters", {
  expect_error(airqualitytr:::validate_parameters("INVALID"), "Invalid parameter")
  expect_error(airqualitytr:::validate_parameters(c("PM10", "INVALID")), "Invalid parameter")
  expect_error(airqualitytr:::validate_parameters(NULL), "required and cannot be NULL")
  expect_error(airqualitytr:::validate_parameters(character(0)), "non-empty character vector")
})

test_that("validate_date_range accepts valid date ranges", {
  expect_silent(airqualitytr:::validate_date_range(
    "2024-01-01 00:00",
    "2024-01-02 00:00"
  ))
})

test_that("validate_date_range rejects invalid date ranges", {
  expect_error(
    airqualitytr:::validate_date_range("2024-01-02 00:00", "2024-01-01 00:00"),
    "start_datetime must be before end_datetime"
  )
  expect_error(
    airqualitytr:::validate_date_range("invalid", "2024-01-02 00:00"),
    "Invalid start_datetime format"
  )
  expect_error(
    airqualitytr:::validate_date_range("2024-01-01 00:00", "invalid"),
    "Invalid end_datetime format"
  )
})

test_that("validate_date_range warns for future dates", {
  # Use a date range that's in the future but not too large (< 1 year)
  future_start <- format(Sys.time() + 86400, "%Y-%m-%d %H:%M")
  future_end <- format(Sys.time() + 86400 * 2, "%Y-%m-%d %H:%M")
  expect_warning(
    airqualitytr:::validate_date_range(future_start, future_end),
    "end_datetime is in the future"
  )
})

test_that("validate_date_range warns for very large ranges", {
  expect_warning(
    airqualitytr:::validate_date_range("2020-01-01 00:00", "2022-01-01 00:00"),
    "Date range is very large"
  )
})

test_that("validate_frequency accepts valid frequencies", {
  expect_silent(airqualitytr:::validate_frequency("hourly"))
  expect_silent(airqualitytr:::validate_frequency("daily"))
})

test_that("validate_frequency rejects invalid frequencies", {
  expect_error(airqualitytr:::validate_frequency("monthly"), "Invalid frequency")
  expect_error(airqualitytr:::validate_frequency(NULL), "required and cannot be NULL")
  expect_error(airqualitytr:::validate_frequency(c("hourly", "daily")), "single character string")
})
