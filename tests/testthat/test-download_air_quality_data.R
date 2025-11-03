test_that("download_air_quality_data validates inputs", {
  expect_error(
    download_air_quality_data(
      station_id = "invalid",
      parameters = "PM10",
      start_datetime = "2024-01-01 00:00",
      end_datetime = "2024-01-02 00:00"
    ),
    "Invalid station_id format"
  )

  expect_error(
    download_air_quality_data(
      station_id = "468478b7-ace5-4bd3-b89a-a9c1c2e53080",
      parameters = "INVALID",
      start_datetime = "2024-01-01 00:00",
      end_datetime = "2024-01-02 00:00"
    ),
    "Invalid parameter"
  )
})

test_that("download_air_quality_data returns correct structure when return_all = FALSE", {
  skip_on_cran()
  skip_on_ci()
  skip_if_offline()

  # This test uses download_air_quality_data which requires CSRF token
  # Mocking is complex due to changing tokens, so we skip on CI/CRAN
  if (use_mocked_api()) {
    skip("Skipping on CI/CRAN - requires live API for CSRF token")
  }

  # Use mocked API if on CI/CRAN, otherwise use real API
  if (use_mocked_api() && fixtures_available()) {
    httptest::with_mock_dir("m", {
      # Use a known station and short time range
      data <- download_air_quality_data(
        station_id = "468478b7-ace5-4bd3-b89a-a9c1c2e53080",
        parameters = "PM10",
        start_datetime = "2024-01-01 00:00",
        end_datetime = "2024-01-02 00:00",
        frequency = "hourly",
        return_all = FALSE
      )

      expect_s3_class(data, "tbl_df")
      expect_true(all(c("time", "station_id", "station_name", "parameter", "value") %in% names(data)))
      expect_s3_class(data$time, "POSIXct")
    })
  } else {
    skip_on_cran()
    skip_on_ci()
    skip_if_offline()

    # Use a known station and short time range
    data <- download_air_quality_data(
      station_id = "468478b7-ace5-4bd3-b89a-a9c1c2e53080",
      parameters = "PM10",
      start_datetime = "2024-01-01 00:00",
      end_datetime = "2024-01-02 00:00",
      frequency = "hourly",
      return_all = FALSE
    )

    expect_s3_class(data, "tbl_df")
    expect_true(all(c("time", "station_id", "station_name", "parameter", "value") %in% names(data)))
    expect_s3_class(data$time, "POSIXct")
  }
})

test_that("download_air_quality_data returns correct structure when return_all = TRUE", {
  skip_on_cran()
  skip_on_ci()
  skip_if_offline()

  if (use_mocked_api()) {
    skip("Skipping on CI/CRAN - requires live API for CSRF token")
  }

  # Use mocked API if on CI/CRAN, otherwise use real API
  if (use_mocked_api() && fixtures_available()) {
    httptest::with_mock_dir("m", {
      result <- download_air_quality_data(
        station_id = "468478b7-ace5-4bd3-b89a-a9c1c2e53080",
        parameters = "PM10",
        start_datetime = "2024-01-01 00:00",
        end_datetime = "2024-01-02 00:00",
        frequency = "hourly",
        return_all = TRUE
      )

      expect_type(result, "list")
      expect_named(result, c("data", "summaries", "monitors", "stations", "options"))
      expect_s3_class(result$data, "tbl_df")
      expect_s3_class(result$summaries, "tbl_df")
      expect_s3_class(result$monitors, "tbl_df")
      expect_s3_class(result$stations, "tbl_df")
      expect_s3_class(result$options, "tbl_df")
    })
  } else {
    skip_on_cran()
    skip_on_ci()
    skip_if_offline()

    result <- download_air_quality_data(
      station_id = "468478b7-ace5-4bd3-b89a-a9c1c2e53080",
      parameters = "PM10",
      start_datetime = "2024-01-01 00:00",
      end_datetime = "2024-01-02 00:00",
      frequency = "hourly",
      return_all = TRUE
    )

    expect_type(result, "list")
    expect_named(result, c("data", "summaries", "monitors", "stations", "options"))
    expect_s3_class(result$data, "tbl_df")
    expect_s3_class(result$summaries, "tbl_df")
    expect_s3_class(result$monitors, "tbl_df")
    expect_s3_class(result$stations, "tbl_df")
    expect_s3_class(result$options, "tbl_df")
  }
})

test_that("download_air_quality_data handles multiple parameters", {
  skip_on_cran()
  skip_on_ci()
  skip_if_offline()

  if (use_mocked_api()) {
    skip("Skipping on CI/CRAN - requires live API for CSRF token")
  }

  # Use mocked API if on CI/CRAN, otherwise use real API
  if (use_mocked_api() && fixtures_available()) {
    httptest::with_mock_dir("m", {
      data <- download_air_quality_data(
        station_id = "468478b7-ace5-4bd3-b89a-a9c1c2e53080",
        parameters = c("PM10", "SO2"),
        start_datetime = "2024-01-01 00:00",
        end_datetime = "2024-01-02 00:00",
        frequency = "hourly"
      )

      expect_true(all(unique(data$parameter) %in% c("PM10", "SO2")))
    })
  } else {
    skip_on_cran()
    skip_on_ci()
    skip_if_offline()

    data <- download_air_quality_data(
      station_id = "468478b7-ace5-4bd3-b89a-a9c1c2e53080",
      parameters = c("PM10", "SO2"),
      start_datetime = "2024-01-01 00:00",
      end_datetime = "2024-01-02 00:00",
      frequency = "hourly"
    )

    expect_true(all(unique(data$parameter) %in% c("PM10", "SO2")))
  }
})

test_that("download_air_quality_data handles daily frequency", {
  skip_on_cran()
  skip_on_ci()
  skip_if_offline()

  if (use_mocked_api()) {
    skip("Skipping on CI/CRAN - requires live API for CSRF token")
  }

  # Use mocked API if on CI/CRAN, otherwise use real API
  if (use_mocked_api() && fixtures_available()) {
    httptest::with_mock_dir("m", {
      data <- download_air_quality_data(
        station_id = "468478b7-ace5-4bd3-b89a-a9c1c2e53080",
        parameters = "PM10",
        start_datetime = "2024-01-01 00:00",
        end_datetime = "2024-01-10 00:00",
        frequency = "daily"
      )

      expect_s3_class(data, "tbl_df")
      expect_true("time" %in% names(data))
    })
  } else {
    skip_on_cran()
    skip_on_ci()
    skip_if_offline()

    data <- download_air_quality_data(
      station_id = "468478b7-ace5-4bd3-b89a-a9c1c2e53080",
      parameters = "PM10",
      start_datetime = "2024-01-01 00:00",
      end_datetime = "2024-01-10 00:00",
      frequency = "daily"
    )

    expect_s3_class(data, "tbl_df")
    expect_true("time" %in% names(data))
  }
})
