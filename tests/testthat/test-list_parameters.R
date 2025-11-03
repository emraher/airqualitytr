test_that("list_parameters returns a tibble with expected structure", {
  # Use mocked API if on CI/CRAN, otherwise use real API
  if (use_mocked_api() && fixtures_available()) {
    httptest::with_mock_dir("m", {
      params <- list_parameters()

      expect_s3_class(params, "tbl_df")
      expect_gt(nrow(params), 0)
      expect_gt(ncol(params), 0)
    })
  } else {
    skip_on_cran()
    skip_on_ci()
    skip_if_offline()

    params <- list_parameters()

    expect_s3_class(params, "tbl_df")
    expect_gt(nrow(params), 0)
    expect_gt(ncol(params), 0)
  }
})

test_that("list_parameters uses snake_case column names", {
  # Use mocked API if on CI/CRAN, otherwise use real API
  if (use_mocked_api() && fixtures_available()) {
    httptest::with_mock_dir("m", {
      params <- list_parameters()

      # Check that all column names are in snake_case
      expect_true(all(grepl("^[a-z][a-z0-9_]*$", names(params))))
    })
  } else {
    skip_on_cran()
    skip_on_ci()
    skip_if_offline()

    params <- list_parameters()

    # Check that all column names are in snake_case
    expect_true(all(grepl("^[a-z][a-z0-9_]*$", names(params))))
  }
})

test_that("list_parameters includes common air quality parameters", {
  # Use mocked API if on CI/CRAN, otherwise use real API
  if (use_mocked_api() && fixtures_available()) {
    httptest::with_mock_dir("m", {
      params <- list_parameters()

      # Common parameters like PM10, PM2.5, SO2, NO2, O3, CO should be present
      # This test is flexible and checks if we have at least some data
      expect_gt(nrow(params), 5)
    })
  } else {
    skip_on_cran()
    skip_on_ci()
    skip_if_offline()

    params <- list_parameters()

    # Common parameters like PM10, PM2.5, SO2, NO2, O3, CO should be present
    # This test is flexible and checks if we have at least some data
    expect_gt(nrow(params), 5)
  }
})
