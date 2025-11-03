test_that("list_stations returns a tibble with expected structure", {
  skip_on_cran()
  skip_on_ci()
  skip_if_offline()

  stations <- list_stations()

  expect_s3_class(stations, "tbl_df")
  expect_true("station_id" %in% names(stations))
  expect_true("station_name" %in% names(stations))
  expect_true("city_name" %in% names(stations))
  expect_gt(nrow(stations), 0)
})

test_that("list_stations uses snake_case column names", {
  skip_on_cran()
  skip_on_ci()
  skip_if_offline()

  stations <- list_stations()

  # Check that all column names are in snake_case
  expect_true(all(grepl("^[a-z][a-z0-9_]*$", names(stations))))
})

test_that("list_stations has station_id as first column", {
  skip_on_cran()
  skip_on_ci()
  skip_if_offline()

  stations <- list_stations()

  expect_equal(names(stations)[1], "station_id")
})

test_that("list_stations returns unique station IDs", {
  skip_on_cran()
  skip_on_ci()
  skip_if_offline()

  stations <- list_stations()

  expect_equal(nrow(stations), length(unique(stations$station_id)))
})
