test_that("list_cities returns a tibble with expected structure", {
  skip_on_cran()
  skip_on_ci()
  skip_if_offline()

  cities <- list_cities()

  expect_s3_class(cities, "tbl_df")
  expect_named(cities, c("city_id", "city_name"))
  expect_gt(nrow(cities), 0)
  expect_true(is.numeric(cities$city_id) || is.character(cities$city_id))
  expect_type(cities$city_name, "character")
})

test_that("list_cities returns unique city IDs", {
  skip_on_cran()
  skip_on_ci()
  skip_if_offline()

  cities <- list_cities()

  expect_equal(nrow(cities), length(unique(cities$city_id)))
})

test_that("list_cities handles API errors gracefully", {
  skip_on_cran()

  # Mock the post_to_defaults_endpoint to simulate an error
  local_mocked_bindings(
    post_to_defaults_endpoint = function(...) {
      stop("API request failed with HTTP status 500")
    }
  )

  expect_error(list_cities(), "API request failed")
})
