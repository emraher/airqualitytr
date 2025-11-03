# HTTP Test Helpers
# This file sets up httptest for mocking HTTP requests in tests

# Configure httptest to use simplified file paths (no subdirectories)
# This keeps paths under 100 bytes for CRAN compliance
httptest::set_requester(function(request) {
  # Simplify the request URL to just use the endpoint name
  # This creates flat fixture files instead of nested directories
  request$url <- gsub("https?://[^/]+/(.*/)?", "", request$url)
  return(request)
})

#' Check if we should use mocked API responses
#'
#' Returns TRUE if tests should use mocked fixtures instead of real API calls.
#' This is enabled by default on CRAN and CI, or when AIRQUALITYTR_USE_MOCKS=true
#'
#' @return Logical indicating whether to use mocked responses
use_mocked_api <- function() {
  # Use mocks on CRAN and CI by default
  on_cran <- !identical(Sys.getenv("NOT_CRAN"), "true")
  on_ci <- Sys.getenv("CI") != "" || Sys.getenv("GITHUB_ACTIONS") != ""

  # Allow explicit control via environment variable
  use_mocks <- Sys.getenv("AIRQUALITYTR_USE_MOCKS")
  if (use_mocks != "") {
    return(tolower(use_mocks) %in% c("true", "1", "yes"))
  }

  # Default: use mocks on CRAN or CI
  on_cran || on_ci
}

#' Check if httptest fixtures are available
#'
#' @return Logical indicating if fixture files exist
fixtures_available <- function() {
  fixture_dir <- testthat::test_path("m")
  dir.exists(fixture_dir) && length(list.files(fixture_dir, recursive = TRUE)) > 0
}

#' Test with Optional Mocks
#'
#' Simplified test helper that automatically uses mocks on CI/CRAN or real API locally.
#' This eliminates the need for duplicated if/else blocks in every test.
#'
#' @param desc Test description string
#' @param code Test code (unevaluated expression)
#' @param use_mocks Logical or NULL. If NULL (default), auto-detect based on environment.
#'
#' @examples
#' test_with_optional_mocks("list_cities returns tibble", {
#'   cities <- list_cities()
#'   expect_s3_class(cities, "tbl_df")
#' })
#'
#' @noRd
test_with_optional_mocks <- function(desc, code) {
  code_expr <- substitute(code)

  testthat::test_that(desc, {
    # Determine whether to use mocks
    use_mocks <- use_mocked_api()

    if (use_mocks && fixtures_available()) {
      # Use httptest mocks
      httptest::with_mock_dir(testthat::test_path("m"), {
        eval(code_expr, parent.frame())
      })
    } else if (use_mocks) {
      # Mocks required but not available
      testthat::skip("httptest fixtures not available. Run tests/record_fixtures.R to create them.")
    } else {
      # Use real API - skip on restricted environments
      testthat::skip_on_cran()
      testthat::skip_on_ci()
      testthat::skip_if_offline()

      eval(code_expr, parent.frame())
    }
  })
}
