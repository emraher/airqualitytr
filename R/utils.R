#' Create Informative HTTP Error Message
#'
#' Generates context-specific error messages based on HTTP status codes
#' with suggestions for resolution.
#'
#' @param status_code HTTP status code
#' @param context Character string describing what operation failed
#' @return Character string with detailed error message
#' @noRd
create_http_error_message <- function(status_code, context = "API request") {
  base_msg <- sprintf("%s failed with HTTP status %d", context, status_code)

  suggestion <- switch(as.character(status_code),
    "400" = "Bad Request - Check that your parameters are correctly formatted.",
    "401" = "Unauthorized - Authentication may be required. Please check API access.",
    "403" = "Forbidden - Access to this resource is denied. The API may require authentication or your IP may be blocked.",
    "404" = "Not Found - The requested resource does not exist. Check station ID or endpoint URL.",
    "429" = "Too Many Requests - You have exceeded the rate limit. Please wait before trying again.",
    "500" = "Internal Server Error - The server encountered an error. This is usually temporary, try again later.",
    "502" = "Bad Gateway - The server is temporarily unavailable. Try again in a few moments.",
    "503" = "Service Unavailable - The API is temporarily down for maintenance. Try again later.",
    "504" = "Gateway Timeout - The server took too long to respond. Try reducing your date range or using daily frequency instead of hourly.",
    # Default for other codes
    sprintf("Unexpected HTTP status code. The API may be experiencing issues.")
  )

  diagnostic_hint <- "\n  - Run test_api_connection() for detailed diagnostics"
  status_hint <- "\n  - Check API status: https://sim.csb.gov.tr"

  sprintf("%s\n  - %s%s%s", base_msg, suggestion, diagnostic_hint, status_hint)
}

#' Get API Timeout Based on Environment
#'
#' Returns appropriate timeout value (in seconds) based on whether the code
#' is running in a CI environment or locally. CI environments use shorter
#' timeouts to fail fast.
#'
#' @return Numeric timeout value in seconds
#' @keywords internal
#' @noRd
get_api_timeout <- function() {
  # Check for CI environment indicators
  is_ci <- Sys.getenv("CI") != "" ||
    Sys.getenv("GITHUB_ACTIONS") != "" ||
    Sys.getenv("GITLAB_CI") != "" ||
    Sys.getenv("CIRCLECI") != ""

  # Return short timeout for CI, longer timeout for local development
  if (is_ci) {
    5  # 5 seconds on CI
  } else {
    30  # 30 seconds for local development
  }
}

#' Convert Column Names to snake_case
#'
#' Converts all column names in a data frame from PascalCase or other formats
#' to snake_case for consistency throughout the package.
#'
#' @param df A data frame or tibble
#' @return The same data frame with column names converted to snake_case
#' @noRd
standardize_column_names <- function(df) {
  if (ncol(df) == 0) {
    return(df)
  }

  old_names <- names(df)
  new_names <- tolower(gsub("([a-z0-9])([A-Z])", "\\1_\\2", old_names))
  names(df) <- new_names

  df
}

#' Build Default Payload for API Requests
#'
#' Creates the standard payload structure used by the Turkish Ministry's air quality API
#' for metadata requests (cities, stations, parameters).
#'
#' @return A named list containing the default payload parameters
#' @noRd
build_defaults_payload <- function() {
  # Use fixed date range for consistent httptest fixture hashing
  # These dates don't affect the returned metadata (stations/cities/parameters)
  end_date <- "2024-01-31T00:00:00"
  start_date <- "2024-01-01T00:00:00"

  # Standard payload structure for defaults endpoint
  list(
    StationId = "",
    StationType_Title = "Hava Kalitesi",
    GroupKey = "00000000-0000-0000-0000-000000000000",
    StationIds = "",
    NearestStationIds = "",
    Parameters = "",
    StartDateTime = start_date,
    EndDateTime = end_date,
    DataPeriods = "8",
    StationType = "1",
    FixtureId = "",
    BasinId = "",
    StationCompanyId = "",
    StationGroupId = "",
    CityId = "",
    StationSubType = "",
    AreaType = "",
    SourceType = "",
    DeviceType = "",
    SubType_Title = "Tip Se\\\\u00E7iniz...",
    DataBank = "true"
  )
}

#' Retry API Call with Exponential Backoff
#'
#' Wraps an API call with retry logic for handling transient failures.
#' Retries on network errors and specific HTTP status codes (500, 502, 503, 504).
#'
#' @param expr Expression to evaluate (the API call)
#' @param max_retries Maximum number of retry attempts (default 3)
#' @param backoff_factor Base backoff time in seconds (default 2)
#' @return Result of the expression if successful
#' @noRd
retry_api_call <- function(expr, max_retries = 3, backoff_factor = 2) {
  attempt <- 1

  while (attempt <= max_retries) {
    result <- tryCatch(
      {
        # Evaluate the expression
        res <- eval(expr, parent.frame())

        # Check if response has a status code (it's an httr response)
        if (inherits(res, "response")) {
          status <- httr::status_code(res)

          # Retry on server errors
          if (status %in% c(500, 502, 503, 504)) {
            if (attempt < max_retries) {
              wait_time <- backoff_factor^attempt
              message(sprintf(
                "Server error (HTTP %d). Retrying in %d seconds... (attempt %d/%d)",
                status, wait_time, attempt, max_retries
              ))
              Sys.sleep(wait_time)
              attempt <- attempt + 1
              next
            } else {
              stop(sprintf(
                "API request failed with status %d after %d attempts",
                status, max_retries
              ), call. = FALSE)
            }
          }
        }

        # Success - return the result
        return(res)
      },
      error = function(e) {
        # Network error or other error
        if (attempt < max_retries) {
          # Check if it's a network-related error
          error_msg <- conditionMessage(e)
          is_network_error <- grepl("could not resolve host|connection|timeout|network",
            error_msg,
            ignore.case = TRUE
          )

          if (is_network_error) {
            wait_time <- backoff_factor^attempt
            message(sprintf(
              "Network error: %s. Retrying in %d seconds... (attempt %d/%d)",
              error_msg, wait_time, attempt, max_retries
            ))
            Sys.sleep(wait_time)
            attempt <- attempt + 1
            return(NULL) # Signal to continue loop
          } else {
            # Non-network error - don't retry
            stop(e)
          }
        } else {
          # Max retries reached
          stop(sprintf(
            "API request failed after %d attempts: %s",
            max_retries, conditionMessage(e)
          ), call. = FALSE)
        }
      }
    )

    # If we got a result, return it
    if (!is.null(result)) {
      return(result)
    }
  }
}

#' Make POST Request to Defaults Endpoint
#'
#' Makes a POST request to the StationDataDownloadNewDefaults endpoint
#' and returns the parsed JSON response. Includes automatic retry logic
#' for transient failures.
#'
#' @param url Character string with the API endpoint URL
#' @param payload Named list with the request payload
#' @return Parsed JSON response from the API
#' @noRd
post_to_defaults_endpoint <- function(url, payload) {
  # Make the POST request with retry logic and environment-specific timeout
  res <- retry_api_call({
    httr::POST(
      url,
      body = payload,
      encode = "form",
      httr::timeout(get_api_timeout()),
      httr::add_headers(
        `X-Requested-With` = "XMLHttpRequest",
        `Content-Type` = "application/x-www-form-urlencoded; charset=UTF-8"
      )
    )
  })

  # Check response status
  status <- httr::status_code(res)
  if (status != 200) {
    error_msg <- create_http_error_message(status, "Metadata request")
    stop(error_msg, call. = FALSE)
  }

  # Parse and return JSON response
  tryCatch(
    {
      jsonlite::fromJSON(httr::content(res, as = "text", encoding = "UTF-8"))
    },
    error = function(e) {
      stop("Failed to parse API response. The API may have returned invalid data.\n",
        "  - Original error: ", conditionMessage(e),
        call. = FALSE
      )
    }
  )
}

#' Setup Parallel Processing for Bulk Downloads
#'
#' Helper function to configure parallel processing using future/furrr.
#' Checks for required packages, sets up workers, and handles cleanup.
#'
#' @param parallel Logical. Whether to enable parallel processing.
#' @param workers Integer or NULL. Number of workers to use.
#' @param show_progress Logical. Whether progress reporting is enabled.
#'
#' @return List with:
#'   \item{enabled}{Logical indicating if parallel processing is enabled}
#'   \item{has_progressr}{Logical indicating if progressr is available}
#'   \item{cleanup}{Function to call for cleanup (restores old plan)}
#'
#' @keywords internal
#' @noRd
setup_parallel_processing <- function(parallel = FALSE, workers = NULL, show_progress = TRUE) {
  if (!parallel) {
    return(list(
      enabled = FALSE,
      has_progressr = FALSE,
      cleanup = function() invisible()
    ))
  }

  # Check for required packages
  if (!requireNamespace("future", quietly = TRUE) || !requireNamespace("furrr", quietly = TRUE)) {
    stop(
      "Parallel processing requires the 'future' and 'furrr' packages.\n",
      "  - Install them with: install.packages(c('future', 'furrr'))\n",
      "  - Or set parallel = FALSE to use sequential processing.",
      call. = FALSE
    )
  }

  # Setup cleanup function
  cleanup_fn <- function() invisible()

  # Set up future plan if workers specified
  if (!is.null(workers)) {
    old_plan <- future::plan()
    future::plan(future::multisession, workers = workers)

    # Return cleanup function that restores old plan
    cleanup_fn <- function() future::plan(old_plan)
  }

  # Check if progressr is available for parallel progress
  has_progressr <- requireNamespace("progressr", quietly = TRUE) && show_progress

  list(
    enabled = TRUE,
    has_progressr = has_progressr,
    cleanup = cleanup_fn
  )
}
