#' Test API Connection
#'
#' Tests the connection to the Turkish Ministry of Environment API and verifies
#' that all required endpoints are accessible. Useful for troubleshooting
#' connection issues before attempting large downloads.
#'
#' @param timeout Timeout in seconds for each test. Default is 10.
#' @param verbose Logical. If TRUE, prints detailed diagnostic information.
#'        Default is TRUE.
#'
#' @return A list containing:
#' \describe{
#'   \item{overall_status}{Character: "success", "partial", or "failure"}
#'   \item{tests}{Tibble with results for each endpoint test}
#'   \item{recommendations}{Character vector of troubleshooting recommendations}
#' }
#'
#' @export
#' @examples
#' \donttest{
#' # Test API connection
#' test_result <- test_api_connection()
#'
#' # Quick check
#' if (test_result$overall_status == "success") {
#'   message("API is accessible")
#' }
#'
#' # Detailed diagnostics
#' print(test_result$tests)
#' }
test_api_connection <- function(timeout = 10, verbose = TRUE) {
  base_url <- "https://sim.csb.gov.tr"

  # Define tests to run
  tests <- list(
    list(
      name = "Base URL",
      url = base_url,
      description = "Can reach the main website"
    ),
    list(
      name = "List Cities",
      test_function = "list_cities",
      description = "Can retrieve city list from API"
    ),
    list(
      name = "List Stations",
      test_function = "list_stations",
      description = "Can retrieve station metadata from API"
    ),
    list(
      name = "List Parameters",
      test_function = "list_parameters",
      description = "Can retrieve parameter list from API"
    )
  )

  if (verbose) {
    cat("Testing API Connection...\n\n")
  }

  # Run tests
  results <- list()

  for (i in seq_along(tests)) {
    test <- tests[[i]]

    if (verbose) {
      cat(sprintf("[%d/%d] %s: ", i, length(tests), test$name))
    }

    result <- list(
      test_name = test$name,
      description = test$description,
      status = "unknown",
      response_time = NA_real_,
      error_message = NA_character_
    )

    tryCatch(
      {
        start_time <- Sys.time()

        if (!is.null(test$url)) {
          # URL test
          response <- httr::GET(test$url, httr::timeout(timeout))
          result$status <- if (httr::status_code(response) == 200) "success" else "failure"
          result$response_time <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))

          if (result$status == "failure") {
            result$error_message <- sprintf("HTTP %d", httr::status_code(response))
          }
        } else if (!is.null(test$test_function)) {
          # Function test
          test_result <- switch(test$test_function,
            "list_cities" = list_cities(),
            "list_stations" = list_stations(),
            "list_parameters" = list_parameters()
          )

          result$response_time <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))

          if (inherits(test_result, "data.frame") && nrow(test_result) > 0) {
            result$status <- "success"
          } else {
            result$status <- "failure"
            result$error_message <- "Empty response"
          }
        }

        if (verbose) {
          cat(sprintf(
            "%s (%.2fs)\n",
            if (result$status == "success") "\\u2713 PASS" else "\\u2717 FAIL",
            result$response_time
          ))
          if (result$status != "success" && !is.na(result$error_message)) {
            cat(sprintf("     Error: %s\n", result$error_message))
          }
        }
      },
      error = function(e) {
        result$status <<- "failure"
        result$error_message <<- conditionMessage(e)
        result$response_time <<- as.numeric(difftime(Sys.time(), start_time, units = "secs"))

        if (verbose) {
          cat(sprintf("\\u2717 FAIL (%.2fs)\n", result$response_time))
          cat(sprintf("     Error: %s\n", result$error_message))
        }
      }
    )

    results[[i]] <- result
  }

  # Convert to tibble
  results_df <- tibble::tibble(
    test_name = sapply(results, function(x) x$test_name),
    description = sapply(results, function(x) x$description),
    status = sapply(results, function(x) x$status),
    response_time = sapply(results, function(x) x$response_time),
    error_message = sapply(results, function(x) x$error_message)
  )

  # Determine overall status
  n_success <- sum(results_df$status == "success")
  n_total <- nrow(results_df)

  overall_status <- if (n_success == n_total) {
    "success"
  } else if (n_success > 0) {
    "partial"
  } else {
    "failure"
  }

  # Generate recommendations
  recommendations <- character(0)

  if (overall_status == "failure") {
    recommendations <- c(
      "Check your internet connection",
      "Verify that https://sim.csb.gov.tr is accessible from your network",
      "Check if your firewall or proxy is blocking the connection",
      "Try again later - the API server may be temporarily down"
    )
  } else if (overall_status == "partial") {
    recommendations <- c(
      "Some API endpoints are not responding",
      "Try the failing operations again",
      "Check the error messages for specific issues",
      "Contact the package maintainer if issues persist"
    )
  } else {
    recommendations <- c(
      "All API endpoints are accessible",
      "You should be able to download air quality data",
      sprintf("Average response time: %.2f seconds", mean(results_df$response_time, na.rm = TRUE))
    )
  }

  if (verbose) {
    cat("\n")
    cat(sprintf(
      "Overall Status: %s (%d/%d tests passed)\n",
      toupper(overall_status), n_success, n_total
    ))
    cat("\nRecommendations:\n")
    for (rec in recommendations) {
      cat(sprintf("  - %s\n", rec))
    }
  }

  # Return diagnostic result
  diagnostic <- list(
    overall_status = overall_status,
    tests = results_df,
    recommendations = recommendations
  )

  class(diagnostic) <- c("api_diagnostic", "list")

  return(diagnostic)
}


#' Print Method for API Diagnostic
#'
#' @param x An API diagnostic object from test_api_connection()
#' @param ... Additional arguments (not used)
#' @return The input \code{api_diagnostic} object, invisibly, to allow further
#'   inspection or programmatic checks after printing.
#'
#' @export
print.api_diagnostic <- function(x, ...) {
  cat("=== API Connection Diagnostic ===\n\n")

  cat(sprintf("Overall Status: %s\n\n", toupper(x$overall_status)))

  cat("Test Results:\n")
  print(x$tests)

  cat("\nRecommendations:\n")
  for (rec in x$recommendations) {
    cat(sprintf("  - %s\n", rec))
  }

  invisible(x)
}


#' Get API Status Information
#'
#' Retrieves current status information about the Turkish Ministry of Environment
#' API, including response times and availability.
#'
#' @return A list containing API status metrics
#'
#' @export
#' @examples
#' \donttest{
#' status <- get_api_status()
#' print(status)
#' }
get_api_status <- function() {
  # Run quick connection test without verbose output
  diagnostic <- test_api_connection(timeout = 5, verbose = FALSE)

  status <- list(
    available = diagnostic$overall_status == "success",
    response_time_avg = mean(diagnostic$tests$response_time, na.rm = TRUE),
    tests_passed = sum(diagnostic$tests$status == "success"),
    tests_total = nrow(diagnostic$tests),
    timestamp = Sys.time()
  )

  return(status)
}


#' Enhanced Error Message Formatter
#'
#' Formats API errors with helpful context and troubleshooting suggestions.
#'
#' @param error_type Character. Type of error (e.g., "http", "network", "timeout")
#' @param details Additional error details
#' @param http_status HTTP status code (if applicable)
#'
#' @return Formatted error message with suggestions
#'
#' @keywords internal
format_api_error <- function(error_type, details = NULL, http_status = NULL) {
  base_message <- switch(error_type,
    "network" = "Network connection failed",
    "timeout" = "Request timed out",
    "http" = sprintf("HTTP error %s", http_status),
    "parse" = "Failed to parse API response",
    "empty" = "API returned empty response",
    "Unknown error occurred"
  )

  # Add HTTP-specific messages
  if (!is.null(http_status)) {
    http_message <- switch(as.character(http_status),
      "400" = "Bad request - invalid parameters",
      "401" = "Unauthorized - authentication failed",
      "403" = "Forbidden - access denied",
      "404" = "Not found - endpoint or resource doesn't exist",
      "429" = "Too many requests - rate limit exceeded",
      "500" = "Internal server error",
      "502" = "Bad gateway - server issue",
      "503" = "Service unavailable - server may be down",
      "504" = "Gateway timeout - server took too long to respond",
      sprintf("HTTP status code: %s", http_status)
    )
    base_message <- paste0(base_message, " - ", http_message)
  }

  # Add suggestions
  suggestions <- character(0)

  if (error_type == "network") {
    suggestions <- c(
      "Check your internet connection",
      "Verify that https://sim.csb.gov.tr is accessible",
      "Try running test_api_connection() for diagnostics"
    )
  } else if (error_type == "timeout") {
    suggestions <- c(
      "The server is slow to respond",
      "Try reducing the date range or number of parameters",
      "Consider using parallel processing with fewer workers"
    )
  } else if (!is.null(http_status)) {
    if (http_status == 429) {
      suggestions <- c(
        "You have made too many requests",
        "Wait a few minutes before trying again",
        "Reduce the number of parallel workers"
      )
    } else if (http_status >= 500) {
      suggestions <- c(
        "The API server is experiencing issues",
        "Try again in a few minutes",
        "Check the Ministry website: https://sim.csb.gov.tr"
      )
    } else if (http_status == 404) {
      suggestions <- c(
        "The requested station or endpoint may not exist",
        "Verify station IDs with list_stations()",
        "Verify parameters with list_parameters()"
      )
    }
  }

  # Format full message
  full_message <- base_message

  if (!is.null(details)) {
    full_message <- paste0(full_message, "\nDetails: ", details)
  }

  if (length(suggestions) > 0) {
    full_message <- paste0(
      full_message,
      "\n\nTroubleshooting suggestions:\n  - ",
      paste(suggestions, collapse = "\n  - ")
    )
  }

  return(full_message)
}
