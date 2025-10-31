#' Validate Station ID Format
#'
#' Checks if a station ID is in valid UUID format.
#'
#' @param station_id Character string to validate
#' @return Invisible TRUE if valid, stops with error if invalid
#' @noRd
validate_station_id <- function(station_id) {
  if (missing(station_id) || is.null(station_id)) {
    stop("station_id is required and cannot be NULL", call. = FALSE)
  }

  if (!is.character(station_id) || length(station_id) != 1) {
    stop("station_id must be a single character string", call. = FALSE)
  }

  if (!nzchar(station_id)) {
    stop("station_id cannot be an empty string", call. = FALSE)
  }

  # UUID pattern: 8-4-4-4-12 hex digits
  uuid_pattern <- "^[0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{12}$"

  if (!grepl(uuid_pattern, station_id, ignore.case = TRUE)) {
    stop(
      "Invalid station_id format. Expected UUID format (8-4-4-4-12 hex digits).\n",
      "  - Example: '468478b7-ace5-4bd3-b89a-a9c1c2e53080'\n",
      "  - Received: '", station_id, "'\n",
      "  - Use list_stations() to get valid station IDs.",
      call. = FALSE
    )
  }

  invisible(TRUE)
}

#' Validate Air Quality Parameters
#'
#' Checks if parameters are valid pollutant names.
#'
#' @param parameters Character vector of parameter names
#' @return Invisible TRUE if valid, stops with error if invalid
#' @noRd
validate_parameters <- function(parameters) {
  if (missing(parameters) || is.null(parameters)) {
    stop("parameters is required and cannot be NULL", call. = FALSE)
  }

  if (!is.character(parameters) || length(parameters) == 0) {
    stop("parameters must be a non-empty character vector", call. = FALSE)
  }

  # Valid parameters based on Turkish air quality monitoring system
  valid_params <- c("PM10", "PM25", "SO2", "CO", "NO2", "NOX", "NO", "O3")

  invalid <- setdiff(parameters, valid_params)

  if (length(invalid) > 0) {
    stop(
      "Invalid parameter(s): ", paste(invalid, collapse = ", "), "\n",
      "  - Valid parameters are: ", paste(valid_params, collapse = ", "), "\n",
      "  - Use list_parameters() to see all available parameters with descriptions.",
      call. = FALSE
    )
  }

  invisible(TRUE)
}

#' Validate Date Range
#'
#' Checks if date range is valid (start before end, parseable dates).
#'
#' @param start_datetime Start date-time string
#' @param end_datetime End date-time string
#' @return Invisible TRUE if valid, stops with error or warns if issues found
#' @noRd
validate_date_range <- function(start_datetime, end_datetime) {
  if (missing(start_datetime) || is.null(start_datetime)) {
    stop("start_datetime is required and cannot be NULL", call. = FALSE)
  }

  if (missing(end_datetime) || is.null(end_datetime)) {
    stop("end_datetime is required and cannot be NULL", call. = FALSE)
  }

  # Try to parse dates
  start_parsed <- tryCatch(
    as.POSIXct(start_datetime),
    error = function(e) {
      stop(
        "Invalid start_datetime format.\n",
        "  - Expected: 'YYYY-MM-DD HH:MM' (e.g., '2024-01-15 10:30')\n",
        "  - Received: '", start_datetime, "'\n",
        "  - Make sure the date is properly formatted with year, month, day, hour, and minute.",
        call. = FALSE
      )
    }
  )

  end_parsed <- tryCatch(
    as.POSIXct(end_datetime),
    error = function(e) {
      stop(
        "Invalid end_datetime format.\n",
        "  - Expected: 'YYYY-MM-DD HH:MM' (e.g., '2024-01-16 14:45')\n",
        "  - Received: '", end_datetime, "'\n",
        "  - Make sure the date is properly formatted with year, month, day, hour, and minute.",
        call. = FALSE
      )
    }
  )

  # Check if start is before end
  if (start_parsed >= end_parsed) {
    stop(
      "start_datetime must be before end_datetime.\n",
      "  - Start: ", start_datetime, "\n",
      "  - End: ", end_datetime, "\n",
      "  - Please ensure the start date comes before the end date.",
      call. = FALSE
    )
  }

  # Warn if end date is in the future
  if (end_parsed > Sys.time()) {
    warning(
      "end_datetime is in the future. Data may not be available yet.\n",
      "  - End: ", end_datetime, "\n",
      "  - Current time: ", format(Sys.time(), "%Y-%m-%d %H:%M"), "\n",
      "  - Air quality data is typically available with a delay of several hours to days.",
      call. = FALSE
    )
  }

  # Warn if date range is very large (> 1 year) for hourly data
  date_diff_days <- as.numeric(difftime(end_parsed, start_parsed, units = "days"))
  if (date_diff_days > 365) {
    warning(
      "Date range is very large (", round(date_diff_days), " days).\n",
      "  - Large date ranges may result in slow downloads or timeouts.\n",
      "  - Consider using smaller time periods (e.g., monthly chunks).\n",
      "  - Or use frequency='daily' instead of 'hourly' for better performance.",
      call. = FALSE
    )
  }

  invisible(TRUE)
}

#' Validate Frequency Parameter
#'
#' Checks if frequency is valid.
#'
#' @param frequency Character string, either "hourly" or "daily"
#' @return Invisible TRUE if valid, stops with error if invalid
#' @noRd
validate_frequency <- function(frequency) {
  if (missing(frequency) || is.null(frequency)) {
    stop("frequency is required and cannot be NULL", call. = FALSE)
  }

  if (!is.character(frequency) || length(frequency) != 1) {
    stop("frequency must be a single character string", call. = FALSE)
  }

  valid_frequencies <- c("hourly", "daily")

  if (!frequency %in% valid_frequencies) {
    stop(
      "Invalid frequency: '", frequency, "'\n",
      "Valid options are: ", paste(valid_frequencies, collapse = ", "),
      call. = FALSE
    )
  }

  invisible(TRUE)
}
