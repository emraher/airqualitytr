#' Download Air Quality Data with Additional Objects
#'
#' Downloads time series air quality data for a specified station by simulating a form POST
#' request to the Turkish Ministry's air quality data service. In addition to retrieving the
#' raw time series (from the "Data" object), this function attempts to extract any other objects
#' present in the JSON response (such as "Summaries", "Monitors", "StationIds", and "Parameters").
#' Station and city names are automatically retrieved from the metadata.
#'
#' @param station_id A character string with the station's unique identifier.
#' @param parameters A character vector of pollutant parameters (e.g., \code{c("PM10", "CO", "NO2")}).
#' @param start_datetime Start date-time in "YYYY-MM-DD HH:MM" format.
#' @param end_datetime End date-time in "YYYY-MM-DD HH:MM" format.
#' @param frequency Either \code{"hourly"} or \code{"daily"}. Default is \code{"hourly"}.
#' @param return_all Logical. If \code{TRUE} (default), a named list with elements \code{data},
#'        \code{summaries}, \code{monitors}, \code{stations}, and \code{options} is returned.
#'        If \code{FALSE}, only the tidied time series data is returned.
#'
#' @return If \code{return_all = TRUE}, a named list with the following elements:
#'   \describe{
#'     \item{data}{A tibble with the time series measurements (columns include \code{time},
#'         \code{station_id}, \code{city_name}, \code{station_name}, \code{parameter}, and \code{value}).
#'         All column names are in snake_case format.}
#'     \item{summaries}{A tibble with summary data (if available; otherwise an empty tibble).
#'         Column names are in snake_case format.}
#'     \item{monitors}{A tibble with monitors metadata (if available; otherwise an empty tibble).
#'         Column names are in snake_case format.}
#'     \item{stations}{A tibble with station metadata (if available; otherwise an empty tibble).
#'         Column names are in snake_case format.}
#'     \item{options}{A tibble with available parameter/option settings (if available; otherwise empty).
#'         Column names are in snake_case format.}
#'   }
#'   If \code{return_all = FALSE}, only the \code{data} tibble is returned with snake_case column names.
#'
#' @importFrom rlang .data
#' @export
#' @examplesIf interactive()
#' # Download one week of hourly PM10 data
#' data <- download_air_quality_data(
#'   station_id = "468478b7-ace5-4bd3-b89a-a9c1c2e53080",
#'   parameters = c("PM10"),
#'   start_datetime = "2024-03-01 00:00",
#'   end_datetime = "2024-03-07 23:59",
#'   frequency = "hourly"
#' )
#' head(data)
#'
#' # Download multiple parameters
#' multi_data <- download_air_quality_data(
#'   station_id = "468478b7-ace5-4bd3-b89a-a9c1c2e53080",
#'   parameters = c("PM10", "NO2", "O3"),
#'   start_datetime = "2024-03-01 00:00",
#'   end_datetime = "2024-03-07 23:59",
#'   frequency = "hourly"
#' )
download_air_quality_data <- function(station_id,
                                      parameters,
                                      start_datetime,
                                      end_datetime,
                                      frequency = c("hourly", "daily"),
                                      return_all = FALSE) {
  # Validate input parameters
  validate_station_id(station_id)
  validate_parameters(parameters)
  validate_date_range(start_datetime, end_datetime)
  frequency <- match.arg(frequency)
  validate_frequency(frequency)

  # Process frequency parameter
  # DataPeriods: 8 = hourly, 16 = daily (based on API testing)
  data_period <- ifelse(frequency == "hourly", 8, 16)

  # Format datetime parameters for API
  start_datetime_formatted <- format(as.POSIXct(start_datetime), "%Y-%m-%dT%H:%M:%S")
  end_datetime_formatted <- format(as.POSIXct(end_datetime), "%Y-%m-%dT%H:%M:%S")

  # Get station metadata to retrieve city_name and station_name
  stations_metadata <- list_stations()

  # Step 1: Start a session and extract the CSRF token with retry logic
  session_url <- "https://sim.csb.gov.tr/STN/STN_Report/StationDataDownloadNew"
  sess <- retry_api_call({
    rvest::session(session_url)
  })
  doc <- xml2::read_html(sess)

  token <- doc |>
    rvest::html_node("input[name=__RequestVerificationToken]") |>
    rvest::html_attr("value")

  if (is.na(token)) {
    stop("Unable to extract CSRF token from API page.\n",
      "  - The API page structure may have changed, or the service is temporarily unavailable.\n",
      "  - Try again in a few moments. If the problem persists, the package may need updating.\n",
      "  - Please report this issue at: https://github.com/emraher/airqualitytr/issues",
      call. = FALSE
    )
  }

  # Step 2: Build the form-encoded POST body
  # Replicate the "Parameters" field as needed for each parameter
  form_body <- c(
    list(
      `__RequestVerificationToken` = token,
      StationType = "1",
      StationIds = station_id,
      StartDateTime = start_datetime_formatted,
      EndDateTime = end_datetime_formatted,
      DataPeriods = data_period
    ),
    stats::setNames(as.list(parameters), rep("Parameters", length(parameters)))
  )

  # Step 3: Send the POST request (reuse session's cookie handle) with retry logic
  post_url <- "https://sim.csb.gov.tr/STN/STN_Report/StationDataDownloadNewData"
  res <- retry_api_call({
    httr::POST(
      url = post_url,
      body = form_body,
      encode = "form",
      handle = sess$handle,
      httr::timeout(get_api_timeout()),
      httr::add_headers(
        `X-Requested-With` = "XMLHttpRequest",
        `Content-Type` = "application/x-www-form-urlencoded; charset=UTF-8"
      )
    )
  })

  # Check for successful response
  status <- httr::status_code(res)
  if (status != 200) {
    error_msg <- create_http_error_message(status, "Data download request")
    stop(error_msg, call. = FALSE)
  }

  # Parse the JSON response
  parsed <- tryCatch(
    {
      jsonlite::fromJSON(httr::content(res, as = "text", encoding = "UTF-8"))
    },
    error = function(e) {
      stop("Failed to parse API response for data download.\n",
        "  - The API may have returned invalid or malformed data.\n",
        "  - Try reducing your date range or using daily frequency instead of hourly.\n",
        "  - Original error: ", conditionMessage(e),
        call. = FALSE
      )
    }
  )

  # Process time series data
  if (is.null(parsed$Object$Data)) {
    # Create empty tibble with correct structure and column types
    data_tbl <- tibble::tibble(
      time = as.POSIXct(character(0), tz = "UTC"),
      station_id = character(0),
      station_name = character(0),
      parameter = character(0),
      value = numeric(0)
    )
    warning("No time series data returned for the specified parameters and date range.\n",
      "  - The station may not have data for this time period.\n",
      "  - Try a different date range or check if the station was operational during this period.\n",
      "  - You can use list_stations() to check station metadata.",
      call. = FALSE
    )
  } else {
    data_tbl <- tibble::as_tibble(parsed$Object$Data) |>
      tidyr::pivot_longer(
        cols = tidyselect::any_of(parameters),
        names_to = "parameter",
        values_to = "value"
      ) |>
      dplyr::mutate(
        ReadTime = as.POSIXct(.data$ReadTime, format = "%Y-%m-%dT%H:%M:%S", tz = "UTC")
      ) |>
      dplyr::left_join(stations_metadata, by = c("Stationid" = "station_id")) |>
      dplyr::select(time = "ReadTime", station_id = "Stationid", .data$station_name:.data$is_portable, .data$parameter, .data$value)
  }

  # Extract additional objects if present in the response
  # 1. Summary data
  summaries_tbl <- if (!is.null(parsed$Object$Summaries)) {
    standardize_column_names(tibble::as_tibble(parsed$Object$Summaries))
  } else {
    tibble::tibble()
  }

  # 2. Monitors metadata
  monitors_tbl <- if (!is.null(parsed$Object$Monitors)) {
    standardize_column_names(tibble::as_tibble(parsed$Object$Monitors))
  } else {
    tibble::tibble()
  }

  # 3. Station metadata
  stations_tbl <- if (!is.null(parsed$Object$StationIds)) {
    standardize_column_names(tibble::as_tibble(parsed$Object$StationIds))
  } else {
    tibble::tibble()
  }

  # 4. Parameter options
  options_tbl <- if (!is.null(parsed$Object$Parameters)) {
    standardize_column_names(tibble::as_tibble(parsed$Object$Parameters))
  } else {
    tibble::tibble()
  }

  # Return results based on return_all parameter
  if (return_all) {
    return(list(
      data = data_tbl,
      summaries = summaries_tbl,
      monitors = monitors_tbl,
      stations = stations_tbl,
      options = options_tbl
    ))
  } else {
    return(data_tbl)
  }
}
