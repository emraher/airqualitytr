#' Check for Invalid Values in Air Quality Data
#'
#' Identifies invalid, impossible, or suspicious values in air quality measurements.
#' This includes negative values, extreme outliers, and measurements that exceed
#' physically plausible limits.
#'
#' @param data A tibble containing air quality data with columns: time, parameter, value
#' @param parameter_limits A named list of maximum plausible values for each parameter.
#'        If NULL (default), uses standard limits based on measurement ranges.
#' @param outlier_threshold Number of standard deviations beyond which a value is considered
#'        an outlier. Default is 5.
#'
#' @return A tibble with the same structure as input data, plus additional columns:
#' \describe{
#'   \item{is_negative}{Logical indicating if value is negative}
#'   \item{exceeds_limit}{Logical indicating if value exceeds plausible maximum}
#'   \item{is_outlier}{Logical indicating if value is a statistical outlier}
#'   \item{is_valid}{Logical indicating if value passes all checks (TRUE = valid)}
#'   \item{quality_flag}{Character describing the issue (if any)}
#' }
#'
#' @export
#' @examples
#' \dontrun{
#' data <- download_air_quality_data(
#'   station_id = "468478b7-ace5-4bd3-b89a-a9c1c2e53080",
#'   parameters = c("PM10", "NO2"),
#'   start_datetime = "2025-03-01 00:00",
#'   end_datetime = "2025-03-07 23:59"
#' )
#'
#' # Check data quality
#' checked_data <- check_invalid_values(data)
#'
#' # See flagged values
#' flagged <- checked_data |> dplyr::filter(!is_valid)
#' }
check_invalid_values <- function(data,
                                 parameter_limits = NULL,
                                 outlier_threshold = 5) {
  if (!inherits(data, "data.frame")) {
    stop("data must be a data frame or tibble", call. = FALSE)
  }

  required_cols <- c("parameter", "value")
  missing_cols <- setdiff(required_cols, names(data))
  if (length(missing_cols) > 0) {
    stop(sprintf("Missing required columns: %s", paste(missing_cols, collapse = ", ")),
      call. = FALSE
    )
  }

  # Default plausible maximum limits (μg/m³ or ppm)
  if (is.null(parameter_limits)) {
    parameter_limits <- list(
      PM10 = 2000, # Very high pollution event
      PM2.5 = 1000, # Very high pollution event
      SO2 = 1000, # Very high pollution event
      CO = 100, # ppm - very high
      NO2 = 1000, # Very high pollution event
      NOX = 2000, # Sum of NO and NO2
      NO = 1000, # Very high
      O3 = 500 # Very high ozone event
    )
  }

  # Check for negative values
  data <- data |>
    dplyr::mutate(is_negative = !is.na(.data$value) & .data$value < 0)

  # Check against plausible limits
  data <- data |>
    dplyr::mutate(
      limit = dplyr::case_when(
        .data$parameter == "PM10" ~ parameter_limits$PM10 %||% NA_real_,
        .data$parameter == "PM2.5" ~ parameter_limits$PM2.5 %||% NA_real_,
        .data$parameter == "SO2" ~ parameter_limits$SO2 %||% NA_real_,
        .data$parameter == "CO" ~ parameter_limits$CO %||% NA_real_,
        .data$parameter == "NO2" ~ parameter_limits$NO2 %||% NA_real_,
        .data$parameter == "NOX" ~ parameter_limits$NOX %||% NA_real_,
        .data$parameter == "NO" ~ parameter_limits$NO %||% NA_real_,
        .data$parameter == "O3" ~ parameter_limits$O3 %||% NA_real_,
        TRUE ~ NA_real_
      ),
      exceeds_limit = !is.na(.data$value) & !is.na(.data$limit) & .data$value > .data$limit
    )

  # Statistical outlier detection by parameter
  data <- data |>
    dplyr::group_by(.data$parameter) |>
    dplyr::mutate(
      param_mean = mean(.data$value, na.rm = TRUE),
      param_sd = sd(.data$value, na.rm = TRUE),
      z_score = abs(.data$value - .data$param_mean) / .data$param_sd,
      is_outlier = !is.na(.data$z_score) & .data$z_score > outlier_threshold
    ) |>
    dplyr::ungroup()

  # Overall validity flag
  data <- data |>
    dplyr::mutate(
      quality_flag = dplyr::case_when(
        is.na(.data$value) ~ "missing",
        .data$is_negative ~ "negative_value",
        .data$exceeds_limit ~ "exceeds_plausible_limit",
        .data$is_outlier ~ "statistical_outlier",
        TRUE ~ "valid"
      ),
      is_valid = .data$quality_flag == "valid"
    ) |>
    dplyr::select(-c("limit", "param_mean", "param_sd", "z_score"))

  return(data)
}


#' Assess Data Completeness
#'
#' Calculates completeness metrics for air quality data, including percentage of
#' missing values, gap identification, and temporal coverage.
#'
#' @param data A tibble containing air quality data with columns: time, parameter, value
#' @param by_parameter Logical. If TRUE, calculates metrics separately for each parameter.
#'        Default is TRUE.
#' @param by_station Logical. If TRUE and station_id column exists, calculates metrics
#'        separately for each station. Default is FALSE.
#'
#' @return A tibble containing completeness metrics:
#' \describe{
#'   \item{parameter}{Parameter name (if by_parameter = TRUE)}
#'   \item{station_id}{Station ID (if by_station = TRUE)}
#'   \item{total_records}{Total number of records}
#'   \item{missing_records}{Number of missing (NA) values}
#'   \item{valid_records}{Number of valid (non-NA) values}
#'   \item{completeness_pct}{Percentage of valid records}
#'   \item{start_date}{First timestamp in data}
#'   \item{end_date}{Last timestamp in data}
#'   \item{expected_records}{Expected number of records based on frequency}
#'   \item{coverage_pct}{Actual vs expected records percentage}
#' }
#'
#' @export
#' @examples
#' \dontrun{
#' data <- download_air_quality_data(
#'   station_id = "468478b7-ace5-4bd3-b89a-a9c1c2e53080",
#'   parameters = c("PM10", "NO2", "O3"),
#'   start_datetime = "2025-03-01 00:00",
#'   end_datetime = "2025-03-31 23:59",
#'   frequency = "hourly"
#' )
#'
#' # Get completeness summary
#' completeness <- assess_completeness(data)
#' print(completeness)
#'
#' # By station (if multiple stations)
#' completeness_by_station <- assess_completeness(data, by_station = TRUE)
#' }
assess_completeness <- function(data,
                                by_parameter = TRUE,
                                by_station = FALSE) {
  if (!inherits(data, "data.frame")) {
    stop("data must be a data frame or tibble", call. = FALSE)
  }

  required_cols <- c("time", "parameter", "value")
  missing_cols <- setdiff(required_cols, names(data))
  if (length(missing_cols) > 0) {
    stop(sprintf("Missing required columns: %s", paste(missing_cols, collapse = ", ")),
      call. = FALSE
    )
  }

  # Handle empty data
  if (nrow(data) == 0) {
    # Return empty tibble with correct structure
    result_cols <- c(
      "total_records", "missing_records", "valid_records",
      "completeness_pct", "start_date", "end_date",
      "expected_records", "coverage_pct"
    )

    if (by_parameter) {
      result_cols <- c("parameter", result_cols)
    }
    if (by_station && "station_id" %in% names(data)) {
      result_cols <- c("station_id", result_cols)
    }

    empty_result <- tibble::tibble(
      !!!setNames(
        rep(list(numeric(0)), length(result_cols) - 2),
        result_cols[!result_cols %in% c("start_date", "end_date")]
      ),
      start_date = as.POSIXct(character(0)),
      end_date = as.POSIXct(character(0))
    )

    return(empty_result)
  }

  # Determine grouping
  group_vars <- character(0)
  if (by_parameter) {
    group_vars <- c(group_vars, "parameter")
  }
  if (by_station && "station_id" %in% names(data)) {
    group_vars <- c(group_vars, "station_id")
  }

  if (length(group_vars) == 0) {
    # No grouping - overall summary
    summary_data <- data |>
      dplyr::summarize(
        total_records = dplyr::n(),
        missing_records = sum(is.na(.data$value)),
        valid_records = sum(!is.na(.data$value)),
        completeness_pct = round(100 * .data$valid_records / .data$total_records, 2),
        start_date = min(.data$time, na.rm = TRUE),
        end_date = max(.data$time, na.rm = TRUE)
      )
  } else {
    # Group by specified variables
    summary_data <- data |>
      dplyr::group_by(dplyr::across(dplyr::all_of(group_vars))) |>
      dplyr::summarize(
        total_records = dplyr::n(),
        missing_records = sum(is.na(.data$value)),
        valid_records = sum(!is.na(.data$value)),
        completeness_pct = round(100 * .data$valid_records / .data$total_records, 2),
        start_date = min(.data$time, na.rm = TRUE),
        end_date = max(.data$time, na.rm = TRUE),
        .groups = "drop"
      )
  }

  # Calculate expected records and coverage
  summary_data <- summary_data |>
    dplyr::mutate(
      time_span_hours = as.numeric(difftime(.data$end_date, .data$start_date, units = "hours")),
      expected_records = .data$time_span_hours + 1, # Assuming hourly data
      coverage_pct = round(100 * .data$total_records / .data$expected_records, 2)
    ) |>
    dplyr::select(-c("time_span_hours"))

  return(summary_data)
}


#' Identify Time Gaps in Air Quality Data
#'
#' Finds gaps in time series data where measurements are missing for extended periods.
#'
#' @param data A tibble containing air quality data with columns: time, parameter, value
#' @param min_gap_hours Minimum gap duration in hours to report. Default is 3 hours.
#' @param by_parameter Logical. If TRUE, identifies gaps separately for each parameter.
#'        Default is TRUE.
#'
#' @return A tibble containing identified gaps:
#' \describe{
#'   \item{parameter}{Parameter name (if by_parameter = TRUE)}
#'   \item{gap_start}{Start time of gap}
#'   \item{gap_end}{End time of gap}
#'   \item{gap_hours}{Duration of gap in hours}
#'   \item{missing_records}{Estimated number of missing records}
#' }
#'
#' @export
#' @examples
#' \dontrun{
#' data <- download_air_quality_data(
#'   station_id = "468478b7-ace5-4bd3-b89a-a9c1c2e53080",
#'   parameters = c("PM10"),
#'   start_datetime = "2025-03-01 00:00",
#'   end_datetime = "2025-03-31 23:59",
#'   frequency = "hourly"
#' )
#'
#' # Find gaps of 6+ hours
#' gaps <- identify_gaps(data, min_gap_hours = 6)
#' print(gaps)
#' }
identify_gaps <- function(data,
                          min_gap_hours = 3,
                          by_parameter = TRUE) {
  if (!inherits(data, "data.frame")) {
    stop("data must be a data frame or tibble", call. = FALSE)
  }

  required_cols <- c("time", "parameter")
  missing_cols <- setdiff(required_cols, names(data))
  if (length(missing_cols) > 0) {
    stop(sprintf("Missing required columns: %s", paste(missing_cols, collapse = ", ")),
      call. = FALSE
    )
  }

  # Sort by time
  data <- data |>
    dplyr::arrange(.data$time)

  if (by_parameter) {
    # Identify gaps for each parameter
    gaps <- data |>
      dplyr::group_by(.data$parameter) |>
      dplyr::mutate(
        time_diff = as.numeric(difftime(dplyr::lead(.data$time), .data$time, units = "hours"))
      ) |>
      dplyr::filter(!is.na(.data$time_diff) & .data$time_diff >= min_gap_hours) |>
      dplyr::mutate(
        gap_start = .data$time,
        gap_end = dplyr::lead(.data$time),
        gap_hours = .data$time_diff,
        missing_records = floor(.data$time_diff)
      ) |>
      dplyr::select("parameter", "gap_start", "gap_end", "gap_hours", "missing_records") |>
      dplyr::ungroup()
  } else {
    # Overall gaps
    gaps <- data |>
      dplyr::mutate(
        time_diff = as.numeric(difftime(dplyr::lead(.data$time), .data$time, units = "hours"))
      ) |>
      dplyr::filter(!is.na(.data$time_diff) & .data$time_diff >= min_gap_hours) |>
      dplyr::mutate(
        gap_start = .data$time,
        gap_end = dplyr::lead(.data$time),
        gap_hours = .data$time_diff,
        missing_records = floor(.data$time_diff)
      ) |>
      dplyr::select("gap_start", "gap_end", "gap_hours", "missing_records")
  }

  return(gaps)
}


#' Generate Data Quality Report
#'
#' Produces a comprehensive data quality report including validity checks,
#' completeness assessment, and gap identification.
#'
#' @param data A tibble containing air quality data with columns: time, parameter, value
#' @param outlier_threshold Number of standard deviations for outlier detection. Default is 5.
#' @param min_gap_hours Minimum gap duration in hours to report. Default is 3.
#'
#' @return A list containing:
#' \describe{
#'   \item{summary}{Overall data quality summary}
#'   \item{completeness}{Completeness metrics by parameter}
#'   \item{invalid_values}{Summary of invalid value counts by type}
#'   \item{gaps}{Identified time gaps}
#'   \item{flagged_data}{Data with quality flags (only invalid records)}
#' }
#'
#' @export
#' @examples
#' \dontrun{
#' data <- download_air_quality_data(
#'   station_id = "468478b7-ace5-4bd3-b89a-a9c1c2e53080",
#'   parameters = c("PM10", "NO2", "O3"),
#'   start_datetime = "2025-03-01 00:00",
#'   end_datetime = "2025-03-31 23:59",
#'   frequency = "hourly"
#' )
#'
#' # Generate quality report
#' report <- quality_report(data)
#'
#' # View summary
#' print(report$summary)
#'
#' # View completeness by parameter
#' print(report$completeness)
#'
#' # View flagged values
#' print(report$flagged_data)
#' }
quality_report <- function(data,
                           outlier_threshold = 5,
                           min_gap_hours = 3) {
  if (!inherits(data, "data.frame")) {
    stop("data must be a data frame or tibble", call. = FALSE)
  }

  # Check for invalid values
  checked_data <- check_invalid_values(data, outlier_threshold = outlier_threshold)

  # Assess completeness
  completeness <- assess_completeness(data, by_parameter = TRUE)

  # Identify gaps
  gaps <- identify_gaps(data, min_gap_hours = min_gap_hours, by_parameter = TRUE)

  # Summary statistics
  invalid_summary <- checked_data |>
    dplyr::group_by(.data$parameter, .data$quality_flag) |>
    dplyr::summarize(count = dplyr::n(), .groups = "drop") |>
    tidyr::pivot_wider(names_from = "quality_flag", values_from = "count", values_fill = 0)

  overall_summary <- tibble::tibble(
    total_records = nrow(data),
    valid_records = sum(checked_data$is_valid, na.rm = TRUE),
    invalid_records = sum(!checked_data$is_valid, na.rm = TRUE),
    validity_pct = round(100 * sum(checked_data$is_valid, na.rm = TRUE) / nrow(data), 2),
    parameters = length(unique(data$parameter)),
    date_range_start = min(data$time, na.rm = TRUE),
    date_range_end = max(data$time, na.rm = TRUE),
    total_gaps = nrow(gaps),
    total_gap_hours = sum(gaps$gap_hours, na.rm = TRUE)
  )

  # Flagged data (only invalid records)
  flagged_data <- checked_data |>
    dplyr::filter(!.data$is_valid) |>
    dplyr::select(
      "time", "parameter", "value", "quality_flag",
      "is_negative", "exceeds_limit", "is_outlier"
    )

  # Return comprehensive report
  report <- list(
    summary = overall_summary,
    completeness = completeness,
    invalid_values = invalid_summary,
    gaps = gaps,
    flagged_data = flagged_data
  )

  class(report) <- c("air_quality_report", "list")

  return(report)
}


#' Print Method for Air Quality Report
#'
#' @param x An air quality report object from quality_report()
#' @param ... Additional arguments (not used)
#'
#' @export
print.air_quality_report <- function(x, ...) {
  cat("=== Air Quality Data Report ===\n\n")

  cat("Overall Summary:\n")
  cat(sprintf("  Total records: %d\n", x$summary$total_records))
  cat(sprintf(
    "  Valid records: %d (%.1f%%)\n",
    x$summary$valid_records, x$summary$validity_pct
  ))
  cat(sprintf("  Invalid records: %d\n", x$summary$invalid_records))
  cat(sprintf("  Parameters: %d\n", x$summary$parameters))
  cat(sprintf(
    "  Date range: %s to %s\n",
    format(x$summary$date_range_start, "%Y-%m-%d"),
    format(x$summary$date_range_end, "%Y-%m-%d")
  ))
  cat(sprintf(
    "  Time gaps: %d (total %.1f hours)\n\n",
    x$summary$total_gaps, x$summary$total_gap_hours
  ))

  cat("Completeness by Parameter:\n")
  print(x$completeness)

  if (nrow(x$invalid_values) > 0) {
    cat("\nInvalid Values by Type:\n")
    print(x$invalid_values)
  }

  if (nrow(x$gaps) > 0) {
    cat(sprintf("\nTime Gaps (>= %d hours):\n", 3))
    print(head(x$gaps, 10))
    if (nrow(x$gaps) > 10) {
      cat(sprintf("  ... and %d more gaps\n", nrow(x$gaps) - 10))
    }
  }

  if (nrow(x$flagged_data) > 0) {
    cat(sprintf("\nFlagged Values: %d records\n", nrow(x$flagged_data)))
    cat("  Use report$flagged_data to view details\n")
  }

  invisible(x)
}
