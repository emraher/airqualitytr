#' Download All Available Data for a Given Station
#'
#' Downloads all available air quality measurements for a specified station by iterating over
#' manageable date ranges. The function automatically retrieves station metadata from the
#' `list_stations()` function and consolidates the chunked downloads into a single tibble.
#'
#' @param station_id A character string representing the station's unique identifier.
#' @param parameters A character vector of pollutant parameters (e.g., \code{c("PM10", "CO", "NO2")}).
#' @param frequency Either \code{"hourly"} or \code{"daily"}. Default is \code{"hourly"}.
#' @param show_progress Logical. If \code{TRUE}, displays informative messages during download. Default is \code{TRUE}.
#' @param chunk_size A character string specifying the size of each download window passed to
#'        \code{seq.POSIXt()} (e.g., \code{"1 month"}, \code{"1 year"}). Defaults to \code{"1 year"}.
#' @param parallel Logical. If \code{TRUE}, downloads will be performed in parallel using the \code{future} and \code{furrr} packages.
#'        You must set up a \code{future} plan before using this option (e.g., \code{future::plan(future::multisession)}).
#'        Default is \code{FALSE}.
#' @param workers Integer. Number of parallel workers to use when \code{parallel = TRUE}. If \code{NULL} (default),
#'        uses the number of workers specified in the active \code{future} plan.
#'
#' @return A tibble containing the available time series air quality data for the given station.
#'         The tibble includes columns such as time, station_id, station_name, parameter, value,
#'         and other metadata columns retrieved from station information. All column names are in snake_case format.
#'
#' @importFrom dplyr bind_rows
#' @importFrom rlang .data
#' @export
#' @examplesIf interactive()
#' # Download all available data for a station
#' all_data <- download_all_data_for_station(
#'   station_id = "468478b7-ace5-4bd3-b89a-a9c1c2e53080",
#'   parameters = c("PM10", "NO2"),
#'   frequency = "hourly",
#'   chunk_size = "3 months"
#' )
download_all_data_for_station <- function(station_id,
                                          parameters,
                                          frequency = c("hourly", "daily"),
                                          show_progress = TRUE,
                                          chunk_size = "1 year",
                                          parallel = FALSE,
                                          workers = NULL) {
  # Validate input parameters
  validate_station_id(station_id)
  validate_parameters(parameters)
  frequency <- match.arg(frequency)
  validate_frequency(frequency)

  # Check for parallel processing requirements
  if (parallel) {
    if (!requireNamespace("future", quietly = TRUE) || !requireNamespace("furrr", quietly = TRUE)) {
      stop("Parallel processing requires the 'future' and 'furrr' packages.\n",
        "  - Install them with: install.packages(c('future', 'furrr'))\n",
        "  - Or set parallel = FALSE to use sequential processing.",
        call. = FALSE
      )
    }

    # Set up future plan if workers specified
    if (!is.null(workers)) {
      old_plan <- future::plan()
      on.exit(future::plan(old_plan), add = TRUE)
      future::plan(future::multisession, workers = workers)
    }

    # Check if progressr is available for parallel progress
    has_progressr <- requireNamespace("progressr", quietly = TRUE) && show_progress
  }

  # Set a broad default date range to attempt to retrieve all available data
  # Using historical start date and future end date to capture all possible records
  start_datetime <- as.POSIXct("1900-01-01 00:00", tz = "UTC")
  end_datetime <- min(as.POSIXct("2099-12-31 23:59", tz = "UTC"), Sys.time())

  # Verify station exists in metadata
  stations_meta <- list_stations()
  station_exists <- any(stations_meta$station_id == station_id)

  if (!station_exists) {
    warning(
      sprintf(
        "Station ID '%s' not found in current station metadata.\n",
        station_id
      ),
      "  - This station may be inactive, decommissioned, or the ID may be incorrect.\n",
      "  - Use list_stations() to see all available stations.\n",
      "  - Data download will proceed but may return no results.",
      call. = FALSE
    )
  }

  # Inform user about the download
  if (show_progress && !parallel) {
    station_name <- if (station_exists) {
      stations_meta$station_name[stations_meta$station_id == station_id][1]
    } else {
      station_id
    }
    message(sprintf(
      "Downloading all available %s data for station: %s",
      frequency, station_name
    ))
    message("This may take a while for stations with long historical records...")
  }

  # Build chunk boundaries to avoid extremely large API calls
  chunk_boundaries <- seq(from = start_datetime, to = end_datetime, by = chunk_size)
  if (length(chunk_boundaries) == 0 || tail(chunk_boundaries, 1) < end_datetime) {
    chunk_boundaries <- c(chunk_boundaries, end_datetime)
  }
  if (length(chunk_boundaries) == 1) {
    chunk_boundaries <- c(chunk_boundaries, end_datetime)
  }

  chunk_starts <- head(chunk_boundaries, -1)
  chunk_ends <- chunk_boundaries[-1] - 1
  chunk_ends[length(chunk_ends)] <- end_datetime

  # Create safe download function for chunks
  safe_chunk_download <- function(chunk_start, chunk_end) {
    if (chunk_start > end_datetime) {
      return(tibble::tibble())
    }

    tryCatch(
      {
        download_air_quality_data(
          station_id = station_id,
          parameters = parameters,
          start_datetime = format(chunk_start, "%Y-%m-%d %H:%M"),
          end_datetime = format(chunk_end, "%Y-%m-%d %H:%M"),
          frequency = frequency,
          return_all = FALSE
        )
      },
      error = function(e) {
        tibble::tibble()
      }
    )
  }

  # Execute downloads (parallel or sequential)
  if (parallel) {
    # Parallel execution with furrr
    n_chunks <- length(chunk_starts)
    if (has_progressr) {
      # Use progressr for parallel progress tracking
      results <- progressr::with_progress({
        p <- progressr::progressor(steps = n_chunks)
        furrr::future_map2(chunk_starts, chunk_ends, function(cs, ce) {
          result <- safe_chunk_download(cs, ce)
          p(sprintf(
            "Chunk %s to %s",
            format(cs, "%Y-%m-%d"),
            format(ce, "%Y-%m-%d")
          ))
          result
        }, .options = furrr::furrr_options(seed = TRUE))
      })
    } else {
      # Parallel without progress
      results <- furrr::future_map2(chunk_starts, chunk_ends, function(cs, ce) {
        safe_chunk_download(cs, ce)
      }, .options = furrr::furrr_options(seed = TRUE))
    }

    # Filter out empty results
    results <- results[sapply(results, nrow) > 0]
  } else {
    # Sequential execution with messages
    results <- list()
    chunk_index <- 1L

    for (i in seq_along(chunk_starts)) {
      chunk_start <- chunk_starts[i]
      chunk_end <- chunk_ends[i]

      if (chunk_start > end_datetime) {
        break
      }

      if (show_progress) {
        message(sprintf(
          "Fetching data from %s to %s",
          format(chunk_start, "%Y-%m-%d %H:%M"),
          format(chunk_end, "%Y-%m-%d %H:%M")
        ))
      }

      chunk_data <- safe_chunk_download(chunk_start, chunk_end)

      if (nrow(chunk_data) > 0) {
        results[[chunk_index]] <- chunk_data
        chunk_index <- chunk_index + 1L
      }
    }
  }

  # Combine results
  if (length(results) > 0) {
    result <- dplyr::bind_rows(results)
  } else {
    result <- tibble::tibble()
  }

  # Inform user about completion
  if (show_progress && nrow(result) > 0) {
    date_range <- range(result$time, na.rm = TRUE)
    message(sprintf(
      "Download complete: %d records from %s to %s",
      nrow(result),
      format(date_range[1], "%Y-%m-%d"),
      format(date_range[2], "%Y-%m-%d")
    ))
  }

  # Return the result
  return(result)
}
