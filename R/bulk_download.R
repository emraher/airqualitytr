#' Bulk Download Air Quality Data for All Stations
#'
#' Downloads time series air quality data (e.g., hourly measurements) for all available stations
#' over a specified date range and for the given set of measurement parameters. This function retrieves
#' station metadata via \code{list_stations()} and the available cities via \code{list_cities()}, then
#' for each station downloads the measurement data using \code{download_air_quality_data()}.
#'
#' @param start_datetime Start date-time in "YYYY-MM-DD HH:MM" format.
#' @param end_datetime End date-time in "YYYY-MM-DD HH:MM" format.
#' @param parameters A character vector of pollutant parameters (e.g., \code{c("PM10", "CO", "NO2")}). Default is \code{c("PM10", "CO", "NO2")}.
#' @param frequency Either \code{"hourly"} or \code{"daily"}. Default is \code{"hourly"}.
#' @param show_progress Logical. If \code{TRUE} and the \code{cli} package is available, displays a progress bar. Default is \code{TRUE}.
#' @param parallel Logical. If \code{TRUE}, downloads will be performed in parallel using the \code{future} and \code{furrr} packages.
#'        You must set up a \code{future} plan before using this option (e.g., \code{future::plan(future::multisession)}).
#'        Default is \code{FALSE}.
#' @param workers Integer. Number of parallel workers to use when \code{parallel = TRUE}. If \code{NULL} (default),
#'        uses the number of workers specified in the active \code{future} plan.
#'
#' @return A tibble containing the combined time series data from all stations. The tibble includes:
#' \describe{
#'   \item{time}{POSIXct timestamp of the measurement.}
#'   \item{station_id}{Station identifier.}
#'   \item{city_name}{City name for the station.}
#'   \item{station_name}{Station name.}
#'   \item{parameter}{Measurement parameter (e.g., "PM10").}
#'   \item{value}{The corresponding measurement value.}
#'   \item{...}{Additional metadata columns in snake_case (city_id, area_type, etc.).}
#' }
#'
#' @export
#' @examples
#' \dontrun{
#' # Download one hour of data for all stations (small example)
#' bulk_data <- bulk_download_air_quality_data(
#'   start_datetime = "2024-03-01 00:00",
#'   end_datetime = "2024-03-01 01:00",
#'   parameters = c("PM10"),
#'   frequency = "hourly"
#' )
#' }
bulk_download_air_quality_data <- function(start_datetime,
                                           end_datetime,
                                           parameters = c("PM10", "CO", "NO2"),
                                           frequency = "hourly",
                                           show_progress = TRUE,
                                           parallel = FALSE,
                                           workers = NULL) {
  # Validate input parameters
  validate_parameters(parameters)
  validate_date_range(start_datetime, end_datetime)
  validate_frequency(frequency)

  # Setup parallel processing
  parallel_config <- setup_parallel_processing(parallel, workers, show_progress)
  on.exit(parallel_config$cleanup(), add = TRUE)
  has_progressr <- parallel_config$has_progressr

  # Check if cli is available for progress bars (sequential mode)
  has_cli <- requireNamespace("cli", quietly = TRUE) && show_progress && !parallel

  # Retrieve station metadata using list_stations()
  stns <- list_stations()

  # If stations data contain a city_id, retrieve and join available cities (if not already joined).
  # Note: list_stations() now returns snake_case column names
  if ("city_id" %in% names(stns)) {
    # city info may already be joined, check for city_name
    if (!"city_name" %in% names(stns)) {
      cities <- list_cities()
      stns <- stns |>
        dplyr::left_join(cities, by = "city_id")
    }
  } else {
    stns <- stns |> dplyr::mutate(city_name = NA_character_)
  }

  # Station metadata from list_stations() now has snake_case names
  # Ensure we have station_id and station_name columns for the loop
  if (!"station_id" %in% names(stns) && "id" %in% names(stns)) {
    stns <- stns |> dplyr::mutate(station_id = .data$id)
  }

  if (!"station_name" %in% names(stns)) {
    if ("station_title" %in% names(stns)) {
      stns <- stns |> dplyr::mutate(station_name = .data$station_title)
    } else if ("name" %in% names(stns)) {
      stns <- stns |> dplyr::mutate(station_name = .data$name)
    } else {
      stns <- stns |> dplyr::mutate(station_name = .data$station_id)
    }
  }

  # Create a safe version of the download function
  safe_download <- function(st_id, st_name, ct_name) {
    tryCatch(
      {
        download_air_quality_data(
          station_id = st_id,
          parameters = parameters,
          start_datetime = start_datetime,
          end_datetime = end_datetime,
          frequency = frequency,
          return_all = FALSE
        )
      },
      error = function(e) {
        tibble::tibble()
      }
    )
  }

  # Prepare station data
  station_data <- stns |> dplyr::select("station_id", "station_name", "city_name")
  n_stations <- nrow(station_data)

  # Execute downloads (parallel or sequential)
  if (parallel) {
    # Parallel execution with furrr
    if (has_progressr) {
      # Use progressr for parallel progress tracking
      results <- progressr::with_progress({
        p <- progressr::progressor(steps = n_stations)
        furrr::future_map(seq_len(n_stations), function(i) {
          result <- safe_download(
            station_data$station_id[i],
            station_data$station_name[i],
            station_data$city_name[i]
          )
          p(sprintf("Station %d/%d", i, n_stations))
          result
        }, .options = furrr::furrr_options(seed = TRUE))
      })
    } else {
      # Parallel without progress
      results <- furrr::future_map(seq_len(n_stations), function(i) {
        safe_download(
          station_data$station_id[i],
          station_data$station_name[i],
          station_data$city_name[i]
        )
      }, .options = furrr::furrr_options(seed = TRUE))
    }
  } else {
    # Sequential execution with cli progress
    if (has_cli) {
      cli::cli_progress_bar(
        "Downloading data from stations",
        total = n_stations,
        format = "{cli::pb_spin} Downloading station {cli::pb_current}/{cli::pb_total} | {cli::pb_bar} {cli::pb_percent} | ETA: {cli::pb_eta}"
      )
    }

    results <- list()
    for (i in seq_len(n_stations)) {
      # Download data for this station
      results[[i]] <- safe_download(
        station_data$station_id[i],
        station_data$station_name[i],
        station_data$city_name[i]
      )

      # Update progress bar
      if (has_cli) {
        cli::cli_progress_update()
      }
    }

    # Complete progress bar
    if (has_cli) {
      cli::cli_progress_done()
    }
  }

  # Combine all results
  combined_data <- dplyr::bind_rows(results)

  return(combined_data)
}
