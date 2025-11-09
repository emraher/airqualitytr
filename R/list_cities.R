#' List Available Cities
#'
#' Retrieves a list of cities available for filtering air quality data.
#'
#' @return A tibble with columns \code{city_id} and \code{city_name}.
#' @importFrom dplyr rename
#' @export
#' @examples
#' \donttest{
#' # Get all available cities
#' cities <- list_cities()
#' head(cities)
#' }
list_cities <- function() {
  # Define the API endpoint
  url <- "https://sim.csb.gov.tr/STN/STN_Report/StationDataDownloadNewDefaults"

  # Build payload and make request
  payload <- build_defaults_payload()
  parsed <- post_to_defaults_endpoint(url, payload)

  # Extract city data
  city_data <- parsed$Object$CityId

  # Format and return the result
  return(tibble::as_tibble(city_data) |>
           dplyr::rename(city_id = Id, city_name = Name))
}
