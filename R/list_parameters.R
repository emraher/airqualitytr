#' List Available Measurement Parameters
#'
#' Retrieves a list of measurement parameters available for querying.
#'
#' @return A tibble with details on available parameters.
#' @export
#' @examples
#' \donttest{
#' # Get all available parameters
#' params <- list_parameters()
#' print(params)
#' }
list_parameters <- function() {
  # Define the API endpoint
  url <- "https://sim.csb.gov.tr/STN/STN_Report/StationDataDownloadNewDefaults"

  # Build payload and make request
  payload <- build_defaults_payload()
  parsed <- post_to_defaults_endpoint(url, payload)

  # Extract parameter data
  parameters <- parsed$Object$Parameters

  # Standardize column names to snake_case and return
  return(standardize_column_names(tibble::as_tibble(parameters)))
}
