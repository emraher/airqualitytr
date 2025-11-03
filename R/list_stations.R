#' List Available Stations
#'
#' Retrieves a metadata table of available stations from the air quality service.
#'
#' @return A tibble of station metadata.
#' @export
#' @examplesIf interactive()
#' # Get all available stations
#' stations <- list_stations()
#' head(stations)
#'
#' # Filter stations by city
#' library(dplyr)
#' ankara_stations <- stations |>
#'   filter(grepl("Ankara", city_name, ignore.case = TRUE))
list_stations <- function() {
  # Define the API endpoint
  url <- "https://sim.csb.gov.tr/STN/STN_Report/StationDataDownloadNewDefaults"

  # Build payload and make request
  payload <- build_defaults_payload()
  parsed <- post_to_defaults_endpoint(url, payload)

  # Extract and join station metadata
  # Base station data
  StationIds <- parsed$Object$StationIds

  # Join with city information
  CityId <- parsed$Object$CityId |>
    dplyr::rename_with(~ c("CityId", "CityName"))

  StationIds <- StationIds |>
    dplyr::left_join(CityId, by = "CityId") |>
    dplyr::relocate(CityName, .after = "CityId")

  # Join with company information
  StationCompanyId <- parsed$Object$StationCompanyId |>
    dplyr::rename_with(~ c("CompanyId", "CompanyIdName")) |>
    dplyr::distinct()

  StationIds <- StationIds |>
    dplyr::left_join(StationCompanyId, by = "CompanyId") |>
    dplyr::relocate(CompanyIdName, .after = "CompanyId")

  # Join with station group information
  StationGroupId <- parsed$Object$StationGroupId |>
    dplyr::rename_with(~ c("StationGroup", "StationGroupName")) |>
    dplyr::distinct() |>
    dplyr::mutate(StationGroup = as.integer(.data$StationGroup))

  StationIds <- StationIds |>
    dplyr::left_join(StationGroupId, by = "StationGroup") |>
    dplyr::relocate(StationGroupName, .after = "StationGroup")

  # Join with sector information
  SectorId <- parsed$Object$SectorId |>
    dplyr::rename_with(~ c("SectorCode", "SectorCodeName")) |>
    dplyr::distinct()

  StationIds <- StationIds |>
    dplyr::left_join(SectorId, by = "SectorCode") |>
    dplyr::relocate(SectorCodeName, .after = "SectorCode")

  # Join with source type information
  SourceType <- parsed$Object$SourceType |>
    dplyr::rename_with(~ c("SourceType", "SourceTypeName")) |>
    dplyr::distinct()

  StationIds <- StationIds |>
    dplyr::left_join(SourceType, by = "SourceType") |>
    dplyr::relocate(SourceTypeName, .after = "SourceType")

  # Join with area type information
  AreaType <- parsed$Object$AreaType |>
    dplyr::rename_with(~ c("AreaType", "AreaTypeName")) |>
    dplyr::distinct()

  StationIds <- StationIds |>
    dplyr::left_join(AreaType, by = "AreaType") |>
    dplyr::relocate(AreaTypeName, .after = "AreaType")

  # Final cleanup - rename columns and remove unused ones
  StationIds <- StationIds |>
    dplyr::rename("StationId" = "id", "StationName" = "Name") |>
    dplyr::select(-.data$Station_Title) |>
    dplyr::select(.data$StationId, tidyselect::everything())

  # Standardize all column names to snake_case
  StationIds <- standardize_column_names(tibble::as_tibble(StationIds))

  # Return the result
  return(StationIds)
}
