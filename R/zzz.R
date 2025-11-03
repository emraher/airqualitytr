#' @importFrom httr POST GET status_code content add_headers timeout
#' @importFrom jsonlite fromJSON
#' @importFrom rvest session html_node html_attr
#' @importFrom tidyr pivot_longer pivot_wider
#' @importFrom rlang .data
#' @importFrom stats sd setNames time
#' @importFrom utils head tail
NULL



if (getRversion() >= "2.15.1") {
  utils::globalVariables(c(
    # PascalCase variables from API JSON responses (not used in dplyr data masking)
    "Id", "Name", "ReadTime", "AreaTypeName", "CityName",
    "CompanyId", "CompanyIdName", "IsPortable", "SectorCode", "SectorCodeName",
    "SourceTypeName", "StationGroupName", "StationName", "Stationid"
    # Note: Variables previously listed here are now explicitly referenced with .data$
    # in dplyr/tidyr chains to follow tidyverse best practices
  ))
}
