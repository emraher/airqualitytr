#' @import httr
#' @importFrom jsonlite fromJSON
#' @import rvest
#' @import tidyr
#' @importFrom rlang .data
#' @importFrom stats sd setNames time
#' @importFrom utils head tail
NULL



if (getRversion() >= "2.15.1") {
  utils::globalVariables(c(
    # Original variables
    "Id", "Name", "ReadTime", "parameter", "value", "AreaTypeName", "CityName",
    "CompanyId", "CompanyIdName", "IsPortable", "SectorCode", "SectorCodeName",
    "SourceTypeName", "StationGroup", "StationGroupName", "StationName",
    "Station_Title", "Stationid", "city_name", "station_id", "station_name",
    "id", "name", ".data",
    # NSE variables used in dplyr/tidyr chains
    "station_title", "is_portable", "StationId", "time", "time_diff",
    "valid_records", "total_records", "start_date", "end_date",
    "time_span_hours", "expected_records", "limit", "param_mean",
    "param_sd", "z_score", "quality_flag", "is_valid"
  ))
}
