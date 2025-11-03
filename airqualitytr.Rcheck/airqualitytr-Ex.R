pkgname <- "airqualitytr"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
base::assign(".ExTimings", "airqualitytr-Ex.timings", pos = 'CheckExEnv')
base::cat("name\tuser\tsystem\telapsed\n", file=base::get(".ExTimings", pos = 'CheckExEnv'))
base::assign(".format_ptime",
function(x) {
  if(!is.na(x[4L])) x[1L] <- x[1L] + x[4L]
  if(!is.na(x[5L])) x[2L] <- x[2L] + x[5L]
  options(OutDec = '.')
  format(x[1L:3L], digits = 7L)
},
pos = 'CheckExEnv')

### * </HEADER>
library('airqualitytr')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
base::assign(".old_wd", base::getwd(), pos = 'CheckExEnv')
cleanEx()
nameEx("assess_completeness")
### * assess_completeness

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: assess_completeness
### Title: Assess Data Completeness
### Aliases: assess_completeness

### ** Examples

## Not run: 
##D data <- download_air_quality_data(
##D   station_id = "468478b7-ace5-4bd3-b89a-a9c1c2e53080",
##D   parameters = c("PM10", "NO2", "O3"),
##D   start_datetime = "2025-03-01 00:00",
##D   end_datetime = "2025-03-31 23:59",
##D   frequency = "hourly"
##D )
##D 
##D # Get completeness summary
##D completeness <- assess_completeness(data)
##D print(completeness)
##D 
##D # By station (if multiple stations)
##D completeness_by_station <- assess_completeness(data, by_station = TRUE)
## End(Not run)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("assess_completeness", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("bulk_download_air_quality_data")
### * bulk_download_air_quality_data

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: bulk_download_air_quality_data
### Title: Bulk Download Air Quality Data for All Stations
### Aliases: bulk_download_air_quality_data

### ** Examples

## Don't show: 
if (interactive()) (if (getRversion() >= "3.4") withAutoprint else force)({ # examplesIf
## End(Don't show)
# Download one hour of data for all stations (small example)
bulk_data <- bulk_download_air_quality_data(
  start_datetime = "2024-03-01 00:00",
  end_datetime = "2024-03-01 01:00",
  parameters = c("PM10"),
  frequency = "hourly"
)
## Don't show: 
}) # examplesIf
## End(Don't show)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("bulk_download_air_quality_data", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("check_invalid_values")
### * check_invalid_values

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: check_invalid_values
### Title: Check for Invalid Values in Air Quality Data
### Aliases: check_invalid_values

### ** Examples

## Not run: 
##D data <- download_air_quality_data(
##D   station_id = "468478b7-ace5-4bd3-b89a-a9c1c2e53080",
##D   parameters = c("PM10", "NO2"),
##D   start_datetime = "2025-03-01 00:00",
##D   end_datetime = "2025-03-07 23:59"
##D )
##D 
##D # Check data quality
##D checked_data <- check_invalid_values(data)
##D 
##D # See flagged values
##D flagged <- checked_data |> dplyr::filter(!is_valid)
## End(Not run)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("check_invalid_values", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("download_air_quality_data")
### * download_air_quality_data

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: download_air_quality_data
### Title: Download Air Quality Data with Additional Objects
### Aliases: download_air_quality_data

### ** Examples

## Don't show: 
if (interactive()) (if (getRversion() >= "3.4") withAutoprint else force)({ # examplesIf
## End(Don't show)
# Download one week of hourly PM10 data
data <- download_air_quality_data(
  station_id = "468478b7-ace5-4bd3-b89a-a9c1c2e53080",
  parameters = c("PM10"),
  start_datetime = "2024-03-01 00:00",
  end_datetime = "2024-03-07 23:59",
  frequency = "hourly"
)
head(data)

# Download multiple parameters
multi_data <- download_air_quality_data(
  station_id = "468478b7-ace5-4bd3-b89a-a9c1c2e53080",
  parameters = c("PM10", "NO2", "O3"),
  start_datetime = "2024-03-01 00:00",
  end_datetime = "2024-03-07 23:59",
  frequency = "hourly"
)
## Don't show: 
}) # examplesIf
## End(Don't show)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("download_air_quality_data", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("download_all_data_for_station")
### * download_all_data_for_station

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: download_all_data_for_station
### Title: Download All Available Data for a Given Station
### Aliases: download_all_data_for_station

### ** Examples

## Don't show: 
if (interactive()) (if (getRversion() >= "3.4") withAutoprint else force)({ # examplesIf
## End(Don't show)
# Download all available data for a station
all_data <- download_all_data_for_station(
  station_id = "468478b7-ace5-4bd3-b89a-a9c1c2e53080",
  parameters = c("PM10", "NO2"),
  frequency = "hourly",
  chunk_size = "3 months"
)
## Don't show: 
}) # examplesIf
## End(Don't show)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("download_all_data_for_station", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("get_api_status")
### * get_api_status

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: get_api_status
### Title: Get API Status Information
### Aliases: get_api_status

### ** Examples

## Not run: 
##D status <- get_api_status()
##D print(status)
## End(Not run)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("get_api_status", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("identify_gaps")
### * identify_gaps

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: identify_gaps
### Title: Identify Time Gaps in Air Quality Data
### Aliases: identify_gaps

### ** Examples

## Not run: 
##D data <- download_air_quality_data(
##D   station_id = "468478b7-ace5-4bd3-b89a-a9c1c2e53080",
##D   parameters = c("PM10"),
##D   start_datetime = "2025-03-01 00:00",
##D   end_datetime = "2025-03-31 23:59",
##D   frequency = "hourly"
##D )
##D 
##D # Find gaps of 6+ hours
##D gaps <- identify_gaps(data, min_gap_hours = 6)
##D print(gaps)
## End(Not run)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("identify_gaps", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("list_cities")
### * list_cities

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: list_cities
### Title: List Available Cities
### Aliases: list_cities

### ** Examples

## Don't show: 
if (interactive()) (if (getRversion() >= "3.4") withAutoprint else force)({ # examplesIf
## End(Don't show)
# Get all available cities
cities <- list_cities()
head(cities)
## Don't show: 
}) # examplesIf
## End(Don't show)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("list_cities", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("list_parameters")
### * list_parameters

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: list_parameters
### Title: List Available Measurement Parameters
### Aliases: list_parameters

### ** Examples

## Don't show: 
if (interactive()) (if (getRversion() >= "3.4") withAutoprint else force)({ # examplesIf
## End(Don't show)
# Get all available parameters
params <- list_parameters()
print(params)
## Don't show: 
}) # examplesIf
## End(Don't show)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("list_parameters", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("list_stations")
### * list_stations

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: list_stations
### Title: List Available Stations
### Aliases: list_stations

### ** Examples

## Don't show: 
if (interactive()) (if (getRversion() >= "3.4") withAutoprint else force)({ # examplesIf
## End(Don't show)
# Get all available stations
stations <- list_stations()
head(stations)

# Filter stations by city
library(dplyr)
ankara_stations <- stations |>
  filter(grepl("Ankara", city_name, ignore.case = TRUE))
## Don't show: 
}) # examplesIf
## End(Don't show)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("list_stations", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("quality_report")
### * quality_report

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: quality_report
### Title: Generate Data Quality Report
### Aliases: quality_report

### ** Examples

## Not run: 
##D data <- download_air_quality_data(
##D   station_id = "468478b7-ace5-4bd3-b89a-a9c1c2e53080",
##D   parameters = c("PM10", "NO2", "O3"),
##D   start_datetime = "2025-03-01 00:00",
##D   end_datetime = "2025-03-31 23:59",
##D   frequency = "hourly"
##D )
##D 
##D # Generate quality report
##D report <- quality_report(data)
##D 
##D # View summary
##D print(report$summary)
##D 
##D # View completeness by parameter
##D print(report$completeness)
##D 
##D # View flagged values
##D print(report$flagged_data)
## End(Not run)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("quality_report", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("test_api_connection")
### * test_api_connection

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: test_api_connection
### Title: Test API Connection
### Aliases: test_api_connection

### ** Examples

## Not run: 
##D # Test API connection
##D test_result <- test_api_connection()
##D 
##D # Quick check
##D if (test_result$overall_status == "success") {
##D   message("API is accessible")
##D }
##D 
##D # Detailed diagnostics
##D print(test_result$tests)
## End(Not run)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("test_api_connection", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
### * <FOOTER>
###
cleanEx()
options(digits = 7L)
base::cat("Time elapsed: ", proc.time() - base::get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')
