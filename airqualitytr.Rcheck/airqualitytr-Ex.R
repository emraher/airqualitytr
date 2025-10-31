pkgname <- "airqualitytr"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
library('airqualitytr')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
base::assign(".old_wd", base::getwd(), pos = 'CheckExEnv')
cleanEx()
nameEx("assess_completeness")
### * assess_completeness

flush(stderr()); flush(stdout())

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



cleanEx()
nameEx("check_invalid_values")
### * check_invalid_values

flush(stderr()); flush(stdout())

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



cleanEx()
nameEx("get_api_status")
### * get_api_status

flush(stderr()); flush(stdout())

### Name: get_api_status
### Title: Get API Status Information
### Aliases: get_api_status

### ** Examples

## Not run: 
##D status <- get_api_status()
##D print(status)
## End(Not run)



cleanEx()
nameEx("identify_gaps")
### * identify_gaps

flush(stderr()); flush(stdout())

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



cleanEx()
nameEx("quality_report")
### * quality_report

flush(stderr()); flush(stdout())

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



cleanEx()
nameEx("test_api_connection")
### * test_api_connection

flush(stderr()); flush(stdout())

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
