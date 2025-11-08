
<!-- README.md is generated from README.Rmd. Please edit that file -->

# airqualitytr

<!-- badges: start -->

[![CRAN
status](https://www.r-pkg.org/badges/version/airqualitytr)](https://CRAN.R-project.org/package=airqualitytr)
[![R-CMD-check](https://github.com/emraher/airqualitytr/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/emraher/airqualitytr/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

**airqualitytr** is an R package that provides an interface to download
and explore air quality data from the Turkish Ministry of Environment,
Urbanization and Climate Change. It allows users to:

- Download time series air quality data for a specified station and
  parameters.
- Retrieve the lists of available cities.
- Retrieve station metadata.
- Retrieve available measurement parameters.

These functions enable researchers and analysts to quickly access and
analyze environmental data in their R workflow.

**Note:** This package was initially created by Emrah Er, with
subsequent improvements made using [Claude
Code](https://claude.com/claude-code).

## Installation

You can install the development version from GitHub using the **pak**
package:

``` r
pak::pak("emraher/airqualitytr")
```

## Usage

Load the package and explore the available options:

### List Available Cities

Retrieve a tibble of cities available for filtering:

``` r
library(airqualitytr)
library(dplyr)
#> 
#> Attaching package: 'dplyr'
#> The following objects are masked from 'package:stats':
#> 
#>     filter, lag
#> The following objects are masked from 'package:base':
#> 
#>     intersect, setdiff, setequal, union

(cities <- list_cities())
#> # A tibble: 81 × 2
#>    city_id                              city_name     
#>    <chr>                                <chr>         
#>  1 fddbbf7e-b853-431e-89d3-3522264e9383 Adana         
#>  2 50cd2278-5573-4069-b7a9-2aacaf11431a Adıyaman      
#>  3 c9b19ecd-1ffc-4616-ab1a-790aaf539bfe Afyonkarahisar
#>  4 12b12d2f-1fd1-41a5-9142-7fe31e5134e3 Ağrı          
#>  5 0d977726-b21e-4ef8-b852-e659286f1778 Aksaray       
#>  6 b1bd61cf-1528-4a3e-9b45-c30cbf223035 Amasya        
#>  7 8dbbe10c-b2c5-44c1-bb3e-90be45aee744 Ankara        
#>  8 c414e607-cb0f-4226-997c-62a9eb563b2b Antalya       
#>  9 d40a8bdb-1402-43e6-9e92-1f1c8484632c Ardahan       
#> 10 a0da922a-ff51-4e7d-9f75-7fe44217d725 Artvin        
#> # ℹ 71 more rows
```

### List Available Stations

Get metadata for all available stations:

``` r
stations <- list_stations()
head(stations)
#> # A tibble: 6 × 20
#>   station_id    station_name city_id city_name town_id location station_sub_type
#>   <chr>         <chr>        <chr>   <chr>     <chr>   <chr>               <int>
#> 1 468478b7-ace… Adana - Çat… fddbbf… Adana     f23102… POINT (…                1
#> 2 9a381f61-657… Adana - Çuk… fddbbf… Adana     f2c0da… POINT (…               23
#> 3 bdc1f3d0-004… Adana - Doğ… fddbbf… Adana     fb3596… POINT (…                1
#> 4 c31a108f-90b… Adana - Met… fddbbf… Adana     fb3596… POINT (…                1
#> 5 c75060c5-d28… Adana - Val… fddbbf… Adana     f2c0da… POINT (…                1
#> 6 2b79754a-c5f… Adana-Seyhan fddbbf… Adana     f2c0da… POINT (…               23
#> # ℹ 13 more variables: company_id <chr>, company_id_name <chr>,
#> #   station_group <int>, station_group_name <chr>, sector_code <int>,
#> #   sector_code_name <chr>, basin_id <lgl>, data_periods <int>,
#> #   area_type <int>, area_type_name <chr>, source_type <int>,
#> #   source_type_name <chr>, is_portable <lgl>
```

### List Available Measurement Parameters

Retrieve a tibble of available measurement parameters (e.g., PM10, CO,
NO2):

``` r
(params <- list_parameters())
#> # A tibble: 8 × 2
#>   id    name  
#>   <chr> <chr> 
#> 1 PM10  PM10  
#> 2 PM25  PM 2.5
#> 3 SO2   SO2   
#> 4 CO    CO    
#> 5 NO2   NO2   
#> 6 NOX   NOX   
#> 7 NO    NO    
#> 8 O3    O3
```

### Download Air Quality Data

Download time series air quality data for a specific station. You must
provide the station’s unique identifier, select one or more measurement
parameters, specify a date range, and choose the data frequency (hourly
or daily).

Here is an example:

``` r
# Example inputs:
station_id <- "468478b7-ace5-4bd3-b89a-a9c1c2e53080"
parameters <- c("PM10", "CO", "NO2")
start_datetime <- "2025-03-16 00:00"
end_datetime <- "2025-04-15 00:00"
frequency <- "hourly"

data <- download_air_quality_data(
  station_id = station_id,
  parameters = parameters,
  start_datetime = start_datetime,
  end_datetime = end_datetime,
  frequency = frequency
)
head(data)
#> # A tibble: 6 × 23
#>   time                station_id station_name city_id city_name town_id location
#>   <dttm>              <chr>      <chr>        <chr>   <chr>     <chr>   <chr>   
#> 1 2025-03-16 00:00:00 468478b7-… Adana - Çat… fddbbf… Adana     f23102… POINT (…
#> 2 2025-03-16 00:00:00 468478b7-… Adana - Çat… fddbbf… Adana     f23102… POINT (…
#> 3 2025-03-16 00:00:00 468478b7-… Adana - Çat… fddbbf… Adana     f23102… POINT (…
#> 4 2025-03-16 01:00:00 468478b7-… Adana - Çat… fddbbf… Adana     f23102… POINT (…
#> 5 2025-03-16 01:00:00 468478b7-… Adana - Çat… fddbbf… Adana     f23102… POINT (…
#> 6 2025-03-16 01:00:00 468478b7-… Adana - Çat… fddbbf… Adana     f23102… POINT (…
#> # ℹ 16 more variables: station_sub_type <int>, company_id <chr>,
#> #   company_id_name <chr>, station_group <int>, station_group_name <chr>,
#> #   sector_code <int>, sector_code_name <chr>, basin_id <lgl>,
#> #   data_periods <int>, area_type <int>, area_type_name <chr>,
#> #   source_type <int>, source_type_name <chr>, is_portable <lgl>,
#> #   parameter <chr>, value <dbl>
```

The resulting tibble contains:

- **time:** POSIXct timestamp of the measurement.
- **station_id:** Station identifier from the API response.
- **city_name:** The city name provided.
- **station_name:** The station name provided.
- **parameter:** The measurement parameter (e.g., “PM10”).
- **value:** The corresponding measurement value.

### Bulk Download

``` r
# Define the date range and parameters
start_datetime <- "2025-03-16 00:00"
end_datetime <- "2025-03-16 01:00" # using a short interval for testing
parameters <- c("PM10")
frequency <- "hourly"

# Bulk download air quality data for all stations
bulk_data <- bulk_download_air_quality_data(
  start_datetime = start_datetime,
  end_datetime = end_datetime,
  parameters = parameters,
  frequency = frequency
)

# Inspect the combined data
glimpse(bulk_data)
```

### Parallel Bulk Downloads

For faster bulk downloads, you can use parallel processing. This is
especially useful when downloading data from many stations or large date
ranges:

``` r
# Install parallel processing packages if not already installed
# install.packages(c("future", "furrr", "progressr"))

library(future)
library(progressr)

# Set up parallel processing with 4 workers
plan(multisession, workers = 4)

# Bulk download with parallel processing
start_datetime <- "2025-03-16 00:00"
end_datetime <- "2025-03-16 01:00"
parameters <- c("PM10", "NO2")
frequency <- "hourly"

# Option 1: Let the function manage the parallel plan
bulk_data <- bulk_download_air_quality_data(
  start_datetime = start_datetime,
  end_datetime = end_datetime,
  parameters = parameters,
  frequency = frequency,
  parallel = TRUE,
  workers = 4 # Number of parallel workers
)

# Option 2: Set up your own future plan
plan(multisession, workers = 4)
bulk_data <- bulk_download_air_quality_data(
  start_datetime = start_datetime,
  end_datetime = end_datetime,
  parameters = parameters,
  frequency = frequency,
  parallel = TRUE # Uses your existing plan
)
plan(sequential) # Reset to sequential processing

# With progress tracking (requires progressr package)
handlers(global = TRUE) # Enable progress handlers
handlers("progress") # Use progress bar

with_progress({
  bulk_data <- bulk_download_air_quality_data(
    start_datetime = start_datetime,
    end_datetime = end_datetime,
    parameters = parameters,
    frequency = frequency,
    parallel = TRUE,
    workers = 4,
    show_progress = TRUE
  )
})

glimpse(bulk_data)
```

You can also use parallel processing for downloading all data from a
single station:

``` r
station_id <- "468478b7-ace5-4bd3-b89a-a9c1c2e53080"
parameters <- c("PM10", "CO", "NO2")

# Download all available data in parallel (parallelizes across time chunks)
plan(multisession, workers = 4)
all_data <- download_all_data_for_station(
  station_id = station_id,
  parameters = parameters,
  frequency = "hourly",
  chunk_size = "3 months", # Smaller chunks = more parallelization
  parallel = TRUE,
  workers = 4
)
plan(sequential)

glimpse(all_data)
```

### Bulk Download 2

``` r
station_id <- "468478b7-ace5-4bd3-b89a-a9c1c2e53080"
parameters <- params$name
frequency <- "hourly"

# Download all available data for the specified station over the broad default range
all_data <- download_all_data_for_station(
  station_id = station_id,
  parameters = parameters,
  frequency = frequency
)

glimpse(all_data)
```

### Bulk Download 3 - Save All Station Data to Parquet Files

For very large bulk downloads, you can process each station sequentially
or use parallel processing with `furrr`:

``` r
library(tidyverse)
library(arrow)
library(future)
library(furrr)
library(airqualitytr)

# Define the parameters to download.
params <- list_parameters()
parameters <- params$name # or simply: c("PM10", "CO", "NO2")

# Retrieve station metadata from your package
stations_meta <- list_stations()

# Join city names (already included in list_stations() output)
cities_meta <- list_cities()
stations_meta <- stations_meta |>
  left_join(cities_meta, by = "city_id")

# Define frequency for downloading data.
frequency <- "hourly" # or "daily"

# Create an output folder to store Parquet files
if (!dir.exists("bulk_station_data")) {
  dir.create("bulk_station_data")
}

# Option 1: Sequential processing (original method)
pwalk(
  list(
    station_id = stations_meta$station_id,
    station_name = stations_meta$station_name,
    city_name = stations_meta$city_name
  ),
  function(station_id, station_name, city_name) {
    message("Downloading data for station: ", station_name, " (", station_id, ")")

    # Call download_all_data_for_station() with only the essential inputs.
    # This function automatically uses a broad date range (e.g. 1900-01-01 to 2099-12-31)
    # to fetch all available data for that station.
    station_data <- download_all_data_for_station(
      station_id = station_id,
      parameters = parameters,
      frequency = frequency
    )

    # Optionally, if station_data might be huge, you can periodically flush memory or
    # simply write the result directly.
    file_name <- file.path("bulk_station_data", paste0("data_", station_id, ".parquet"))
    arrow::write_parquet(station_data, sink = file_name, compression = "snappy")

    message("Saved data to: ", file_name)
  }
)

# Option 2: Parallel processing with furrr (much faster!)
plan(multisession, workers = 4)

future_pwalk(
  list(
    station_id = stations_meta$station_id,
    station_name = stations_meta$station_name,
    city_name = stations_meta$city_name
  ),
  function(station_id, station_name, city_name) {
    # Download all available data with internal parallel processing for chunks
    station_data <- download_all_data_for_station(
      station_id = station_id,
      parameters = parameters,
      frequency = frequency,
      parallel = TRUE, # Also parallelize time chunks
      show_progress = FALSE # Disable messages in parallel
    )

    # Save to Parquet file
    file_name <- file.path("bulk_station_data", paste0("data_", station_id, ".parquet"))
    arrow::write_parquet(station_data, sink = file_name, compression = "snappy")

    return(file_name)
  },
  .options = furrr_options(seed = TRUE)
)

plan(sequential) # Reset to sequential processing
```

## Troubleshooting

### Common Issues and Solutions

#### “Invalid station_id format”

**Problem:** Station ID is not in valid UUID format.

**Solution:**

``` r
# Get valid station IDs from list_stations()
stations <- list_stations()
valid_station_id <- stations$station_id[1]

# Use this ID in your downloads
data <- download_air_quality_data(
  station_id = valid_station_id,
  parameters = c("PM10"),
  start_datetime = "2025-03-16 00:00",
  end_datetime = "2025-03-17 00:00"
)
```

#### “start_datetime must be before end_datetime”

**Problem:** Date range is reversed or invalid.

**Solution:**

``` r
# Correct format: "YYYY-MM-DD HH:MM"
start_datetime <- "2025-03-01 00:00" # Earlier date
end_datetime <- "2025-03-31 23:59" # Later date

# Check your dates before downloading
lubridate::ymd_hm(start_datetime) < lubridate::ymd_hm(end_datetime)
```

#### Empty or No Data Returned

**Problem:** Query returns empty tibble.

**Possible causes:**

1.  Station has no data for the specified date range
2.  Parameters not measured at this station
3.  Network connectivity issues
4.  API temporarily unavailable

**Solution:**

``` r
# Check if station exists and is active
stations <- list_stations()
station_info <- stations |> filter(station_id == your_station_id)

# Try a recent date range (last 7 days)
data <- download_air_quality_data(
  station_id = your_station_id,
  parameters = c("PM10"),
  start_datetime = format(Sys.time() - 7 * 24 * 60 * 60, "%Y-%m-%d %H:%M"),
  end_datetime = format(Sys.time(), "%Y-%m-%d %H:%M"),
  frequency = "hourly"
)

# Check multiple stations to verify API connectivity
```

#### Slow Downloads

**Problem:** Downloads taking too long.

**Solutions:**

``` r
# 1. Use parallel processing
bulk_data <- bulk_download_air_quality_data(
  start_datetime = "2025-03-16 00:00",
  end_datetime = "2025-03-16 12:00",
  parameters = c("PM10"),
  parallel = TRUE,
  workers = 4
)

# 2. Reduce date range
# Instead of years, download months at a time

# 3. Use daily instead of hourly frequency
data <- download_air_quality_data(
  station_id = station_id,
  parameters = c("PM10"),
  start_datetime = "2024-01-01 00:00",
  end_datetime = "2024-12-31 23:59",
  frequency = "daily" # Much smaller dataset
)

# 4. Download fewer parameters at once
```

#### “Package ‘future’ not found” (Parallel Processing)

**Problem:** Trying to use parallel processing without required
packages.

**Solution:**

``` r
install.packages(c("future", "furrr", "progressr"))
library(future)
library(furrr)
```

#### Network Timeouts or Connection Errors

**Problem:** API requests timing out.

**Solutions:**

1.  Check internet connection
2.  Try again later (API might be under maintenance)
3.  Reduce number of parallel workers
4.  Use smaller date ranges or fewer parameters

``` r
# Reduce parallel workers if getting timeouts
data <- bulk_download_air_quality_data(
  start_datetime = "2025-03-16 00:00",
  end_datetime = "2025-03-16 12:00",
  parameters = c("PM10"),
  parallel = TRUE,
  workers = 2 # Reduced from 8
)
```

#### Memory Errors in Parallel Mode

**Problem:** Out of memory when using parallel processing.

**Solutions:**

``` r
# 1. Reduce number of workers
workers <- 2 # Instead of 8

# 2. Process in smaller batches
# 3. Write results to disk immediately (use Parquet/Arrow)
# 4. Use daily frequency instead of hourly
```

#### Progress Bar Not Showing

**Problem:** No progress indication during downloads.

**Solution:**

``` r
# For sequential downloads, ensure cli is installed
install.packages("cli")

# For parallel downloads, use progressr
install.packages("progressr")
library(progressr)

handlers(global = TRUE)
handlers("progress")

with_progress({
  data <- bulk_download_air_quality_data(
    start_datetime = "2025-03-16 00:00",
    end_datetime = "2025-03-16 12:00",
    parameters = c("PM10"),
    parallel = TRUE,
    workers = 4,
    show_progress = TRUE
  )
})
```

### Getting More Help

If you’re still experiencing issues:

1.  **Check function documentation**:

    ``` r
    ?download_air_quality_data
    ?bulk_download_air_quality_data
    ?list_stations
    ```

2.  **Review vignettes** for detailed examples:

    ``` r
    vignette("getting-started", package = "airqualitytr")
    ```

3.  **Report bugs**: <https://github.com/emraher/airqualitytr/issues>

    - Include your R version: `R.version.string`
    - Include package version: `packageVersion("airqualitytr")`
    - Provide a minimal reproducible example
    - Include any error messages

## Data Source and Citation

### Data Source

This package accesses air quality monitoring data from the **Turkish
Ministry of Environment, Urbanization and Climate Change** (Çevre,
Şehircilik ve İklim Değişikliği Bakanlığı).

**Official Data Portal:** <https://sim.csb.gov.tr/>

### Licensing and Usage

**Important:** Before using this data in research or publications,
please:

1.  **Check the official data policy** at the Ministry’s website for:
    - Data licensing terms
    - Citation requirements
    - Usage restrictions and permissions
    - Redistribution rights
2.  **Cite appropriately** in your work:
    - Cite the data source (Turkish Ministry of Environment)
    - Cite this R package if used for data access
    - Follow any specific citation requirements from the Ministry

### Suggested Citation Format

**For the data:**

    Turkish Ministry of Environment, Urbanization and Climate Change. (Year).
    Air Quality Monitoring Data. Retrieved from <https://sim.csb.gov.tr/>

**For this package:**

``` r
citation("airqualitytr")
```

### Data Quality and Limitations

Please note:

- Data is provided as-is from the Ministry’s monitoring network
- This package does not validate or quality-control the data
- Users are responsible for their own data quality checks
- Station metadata and availability may change over time
- Historical data availability varies by station
- Some stations may have gaps or missing measurements

**Always perform your own data quality assessment before analysis.**

## Code of Conduct

Please note that the airqualitytr project is released with a
[Contributor Code of
Conduct](https://eremrah.com/airqualitytr/CODE_OF_CONDUCT.html). By
contributing to this project, you agree to abide by its terms.
