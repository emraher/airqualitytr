## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.width = 7,
  fig.height = 5
)

## ----eval=FALSE---------------------------------------------------------------
# # Install pak if not already installed
# if (!requireNamespace("pak", quietly = TRUE)) {
#   install.packages("pak")
# }
# 
# # Install airqualitytr
# pak::pak("emraher/airqualitytr")

## ----eval=TRUE----------------------------------------------------------------
library(airqualitytr)
library(dplyr)
library(ggplot2)
library(lubridate)
library(purrr)

## ----eval=TRUE----------------------------------------------------------------
# Get all available stations
stations <- list_stations()

# Glimpse stations
glimpse(stations)

## ----eval=TRUE----------------------------------------------------------------
# Find stations in Ankara
ankara_stations <- stations |>
  filter(grepl("Ankara", city_name, ignore.case = TRUE))

glimpse(ankara_stations)

# Find a specific station
selected_station <- stations |>
  filter(grepl("Sıhhıye", station_name, ignore.case = TRUE)) |>
  slice(1)

glimpse(selected_station)

station_id <- selected_station$station_id

## ----eval=TRUE----------------------------------------------------------------
# List available parameters
(parameters <- list_parameters())

## ----eval=TRUE----------------------------------------------------------------
# Download one month of hourly PM10 data
data <- download_air_quality_data(
  station_id = station_id,
  parameters = c("PM10"),
  start_datetime = "2025-03-01 00:00",
  end_datetime = "2025-03-31 23:59",
  frequency = "hourly"
)

# View the results
glimpse(data)

## ----eval=TRUE----------------------------------------------------------------
# Download PM10, NO2, and O3
multi_param_data <- download_air_quality_data(
  station_id = station_id,
  parameters = c("PM10", "NO2", "O3"),
  start_datetime = "2025-03-01 00:00",
  end_datetime = "2025-03-07 23:59",
  frequency = "hourly"
)

# Data is in long format - easy to analyze and plot
ggplot(multi_param_data, aes(x = time, y = value, color = parameter)) +
  geom_line() +
  facet_wrap(~parameter, scales = "free_y", ncol = 1) +
  theme_minimal() +
  labs(
    title = "Air Quality Measurements",
    subtitle = unique(multi_param_data$station_name),
    x = "Time",
    y = "Concentration",
    color = "Pollutant"
  )

## ----eval=TRUE----------------------------------------------------------------
# Hourly data - more detail, larger datasets
hourly_data <- download_air_quality_data(
  station_id = station_id,
  parameters = c("PM10"),
  start_datetime = "2025-03-01 00:00",
  end_datetime = "2025-03-07 23:59",
  frequency = "hourly"
)

# Daily data - aggregated, smaller datasets
# Note: Use longer date range for daily data as it aggregates measurements
daily_data <- download_air_quality_data(
  station_id = station_id,
  parameters = c("PM10"),
  start_datetime = "2025-03-01 00:00",
  end_datetime = "2025-03-31 23:59",
  frequency = "daily"
)

# Compare sizes
nrow(hourly_data) # ~168 rows (7 days * 24 hours)
nrow(daily_data) # ~31 rows (31 days)

## ----eval=TRUE----------------------------------------------------------------
(cities <- list_cities())

# Count stations per city
stations_per_city <- stations |>
  count(city_name) |>
  arrange(desc(n))

stations_per_city

## ----eval=TRUE----------------------------------------------------------------
# Stations in major cities
major_cities <- c("İstanbul", "Ankara", "İzmir")

(major_city_stations <- stations |>
  filter(city_name %in% major_cities))

# Stations with specific names
(traffic_stations <- stations |>
  filter(grepl("Trafik|Traffic", station_name, ignore.case = TRUE)))

# Get unique station types if available in metadata
if ("area_type_name" %in% names(stations)) {
  table(stations$area_type_name)
}

## ----eval=TRUE----------------------------------------------------------------
# Check timezone
attr(data$time, "tzone") # "UTC"

# Convert to Turkey time (UTC+3) if needed
data_turkey_time <- data |>
  mutate(time_local = with_tz(time, tzone = "Europe/Istanbul"))

## ----eval=TRUE----------------------------------------------------------------
# Check for missing values
summary(data$value)

# Count missing by parameter
data |>
  group_by(parameter) |>
  summarize(
    total = n(),
    missing = sum(is.na(value)),
    pct_missing = round(100 * missing / total, 1)
  )

# Remove missing values for analysis
clean_data <- data |>
  filter(!is.na(value))

## ----eval=TRUE----------------------------------------------------------------
# Invalid station ID format
try(
  download_air_quality_data(
    station_id = "invalid-id",
    parameters = c("PM10"),
    start_datetime = "2025-03-01 00:00",
    end_datetime = "2025-03-31 23:59"
  )
)
# Error: Invalid station_id format. Must be a valid UUID.

# Invalid date range
try(
  download_air_quality_data(
    station_id = station_id,
    parameters = c("PM10"),
    start_datetime = "2025-03-31 00:00",
    end_datetime = "2025-03-01 00:00" # End before start!
  )
)
# Error: start_datetime must be before end_datetime

# Invalid parameter
try(
  download_air_quality_data(
    station_id = station_id,
    parameters = c("INVALID_PARAM"),
    start_datetime = "2025-03-01 00:00",
    end_datetime = "2025-03-31 23:59"
  )
)
# Error with suggestions for valid parameters

## ----eval=TRUE----------------------------------------------------------------
# Select 3 stations from different cities
comparison_stations <- stations |>
  filter(station_name %in% c("İstanbul - Beşiktaş", "Ankara - Çankaya", "İzmir - Karabağlar")) |>
  group_by(city_name) |>
  slice_sample(n = 1) |>
  ungroup()

# Download data for each station
comparison_data <- map_dfr(
  comparison_stations$station_id,
  ~ download_air_quality_data(
    station_id = .x,
    parameters = c("PM10"),
    start_datetime = "2025-03-01 00:00",
    end_datetime = "2025-03-07 23:59",
    frequency = "hourly"
  )
)

# Compare stations
ggplot(comparison_data, aes(x = time, y = value, color = station_name)) +
  geom_line(alpha = 0.7) +
  theme_minimal() +
  labs(
    title = "PM10 Comparison Across Stations",
    x = "Time",
    y = "PM10 (μg/m³)",
    color = "Station"
  )

## ----eval=TRUE----------------------------------------------------------------
# Download hourly data
hourly <- download_air_quality_data(
  station_id = station_id,
  parameters = c("PM10"),
  start_datetime = "2025-03-01 00:00",
  end_datetime = "2025-03-31 23:59",
  frequency = "hourly"
)

# Calculate daily statistics
daily_stats <- hourly |>
  filter(!is.na(value)) |>
  mutate(date = as.Date(time)) |>
  group_by(date, station_name, parameter) |>
  summarize(
    mean = mean(value),
    min = min(value),
    max = max(value),
    sd = sd(value),
    n = n(),
    .groups = "drop"
  )

daily_stats

# Plot with error bars
ggplot(daily_stats, aes(x = date, y = mean)) +
  geom_ribbon(aes(ymin = mean - sd, ymax = mean + sd), alpha = 0.2) +
  geom_line() +
  geom_point() +
  theme_minimal() +
  labs(
    title = "Daily PM10 Mean with Standard Deviation",
    x = "Date",
    y = "PM10 (μg/m³)"
  )

## ----eval=TRUE----------------------------------------------------------------
# Download one month of hourly PM10 data
data <- download_air_quality_data(
  station_id = "ed5b48d0-14c4-47a4-8ad1-b5db53ae72c9",
  parameters = c("PM10"),
  start_datetime = "2025-03-01 00:00",
  end_datetime = "2025-03-31 23:59",
  frequency = "hourly"
)

# Find hours exceeding WHO guidelines (PM10 > 45 μg/m³ daily mean)
exceedances <- data |>
  filter(!is.na(value)) |>
  mutate(date = as.Date(time)) |>
  group_by(date, station_name) |>
  summarize(
    daily_mean = mean(value),
    daily_max = max(value),
    .groups = "drop"
  ) |>
  filter(daily_mean > 45)

exceedances

# Percentage of days exceeding guidelines
pct_exceeding <- 100 * nrow(exceedances) / n_distinct(as.Date(data$time))
glue::glue("{scales::number(pct_exceeding, accuracy = 0.1)}% of days exceeded WHO PM10 guidelines\n")

