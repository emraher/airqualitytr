## R CMD check results

0 errors | 0 warnings | 1 note

* NOTE: checking for future file timestamps  
  - “unable to verify current time”

This NOTE appears to be related to time settings on the check machine; no files in the package use future timestamps.

## Test environments

* local: macOS 15.6.1 (Sequoia), R 4.5.1
* GitHub Actions: macOS-latest, R-release
* GitHub Actions: ubuntu-latest, R-release
* GitHub Actions: windows-latest, R-release
* R-hub via GitHub Actions: Linux (R-devel), macOS-arm64 (R-devel), Windows (R-devel)

## Submission notes

This is a new submission.

The package provides functions to download and explore air quality monitoring data from the Turkish Ministry of Environment, Urbanization and Climate Change. It enables researchers to access hourly and daily measurements of various air pollutants (PM10, PM2.5, SO2, CO, NO2, NOX, NO, O3) across monitoring stations throughout Turkey.

**NOX spelling**: This is not a misspelling. NOX (nitrogen oxides) is a standard atmospheric chemistry term and is included in our WORDLIST file.

**URL timeout for https://sim.csb.gov.tr/**: This is the official Turkish government data portal URL. The site is accessible but may occasionally timeout from CRAN servers due to geographic distance or firewall settings. The URL has been verified to work correctly.
