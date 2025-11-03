## R CMD check results

0 errors | 0 warnings | 0 notes

## Test environments

* local: macOS 15.6.1 (Sequoia), R 4.5.1
* GitHub Actions:
  * macOS-latest (release)
  * windows-latest (release)
  * ubuntu-latest (release)

## Submission notes

This is a new submission.

The package provides functions to download and explore air quality monitoring data from the Turkish Ministry of Environment, Urbanization and Climate Change. It enables researchers to access hourly and daily measurements of various air pollutants (PM10, PM2.5, SO2, CO, NO2, NOX, NO, O3) across monitoring stations throughout Turkey.

**NOX spelling**: This is not a misspelling. NOX (nitrogen oxides) is a standard atmospheric chemistry term and is included in our WORDLIST file.

**URL timeout for https://sim.csb.gov.tr/**: This is the official Turkish government data portal URL. The site is accessible but may occasionally timeout from CRAN servers due to geographic distance or firewall settings. The URL has been verified to work correctly. This is a reference URL only and is not required for package functionality.

## Downstream dependencies

There are currently no downstream dependencies for this package.
