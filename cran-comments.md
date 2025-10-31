## R CMD check results

0 errors | 0 warnings | 0 notes

* This is a new release.

## Test environments

* local: macOS 15.6.1 (aarch64-apple-darwin24.4.0), R 4.5.1
* GitHub Actions (pending setup):
  - ubuntu-latest (R-release)
  - windows-latest (R-release)
  - macOS-latest (R-release)

## Downstream dependencies

There are currently no downstream dependencies for this package.

## Additional notes

* This package provides the first R interface to Turkish Ministry of Environment air quality data
* All examples use `\dontrun{}` to avoid making API calls during CRAN checks
* Tests use `skip_on_cran()` and `skip_if_offline()` to prevent failures during automated checks
* Vignette dependencies (ggplot2, lubridate, purrr, glue, scales) are listed in Suggests
* The package includes comprehensive input validation with helpful error messages
