#!/usr/bin/env Rscript
# Script to Record HTTP Fixtures for Testing
#
# This script records real API responses from the Turkish Ministry of Environment
# API and saves them as fixtures for use in automated testing. This allows tests
# to run without requiring internet access or hitting the real API.
#
# USAGE:
# Run from R console: source("tests/record_fixtures.R")
# Run from command line: Rscript tests/record_fixtures.R
#
# REQUIREMENTS:
#   - Active internet connection
#   - Access to sim.csb.gov.tr
#   - httptest package installed
#
# The fixtures will be saved to: tests/testthat/fixtures/

library(httptest)
library(airqualitytr)

# Use VERY SHORT directory name and FLATTEN structure completely
fixture_dir <- "tests/testthat/m"  # Just "m" for "mocks"

if (!dir.exists(fixture_dir)) {
  dir.create(fixture_dir, recursive = TRUE)
}

message("Recording HTTP fixtures to: ", fixture_dir)
message("This will make real API calls to sim.csb.gov.tr")
message("NOTE: httptest will create subdirectories, but we'll flatten them automatically")
message("")

# Just start capturing - let httptest do its thing
# We'll flatten the structure afterwards
start_capturing(path = fixture_dir)

tryCatch({
  message("1. Recording list_cities() ...")
  cities <- list_cities()
  message("   ✓ Recorded ", nrow(cities), " cities")

  message("2. Recording list_parameters() ...")
  params <- list_parameters()
  message("   ✓ Recorded ", nrow(params), " parameters")

  message("3. Recording list_stations() ...")
  stations <- list_stations()
  message("   ✓ Recorded ", nrow(stations), " stations")

  message("4. Recording download_air_quality_data() ...")
  # Use a known station and short date range for testing
  # Station: Ankara - Siteler (example)
  tryCatch({
    test_data <- download_air_quality_data(
      station_id = "468478b7-ace5-4bd3-b89a-a9c1c2e53080",
      parameters = "PM10",
      start_datetime = "2024-01-01 00:00",
      end_datetime = "2024-01-02 00:00",
      frequency = "hourly",
      return_all = FALSE
    )
    message("   ✓ Recorded air quality data (", nrow(test_data), " rows)")
  }, error = function(e) {
    message("   ⚠ Warning: Could not record download_air_quality_data(): ", e$message)
    message("   This is OK - some tests will fall back to real API calls")
  })

  message("5. Recording test_api_connection() ...")
  tryCatch({
    api_status <- test_api_connection(timeout = 5, verbose = FALSE)
    message("   ✓ Recorded API connection test")
  }, error = function(e) {
    message("   ⚠ Warning: Could not record test_api_connection(): ", e$message)
  })

}, error = function(e) {
  message("ERROR recording fixtures: ", e$message)
  stop(e)
}, finally = {
  # Stop capturing
  stop_capturing()
})

message("")
message("✅ Fixture recording complete!")
message("")
message("🔧 Flattening directory structure...")

# ALWAYS flatten - find ALL files recursively
all_files <- list.files(fixture_dir, recursive = TRUE, full.names = TRUE)
files_to_move <- all_files[!file.info(all_files)$isdir]

if (length(files_to_move) > 0) {
  moved_count <- 0
  for (file_path in files_to_move) {
    # Get the target path (root of fixture_dir)
    new_path <- file.path(fixture_dir, basename(file_path))

    # Only move if it's in a subdirectory
    if (file_path != new_path) {
      # Check if target already exists (avoid overwrite)
      if (file.exists(new_path)) {
        message("  ⚠ Skipping ", basename(file_path), " (already exists)")
      } else {
        success <- file.rename(file_path, new_path)
        if (success) {
          message("  ✓ Moved: ", basename(file_path))
          moved_count <- moved_count + 1
        } else {
          message("  ✗ Failed to move: ", basename(file_path))
        }
      }
    }
  }

  if (moved_count > 0) {
    message("  → Moved ", moved_count, " file(s) to root")
  }
}

# Remove ALL subdirectories
subdirs <- list.dirs(fixture_dir, full.names = TRUE, recursive = TRUE)
subdirs <- subdirs[subdirs != fixture_dir]  # Exclude the fixture_dir itself

if (length(subdirs) > 0) {
  # Sort in reverse to delete deepest first
  subdirs <- rev(sort(subdirs))
  removed_count <- 0
  for (subdir in subdirs) {
    if (unlink(subdir, recursive = TRUE) == 0) {
      removed_count <- removed_count + 1
    }
  }
  if (removed_count > 0) {
    message("  ✓ Removed ", removed_count, " subdirectorie(s)")
  }
}

message("")
message("📁 Final structure (flat):")

# Get final list of files (should all be in root now)
recorded_files <- list.files(fixture_dir, recursive = FALSE, full.names = FALSE)

if (length(recorded_files) > 0) {
  total_size <- 0
  for (f in recorded_files) {
    full_path <- file.path(fixture_dir, f)
    size_kb <- round(file.info(full_path)$size / 1024, 1)
    # Calculate path as it appears in package tarball
    path_length <- nchar(file.path("airqualitytr/tests/testthat/m", f))
    status <- if (path_length <= 100) "✓" else "⚠"
    message(sprintf("  %s %s (%s KB, %d bytes)", status, f, size_kb, path_length))
    total_size <- total_size + file.info(full_path)$size
  }
  message("")
  message(sprintf("Total: %d files, %.1f KB", length(recorded_files), total_size / 1024))

  # Check if any paths are too long
  long_paths <- sapply(recorded_files, function(f) {
    nchar(file.path("airqualitytr/tests/testthat/m", f)) > 100
  })
  if (any(long_paths)) {
    message("")
    message("⚠ WARNING: Some paths are still > 100 bytes!")
    message("   Files with long paths:")
    for (f in recorded_files[long_paths]) {
      message("     - ", f, " (", nchar(file.path("airqualitytr/tests/testthat/m", f)), " bytes)")
    }
    message("   This may cause issues on CRAN.")
  } else {
    message("")
    message("✅ All paths are under 100 bytes - CRAN compatible!")
  }
} else {
  message("  ⚠ No fixtures were recorded. Check the error messages above.")
}

message("")
message("NEXT STEPS:")
message("1. Commit the fixtures to git: git add tests/testthat/m/")
message("2. Tests will now use these mocked responses on CI/CRAN")
message("3. To update fixtures, run this script again")
message("")
message("To force tests to use real API (for local development):")
message("  Sys.setenv(AIRQUALITYTR_USE_MOCKS = 'false')")
