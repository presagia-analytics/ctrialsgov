
#' Get the Latest Daily Snapshot from clinicaltrials.gov
#'
#' This function does the following.
#'  1. Downloads the latest zip file containing pipe-delimited file from 
#'     https://aact.ctti-clinicaltrials.org/pipe_files.
#'  2. Unzips the zip file.
#'  3. Creates a duckdb database from the unzipped, pipe-delimited files.
#'  4. Creates a derived duckdb database used by the package for querying and
#'     including other calculated features.
#' @param db_path the path where the database should be written to.
#' @param db_derived_path the path where the derived database should be 
#' written to.
#' @param tmp_dir the temporary directory where the zip file should be stored
#' along with the unzipped pipe-delimited files.
#' @param cleanup logical flag; should the zip and pipe-delimted files be 
#' removed after the databases have been created? (Default is `TRUE`)
#' @param verbose logical flag; should progress messages be printed? 
#' (Default is `TRUE`)
#' @param ask_understand logical flag; does the user have to answer whether or
#' not they understand they are going to download the database?
#' @importFrom cli cli_inform
#' @importFrom curl curl curl_download
#' @importFrom DBI dbDisconnect
#' @importFrom utils unzip
#' @importFrom yesno yesno
#' @export
ctgov_get_latest_snapshot = function(
  db_path = "ctgov.duckdb",
  db_derived_path = "ctgov-derived.duckdb",
  tmp_dir = tempdir(),
  cleanup = TRUE,
  verbose = TRUE,
  ask_understand = TRUE) {

  if (ask_understand) {
    if (!isTRUE(
          yesno(
            paste0("Do you understand that you are about to download the\n",
                  "entire, current database snapshot?")
          )
        )
      ) {
      return(invisible(NULL))
    }
  }

  download_url <- 
    "https://aact.ctti-clinicaltrials.org/downloads/snapshots?type=flatfiles"
  snap_url_pattern <- "_export_ctgov.zip"
  latest_entry = curl(download_url) |>
    readLines() |> 
    (\(x) x[grep(snap_url_pattern, x)])() |>
    (\(x) x[grep("href", x)])() |>
    head(1)
  download_name <- sub('.*download="([^"]+)".*', '\\1', latest_entry)
  dl_url <- sub('.*href="([^"]+)".*', '\\1', latest_entry)

  if (verbose) cli_inform("Downloading {download_name}.")

  zip_name = latest_entry[5]

  destfile = file.path(tmp_dir, download_name)

  curl_download(
    dl_url, 
    destfile = file.path(tmp_dir, download_name), 
    quiet = !verbose
  )

  zip_files = utils::unzip(destfile, list = TRUE)

  utils::unzip(destfile, exdir = tmp_dir)

  unlink(c(db_path, db_derived_path))
  con = suppressWarnings(
    ctgov_create_duckdb(tmp_dir, db_path, verbose = verbose)
  )
  ctgov_create_data(con, db_derived_path, verbose = verbose)

  if (cleanup) {
    unlink(file.path(tmp_dir, zip_files$Name))
    unlink(destfile)
  }
  return(invisible(TRUE)) 
}

#' Retrieve Snapshot Download Metadata for a Given Year
#'
#' This function fetches metadata about snapshot downloads from the AACT website
#' for a specified year. It validates the year (ensuring it's within the supported
#' range), constructs the correct URL to access flat-file snapshot listings, parses
#' the HTML to extract relevant download names, URLs, and snapshot dates, and
#' returns them as a tidy tibble.
#'
#' @param year Integer. The year for which to retrieve snapshot download metadata.
#'   Must be between 2017 and the current year (inclusive). Years outside this range
#'   will result in a CLI-friendly error.
#'
#' @return A tibble with the following columns:
#' \describe{
#'   \item{download_name}{Character. The filename of the snapshot archive (e.g., `20250731_export_ctgov.zip`).}
#'   \item{dl_url}{Character. The URL from which the snapshot can be downloaded.}
#'   \item{snap_date}{Date. The date of the snapshot, parsed from the filename.}
#' }
#'
#' @details
#' The function builds a download page URL using the year provided, then parses the
#' resulting HTML content to find hyperlinks that match expected snapshot filename
#' patterns (e.g., `_export_ctgov.zip`, `_pipe-delimited-export.zip`). It extracts
#' both the download filename and URL, then parses the date embedded in the filename
#' to produce a proper date column. If the year is before 2017 or in the future,
#' the function aborts with an informative message.
#'
#' @examples
#' \dontrun{
#' # Get snapshot metadata for 2025
#' snaps_2025 <- get_snap_downloads_by_year(2025)
#' print(snaps_2025)
#'
#' # Attempting to query before 2017 or in the future will abort
#' get_snap_downloads_by_year(2015)
#' get_snap_downloads_by_year(lubridate::year(Sys.Date()) + 1)
#' }
#'
#' @importFrom cli cli_abort
#' @importFrom curl curl
#' @importFrom lubridate year ymd
#' @importFrom purrr map_chr
#' @export
get_snap_downloads_by_year <- function(year) {
  if ((year > year(Sys.Date()))) {
    cli_abort("You can't query future snapshots")
  }
  if (year < 2017) {
    cli_abort("No snapshots before 2017")
  }
  download_url <- 
    sprintf(
      "https://aact.ctti-clinicaltrials.org/downloads/snapshots?type=flatfiles&year=%s",
      as.character(year)
    )
  snap_url_pattern <- "(_export_ctgov.zip|_pipe-delimited-export.zip)"
  entries = curl(download_url) |>
    readLines() |> 
    (\(x) x[grep(snap_url_pattern, x)])() |>
    (\(x) x[grep("href", x)])() 
  download_name <- sub('.*download="([^"]+)".*', '\\1', entries)
  dl_url <- sub('.*href="([^"]+)".*', '\\1', entries)

  snap_date <- strsplit(download_name, "_") |>
    map_chr(~ .x[1]) |>
    ymd()

  return(
    tibble(
      download_name = download_name,
      dl_url = dl_url,
      snap_date = snap_date
    )
  )
}

#' Retrieve the First Available Snapshot Download After a Specified Date
#'
#' This function returns the first snapshot download record of a given
#' **year** and **month**, based on the snapshot dates. If the month falls
#' in the future relative to the current date, it throws an error. For
#' dates at the end of the year where no snapshot exists, it will attempt
#' to fetch the first snapshot of the next year.
#'
#' @param year Integer. The year for which to search snapshot downloads.
#' @param month Integer (1–12). The month for which to search.
#'
#' @return A tibble containing the snapshot download record for the first
#'   snapshot date on or after the input year and month. If none exists
#'   for December, it will recursively look at January of the following year.
#'
#' @details
#' This function uses `get_snap_downloads_by_year()` to fetch all snapshot
#' downloads for the specified year, arranges them by `snap_date`, and
#' filters to keep the first entry on or after the start of the specified month.
#' If no entry exists and the month is December, it recurses to the next year.
#'
#' It will abort with a CLI-friendly message if the requested year/month
#' lies in the future relative to `Sys.Date()`.
#'
#' @examples
#' \dontrun{
#' # Assuming snapshot data exists for these dates:
#' get_first_snap_download(2025, 5)
#' get_first_snap_download(2025, 12)
#' }
#'
#' @importFrom dplyr arrange filter
#' @importFrom lubridate month year ymd 
#' @importFrom tibble tibble
#' @export
get_first_snap_download <- function(year, month) {
  if ((year > year(Sys.Date())) || 
      (year == year(Sys.Date()) && month >= month(Sys.Date))) {
    cli_abort("You can't query future snapshots")
  }

  snap_date <- NULL
  r <- get_snap_downloads_by_year(year) |>
    arrange(snap_date) |>
    filter(snap_date >= ymd(sprintf("%s-%s-01", year, month))) |>
    head(1)
  if (nrow(r) == 0 && month == 12) {
    r <- get_first_snap_download(year + 1, 1)
  }
  return(r)
}

#' Download and Initialize the First Available AACT Snapshot for a Given 
#' Year/Month
#'
#' This function retrieves the earliest ClinicalTrials.gov (AACT) snapshot 
#' available on or after the specified year and month. It downloads the 
#' snapshot ZIP archive, extracts its contents, and initializes two DuckDB 
#' databases: one derived from raw data. Optionally, it cleans up temporary 
#' files after successful processing.
#'
#' @param year Integer. Target year for selecting a snapshot. Must be 
#' between 2017 and the current year.
#' @param month Integer (1–12). Target month within the given year.
#' @param db_path Character. File path to store the primary DuckDB database 
#' (default: `"ctgov.duckdb"`).
#' @param db_derived_path Character. File path to store the derived DuckDB 
#' database (default: `"ctgov-derived.duckdb"`).
#' @param tmp_dir Character. Directory for temporary files like the 
#' downloaded ZIP and extracted contents (default: `tempdir()`).
#' @param cleanup Logical. Whether to delete downloaded and extracted files 
#' after processing (default: `TRUE`).
#' @param verbose Logical. Whether to inform the user of progress messages 
#' (default: `TRUE`).
#' @param ask_understand Logical. Whether to prompt the user with a 
#' confirmation before downloading the full snapshot (default: `TRUE`).
#'
#' @returns Invisibly returns `TRUE` if processing completes successfully; 
#' returns `NULL` if the user declines the confirmation prompt.
#'
#' @details
#' The flow of operations is:
#' 1. Optionally prompt the user to confirm they've read and understood the 
#' implications of downloading a full ClinicalTrials.gov snapshot.
#' 2. Find the first available snapshot after the specified year and month 
#' using `get_first_snap_download()`.
#' 3. Download the snapshot ZIP archive to `tmp_dir`.
#' 4. Extract the archive and instantiate the specified DuckDB databases 
#' (`db_path`, `db_derived_path`) via helper functions.
#' 5. Optionally clean up temporary files.
#'
#' If the `verbose` flag is `TRUE`, progress messages are emitted using 
#' `cli_inform()`. If the user declines the confirmation 
#' (`ask_understand = TRUE`), the function exits silently with `NULL`.
#'
#' @examples
#' \dontrun{
#' # Download the first snapshot available in May 2025
#' ctgov_get_first_snapshot_year_month(2025, 5)
#'
#' # Non-interactive invocation (silent, no prompt)
#' ctgov_get_first_snapshot_year_month(
#'   2025, 
#'   5, 
#'   ask_understand = FALSE, 
#'   verbose = FALSE
#' )
#' }
#'
#' @importFrom cli cli_inform
#' @importFrom curl curl curl_download
#' @importFrom DBI dbDisconnect
#' @importFrom lubridate month year
#' @importFrom tibble tibble
#' @importFrom utils unzip
#' @importFrom yesno yesno
#' @export
ctgov_get_first_snapshot_year_month <- function(
  year,
  month,
  db_path = "ctgov.duckdb",
  db_derived_path = "ctgov-derived.duckdb",
  tmp_dir = tempdir(),
  cleanup = TRUE,
  verbose = TRUE,
  ask_understand = TRUE) {

  if (ask_understand) {
    if (!isTRUE(
          yesno(
            paste0("Do you understand that you are about to download the\n",
                  "entire, current database snapshot?")
          )
        )
      ) {
      return(invisible(NULL))
    }
  }

  snap <- get_first_snap_download(year, month)

  if (verbose) cli_inform("Downloading {snap$download_name}.")

  destfile = file.path(tmp_dir, snap$download_name)

  curl_download(
    snap$dl_url, 
    destfile = file.path(tmp_dir, snap$download_name), 
    quiet = !verbose
  )

  zip_files = utils::unzip(destfile, list = TRUE)

  utils::unzip(destfile, exdir = tmp_dir)

  unlink(c(db_path, db_derived_path))
  con = suppressWarnings(
    ctgov_create_duckdb(tmp_dir, db_path, verbose = verbose)
  )
  ctgov_create_data(con, db_derived_path, verbose = verbose)

  if (cleanup) {
    unlink(file.path(tmp_dir, zip_files$Name))
    unlink(destfile)
  }
  return(invisible(TRUE)) 
}
