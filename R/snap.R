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
#' @importFrom utils unzip
#' @importFrom curl curl curl_download
#' @importFrom DBI dbDisconnect
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

  latest_entry = curl("https://aact.ctti-clinicaltrials.org/pipe_files") |>
    readLines() |> 
    (\(x) x[grep("_export.zip", x)[1]])()
  latest_entry = strsplit("\\s+", x = latest_entry) |>
    unlist() |>
    suppressWarnings()
    
  dl_url = gsub("^href=", "", latest_entry[3])

  zip_name = latest_entry[5]

  destfile = file.path(tmp_dir, zip_name)

  curl_download(
    dl_url, 
    destfile = file.path(tmp_dir, zip_name), 
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
