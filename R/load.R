#' Initialize the connection
#'
#' This function must be run prior to other functions in the package. It
#' creates a parsed and cached version of the clinical trials dataset in
#' memory in R. This makes other function calls relatively efficient. other
#'
#' @param con       an DBI connection object to the database
#' @param verbose   logical flag; should progress messages be printed?;
#'                  defaults to \code{TRUE}
#'
#' @author Taylor B. Arnold, \email{taylor.arnold@@acm.org}
#' @return does not return any value; used only for side effects
#'
#' @export
#' @importFrom rlang .data
#' @importFrom DBI dbGetQuery
#' @importFrom tibble as_tibble
#' @importFrom dplyr filter if_else transmute group_by ungroup left_join nest_by desc arrange select inner_join
#' @importFrom stringi stri_trim stri_replace_all stri_sub stri_paste
#' @importFrom lubridate today
ctgov_create_data <- function(con, verbose = TRUE) {
  assert(is.logical(verbose) & length(verbose) == 1L)

  # Save the connection in case needed later
  .volatiles$con <- con

  # Grab the data
  cmsg(verbose, "[%s] LOADING DATA TABLES\n", isotime())
  tbl_study <- tibble::as_tibble(
    DBI::dbGetQuery(
      con,
      sprintf(
        paste(c("select nct_id, ",
          "start_date, phase, enrollment, brief_title, official_title, ",
          "primary_completion_date, study_type, overall_status as rec_status, ",
          "completion_date, last_update_posted_date as last_update ",
          "from %sstudies;"),
          collapse = ""),
        format_schema()
      )
    )
  )

  tbl_conds <- tibble::as_tibble(DBI::dbGetQuery(
    con,
    sprintf("select nct_id, name from %sconditions;", format_schema())
  ))

  tbl_inter <- tibble::as_tibble(
    DBI::dbGetQuery(
      con,
      sprintf(
        paste0(
          "select nct_id, intervention_type, name, description from ",
          "%sinterventions;"
        ),
        format_schema()
      )
    )
  )

  tbl_outcm <- tibble::as_tibble(
    DBI::dbGetQuery(
      con,
      sprintf(
        paste0(
          "select nct_id, outcome_type, measure, time_frame, description from ",
          "%sdesign_outcomes;"
        ),
        format_schema()
      )
    )
  )

  tbl_bfsum <- tibble::as_tibble(
    DBI::dbGetQuery(
      con,
      sprintf(
        "select nct_id, description from %sbrief_summaries;",
        format_schema()
      )
    )
  )

  tbl_idinf <- tibble::as_tibble(
    DBI::dbGetQuery(
      con,
      sprintf("select * from %sid_information;", format_schema())
    )
  )

  tbl_spons <- tibble::as_tibble(
    DBI::dbGetQuery(
      con,
      sprintf(
        "select * from %ssponsors where lead_or_collaborator = 'lead';",
        format_schema()
      )
    )
  )

  tbl_desig <- tibble::as_tibble(
    DBI::dbGetQuery(
      con,
      sprintf(
        paste(c("select nct_id, allocation, intervention_model, ",
          "observational_model, primary_purpose, time_perspective, masking ",
          "masking_description, intervention_model_description ",
          "from %sdesigns;"),
          collapse = ""),
        format_schema()
      )
    )
  )

  tbl_eligb <- tibble::as_tibble(
    DBI::dbGetQuery(
      con,
      sprintf(
        paste(c("select nct_id, sampling_method, gender, minimum_age,  ",
          "maximum_age, population, criteria from %seligibilities;"),
          collapse = ""),
        format_schema()
      )
    )
  )

  tbl_refs <- tibble::as_tibble(
    DBI::dbGetQuery(
      con,
      sprintf(
        paste(c("select nct_id, pmid, citation ",
          "from %sstudy_references;"),
          collapse = ""),
        format_schema()
      )
    )
  )

  tbl_outcome <- tibble::as_tibble(
    DBI::dbGetQuery(
      con,
      sprintf(
        paste(c("select id, nct_id, outcome_type, title ",
          "from %soutcomes;"),
          collapse = ""),
        format_schema()
      )
    )
  )

  tbl_outcome_ana <- tibble::as_tibble(
    DBI::dbGetQuery(
      con,
      sprintf(
        paste(c("select outcome_id, param_type, param_value, p_value ",
          "from %soutcome_analyses;"),
          collapse = ""),
        format_schema()
      )
    )
  )

  # Create a few variables
  cmsg(verbose, "[%s] CREATE VARIABLES\n", isotime())
  tbl_inter$name <- sprintf(
    "%s: %s", tbl_inter$intervention_type, tbl_inter$name
  )
  tbl_study <- dplyr::filter(tbl_study, !duplicated(.data$nct_id))
  tbl_bfsum <- dplyr::filter(tbl_bfsum, !duplicated(.data$nct_id))
  tbl_spons <- dplyr::filter(tbl_spons, !duplicated(.data$nct_id))
  tbl_desig <- dplyr::filter(tbl_desig, !duplicated(.data$nct_id))
  tbl_eligb <- dplyr::filter(tbl_eligb, !duplicated(.data$nct_id))

  # Clean and select a field extra fields
  tbl_eligb$minimum_age <- convert_age_string(tbl_eligb$minimum_age)
  tbl_eligb$maximum_age <- convert_age_string(tbl_eligb$maximum_age)
  tbl_spons <- dplyr::select(
    tbl_spons,
    .data$nct_id, sponsor = .data$name, sponsor_type = .data$agency_class
  )

  # Get some ids
  cmsg(verbose, "[%s] STORE SECONDARY IDS\n", isotime())
  tbl_eudra <- dplyr::filter(
    tbl_idinf,
    .data$id_type == "secondary_id",
    !duplicated(.data$nct_id)
  )
  tbl_eudra <- dplyr::transmute(
    tbl_eudra, nct_id = .data$nct_id, eudract_num = .data$id_value
  )
  tbl_orgid <- dplyr::filter(tbl_idinf, .data$id_type == "org_study_id")
  tbl_orgid <- dplyr::group_by(tbl_orgid, .data$nct_id)
  tbl_orgid <- dplyr::transmute(
    tbl_orgid,
    nct_id = .data$nct_id,
    other_id = paste(.data$id_value, collapse = "\n")
  )
  tbl_orgid <- dplyr::ungroup(tbl_orgid)

  tbl_study <- dplyr::left_join(tbl_study, tbl_bfsum, by = "nct_id")
  tbl_study <- dplyr::left_join(tbl_study, tbl_eudra, by = "nct_id")
  tbl_study <- dplyr::left_join(tbl_study, tbl_orgid, by = "nct_id")

  # Fix description
  cmsg(verbose, "[%s] FORMAT FREE TEXT FIELDS\n", isotime())
  tbl_study$description <- clean_text_field(tbl_study$description)

  # Collapse columns to "nct_id"
  cmsg(verbose, "[%s] SAVE CONDITIONS AND DRUG NAMES\n", isotime())

  tbl_conds$name <- stringi::stri_replace_all(tbl_conds$name, " ", fixed = "|")
  tbl_conds <- dplyr::summarize(
    dplyr::group_by(tbl_conds, .data$nct_id),
    conditions = stringi::stri_paste(.data$name, collapse = "|")
  )

  tbl_inter_nest <- dplyr::nest_by(
    tbl_inter, .data$nct_id, .key = "interventions"
  )

  tbl_outcm_nest <- dplyr::nest_by(
    tbl_outcm, .data$nct_id, .key = "outcomes"
  )

  # Join into combined tables
  cmsg(verbose, "[%s] STORE COMBINED DATA\n", isotime())

  tbl_join <- dplyr::left_join(tbl_study, tbl_desig, by = "nct_id")
  tbl_join <- dplyr::left_join(tbl_join, tbl_eligb, by = "nct_id")
  tbl_join <- dplyr::left_join(tbl_join, tbl_spons, by = "nct_id")
  tbl_join <- dplyr::left_join(tbl_join, tbl_conds, by = "nct_id")
  tbl_join <- dplyr::left_join(tbl_join, tbl_inter_nest, by = "nct_id")
  tbl_join <- dplyr::left_join(tbl_join, tbl_outcm_nest, by = "nct_id")
  tbl_join <- dplyr::arrange(tbl_join, dplyr::desc(.data$start_date))

  # Save the main data in memory
  cmsg(verbose, "[%s] LOADING %d ROWS OF DATA\n", isotime(), nrow(tbl_join))
  .volatiles$tbl$join <- tbl_join
  make_categories()

  # Create publications data
  cmsg(verbose, "[%s] CREATING PUBLICATION DATA\n", isotime())
  tbl_refs$doi <- stringi::stri_sub(
    stringi::stri_extract_first(
      tbl_refs$citation, regex = "doi: [^ ]+"
    ), 6L, -1L
  )
  .volatiles$tbl$refs <- tbl_refs

  # Create outcome data
  cmsg(verbose, "[%s] CREATING OUTCOME DATA\n", isotime())
  tbl_outcome <- dplyr::inner_join(
    tbl_outcome, tbl_outcome_ana, by = c("id" = "outcome_id")
  )
  tbl_outcome <- dplyr::select(tbl_outcome, -.data$id)
  .volatiles$tbl$outcome <- tbl_outcome
}

#' Load sample dataset
#'
#' This function loads a sample dataset for testing and prototyping purposes.
#' after running, all of the functions in the package can then be used with
#' this sample data. It consists of a random sample of trials that was available
#' from ClinicalTrials.gov at the time of the package creation.
#'
#' @param cancer_studies     logical; should we load a currated list of cancer
#'                           clinical trials
#'
#' @author Taylor B. Arnold, \email{taylor.arnold@@acm.org}
#' @return does not return any value; used only for side effects
#'
#' @export
#' @importFrom utils data
ctgov_load_sample <- function(cancer_studies = FALSE)
{
  if (!cancer_studies)
  {
    data("tbl_join_sample", package = "ctrialsgov", envir = (en <- new.env()))
    .volatiles$tbl <- en$tbl_join_sample
  } else {
    data("cancer_studies", package = "ctrialsgov", envir = (en <- new.env()))
    .volatiles$tbl <- en$cancer_studies
  }

  make_categories()
}

#' Download and/or load cached data
#'
#' This function downloads a saved version of the full clinical trials dataset
#' from the package's development repository on GitHub (~150MB) and loads it
#' into R for querying. The data will be cached so that it can be re-loaded
#' without downloading. We try to update the cache frequently so this is a
#' convenient way of grabbing the data if you do not need the most up-to-date
#' version of the database.
#'
#' @param force_download   logical flag; should the cache be re-downloaded if
#'                         it already exists? defaults to \code{FALSE}
#'
#' @author Taylor B. Arnold, \email{taylor.arnold@@acm.org}
#' @return does not return any value; used only for side effects
#'
#' @export
#' @importFrom dplyr bind_rows
#' @importFrom utils download.file
ctgov_load_cache <- function(force_download = FALSE) {
  assert(is.logical(force_download) & length(force_download) == 1L)

  # local and GitHub base links
  dname <- system.file("extdata", package = "ctrialsgov")
  base_url <- paste0(
    "https://github.com/presagia-analytics/ctrialsgov/releases/download",
    "/data/tbl_data.Rds"
  )
  fp <- file.path(dname, "tbl_data.Rds")

  # download the files if needed
  if ( (!file.exists(fp)) | force_download)
  {
    download.file(base_url, fp)
  }

  # combine the datasets and store in the volatiles object
  .volatiles$tbl <- readRDS(fp)
  make_categories()
}

#' Save Current Database
#'
#' Saves a version of the current active database as a binary R
#' file.
#'
#' @param file      a character string naming a file; should have
#'                  the extension 'rds'
#'
#' @author Taylor B. Arnold, \email{taylor.arnold@@acm.org}
#' @return does not return any value; used only for side effects
#'
#' @export
ctgov_save_file <- function(file) {
  assert(is.character(file) & length(file) == 1L)

  saveRDS(.volatiles$tbl, file)
}

#' Load Database from File
#'
#' Loads a version of the current active database as a binary R
#' file.
#'
#' @param file      a character string naming a file; should have
#'                  the extension 'rds'
#'
#' @author Taylor B. Arnold, \email{taylor.arnold@@acm.org}
#' @return does not return any value; used only for side effects
#'
#' @export
ctgov_load_file <- function(file) {
  assert(is.character(file) & length(file) == 1L)

  .volatiles$tbl <- readRDS(file)
  make_categories()
}
