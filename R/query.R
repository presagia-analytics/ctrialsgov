#' Query the ClinicalTrials.gov dataset
#'
#' This function selects a subset of the clinical trials data by using a
#' a variety of different search parameters. These include free text search
#' keywords, range queries for the continuous variables, and exact matches for
#' categorical fields. The function \code{ctgov_get_query_terms} shows the
#' categorical levels for the latter. The function will either take the entire
#' dataset loaded into the package environment or a previously queried input.
#'
#'
#' @param data                    a dataset to search over; set to \code{NULL}
#'                                to use the full dataset that is currently
#'                                loaded
#'
#' @param description_kw          character vector of keywords to search in the
#'                                intervention description field. Set to
#'                                \code{NULL} to avoid searching this field.
#'
#' @param sponsor_kw              character vector of keywords to search in the
#'                                sponsor (the company that submitted the study).
#'                                Set to \code{NULL} to avoid searching this
#'                                field.
#'
#' @param brief_title_kw          character vector of keywords to search in the
#'                                brief title field. Set to
#'                                \code{NULL} to avoid searching this field.
#'
#' @param official_title_kw       character vector of keywords to search in the
#'                                official title field. Set to
#'                                \code{NULL} to avoid searching this field.
#'
#' @param criteria_kw             character vector of keywords to search in the
#'                                criteria field. Set to
#'                                \code{NULL} to avoid searching this field.
#'
#' @param intervention_kw         character vector of keywords to search in the
#'                                intervention names field. Set to
#'                                \code{NULL} to avoid searching this field.
#'
#' @param intervention_desc_kw    character vector of keywords to search in the
#'                                intervention description field. Set to
#'                                \code{NULL} to avoid searching this field.
#'
#' @param conditions_kw           character vector of keywords to search in the
#'                                conditions field. Set to
#'                                \code{NULL} to avoid searching this field.
#'
#' @param population_kw           character vector of keywords to search in the
#'                                population field. Set to
#'                                \code{NULL} to avoid searching this field.
#'
#' @param date_range              string of length two formatted as "YYYY-MM-DD"
#'                                describing the earliest and latest data to
#'                                include in the results. Use a missing value
#'                                for either value search all dates. Set to
#'                                \code{NULL} to avoid searching this field.
#'
#' @param enrollment_range        numeric of length two describing the smallest
#'                                and largest enrollment sizes to
#'                                include in the results. Use a missing value
#'                                for either value to avoid filtering. Set to
#'                                \code{NULL} to avoid searching this field.
#'
#' @param minimum_age_range       numeric of length two describing the smallest
#'                                and largest minmum age (in years) to
#'                                include in the results. Use a missing value
#'                                for either value to avoid filtering. Set to
#'                                \code{NULL} to avoid searching this field.
#'
#' @param maximum_age_range       numeric of length two describing the smallest
#'                                and largest maximum age (in years) to
#'                                include in the results. Use a missing value
#'                                for either value to avoid filtering. Set to
#'                                \code{NULL} to avoid searching this field.
#'
#' @param study_type              character vector of study types to include
#'                                in the output. Set to \code{NULL} to avoid
#'                                searching this field.
#'
#' @param allocation              character vector of allocations to include
#'                                in the output. Set to \code{NULL} to avoid
#'                                searching this field.
#'
#' @param intervention_model      character vector of interventions to include
#'                                in the output. Set to \code{NULL} to avoid
#'                                searching this field.
#'
#' @param observational_model     character vector of observations to include
#'                                in the output. Set to \code{NULL} to avoid
#'                                searching this field.
#'
#' @param primary_purpose         character vector of primary purposes to
#'                                include in the output. Set to \code{NULL} to
#'                                avoid searching this field.
#'
#' @param time_perspective        character vector of time perspectives to
#'                                include in the output. Set to \code{NULL} to
#'                                avoid searching this field.
#'
#' @param masking_description     character vector of maskings to include
#'                                in the output. Set to \code{NULL} to avoid
#'                                searching this field.
#'
#' @param sampling_method         character vector of sampling methods to
#'                                include in the output. Set to \code{NULL} to
#'                                avoid searching this field.
#'
#' @param phase                   character vector of phases to include
#'                                in the output. Set to \code{NULL} to avoid
#'                                searching this field.
#'
#' @param gender                  character vector of genders to include
#'                                in the output. Set to \code{NULL} to avoid
#'                                searching this field.
#'
#' @param sponsor_type            character vector of sponsor types to include
#'                                in the output. Set to \code{NULL} to avoid
#'                                searching this field.
#'
#' @param ignore_case             logical. Should the search ignore
#'                                capitalization. The default is \code{TRUE}.
#'
#' @param match_all               logical. Should the results required matching
#'                                all the keywords? The default is \code{FALSE}.
#'
#'
#' @author Taylor B. Arnold, \email{taylor.arnold@@acm.org}
#' @return a tibble object queried from the loaded database
#'
#' @importFrom tibble as_tibble
#' @importFrom DBI dbReadTable
#' @importFrom rlang .data .env
#' @importFrom dplyr filter collect tbl
#' @export
ctgov_query <- function(
  data = NULL,
  description_kw = NULL,
  sponsor_kw = NULL,
  brief_title_kw = NULL,
  official_title_kw = NULL,
  criteria_kw = NULL,
  intervention_kw = NULL,
  intervention_desc_kw = NULL,
  conditions_kw = NULL,
  population_kw = NULL,
  date_range = NULL,
  enrollment_range = NULL,
  minimum_age_range = NULL,
  maximum_age_range = NULL,
  study_type = NULL,
  allocation = NULL,
  intervention_model = NULL,
  observational_model = NULL,
  primary_purpose =  NULL,
  time_perspective = NULL,
  masking_description = NULL,
  sampling_method = NULL,
  phase = NULL,
  gender = NULL,
  sponsor_type = NULL,
  ignore_case = TRUE,
  match_all = FALSE
) {

  ############################################################################
  # check query input types
  assert(is.null(description_kw) | is.character(description_kw))
  assert(is.null(sponsor_kw) | is.character(sponsor_kw))
  assert(is.null(brief_title_kw) | is.character(brief_title_kw))
  assert(is.null(official_title_kw) | is.character(official_title_kw))
  assert(is.null(intervention_kw) | is.character(intervention_kw))
  assert(is.null(intervention_desc_kw) | is.character(intervention_desc_kw))
  assert(is.null(conditions_kw) | is.character(conditions_kw))
  assert(is.null(population_kw) | is.character(population_kw))

  assert(is.null(date_range) |
         (is.character(date_range) & length(date_range) == 2L))
  assert(is.null(enrollment_range) |
          (is.numeric(enrollment_range) & length(enrollment_range) == 2L))
  assert(is.null(minimum_age_range) |
         (is.numeric(minimum_age_range) & length(minimum_age_range) == 2L))
  assert(is.null(maximum_age_range) |
         (is.numeric(maximum_age_range) & length(maximum_age_range) == 2L))

  assert(is.null(study_type) | is.character(study_type))
  assert(is.null(allocation) | is.character(allocation))
  assert(is.null(intervention_model) | is.character(intervention_model))
  assert(is.null(observational_model) | is.character(observational_model))
  assert(is.null(primary_purpose) | is.character(primary_purpose))
  assert(is.null(time_perspective) | is.character(time_perspective))
  assert(is.null(masking_description) | is.character(masking_description))
  assert(is.null(phase) | is.character(phase))
  assert(is.null(gender) | is.character(gender))
  assert(is.null(sponsor_type) | is.character(sponsor_type))

  ############################################################################
  # check that search options match choices; do this first to makes sure any
  # errors are caught fast
  if (!is.null(study_type)) { match.arg(study_type, .volatiles$ol$study_type, TRUE) }
  if (!is.null(allocation)) { match.arg(allocation, .volatiles$ol$allocation, TRUE) }
  if (!is.null(intervention_model))
  {
    match.arg(intervention_model, .volatiles$ol$intervention_model, TRUE)
  }
  if (!is.null(observational_model)) {
    match.arg(observational_model, .volatiles$ol$observational_model, TRUE)
  }
  if (!is.null(primary_purpose)) {
    match.arg(primary_purpose, .volatiles$ol$primary_purpose, TRUE)
  }
  if (!is.null(time_perspective))
  {
    match.arg(time_perspective, .volatiles$ol$time_perspective, TRUE)
  }
  if (!is.null(masking_description))
  {
    match.arg(masking_description, .volatiles$ol$masking_description, TRUE)
  }
  if (!is.null(sampling_method))
  {
    match.arg(sampling_method, .volatiles$ol$sampling_method, TRUE)
  }
  if (!is.null(phase)) { match.arg(phase, .volatiles$ol$phase, TRUE) }
  if (!is.null(gender)) { match.arg(phase, .volatiles$ol$gender, TRUE) }
  if (!is.null(sponsor_type)) { match.arg(sponsor_type, .volatiles$ol$sponsor_type, TRUE) }

  ############################################################################
  # convert the date range to a date object; again, do this first to make sure
  # errors are caught fast
  if (!is.null(date_range)) { date_range <- lubridate::ymd(date_range) }

  ############################################################################
  # if no data was given, grab the current version of the data
  if (is.null(data))
  {
    assert_data_loaded()
    z <- tbl(.volatiles$con, "join")
  } else {
    z <- data
  }

  ############################################################################
  # apply each of the categorical filters; these are fast so do them first
  if (!is.null(study_type))
  {
    z <- filter(z, .data$study_type %in% .env$study_type)
  }
  if (!is.null(allocation))
  {
    z <- filter(z, .data$allocation %in% .env$allocation)
  }
  if (!is.null(intervention_model))
  {
    z <- filter(z, .data$intervention_model %in% .env$intervention_model)
  }
  if (!is.null(observational_model))
  {
    z <- filter(z, .data$observational_model %in% .env$observational_model)
  }
  if (!is.null(primary_purpose))
  {
    z <- filter(z, .data$primary_purpose %in% .env$primary_purpose)
  }
  if (!is.null(time_perspective))
  {
    z <- filter(z, .data$time_perspective %in% .env$time_perspective)
  }
  if (!is.null(masking_description))
  {
    z <- filter(z, .data$masking_description %in% .env$masking_description)
  }
  if (!is.null(sampling_method))
  {
    z <- filter(z, .data$sampling_method %in% .env$sampling_method)
  }
  if (!is.null(phase))
  {
    z <- filter(z, .data$phase %in% .env$phase)
  }
  if (!is.null(gender))
  {
    z <- filter(z, .data$gender %in% .env$gender)
  }
  if (!is.null(sponsor_type))
  {
    z <- filter(z, .data$sponsor_type %in% .env$sponsor_type)
  }

  ############################################################################
  # apply each of continuous range filters; these are also fast
  if (!is.null(date_range))
  {
    if (!is.na(date_range[1]))
    {
      val <- date_range[1]
      z <- filter(z, .data$start_date >= .env$val)
    }
    if (!is.na(date_range[2]))
    {
      val <- date_range[2]
      z <- filter(z, .data$start_date <= .env$val)
    }
  }
  if (!is.null(enrollment_range))
  {
    if (!is.na(enrollment_range[1]))
    {
      val <- enrollment_range[1]
      z <- filter(z, .data$enrollment >= .env$val)
    }
    if (!is.na(enrollment_range[2]))
    {
      val <- enrollment_range[2]
      z <- filter(z, .data$enrollment <= .env$val)
    }
  }
  if (!is.null(minimum_age_range))
  {
    if (!is.na(minimum_age_range[1]))
    {
      val <- minimum_age_range[1]
      z <- filter(z, .data$minimum_age >= .env$val)
    }
    if (!is.na(minimum_age_range[2]))
    {
      val <- minimum_age_range[2]
      z <- filter(z, .data$minimum_age <= .env$val)
    }
  }
  if (!is.null(maximum_age_range))
  {
    if (!is.na(maximum_age_range[1]))
    {
      val <- maximum_age_range[1]
      z <- filter(z, .data$maximum_age >= .env$val)
    }
    if (!is.na(maximum_age_range[2]))
    {
      val <- maximum_age_range[2]
      z <- filter(z, .data$maximum_age <= .env$val)
    }
  }

  ############################################################################
  # finally, do the keyword searches
  if (!is.null(description_kw))
  {
    z <- query_kwds(z, description_kw, "description", ignore_case, match_all)
  }
  if (!is.null(sponsor_kw))
  {
    z <- query_kwds(z, sponsor_kw, "sponsor", ignore_case, match_all)
  }
  if (!is.null(brief_title_kw))
  {
    z <- query_kwds(z, brief_title_kw, "brief_title", ignore_case, match_all)
  }
  if (!is.null(official_title_kw))
  {
    z <- query_kwds(z, official_title_kw, "official_title", ignore_case, match_all)
  }
  if (!is.null(criteria_kw))
  {
    z <- query_kwds(z, criteria_kw, "criteria", ignore_case, match_all)
  }
  if (!is.null(intervention_desc_kw))
  {
    z <- query_kwds(
      z,
      intervention_desc_kw,
      "intervention_model_description",
      ignore_case,
      match_all
    )
  }
  if (!is.null(conditions_kw))
  {
    z <- query_kwds(z, conditions_kw, "conditions", ignore_case, match_all)
  }
  if (!is.null(population_kw))
  {
    z <- query_kwds(z, population_kw, "population", ignore_case, match_all)
  }

  # return the results
  return(dplyr::collect(z))
}


#' Query design data from the ClinicalTrials.gov dataset
#'
#' This function selects a subset of the clinical trials data by using a
#' a variety of different search parameters. These include free text search
#' keywords, range queries for the continuous variables, and exact matches for
#' categorical fields. The function \code{ctgov_get_query_terms} shows the
#' categorical levels for the latter. The function will either take the entire
#' dataset loaded into the package environment or a previously queried input.
#'
#'
#' @param data                    a dataset to search over; set to \code{NULL}
#'                                to use the full dataset that is currently
#'                                loaded
#'
#' @param design_kw               character vector of keywords to search in the
#'                                outcome measures field. Set to
#'                                \code{NULL} to avoid searching this field.
#'
#' @param design_desc_kw          character vector of keywords to search in the
#'                                outcome description field. Set to
#'                                \code{NULL} to avoid searching this field.
#'
#' @param ignore_case             logical. Should the search ignore
#'                                capitalization. The default is \code{TRUE}.
#'
#' @param match_all               logical. Should the results required matching
#'                                all the keywords? The default is \code{FALSE}.
#'
#'
#' @author Taylor B. Arnold, \email{taylor.arnold@@acm.org}
#' @return a tibble object queried from the loaded database
#'
#' @importFrom tibble as_tibble
#' @importFrom DBI dbReadTable
#' @importFrom dplyr collect
#' @export
ctgov_query_design <- function(
  data = NULL,
  design_kw = NULL,
  design_desc_kw = NULL,
  ignore_case = TRUE,
  match_all = FALSE
) {
  ############################################################################
  # check query input types
  assert(is.null(design_kw) | is.character(design_kw))
  assert(is.null(design_desc_kw) | is.character(design_desc_kw))

  ############################################################################
  # if no data was given, grab the current version of the data
  if (is.null(data))
  {
    assert_data_loaded()
    z <- tbl(.volatiles$con, "design")
  } else {
    z <- data
  }

  ############################################################################
  # do the keyword searches
  if (!is.null(design_kw))
  {
    z <- query_kwds(z, design_kw, "measure", ignore_case, match_all)
  }
  if (!is.null(design_desc_kw))
  {
    z <- query_kwds(z, design_desc_kw, "description", ignore_case, match_all)
  }

  # return the results
  return(dplyr::collect(z))
}


#' Query intervention data from the ClinicalTrials.gov dataset
#'
#' This function selects a subset of the clinical trials data by using a
#' a variety of different search parameters. These include free text search
#' keywords, range queries for the continuous variables, and exact matches for
#' categorical fields. The function \code{ctgov_get_query_terms} shows the
#' categorical levels for the latter. The function will either take the entire
#' dataset loaded into the package environment or a previously queried input.
#'
#'
#' @param data                    a dataset to search over; set to \code{NULL}
#'                                to use the full dataset that is currently
#'                                loaded
#'
#' @param intervention_kw         character vector of keywords to search in the
#'                                intervention names field. Set to
#'                                \code{NULL} to avoid searching this field.
#'
#' @param ignore_case             logical. Should the search ignore
#'                                capitalization. The default is \code{TRUE}.
#'
#' @param match_all               logical. Should the results required matching
#'                                all the keywords? The default is \code{FALSE}.
#'
#'
#' @author Taylor B. Arnold, \email{taylor.arnold@@acm.org}
#' @return a tibble object queried from the loaded database
#'
#' @importFrom tibble as_tibble
#' @importFrom DBI dbReadTable
#' @importFrom dplyr collect
#' @export
ctgov_query_intervention <- function(
  data = NULL,
  intervention_kw = NULL,
  ignore_case = TRUE,
  match_all = FALSE
) {
  ############################################################################
  # check query input types
  assert(is.null(intervention_kw) | is.character(intervention_kw))

  ############################################################################
  # if no data was given, grab the current version of the data
  if (is.null(data))
  {
    assert_data_loaded()
    z <- tbl(.volatiles$con, "inter")
  } else {
    z <- data
  }

  ############################################################################
  # do the keyword searches
  if (!is.null(intervention_kw))
  {
    z <- query_kwds(z, intervention_kw, "name", ignore_case, match_all)
  }

  # return the results
  return(dplyr::collect(z))
}


#' Query references table from the ClinicalTrials.gov dataset
#'
#' This function selects a subset of the clinical trials data by using a
#' a variety of different search parameters. These include free text search
#' keywords, range queries for the continuous variables, and exact matches for
#' categorical fields. The function \code{ctgov_get_query_terms} shows the
#' categorical levels for the latter. The function will either take the entire
#' dataset loaded into the package environment or a previously queried input.
#'
#'
#' @param data                    a dataset to search over; set to \code{NULL}
#'                                to use the full dataset that is currently
#'                                loaded
#'
#' @param ignore_case             logical. Should the search ignore
#'                                capitalization. The default is \code{TRUE}.
#'
#' @param match_all               logical. Should the results required matching
#'                                all the keywords? The default is \code{FALSE}.
#'
#' @author Taylor B. Arnold, \email{taylor.arnold@@acm.org}
#' @return a tibble object queried from the loaded database
#'
#' @importFrom tibble as_tibble
#' @importFrom DBI dbReadTable
#' @importFrom dplyr collect
#' @export
ctgov_query_references <- function(
  data = NULL,
  ignore_case = TRUE,
  match_all = FALSE
) {
  ############################################################################
  # check query input types

  ############################################################################
  # if no data was given, grab the current version of the data
  if (is.null(data))
  {
    assert_data_loaded()
    z <- tbl(.volatiles$con, "refs")
  } else {
    z <- data
  }

  ############################################################################
  # do the keyword searches

  # return the results
  return(dplyr::collect(z))
}


#' Query outcome table from the ClinicalTrials.gov dataset
#'
#' This function selects a subset of the clinical trials data by using a
#' a variety of different search parameters. These include free text search
#' keywords, range queries for the continuous variables, and exact matches for
#' categorical fields. The function \code{ctgov_get_query_terms} shows the
#' categorical levels for the latter. The function will either take the entire
#' dataset loaded into the package environment or a previously queried input.
#'
#'
#' @param data                    a dataset to search over; set to \code{NULL}
#'                                to use the full dataset that is currently
#'                                loaded
#'
#' @param ignore_case             logical. Should the search ignore
#'                                capitalization. The default is \code{TRUE}.
#'
#' @param match_all               logical. Should the results required matching
#'                                all the keywords? The default is \code{FALSE}.
#'
#' @author Taylor B. Arnold, \email{taylor.arnold@@acm.org}
#' @return a tibble object queried from the loaded database
#'
#' @importFrom tibble as_tibble
#' @importFrom DBI dbReadTable
#' @importFrom dplyr collect
#' @export
ctgov_query_outcome <- function(
  data = NULL,
  ignore_case = TRUE,
  match_all = FALSE
) {
  ############################################################################
  # check query input types

  ############################################################################
  # if no data was given, grab the current version of the data
  if (is.null(data))
  {
    assert_data_loaded()
    z <- tbl(.volatiles$con, "outcome")
  } else {
    z <- data
  }

  ############################################################################
  # do the keyword searches

  # return the results
  return(dplyr::collect(z))
}


#' Query endpoint table from the ClinicalTrials.gov dataset
#'
#' This function selects a subset of the clinical trials data by using a
#' a variety of different search parameters. These include free text search
#' keywords, range queries for the continuous variables, and exact matches for
#' categorical fields. The function \code{ctgov_get_query_terms} shows the
#' categorical levels for the latter. The function will either take the entire
#' dataset loaded into the package environment or a previously queried input.
#'
#'
#' @param data                    a dataset to search over; set to \code{NULL}
#'                                to use the full dataset that is currently
#'                                loaded
#'
#' @param ignore_case             logical. Should the search ignore
#'                                capitalization. The default is \code{TRUE}.
#'
#' @param match_all               logical. Should the results required matching
#'                                all the keywords? The default is \code{FALSE}.
#'
#' @author Taylor B. Arnold, \email{taylor.arnold@@acm.org}
#' @return a tibble object queried from the loaded database
#'
#' @importFrom tibble as_tibble
#' @importFrom dplyr collect
#' @importFrom DBI dbReadTable
#' @export
ctgov_query_endpoint <- function(
  data = NULL,
  ignore_case = TRUE,
  match_all = FALSE
) {
  ############################################################################
  # check query input types

  ############################################################################
  # if no data was given, grab the current version of the data
  if (is.null(data))
  {
    assert_data_loaded()
    z <- tbl(.volatiles$con, "epoint")
  } else {
    z <- data
  }

  ############################################################################
  # do the keyword searches

  # return the results
  return(dplyr::collect(z))
}


#' Query the ClinicalTrials.gov dataset
#'
#' Returns a list showing the available category levels for querying the data
#' with the \code{ctgov_query} function.
#'
#' @return a named list of allowed categorical values for the query
#'
#' @export
ctgov_get_query_terms <- function()
{
  return(.volatiles$ol)
}
