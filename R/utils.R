assert <- function(statement, msg="")
{
  if (!statement)
  {
    stop(msg, call.=(msg==""))
  }
}

assert_data_loaded <- function()
{
  assert(
    !is.null(.volatiles$tbl_join),
    paste0(c(
      "Before running a query you must load the clinical trials data into ",
      "R using one of these functions: ctgov_create_data, ctgov_load_sample, ",
      "or ctgov_load_cache. See the function examples and package vignettes ",
      "for further intructions."
    ), collapse = "")
  )
}

#' @importFrom stringi stri_trim stri_replace_all
clean_text_field <- function(field)
{
  field <- stringi::stri_replace_all(field, " ", regex = "[\t\r\n]")
  field <- stringi::stri_trim(stringi::stri_replace_all(
    field, " ", regex = "[ ]+"
  ))
  field
}

#' @importFrom dplyr if_else
#' @importFrom stringi stri_extract_first
convert_age_string <- function(age_string)
{
  type <- stringi::stri_extract_first(age_string, regex = "[A-Z][a-z]+")
  age <- as.numeric(
    stringi::stri_extract_first(age_string, regex = "[0-9]+")
  )
  age <- if_else(type == "Days", age / 365, age)
  age <- if_else(type == "Week", age / 52, age)
  age <- if_else(type == "Weeks", age / 52, age)
  age <- if_else(type == "Month", age / 12, age)
  age <- if_else(type == "Months", age / 12, age)
  age
}

#' @importFrom stringi stri_detect
search_kw <- function(txt, query, ignore_case = FALSE, match_all = FALSE)
{
  # if ignoring case, add regex flag
  if (ignore_case) { query <- sprintf("(?i)%s", query) }

  # search each of the keywords
  ind <- vector("list", length(query))
  for (j in seq_along(query))
  {
    ind[[j]] <- which(stringi::stri_detect(txt, regex = query[j]))
  }

  # aggregate search results based on the `match_all` flag.
  if (match_all)
  {
    ind <- sort(unique(Reduce(intersect, ind)))
  } else {
    ind <- sort(unique(unlist(ind)))
  }

  return(ind)
}

ol <- list(
  study_type = c("Interventional", "Observational",
                 "Observational [Patient Registry]", "Expanded Access"),
  allocation = c("Randomized", "N/A", "Non-Randomized"),
  intervention_model = c("Parallel Assignment", "Single Group Assignment",
                         "Crossover Assignment", "Sequential Assignment",
                         "Factorial Assignment"),
  observational_model = c("Cohort", "Case-Control", "Case-Only", "Other",
                          "Ecologic or Community", "Case-Crossover",
                          "Defined Population", "Family-Based"),
  primary_purpose =  c("Treatment", "Prevention", "Basic Science",
                       "Supportive Care", "Other", "Diagnostic",
                       "Health Services Research", "Screening",
                       "Device Feasibility", "Educational/Counseling/Training"),
  time_perspective = c("Prospective", "Retrospective",
                       "Cross-Sectional", "Other"),
  masking_description = c("None (Open Label)", "Single", "Double", "Triple",
                          "Quadruple"),
  sampling_method = c("", "Non-Probability Sample", "Probability Sample"),
  phase = c("N/A", "Early Phase 1", "Phase 1", "Phase 1/Phase 2", "Phase 2",
            "Phase 2/Phase 3", "Phase 3", "Phase 4"),
  gender = c("All", "Female", "Male"),
  sponsor_type = c("Industry", "NIH", "U.S. Fed", "Other")
)

ifnull <- function(value, default)
{
  if (is.null(value)) { return(default) }

  return(value)
}

cmsg <- function(verbose, fmt, ...)
{
  if (verbose)
  {
    cat(sprintf(fmt, ...))
  }
}

isotime <- function() {
  tm <- as.POSIXlt(Sys.time())
  return(strftime(tm , "%Y-%m-%dT%H:%M:%OS"))
}
