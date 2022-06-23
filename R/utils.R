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
    !is.null(.volatiles$con),
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

get_lvl <- function(v) {
  v <- unique(v)
  v <- v[!is.na(v)]
  v <- sort(v)
  v
}

#' @importFrom DBI dbReadTable
make_categories <- function()
{
  assert_data_loaded()

  join <- dbReadTable(.volatiles$con, name = "join")
  .volatiles$ol <- list(
    study_type = get_lvl(join$study_type),
    allocation = get_lvl(join$allocation),
    intervention_model = get_lvl(join$intervention_model),
    observational_model = get_lvl(join$observational_model),
    primary_purpose = get_lvl(join$primary_purpose),
    time_perspective = get_lvl(join$time_perspective),
    masking_description = get_lvl(join$masking_description),
    sampling_method = get_lvl(join$sampling_method),
    phase = get_lvl(join$phase),
    gender = get_lvl(join$gender),
    sponsor_type = get_lvl(join$sponsor_type)
  )

}

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
