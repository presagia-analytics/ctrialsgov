#' Return Full Table of References
#'
#' This function returns the currently loaded version of the references data
#' from the clinical trials database.
#'
#' @author Taylor B. Arnold, \email{taylor.arnold@@acm.org}
#' @return a tibble object of the results
#' @export
ctgov_table_references <- function()
{
  assert_data_loaded()
  .volatiles$tbl$refs
}

#' Return Full Table of Outcomes
#'
#' This function returns the currently loaded version of the outcomes data
#' from the clinical trials database.
#'
#' @author Taylor B. Arnold, \email{taylor.arnold@@acm.org}
#' @return a tibble object of the results
#' @export
ctgov_table_outcomes <- function()
{
  assert_data_loaded()
  .volatiles$tbl$outcome
}

#' Return Full Table of Trials
#'
#' This function returns the currently loaded version of the joined data
#' from the clinical trials database. This is the same as running the query
#' function with no parameters, but included for consistency.
#'
#' @author Taylor B. Arnold, \email{taylor.arnold@@acm.org}
#' @return a tibble object of the results
#' @export
ctgov_table_joined <- function()
{
  assert_data_loaded()
  .volatiles$tbl$join
}
