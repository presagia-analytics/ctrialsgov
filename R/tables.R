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

#' Return Full Table of Endpoint results
#'
#' This function returns the currently loaded version of a dataset showing the
#' whether the reported data suggests that the study endpoint was either met or
#' not met. This is an algorithmically generated field and is likey to contain
#' errors.
#'
#' @author Taylor B. Arnold, \email{taylor.arnold@@acm.org}
#' @return a tibble object of the results
#' @export
ctgov_table_joined <- function()
{
  assert_data_loaded()
  .volatiles$tbl$epoint
}
