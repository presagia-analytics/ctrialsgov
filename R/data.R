#' Sample Clinical Trials Dataset
#'
#' Data frame containing a 2.5 percent random sample of clinical trials.
#'
#' @name tbl_join_sample
#' @docType data
#' @keywords data
NULL

#' Sample of Industry Cancer Trials from 2021
#'
#' Cancer clinical trials based on a query where:
#' `study_type` is "Interventional";
#' `sponsor_type` is "Industry";
#' `date_range` is trials from 2021-01-01 or newer;
#' The `description` includes the keyword "cancer";
#' `phase` is reported (not NA);
#' `primary_purpose` is "Treatment";
#' `minimum_enrollment` is 100.
#'
#' @name cancer_studies
#' @docType data
#' @keywords data
NULL

#' Conditions Lookup Table
#'
#' Data frame giving a normalisation mapping for the conditions field in the
#' clinical trials database
#'
#' @name condition_lookup
#' @docType data
#' @keywords data
NULL
