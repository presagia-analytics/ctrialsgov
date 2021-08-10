#' Sample Clinical Trials Dataset
#'
#' Data frame containing a 2.5% random sample of clinical trials.
#'
#' @name tbl_join_sample
#' @docType data
#' @keywords data
NULL

#' Sample of Industry Cancer Trials from 2021
#'
#' Cancer clinical trials based on
#' a query where:
#'
#' 1. `study_type` is "Interventional".
#' 2. `sponsor_type` is "Industry".
#' 3. `date_range` is trials from 2021-01-01 or newer.
#' 4. The `description` includes the keyword "cancer".
#' 5. `phase` is reported (not NA).
#' 6. `primary_purpose` is "Treatment".
#' 7. `minimum_enrollment` is 100.
#'
#' @name cancer_studies
#' @docType data
#' @keywords data
NULL
