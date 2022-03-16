#' Query the ClinicalTrials.gov dataset
#'
#' This function selects a subset of the clinical trials data by using a
#' a variety of different search parameters. These include free text search
#' keywords, range queries for the continuous variables, and exact matches for
#' categorical fields. The function \code{ctgov_query_terms} shows the
#' categorical levels for the latter. The function will either take the entire
#' dataset loaded into the package environment or a previously queried input.
#'
#'
#' @param data                    a dataset from which to extract the conditions
#'
#' @author Taylor B. Arnold, \email{taylor.arnold@@acm.org}
#' @return a tibble object giving normalised conditions for the input data
#'
#' @importFrom tokenizers tokenize_ngrams
#' @importFrom stringi stri_split stri_count stri_trans_tolower
#' @importFrom dplyr group_by inner_join filter select
#' @importFrom tibble tibble
#' @importFrom rlang .data
#' @export
ctgov_norm_conditions <- function(data)
{
  conds <- stringi::stri_split(data$conditions, fixed = "|")
  df <- tibble::tibble(
    nct_id = rep(data$nct_id, sapply(conds, length)),
    cond_id = unlist(sapply(sapply(conds, length), seq_len)),
    text = stringi::stri_trans_tolower(unlist(conds))
  )

  ng <- tokenizers::tokenize_ngrams(df$text, n_min = 1L, n = 7L)
  ngram_df <- tibble::tibble(
    nct_id = rep(df$nct_id, sapply(ng, length)),
    cond_id = rep(df$cond_id, sapply(ng, length)),
    ngram = unlist(ng),
    nterm = stringi::stri_count(.data$ngram, fixed = " ") + 1L
  )

  data(condition_lookup)
  res <- dplyr::inner_join(ngram_df, condition_lookup, by = "ngram")
  res <- dplyr::filter(dplyr::group_by(res, .data$nct_id), nterm == max(.data$nterm))
  res <- dplyr::select(res, .data$nct_id, condition = .data$display)
  res
}
