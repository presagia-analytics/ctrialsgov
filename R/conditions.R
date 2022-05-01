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
#' @importFrom tokenizers tokenize_ngrams tokenize_skip_ngrams
#' @importFrom stringi stri_split stri_count stri_trans_tolower
#' @importFrom dplyr group_by inner_join filter select ungroup left_join .data
#' @importFrom tibble tibble
#' @importFrom rlang .data
#' @importFrom purrr map2
#' @export
ctgov_norm_conditions <- function(data)
{
  conds <- stringi::stri_split(data$conditions, fixed = "|")
  df <- tibble::tibble(
    nct_id = rep(data$nct_id, sapply(conds, length)),
    cond_id = unlist(sapply(sapply(conds, length), seq_len)),
    cond = unlist(conds),
    text = stringi::stri_trans_tolower(.data$cond)
  )

  ng <- tokenizers::tokenize_ngrams(df$text, n_min = 1L, n = 7L)
  ns <- tokenizers::tokenize_skip_ngrams(df$text, n_min = 2L, n = 2L, k = 1L)
  na <- purrr::map2(ng, ns, function(u, v) c(u, v))
  nt <- purrr::map2(
    ng, ns, function(u, v) c(rep(1, length(u)), rep(2, length(v)))
  )

  ngram_df <- tibble::tibble(
    nct_id = rep(df$nct_id, sapply(na, length)),
    cond_id = rep(df$cond_id, sapply(na, length)),
    ngram = unlist(na),
    nterm = stringi::stri_count(.data$ngram, fixed = " ") + 1L,
    type = unlist(nt)
  )

  # create a results table of the normalised data
  res <- dplyr::inner_join(ngram_df, condition_lookup, by = "ngram")
  res <- dplyr::group_by(res, .data$nct_id, .data$cond_id)
  res <- dplyr::filter(res, nterm == max(.data$nterm))
  res <- dplyr::filter(res, type == min(.data$type))
  res <- dplyr::filter(res, ncount == max(.data$ncount))
  res <- dplyr::ungroup(res)
  res <- dplyr::select(res, .data$nct_id, condition = .data$display)
  res <- unique(res)

  # combine with the original data
  dt <- dplyr::select(data, nct_id)
  dt <- dplyr::left_join(dt, res, by = "nct_id")
  index <- which(is.na(dt$condition))
  dt$norm_flag <- !is.na(dt$condition)
  if (length(index))
  {
    dt$condition[index] <- df$cond[match(dt$nct_id[index], df$nct_id)]
  }
  dt
}
