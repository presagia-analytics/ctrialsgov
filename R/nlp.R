#' Keywords in Context
#'
#' Take a
#'
#' @param term          search term as a string
#' @param text          vector of text to search
#' @param n             number of results to return; default is Inf
#' @param ignore_case   should search ignore case? default is TRUE
#' @param width         how many characters to show as context
#'
#' @return  a character vector of the search results
#'
#' @export
ctgov_kwic <- function(
  term,
  text,
  names = NULL,
  n = Inf,
  ignore_case = TRUE,
  use_color = FALSE,
  width = 20L,
  output = c("cat", "character", "data.frame")
)
{
  if (ignore_case) {pre <- "(?i)"} else {pre <- ""}
  if (!is.null(names)) { assert(length(text) == length(names)) }
  output <- match.arg(output)

  these <- stringi::stri_locate_all(
    text, regex = sprintf("%s%s", pre, term)
  )
  these_okay <- sapply(these, function(v) !is.na(v[1,1]), USE.NAMES = FALSE)
  these <- lapply(these[these_okay], tibble::as_tibble)
  df <- bind_rows(these)
  df$text <- rep(text[these_okay], sapply(these, nrow))
  if (is.null(names))
  {
    df$name <- ""
  } else {
    names <- rep(names[these_okay], sapply(these, nrow))
    max_width <- max(stringi::stri_length(names))
    pattern <- paste0("[%", max_width, "s] ", collapse = "")
    df$name <- sprintf(pattern, names)
  }

  df$start_w <- df$start - width
  df$end_w <- df$end + width

  df$pad_left <- 0L
  df$pad_left[df$start_w <= 0] <- abs(df$start_w[df$start_w <= 0]) + 1L
  df$pad_left <- sapply(
    df$pad_left, function(v) paste0(rep(" ", v), collapse = "")
  )
  df$start_w[df$start_w <= 0] <- 1L

  df$left <- paste0(df$pad_left, stringi::stri_sub(df$text, df$start_w, df$start - 1L))
  df$term <- stringi::stri_sub(df$text, df$start, df$end)
  df$right <- stringi::stri_sub(df$text, df$end + 1L, df$end_w)

  if ((output == "cat" ) & (use_color))
  {
    cat(
      sprintf("%s%s\033[31m%s\033[39m%s\n", df$name, df$left, df$term, df$right),
      sep = ""
    )
  } else if ((output == "cat" ) & (!use_color)) {
    cat(
      sprintf("%s%s|%s|%s\n", df$name, df$left, df$term, df$right),
      sep = ""
    )
  } else if (output == "character") {
    return(paste0(df$left, df$term, df$right))
  } else {
    return(dplyr::select(df, left, term, right, start, end))
  }
}

#' @export
ctgov_tfidf <- function(..., max_terms = 10000, tolower = TRUE, nterms = 5L)
{
  tokens <- get_tokens(...)
  cnts <- make_token_counts(tokens, max_terms = max_terms, tolower = tolower)
  tfidf <- create_tfidf_mat(cnts)
  terms <- create_tfidf_terms(tfidf, nterms = nterms)

  return(terms)
}

get_tokens <- function(...)
{
  inputs <- list(...)
  nsize <- length(inputs[[1]])
  assert(all(sapply(inputs, length) == nsize))

  tokens <- vector("list", nsize)
  for (iput in inputs)
  {
    tokens <- purrr::map2(
      tokens, stringi::stri_split_boundaries(iput, tokens_only = TRUE), c
    )
  }

  tokens
}

make_token_counts <- function(tokens, max_terms = 10000, tolower = TRUE)
{
  terms <- stringi::stri_trim(unlist(tokens))
  if (tolower) { terms <- stringi::stri_trans_tolower(terms) }
  terms <- stringi::stri_replace_all(terms, "", regex = "\\W")
  docs <- rep(seq_along(tokens), sapply(tokens, length))

  vocabulary <- names(sort(table(terms), decreasing = TRUE))
  if (length(vocabulary) > max_terms) { vocabulary <- vocabulary[seq(1, max_terms)] }

  index <- match(terms, vocabulary)
  terms <- terms[!is.na(index)]
  docs <- docs[!is.na(index)]

  term_counts <- Matrix::sparseMatrix(
     i = docs,
     j = index[!is.na(index)],
     x = rep(1, length(docs)),
     dims = c(length(tokens), length(vocabulary))
   )
   term_counts <- methods::as(term_counts, "dgTMatrix")
   colnames(term_counts) <- vocabulary

   return(term_counts)
}

create_tfidf_mat <- function(cnts)
{
  tf <- cnts
  tf@x <- 1 + log2(tf@x)
  tf@x[tf@x < 0] <- 0
  idf <- log2(nrow(tf) / apply(tf > 0, 2, sum))

  tfidf <- Matrix::t(Matrix::t(tf) * idf)
  tfidf <- methods::as(tfidf, "dgTMatrix")
  tfidf
}

create_tfidf_terms <- function(tfidf, nterms = 5L)
{
  df <- tibble::tibble(
    doc = tfidf@i,
    term = colnames(tfidf)[tfidf@j + 1L],
    count = tfidf@x
  )
  df <- dplyr::arrange(group_by(df, doc), desc(count))
  df <- dplyr::slice_head(df, n = nterms)
  dplyr::summarize(df, terms = paste0(term, collapse = "|"))
}

#' @export
ctgov_pca_order <- function(term_counts)
{
  tf <- term_counts
  tf@x <- 1 + log2(tf@x)
  tf@x[tf@x < 0] <- 0
  idf <- log2(nrow(tf) / apply(tf > 0, 2, sum))

  tfidf <- Matrix::t(Matrix::t(tf) * idf)
  tfidf <- methods::as(tfidf, "dgTMatrix")

  x <- as.matrix(tfidf)
  pca <- as.numeric(irlba::prcomp_irlba(t(x), n = 1, scale. = TRUE)$rotation)
  pca
}
