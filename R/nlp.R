#' Keywords in Context
#'
#' Takes a keyword and vector of text and returns instances where the keyword
#' is found within the text.
#'
#' @param term          search term as a string
#' @param text          vector of text to search
#' @param names         optional vector of names corresponding to the text
#' @param n             number of results to return; default is Inf
#' @param ignore_case   should search ignore case? default is TRUE
#' @param use_color     printed results include ASCII color escape sequences;
#'                      these are set to \code{FALSE} because they only work
#'                      correctly when returned in the terminal
#' @param width         how many characters to show as context
#' @param output        what kind of output to provide; default prints the
#'                      results using \code{cat}
#'
#' @return  either nothing, character vector, or data frame depending on the
#'          the requested return type
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
    return(dplyr::select(
      df, .data$left, .data$term, .data$right, .data$start, .data$end
    ))
  }
}

#' TF-IDF Keywords
#'
#' Takes one or more vectors of text and returns a vector of keywords.
#'
#' @param ...           one or more vectors of text to search; must all be the
#'                      same length
#' @param max_terms     maximum number of terms to consider for keywords
#' @param tolower       should keywords respect the case of the raw terms
#' @param nterms        number of keyord terms to include
#' @param min_df        minimum proportion of documents that a term should
#'                      be present in to be included in the keywords
#' @param max_df        maximum proportion of documents that a term should
#'                      be present in to be included in the keywords
#' @param nterms        number of keyord terms to include
#'
#' @return  a character vector of detected keywords
#'
#' @export
ctgov_tfidf <- function(
  ..., max_terms = 10000, tolower = TRUE, nterms = 5L, min_df = 0, max_df = 1
)
{
  tokens <- get_tokens(...)
  cnts <- make_token_counts(
    tokens,
    max_terms = max_terms,
    tolower = tolower,
    min_df = min_df,
    max_df = max_df
  )
  tfidf <- create_tfidf_mat(cnts)
  terms <- create_tfidf_terms(tfidf, nterms = nterms)

  return(terms)
}

#' Similarity Matrix
#'
#' Takes one or more vectors of text and returns a similarity matrix.
#'
#' @param ...           one or more vectors of text to search; must all be the
#'                      same length
#' @param max_terms     maximum number of terms to consider for keywords
#' @param tolower       should keywords respect the case of the raw terms
#' @param min_df        minimum proportion of documents that a term should
#'                      be present in to be included in the keywords
#' @param max_df        maximum proportion of documents that a term should
#'                      be present in to be included in the keywords
#'
#' @return  a distance matrix
#'
#' @export
ctgov_text_similarity <- function(
  ..., max_terms = 10000, tolower = TRUE, min_df = 0, max_df = 1
)
{
  # get the TF-IDF matrix
  tokens <- get_tokens(...)
  if (length(tokens) > 1000)
  {
    warning("It is not recommend to create similarity scores for more than",
            "1000 records. This may take a while.")
  }
  cnts <- make_token_counts(
    tokens,
    max_terms = max_terms,
    tolower = tolower,
    min_df = min_df,
    max_df = max_df
  )
  tfidf <- create_tfidf_mat(cnts)

  # create the similarity scores
  x <- as.matrix(tfidf)
  sim <- x / sqrt(rowSums(x * x))
  sim <- sim %*% t(sim)
  sim
}


###############################################################################
# helper functions below; not exported

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

make_token_counts <- function(
  tokens, max_terms = 10000, tolower = TRUE, min_df = 0, max_df = 1
)
{
  # clean the tokens
  tokens_c <- lapply(tokens, stringi::stri_trim)
  if (tolower) { tokens_c <- lapply(tokens_c, stringi::stri_trans_tolower)}
  tokens_c <- lapply(tokens_c, stringi::stri_replace_all, "", regex = "\\W")

  terms <- unlist(tokens_c)
  docs <- rep(seq_along(tokens), sapply(tokens, length))

  # determine the vocabulary set:
  N <- length(tokens_c)
  possible_vocab <- unlist(lapply(tokens_c, unique))
  possible_vocab <- table(possible_vocab) / N
  possible_vocab <- possible_vocab[possible_vocab >= min_df &
      possible_vocab <= max_df]
  possible_vocab <- sort(possible_vocab, decreasing = TRUE)
  vocabulary <- names(possible_vocab[seq(1, min(max_terms,
      length(possible_vocab)))])

  assert(length(vocabulary) >= 1, "vocabulary length is too small to continue")

  # match vocabulary to
  index <- match(terms, vocabulary)
  terms <- terms[!is.na(index)]
  docs <- docs[!is.na(index)]

  term_counts <- Matrix::sparseMatrix(
     i = docs,
     j = index[!is.na(index)],
     x = rep(1, length(docs)),
     dims = c(length(tokens), length(vocabulary))
   )
   term_counts <- methods::as(term_counts, "TsparseMatrix")
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

#' @importFrom rlang .data
create_tfidf_terms <- function(tfidf, nterms = 5L)
{
  df <- tibble::tibble(
    doc = tfidf@i,
    term = colnames(tfidf)[tfidf@j + 1L],
    count = tfidf@x
  )
  df <- dplyr::arrange(
    dplyr::group_by(df, .data$doc), dplyr::desc(.data$count)
  )
  df <- dplyr::slice_head(df, n = nterms)
  dplyr::summarize(df, terms = paste0(.data$term, collapse = "|"))
}
