# This script creates a lookup table mapping conditions found in the clinical
# trials database and standardised condition names. This is a surprisingly
# subtle task for two reasons. First, it is not clear when a NP or AP constitues
# a concrete unit ("Breast Cancer" or "Type II Diabeties") or is primarily
# descriptive. Secondly, there are multiple names for the same disease and the
# database does not have a standardised vocabulary. These combined issues
# require the use of an external database, in this case we have used Wikipedia
# pages. If a disease has its own page we assume it is well-known and
# well-defined enough to be a unique category. We can also use the redirects
# to disambiguate different names for the same condition.

# Note, this script takes a while to run because it needs to make a number of
# API calls to Wikipedia and it reads a fairly large deliminated file. Make sure
# you have enough memory (tested on a machine with 16GB) to read everything
# without swapping.

# 1. Load all of the required packages and set of functions written for calling
# the Wikipedia API
library(tidyverse)
library(ctrialsgov)
library(tokenizers)
library(stringi)
library(cleanNLP)
library(httr)
library(jsonlite)
library(xml2)
library(usethis)

source("cache.R")

# 2. Create a dataset from the ctrialsgov cache, using all industry studies
# from a 10 year period; we want to create a dataset mapping each trial to all
# of the listed conditions
ctgov_load_cache()
df <- ctgov_query(
  study_type = 'Interventional',
  sponsor_type = 'INDUSTRY',
  date_range = c('2012-01-01', '2022-01-01')
)
df <- filter(df, !is.na(conditions))
df <- filter(df, !is.na(description))
df <- select(df, nct_id, conditions)
ctgov_load_sample() # clear large dataset in memory

conds <- stri_split(df$conditions, fixed = "|")
df <- tibble(
  doc_id = seq_along(unlist(conds)),
  nct_id = rep(df$nct_id, sapply(conds, length)),
  text = stri_trans_tolower(unlist(conds))
)

# 3. Create a vector of all the unique tokens in the conditions data. Will use
# these to filter the Wikipedia data
tokens <- unlist(stri_split(df$text, fixed = ' '))
tokens <- stri_replace_all(tokens, '', fixed = ',')
tokens <- table(tokens)
tokens <- names(tokens)[tokens > 4]
tokens <- tokens[stri_length(tokens) > 2L]
tokens <- setdiff(tokens, word_frequency$word[1:50])
tokens <- paste0(tokens, "_")

# 4. Load the Wikipedia dataset into R
dir.create("temp", showWarnings = FALSE)
fout <- file.path("temp", "enwiki-20211201-all-titles.gz")
if (!file.exists(fout))
{
  url <- paste0(c(
    "https://dumps.wikimedia.org/enwiki/20211201/",
    "enwiki-20211201-all-titles.gz"
  ))
  download.file(url, fout)
}
gc()
wiki <- read_delim(fout, delim = '\t')
wiki <- wiki[wiki$page_namespace == 0,]
wiki <- wiki$page_title
gc()
wiki_lower <- stri_trans_tolower(wiki)
gc()

# 5. Filter the Wikipedia dataset to include only terms that appear somewhere in
# the conditions data.
index <- rep(FALSE, length(wiki_lower))
nc <- sort(unique(stri_length(tokens)))
for (j in seq_along(nc))
{
  temp <- stri_sub(wiki_lower, 1L, nc[j])
  these <- match(temp, tokens)
  index[!is.na(these)] <- TRUE
  cat(sprintf("Done with %d of %d\n", j, length(nc)))
  gc()
}

these <- match(wiki_lower, stri_sub(tokens, 1, -2L))
index[!is.na(these)] <- TRUE
wiki <- tibble(
   wiki_cased = wiki[index],
   wiki_lower = stri_replace_all(wiki_lower[index], ' ', fixed = '_')
)
rm(wiki_lower)
gc()

# 6. Determine the subset of Wikipedia pages to use using the logic that each
# disease should be matched to the most specific (in other words, longest)
# page name that contains the condition description
ng <- tokenize_ngrams(df$text, n_min = 1L, n = 7L)
ngram_df <- tibble(
  doc_id = rep(df$doc_id, sapply(ng, length)),
  nct_id = rep(df$nct_id, sapply(ng, length)),
  ngram = unlist(ng),
  nterm = stri_count(ngram, fixed = " ") + 1L
)

ngram_df <- ngram_df[!is.na(match(ngram_df$ngram, wiki$wiki_lower)),]
ngram_df <- ngram_df %>%
  group_by(doc_id, nct_id) %>%
  filter(nterm == max(nterm)) %>%
  ungroup() %>%
  select(nct_id, ngram) %>%
  unique() %>%
  left_join(
    filter(wiki, !duplicated(wiki_lower)),
    by = c("ngram" = "wiki_lower")
  )

terms <- ngram_df %>%
  group_by(wiki_cased) %>%
  summarize(n = n()) %>%
  arrange(desc(n)) %>%
  mutate(display = NA_character_)

# 7. Cycle through Wikipedia pages listed in the terms dataset and download the
# page data using the Wikipedia API. This will give us the text as well as any
# redirects. The results are cached, but the first cycle will take about 30
# minutes to finish. Along the way, store the display version of the
# (redirected, if applicable) page.
url_base <- modify_url("https://en.wikipedia.org/w/api.php",
                       query = list(
                         action = "parse", format = "json", redirects = "true"
                       ))

for (j in seq_len(nrow(terms)))
{
  url_str <- modify_url(url_base, query = list(page = terms$wiki_cased[j]))
  res <- http_cache_get(url_str, cache_dir = "cache")
  obj <- content(res, type = "application/json")
  terms$display[j] <- null_to_na(obj$parse$displaytitle)
  cat(sprintf("%s => %s\n", terms$wiki_cased[j], terms$display[j]))
}

# 8. Parse the Wikipedia API results to get a short gloss based on the first
# real paragraph of data.
terms$gloss <- NA_character_
for (j in seq_len(nrow(terms)))
{
  url_str <- modify_url(url_base, query = list(page = terms$wiki_cased[j]))
  res <- http_cache_get(url_str, cache_dir = "cache")
  obj <- content(res, type = "application/json")
  if (!is.null(obj$parse$text))
  {
    xobj <- read_xml(obj$parse$text[[1]])
    ptag <- xml_text(xml_find_all(xobj, ".//p"))
    ptag <- ptag[stri_length(ptag) > 25L]
    terms$gloss[j] <- ptag[1]
  }
}

# 9. Manual clean final index
ts <- terms
ts <- filter(ts, display != "Subject")
ts <- filter(ts,
  !(wiki_cased %in% c("Healthy", "Volunteers", "CELL", "Syndrome",
   "Overweight", "SKIN", "SAFETY", "MALE", "ADULT", "NON", "Related",
   "Study", "TYPE", "SOLID", "Patients", "Human", "Surgery", "Severe",
   "POST", "Diseases", "LINES", "Moderate", "Associated", "Relapsed",
   "Aging", "HEALTH", "Bacterial", "GRADE", "high", "Adults", "Sleep",
   "Growth", "OPEN", "FAT", "HIP", "Tolerance")))
ts <- filter(ts, !is.na(gloss))
ts <- filter(ts, !stri_detect(display, fixed = "disamb"))
ts$ngram <- stri_trans_tolower(ts$wiki_cased)
ts$ngram <- stri_replace_all(ts$ngram, ' ', fixed = '_')
ts <- select(ts, wiki_cased, ngram, display, gloss)

# 10. Save Results
condition_lookup <- ts
use_data(condition_lookup, overwrite = TRUE)
