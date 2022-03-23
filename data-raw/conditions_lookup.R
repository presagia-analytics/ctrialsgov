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

# 4. Load the Wikipedia dataset into R; note, you may need to download this
# with curl or wget as I've had trouble with the file timing out in R
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
ns <- tokenize_skip_ngrams(df$text, n_min = 2L, n = 2L, k = 1L)
ngram_df <- bind_rows(tibble(
  doc_id = rep(df$doc_id, sapply(ng, length)),
  nct_id = rep(df$nct_id, sapply(ng, length)),
  ngram = unlist(ng),
  nterm = stri_count(ngram, fixed = " ") + 1L,
  type = 1
),tibble(
  doc_id = rep(df$doc_id, sapply(ns, length)),
  nct_id = rep(df$nct_id, sapply(ns, length)),
  ngram = unlist(ns),
  nterm = stri_count(ngram, fixed = " ") + 1L,
  type = 2
))

ngram_df <- ngram_df %>%
  group_by(doc_id, nct_id, ngram, nterm) %>%
  summarise(type = min(type)) %>%
  ungroup() %>%
  arrange(doc_id, nct_id, ngram)

ngram_df <- ngram_df[!is.na(match(ngram_df$ngram, wiki$wiki_lower)),]
ngram_df <- ngram_df %>%
  # group_by(doc_id, nct_id) %>%
  # filter(nterm == max(nterm)) %>%
  # ungroup() %>%
  select(nct_id, doc_id, ngram, type) %>%
  unique() %>%
  left_join(
    wiki,
    # filter(wiki, !duplicated(wiki_lower)),
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
categories <- vector("list", nrow(terms))
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
    categories[[j]] <- map_chr(obj$parse$categories, ~ ..1$`*`)
  }
}
terms$categories <- categories

# 9. Automated cleaning of the results
terms <- filter(terms, !is.na(gloss))
terms <- filter(terms, !stri_detect(display, fixed = "<i>"))
terms <- filter(terms, !map_lgl(categories, ~ "Disambiguation_pages" %in% ..1))
terms <- terms %>%
  mutate(temp = stri_trans_tolower(wiki_cased)) %>%
  mutate(ncap = stri_count(wiki_cased, regex = "[A-Z]")) %>%
  group_by(temp) %>%
  filter(ncap == min(ncap)) %>%
  ungroup() %>%
  select(-temp, -ncap)

allowed_terms <- df %>%
  group_by(text) %>%
  summarise(n = n()) %>%
  arrange(desc(n)) %>%
  filter(n >= 2)

terms <- terms %>%
  mutate(text = stri_trans_tolower(display)) %>%
  semi_join(allowed_terms, by = "text")

#filter(terms, !map_lgl(categories, ~ "Wikipedia_medicine_articles_ready_to_translate" %in% ..1))

# 10. Manual cleaning of the lookup index (mostly not needed anymore, but
# keeping just in case)
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

# 11. Add counts for deduping
cnts <- ngram_df %>%
  select(-type, -wiki_cased) %>%
  mutate(nterm = stri_count(ngram, fixed = " ") + 1L) %>%
  inner_join(ts, by = "ngram") %>%
  group_by(doc_id, nct_id) %>%
  filter(nterm == max(nterm)) %>%
  ungroup() %>%
  select(nct_id, doc_id, display) %>%
  unique() %>%
  group_by(display) %>%
  summarise(ncount = n()) %>%
  arrange(desc(ncount))

ts <- ts %>%
  left_join(cnts, by = "display")

# 11. Save Results
condition_lookup <- select(ts, -gloss)
use_data(condition_lookup, overwrite = TRUE)
