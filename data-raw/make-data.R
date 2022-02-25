library(usethis)
library(ctrialsgov)
library(RPostgreSQL)
library(dplyr)
library(lubridate)

# Create dataset based on local installation of PostgreSQL
drv <- dbDriver('PostgreSQL')
con <- dbConnect(drv, dbname="aact")
#format_schema <- ctrialsgov:::format_schema
ctgov_create_data(con)

# For R package datasets, need to remove non-ASCII characters
remove_nonascii <- function(df)
{
  these <- which(sapply(df, function(v) class(v)[1]) == "character")
  for (j in these) { df[[j]] <- iconv(df[[j]], "latin1", "ASCII", sub="") }
  df
}

# Create sample data (this file is less than 3 MB, small enough to be included
# on CRAN)
set.seed(1L)
tbl_join_sample <- ctrialsgov:::.volatiles$tbl$join
tbl_join_sample <- filter(tbl_join_sample, year(start_date) <= year(today()))
tbl_join_sample <- slice_sample(tbl_join_sample, prop = 0.005)
tbl_join_sample <- arrange(tbl_join_sample, desc(start_date))
tbl_join_sample <- remove_nonascii(tbl_join_sample)
tbl_join_sample$interventions <- lapply(
  tbl_join_sample$interventions, remove_nonascii
)

tosave <- ctrialsgov:::.volatiles$tbl
for (j in seq_along(tosave))
{
  tosave[[j]] <- semi_join(tosave[[j]], tbl_join_sample, by = "nct_id")
}

print(object.size(tosave), units = "MB")
tbl_join_sample <- tosave
use_data(tbl_join_sample, overwrite = TRUE)

# Build cancer studies dataset for the package
cancer_studies <- ctgov_query(
  study_type = "Interventional",
  sponsor_type = "INDUSTRY",
  date_range = c("2021-01-01", NA),
  description_kw = c("cancer", "carcinoma")) %>%
  filter(
    !is.na(phase) &
    primary_purpose == "Treatment",
    enrollment > 100
  )
print(object.size(cancer_studies), units = "MB")

cancer_studies <- remove_nonascii(cancer_studies)
cancer_studies$interventions <- lapply(
  cancer_studies$interventions, remove_nonascii
)

tosave <- ctrialsgov:::.volatiles$tbl
for (j in seq_along(tosave))
{
  tosave[[j]] <- semi_join(tosave[[j]], cancer_studies, by = "nct_id")
}

print(object.size(tosave), units = "MB")
cancer_studies <- tosave
use_data(cancer_studies, overwrite = TRUE)

# Save larger files (in multiple parts to get around GH limits)
ctgov_save_file("cache/tbl_data.Rds")








# Save larger files (in multiple parts to get around GH limits)
# z <- ctrialsgov:::.volatiles$tbl_join
# dir.create("data", FALSE)
#
# nparts <- 6L
# nrows <- nrow(z)
# bsize <- ceiling(nrows / nparts)
# ids <- rep(seq_len(nparts), each = bsize)
# ids <- ids[seq_len(nrows)]
#
# for (j in seq_len(nparts))
# {
#   tbl_join <- z[ids == j,]
#   saveRDS(tbl_join, file = file.path("data", sprintf("tbl_join_%02d.rds", j)))
# }

# Code to commit the data to a new branch

# git branch -d fdata
# git checkout -b fdata
# git add -f data/*.rds
# git commit -m "[auto] update cached data"
# git push -u origin fdata
# git checkout main
