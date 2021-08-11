library(usethis)
library(ctrialsgov)
library(RPostgreSQL)
library(dplyr)
library(lubridate)

# Create dataset based on local installation of PostgreSQL
drv <- dbDriver('PostgreSQL')
con <- dbConnect(drv, dbname="aact")
ctgov_create_data(con)

# For R package datasets, need to remove non-ASCII characters
remove_nonascii <- function(df)
{
  these <- which(sapply(df, function(v) class(v)[1]) == "character")
  for (j in these) { df[[j]] <- iconv(df[[j]], "latin1", "ASCII", sub="") }
  df
}

# Create a 1% sample data (this file is less than 3 MB, small enough to be
# included on CRAN)
set.seed(1L)
tbl_join_sample <- ctrialsgov:::.volatiles$tbl_join
tbl_join_sample <- filter(tbl_join_sample, year(start_date) <= year(today()))
tbl_join_sample <- slice_sample(tbl_join_sample, prop = 0.01)
tbl_join_sample <- arrange(tbl_join_sample, desc(start_date))
tbl_join_sample <- remove_nonascii(tbl_join_sample)
tbl_join_sample$interventions <- lapply(
  tbl_join_sample$interventions, remove_nonascii
)

print(object.size(tbl_join_sample), units = "MB")
use_data(tbl_join_sample, overwrite = TRUE)

# Build cancer studies dataset for the package
cancer_studies <- ctgov_query(
  study_type = "Interventional",
  sponsor_type = "Industry",
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
use_data(cancer_studies, overwrite = TRUE)

# Save larger files (in multiple parts to get around GH limits)
z <- ctrialsgov:::.volatiles$tbl_join

tbl_join_01 <- filter(z, year(start_date) > 2018L | is.na(year(start_date)))
tbl_join_02 <- filter(z, year(start_date) > 2015L & year(start_date) <= 2018L)
tbl_join_03 <- filter(z, year(start_date) > 2010L & year(start_date) <= 2015L)
tbl_join_04 <- filter(z, year(start_date) <= 2010L)

nr <- nrow(tbl_join_01) + nrow(tbl_join_02) + nrow(tbl_join_03) +
      nrow(tbl_join_04)
stopifnot(nr == nrow(z))

saveRDS(tbl_join_01, file = file.path("data", "tbl_join_01.rds"))
saveRDS(tbl_join_02, file = file.path("data", "tbl_join_02.rds"))
saveRDS(tbl_join_03, file = file.path("data", "tbl_join_03.rds"))
saveRDS(tbl_join_04, file = file.path("data", "tbl_join_04.rds"))

# Code to commit the data to a new branch

# git branch -d fdata
# git checkout -b fdata
# git add -f data/*.rds
# git commit -m "[auto] update cached data"
# git push -u origin fdata
# git checkout main
