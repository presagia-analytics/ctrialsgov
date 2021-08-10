library(usethis)
library(ctrialsgov)
library(RPostgreSQL)
library(dplyr)

drv <- dbDriver('PostgreSQL')
con <- dbConnect(drv, dbname="aact")
ctgov_create_data(con)

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

## code to prepare `DATASET` dataset goes here

use_data(cancer_studies, overwrite = TRUE)
