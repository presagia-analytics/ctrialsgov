# This short script will create the datasets contained in the
# directory "db" that are used in the paper; this code is also
# in the Rmd file, but set to not run due to it's long run time
# and need to download a large zip file

library(ctrialsgov)

dir.create("db", FALSE)
ctgov_get_latest_snapshot(db_path = file.path("db", "ctgov.duckdb"),
                          db_derived_path = file.path("db", "ctgov-derived.duckdb"))
