% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/load.R
\name{ctgov_load_cache}
\alias{ctgov_load_cache}
\title{Download and/or load cached data}
\usage{
ctgov_load_cache(force_download = FALSE)
}
\arguments{
\item{force_download}{logical flag; should the cache be re-downloaded if
it already exists? defaults to \code{FALSE}}
}
\value{
does not return any value; used only for side effects
}
\description{
This function downloads a saved version of the full clinical trials dataset
from the package's development repository on GitHub (~150MB) and loads it
into R for querying. The data will be cached so that it can be re-loaded
without downloading. We try to update the cache frequently so this is a
convenient way of grabbing the data if you do not need the most up-to-date
version of the database.
}
\author{
Taylor B. Arnold, \email{taylor.arnold@acm.org}
}
