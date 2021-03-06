% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/conditions.R
\name{ctgov_norm_conditions}
\alias{ctgov_norm_conditions}
\title{Query the ClinicalTrials.gov dataset}
\usage{
ctgov_norm_conditions(data)
}
\arguments{
\item{data}{a dataset from which to extract the conditions}
}
\value{
a tibble object giving normalised conditions for the input data
}
\description{
This function selects a subset of the clinical trials data by using a
a variety of different search parameters. These include free text search
keywords, range queries for the continuous variables, and exact matches for
categorical fields. The function \code{ctgov_query_terms} shows the
categorical levels for the latter. The function will either take the entire
dataset loaded into the package environment or a previously queried input.
}
\author{
Taylor B. Arnold, \email{taylor.arnold@acm.org}
}
