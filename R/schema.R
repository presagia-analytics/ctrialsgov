
#' @title Get and Set the Default Schema
#'
#' @description This function sets the schema in which tables in which
#' the CT Trials tables reside. 
#'
#' Get the current schema eiter of the following.
#'
#' ctgov_schema()
#' ctgov_get_schema()
#'
#' Set the current schema with the following.
#'
#' ctgov_schema(<SCHEMA NAME>)
#' ctgov_set_schema(<SCHEMA NAME>)
#'
#' A return of "" from the get functions indicates a schema is not specified.
#' @aliases ctgov_get_schema ctgov_set_schema
#' @param schema the name of the schema. (Default is NULL - None)
#' @export
ctgov_schema <- function(schema = NULL) {
  if (is.null(schema)) {
    ctgov_get_schema()
  } else {
    ctgov_set_schema(schema)
  }
}

#' @export
ctgov_get_schema <- function() {
  if (exists('schema', where = .volatiles, inherits = FALSE)) {
    .volatiles$schema
  } else {
    ""
  }
}

# Internal function used to format SQL queries with schemas.
format_schema <- function() {

  schema <- ctgov_get_schema() 
  if (schema != "") {
    ncs <- nchar(schema)
    if (substr(schema, ncs, ncs) != ".") {
      schema <- paste0(schema, ".")
    }
  }
  schema
}

#' @export
ctgov_set_schema <- function(schema) {
  if (!isTRUE(is.character(schema))) {
    stop("schema must be a charater.")
  }
  if (length(schema) != 1) {
    stop("schema must be a single string.")
  }

  tryCatch(
    {
      assign('schema', schema, pos = .volatiles, inherits = FALSE)
      invisible(TRUE)
    },
    error = function(e) {
      if (exists('schema', where = .volatiles, inherits = FALSE)) {
        remove('schema', envir = .volatiles)
      }
      e
    }
  )
}


