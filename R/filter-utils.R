#' Does a Term Appear in a Vector of Strings?
#'
#' @param s the vector of strings.
#' @param pattern the pattern to search for.
#' @param ignore_case should the case be ignored? Default TRUE
#' 
#' @return a single logical value
#' 
#' @export
has_term <- function(s, pattern, ignore_case = TRUE) {
  any(grepl(pattern, s, ignore.case = ignore_case))
}
