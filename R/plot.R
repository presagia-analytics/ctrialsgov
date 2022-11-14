#' @title Create a Gantt Labeler for Timeline Tooltips
#'
#' @param x the data.frame object returned from a query.
#' @importFrom purrr map_chr
#' @importFrom stringi stri_split
#' @return a string that can be used as a label in ggplotly
#' @export
ctgov_gantt_labeller <- function(x) {

  link <- paste0("https://clinicaltrials.gov/ct2/show/NCT", x$nct_id)
  hrefs <- sprintf("NCTID: <a href=\"%s\">%s</a>", link, x$nct_id)
  conditions <-
    map_chr(
      x$conditions,
      ~ .x |>
        stri_split(fixed = "|") |>
        unlist() |>
        trimws() |>
        paste(collapse = "</br>&nbsp;&nbsp;&nbsp;&nbsp;", sep = "")
    )

#  interventions <-
#    map_chr(
#      x$interventions,
#      ~ paste0(.x$name, collapse = "</br>&nbsp;&nbsp;&nbsp;&nbsp;")
#    )
  paste0(
    "</br>", "NCT ID: ", trimws(x$nct_id), "</br>",
#    "Status: ", x$status, "</br>",
    "Sponsor: ", trimws(x$sponsor), "</br>",
    "Start Date: ", trimws(x$start_date), "</br>",
    "Completion Date: ", trimws(x$primary_completion_date), "</br>",
    "Conditions:</br>&nbsp;&nbsp;&nbsp;&nbsp;", conditions, "</br>",
#    "Interventions:</br>&nbsp;&nbsp;&nbsp;&nbsp;", interventions, "</br>",
    "Enrollment: ", trimws(x$enrollment), "</br>",
    sep = ""
  )
}

#' @title Plot a Timeline for a Set of Clinical Trials
#'
#' @param x the data.frame object returned from a query.
#' @param mapping the aesthetic mapping. 
#'
#' @importFrom ggplot2 ggplot aes_string geom_tile enexpr xlab ylab guides
#' guide_legend
#' @importFrom lubridate days
#' @importFrom rlang quo_get_expr
#' @importFrom utils modifyList
#' @seealso ctgov_gantt_labeller
#' @export
ctgov_plot_timeline <- function(
  x,
  mapping = aes_string(nct_id = "nct_id", start = "start_date", 
                       end = "primary_completion_date", 
                       y = "nct_id", fill = "phase")
  ) {

  start = as.character(quo_get_expr(mapping$start))
  end = as.character(quo_get_expr(mapping$end))
  y = as.character(quo_get_expr(mapping$y))
  
  x$width <- as.integer(x[[end]]) - as.integer(x[[start]])
#  x$tooltip <- tooltip
  x$center <- x[[start]] + days(round(x$width / 2))
  x = x[order(x[[start]]),]
  x[[y]] = factor(x[[y]], levels = x[[y]])
  mapping = utils::modifyList(mapping, aes_string(x = "center", width = "width"))
  p <- ggplot(data = x, mapping = mapping) +
    geom_tile(height = 0.8) +
    ylab(y) +
    xlab("Date")
  if (!is.null(mapping$fill)) {  
    fill = as.character(quo_get_expr(mapping$fill)) 
    guides(fill = guide_legend(fill))
  }
  class(p) <- c("ctgov_bar_plot", class(p))
  p
}


#' @title Convert a ctrialsgov Visualization to Plotly
#'
#' @param p the plot returned by `ctgov_plot_timeline()`.
#' @param ... currently not used.
#' @return a Plotly object
#'
#' @importFrom plotly ggplotly
#' @export
ctgov_to_plotly = function(p, ...) {
  UseMethod("ctgov_to_plotly", p)
}

#' @export
ctgov_to_plotly.default = function(p, ...) {
  stop(
    "Don't know how to create plotly plot from object of type:",
    paste(class(p), collapse = " ")
  )
}

#' @export
ctgov_to_plotly.gg = function(p, ...) {
  ggplotly(p, ...)
}

#' @title Convert a ctrialsgov Visualization to Plotly
#'
#' @param p the plot returned by `ctgov_plot_timeline()`.
#' @param tooltip_fun = ctgov_gantt_labeller
#' @param ... currently not used.
#' @return a Plotly object
#'
#' @importFrom plotly ggplotly layout
#' @importFrom htmlwidgets onRender
#' @export
ctgov_to_plotly.ctgov_bar_plot = function(p, tooltip_fun = ctgov_gantt_labeller, ...) {

  class(p) <- class(p)[-1]
  pp <- ggplotly(p, tooltip = "tooltip")
  pp <-  plotly::layout(pp, hoverlabel = list(align = "left"))

  # this gets the y-axis category values
  cats <- pp$x$layout$yaxis$categoryarray

  pp <- htmlwidgets::onRender(
    pp,
    "
      function(el, x) {
        el.on('plotly_click', function(d) {
          // the trial id is found in d.points[0].yaxis.ticktext
          // indexed by the data.y value (mean of first two)
          var id = d.points[0].yaxis.ticktext[((d.points[0].data.y[0] + d.points[0].data.y[1]) / 2) - 1];
          var websitelink = 'https://clinicaltrials.gov/ct2/show/' + id;
          window.open(websitelink);
        });
      }
    "
  )

  pp
}
