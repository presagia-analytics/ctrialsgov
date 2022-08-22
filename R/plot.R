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
      ~ .x %>%
        stri_split(fixed = "|") %>%
        unlist() %>%
        trimws() %>%
        paste(collapse = "</br>&nbsp;&nbsp;&nbsp;&nbsp;", sep = "")
    )

  interventions <-
    map_chr(
      x$interventions,
      ~ paste0(.x$name, collapse = "</br>&nbsp;&nbsp;&nbsp;&nbsp;")
    )
  paste0(
    "</br>", "NCT ID: ", trimws(x$nct_id), "</br>",
#    "Status: ", x$status, "</br>",
    "Sponsor: ", trimws(x$sponsor), "</br>",
    "Start Date: ", trimws(x$start_date), "</br>",
    "Completion Date: ", trimws(x$primary_completion_date), "</br>",
    "Conditions:</br>&nbsp;&nbsp;&nbsp;&nbsp;", conditions, "</br>",
    "Interventions:</br>&nbsp;&nbsp;&nbsp;&nbsp;", interventions, "</br>",
    "Enrollment: ", trimws(x$enrollment), "</br>",
    sep = ""
  )
}

#' @title Plot a Timeline for a Set of Clinical Trials
#'
#' @param x the data.frame object returned from a query.
#' @param start_date the start date column name. (Default is "start_date")
#' @param completion_date the date the trial is set to be complete.
#' (Default "primary_completion_date").
#' (Default is "primary_completion_date")
#' @param label_column the column denoting the labels for the y-axis.
#' (Default is "nct_id")
#' @param color the column to be used for coloring. (Default is label_column)
#' @param tooltip the tooltips for each of trials.
#' (Default is `ctgov_gantt_labeller(x)`).
#'
#' @importFrom ggplot2 ggplot aes_string geom_tile enexpr xlab ylab guides
#' guide_legend
#' @importFrom lubridate days
#' @seealso ctgov_gantt_labeller
#' @export
ctgov_plot_timeline <- function(
  x,
  start_date = "start_date",
  completion_date = "primary_completion_date",
  label_column = "nct_id",
  color = label_column,
  tooltip = ctgov_gantt_labeller(x)) {

  x$width <- as.integer(x[[completion_date]]) - as.integer(x[[start_date]])
  x$tooltip <- tooltip
  x[[start_date]] <- x[[start_date]] + days(round(x$width / 2))
  p <- ggplot(
      data = x,
      aes_string(
        x = start_date,
        y = label_column,
        fill = color,
        width = "width",
        text = "tooltip"
      )
    ) +
    geom_tile(height = 0.8) +
    ylab(label_column) +
    xlab("Date") +
    guides(fill = guide_legend(color))
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
ctgov_to_plotly <- function(p, ...) {
  UseMethod("ctgov_to_plotly", p)
}

#' @export
ctgov_to_plotly.default <- function(p, ...) {
  stop(
    "Don't know how to create plotly plot from object of type:",
    paste(class(p), collapse = " ")
  )
}

#' @export
ctgov_to_plotly.gg <- function(p, ...) {
  ggplotly(p, ...)
}

#' @title Convert a ctrialsgov Visualization to Plotly
#'
#' @param p the plot returned by `ctgov_plot_timeline()`.
#' @param ... currently not used.
#' @return a Plotly object
#'
#' @importFrom plotly ggplotly layout
#' @importFrom htmlwidgets onRender
#' @export
ctgov_to_plotly.ctgov_bar_plot <- function(p, ...) {
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
