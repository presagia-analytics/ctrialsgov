#' @title Create a Gantt Labeler for Timeline Tooltips
#'
#' @param x the data.frame object returned from a query.
#' @importFrom purrr map_chr
#' @importFrom stringi stri_split
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
        paste(collapse = "\n\t\t")
    )

  interventions <-
    map_chr(
      x$interventions,
      ~ paste(.x$name, collapse = "\n\t\t")
    )

  paste0(
    "</br>", "NCT ID: ", x$nct_id, "</br>",
#    "Status: ", x$status, "</br>",
    "Sponsor: ", x$sponsor, "</br>",
    "Start Date: ", x$start_date, "</br>",
    "Completion Date: ", x$primary_completion_date, "</br>",
    "Conditions:\n\t\t", conditions, "</br>",
    "Interventions:\n\t\t", interventions, "</br>",
    "Enrollment: ", x$enrollment, "</br>"
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
  ggplot(
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
}


#' @title Convert a ctrialsgov Visualization to Plotly
#'
#' @param p the plot returned by `ctgov_plot_timeline()`.
#' @importFrom plotly ggplotly
#' @export
ctgov_to_plotly <- function(p) {
  pp <- plotly::ggplotly(p, tooltip = "text")

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
