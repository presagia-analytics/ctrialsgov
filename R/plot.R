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
#' @importFrom ggplot2 ggplot aes geom_segment enexpr xlab ylab guides
#' guide_legend
#' @seealso ctgov_gantt_labeller
#' @export
ctgov_plot_timeline <- function(
  x,
  start_date = "start_date",
  completion_date = "primary_completion_date",
  label_column = "nct_id",
  color = label_column,
  tooltip = ctgov_gantt_labeller(x)) {

  ggplot(
      data = x,
      aes(
        x = enexpr(start_date),
        xend = eval(as.name(completion_date)),
        y = eval(as.name(label_column)),
        yend = eval(as.name(label_column)),
        color = eval(as.name(color)),
        text = tooltip
      )
    ) +
    geom_segment(size = 3) +
    ylab(label_column) +
    xlab("Date") +
    guides(color = guide_legend(color))
}


#' @title Convert a ctrialsgov Visualization to Plotly
#'
#' @param p the plot returned by `ctgov_plot_timeline()`.
#' @importFrom plotly ggplotly
#' @export
ctgov_to_plotly <- function(p) {
  ggplotly(p, tooltip = "text")
  # TODO: As Ryan about the following
  # p$x$data[[1]]$customdata <-
  #   paste0("https://clinicaltrials.gov/ct2/show/", x[['nct_id']])
  # p <- onRender(
  #   p,
  #   "
  #     function(el,x){
  #     el.on('plotly_click', function(d) {
  #     var websitelink = d.points[0].customdata;
  #     window.open(websitelink);
  #     });
  #     }
  #     "
  # )
}
