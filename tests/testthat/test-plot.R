library(ctrialsgov)

test_that("sample that plots do not error", {
  ctgov_load_sample()

  res <- ctgov_query(
    description_kw = "cancer",
    enrollment_range = c(100, 200),
    date_range = c("2019-01-01", "2020-02-01")
  )
  p <- ctgov_plot_timeline(res)
  pp <- ctgov_to_plotly(p)

  expect_equal(class(p), c("gg", "ggplot"))
  expect_equal(class(pp), c("plotly", "htmlwidget"))
})
