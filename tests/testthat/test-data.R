library(ctrialsgov)

test_that("sample data loads okay", {
  ctgov_load_sample()
  res <- ctgov_query(description_kw = "cancer")
  expect_equal(ncol(res), 30L)
})
