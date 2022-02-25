library(ctrialsgov)
library(stringi)
library(lubridate)

test_that("check standard keyword queries", {
  ctgov_load_sample()

  res <- ctgov_query(description_kw = "cancer")
  expect_true(all(stri_detect(res$description, regex = "(?i)cancer")))

  res <- ctgov_query(sponsor_kw = "cancer")
  expect_true(all(stri_detect(res$sponsor, regex = "(?i)cancer")))

  res <- ctgov_query(brief_title_kw = "cancer")
  expect_true(all(stri_detect(res$brief_title, regex = "(?i)cancer")))

  res <- ctgov_query(official_title_kw = "cancer")
  expect_true(all(stri_detect(res$official_title, regex = "(?i)cancer")))

  res <- ctgov_query(intervention_desc_kw = "cancer")
  expect_true(all(
    stri_detect(res$intervention_model_description, regex = "(?i)cancer")
  ))

  res <- ctgov_query(conditions_kw = "cancer")
  expect_true(all(stri_detect(res$conditions, regex = "(?i)cancer")))

  res <- ctgov_query(population_kw = "cancer")
  expect_true(all(stri_detect(res$population, regex = "(?i)cancer")))

})


test_that("check range queries", {
  ctgov_load_sample()

  res <- ctgov_query(date_range = c("2010-01-01", "2010-12-31"))
  expect_true(all(year(res$start_date) == 2010L))

  res <- ctgov_query(enrollment_range = c(100, 120))
  expect_true(all(res$enrollment >= 100))
  expect_true(all(res$enrollment <= 120))

  res <- ctgov_query(enrollment_range = c(100, 120))
  expect_true(all(res$enrollment >= 100))
  expect_true(all(res$enrollment <= 120))

  res <- ctgov_query(minimum_age_range = c(5, 10))
  expect_true(all(res$minimum_age >= 5))
  expect_true(all(res$minimum_age <= 10))

  res <- ctgov_query(maximum_age_range = c(5, 10))
  expect_true(all(res$maximum_age >= 5))
  expect_true(all(res$maximum_age <= 10))
})

test_that("check categorical queries", {
  ctgov_load_sample()

  res <- ctgov_query(study_type = "Interventional")
  expect_true(all(res$study_type == "Interventional"))

  res <- ctgov_query(allocation = "Randomized")
  expect_true(all(res$allocation == "Randomized"))

  res <- ctgov_query(intervention_model = "Parallel Assignment")
  expect_true(all(res$intervention_model == "Parallel Assignment"))

  res <- ctgov_query(observational_model = "Cohort")
  expect_true(all(res$observational_model == "Cohort"))

  res <- ctgov_query(primary_purpose = "Treatment")
  expect_true(all(res$primary_purpose == "Treatment"))

  res <- ctgov_query(time_perspective = "Prospective")
  expect_true(all(res$time_perspective == "Prospective"))

  res <- ctgov_query(masking_description = "Triple")
  expect_true(all(res$masking_description == "Triple"))

  res <- ctgov_query(sampling_method = "Non-Probability Sample")
  expect_true(all(res$sampling_method == "Non-Probability Sample"))

  res <- ctgov_query(phase = "Phase 2")
  expect_true(all(res$phase == "Phase 2"))

  res <- ctgov_query(gender = "All")
  expect_true(all(res$gender == "All"))

  res <- ctgov_query(sponsor_type = "INDUSTRY")
  expect_true(all(res$sponsor_type == "INDUSTRY"))
})
