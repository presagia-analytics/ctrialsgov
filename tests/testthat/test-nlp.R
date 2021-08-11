library(ctrialsgov)
library(stringi)

test_that("check keywords in context", {
  ctgov_load_sample()
  z <- ctgov_query(study_type = "Interventional")

  res <- ctgov_kwic("bladder", z$brief_title, output = "character")
  expect_true(all(stri_detect(res, regex = "(?i)|bladder|")))

  res <- ctgov_kwic("bladder", z$brief_title, z$nct_id, output = "data.frame")
  expect_true(all(stri_detect(res$term, regex = "(?i)bladder")))

  res <- ctgov_kwic("bladder", z$brief_title, z$nct_id)
  expect_true(is.null(res))
})

test_that("check tfidf keywords", {
  iput <- list(letters[1:10], letters[1:5], letters[1:20])
  iput <- sapply(iput, paste, collapse = " ")
  terms <- ctgov_tfidf(iput)$terms
  expect_true(all(terms == c("f|g|h|i|j", "a|b|c|d|e", "k|l|m|n|o")))
})

test_that("check similarity scores", {
  iput <- list(letters[1:10], letters[15:20], letters[1:20])
  iput <- sapply(iput, paste, collapse = " ")
  smat <- ctgov_text_similarity(iput)

  expect_equal(dim(smat), c(3L, 3L))
  expect_equal(smat, t(smat))
  expect_true(max(abs(diag(smat) - 1)) < 1e-10)
  expect_true(max(abs(smat[1, 2])) < 1e-10)
  expect_true(max(abs(smat[2, 1])) < 1e-10)
})
