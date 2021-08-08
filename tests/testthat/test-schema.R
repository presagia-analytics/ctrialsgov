
test_that("schema works", {
  expect_equal(ctgov_schema(), "")

  ctgov_schema("test")
  expect_equal(ctgov_schema(), "test")

  expect_error(ctgov_set_schema(c("a", "b")))
  expect_error(ctgov_set_schema(3))

  ctgov_schema("")
  expect_equal(format_schema(), "")

  ctgov_schema("test")
  expect_equal(format_schema(), "test.")
})
