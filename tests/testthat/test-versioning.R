context("versioning")

test_that("can't deprecate function multiple times", {
  foo <- function() "returned"
  foo <- deprecate(foo, "0.1.0")
  expect_error(deprecate(foo, "0.1.0"), "Function `foo` is already deprecated")
})
