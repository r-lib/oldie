context("version")

test_that("versions are vectors", {
  ver <- ver("1.0.0")
  expect_length(ver, 3)
  expect_identical(ver[[3]], 0L)
  ver[[3]] <- 9L
  expect_identical(ver[[3]], 9L)
})
