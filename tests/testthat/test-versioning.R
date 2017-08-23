context("versioning")

test_that("correct deprecation message is built", {
  cycle <- c("1.0.0", "2.0.0", "3.0.0")
  expect_identical(deprecated_function_msg("pkg::foo()", "1.0.0", 1, "bar()"), "`pkg::foo()` is soft-deprecated as of version 1.0.0, please use `bar()` instead")
  expect_identical(deprecated_function_msg("pkg::foo()", "2.0.0", 2), "`pkg::foo()` is deprecated as of version 2.0.0")
  expect_identical(deprecated_function_msg("pkg::foo()", "3.0.0", 3), "`pkg::foo()` is defunct as of version 3.0.0")
})

test_that("can't deprecate function multiple times", {
  foo <- function() "returned"
  foo <- deprecate(foo, "0.1.0")
  expect_error(deprecate(foo, "0.1.0"), "Function `foo` is already deprecated")
})

test_that("deprecated function does not signal if not in deprecation cycle", {
  foo <- function() "returned"
  foo <- set_env(foo, ns_env("rlang"))

  future_ver <- as.character(future_rlang_ver())
  foo <- deprecate(foo, c(future_ver, "", ""))

  expect_true(is_deprecated(foo))
  expect_condition(foo(), NA)
})

test_that("deprecated function signals soft-deprecation", {
  foo <- function() "returned"
  foo <- set_env(foo, ns_env("rlang"))

  past_ver <- as.character(past_rlang_ver())
  foo <- deprecate(foo, c(past_ver, "", ""))

  msg <- sprintf("deprecated as of version %s", past_ver)
  expect_condition(foo(), "deprecated", msg)
})

test_that("deprecated function signals deprecation", {
  foo <- function() "returned"
  foo <- set_env(foo, ns_env("rlang"))

  past_ver <- as.character(past_rlang_ver())
  foo <- deprecate(foo, c("", past_ver, ""))

  msg <- sprintf("deprecated as of version %s", past_ver)
  expect_warning(foo(), msg)
})

test_that("deprecated function signals itself defunct", {
  foo <- function() "returned"
  foo <- set_env(foo, ns_env("rlang"))

  past_ver <- as.character(past_rlang_ver())
  foo <- deprecate(foo, c("", "", past_ver))

  expect_error(foo(), "defunct as of version 0.1.0")
})
