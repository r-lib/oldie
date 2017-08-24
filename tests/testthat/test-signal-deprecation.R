context("signal")

defunct_cycle <- c("", "", as.character(past_rlang_ver()))
rlang_fn <- set_env(function() "returned", ns_env("rlang"))

test_that("correct deprecation message is built", {
  cycle <- c("1.0.0", "2.0.0", "3.0.0")
  expect_identical(deprecated_function_msg("pkg::foo()", "1.0.0", 1, "bar()"), "`pkg::foo()` is soft-deprecated as of version 1.0.0, please use `bar()` instead")
  expect_identical(deprecated_function_msg("pkg::foo()", "2.0.0", 2), "`pkg::foo()` is deprecated as of version 2.0.0")
  expect_identical(deprecated_function_msg("pkg::foo()", "3.0.0", 3), "`pkg::foo()` is defunct as of version 3.0.0")
})

test_that("deprecated function does not signal if not in deprecation cycle", {
  future_ver <- as.character(future_rlang_ver())
  oldie <- deprecate(rlang_fn, c(future_ver, "", ""))

  expect_true(is_deprecated(oldie))
  expect_condition(oldie(), NA)
})

test_that("deprecated function signals soft-deprecation", {
  past_ver <- as.character(past_rlang_ver())
  oldie <- deprecate(rlang_fn, c(past_ver, "", ""))

  msg <- sprintf("deprecated as of version %s", past_ver)
  expect_condition(oldie(), "deprecated", msg)
})

test_that("deprecated function signals deprecation", {
  past_ver <- as.character(past_rlang_ver())
  oldie <- deprecate(rlang_fn, c("", past_ver, ""))

  msg <- sprintf("deprecated as of version %s", past_ver)
  expect_warning(oldie(), msg)
})

test_that("deprecated function signals itself defunct", {
  past_ver <- as.character(past_rlang_ver())
  oldie <- deprecate(rlang_fn, c("", "", past_ver))

  expect_error(oldie(), "defunct as of version 0.1.0")
})

  expect_error(foo(), "defunct as of version 0.1.0")
})
