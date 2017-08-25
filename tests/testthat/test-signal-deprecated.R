context("signal")

past_ver <- as.character(past_rlang_ver())

soft_deprecated_cycle <- c(past_ver, "", "")
deprecated_cycle <- c("", past_ver, "")
defunct_cycle <- c("", "", past_ver)


# Functions ----------------------------------------------------------

rlang_fn <- set_env(function() "returned", ns_env("rlang"))

test_that("correct deprecation message is built", {
  cycle <- c("1.0.0", "2.0.0", "3.0.0")
  expect_identical(deprecated_function_msg("foo", "1.0.0", 1, "bar"), "`foo()` is soft-deprecated as of version 1.0.0, please use `bar()` instead")
  expect_identical(deprecated_function_msg("foo", "2.0.0", 2), "`foo()` is deprecated as of version 2.0.0")
  expect_identical(deprecated_function_msg("foo", "3.0.0", 3), "`foo()` is defunct as of version 3.0.0")
})

test_that("deprecated function does not signal if not in deprecation cycle", {
  future_ver <- as.character(future_rlang_ver())
  oldie <- deprecate(rlang_fn, c(future_ver, "", ""))

  expect_true(is_deprecated(oldie))
  expect_condition(oldie(), NA)
})

test_that("deprecated function signals soft-deprecation", {
  oldie <- deprecate(rlang_fn, soft_deprecated_cycle)
  msg <- sprintf("deprecated as of version %s", past_ver)
  expect_condition(oldie(), "deprecated", msg)
})

test_that("deprecated function signals deprecation", {
  oldie <- deprecate(rlang_fn, deprecated_cycle)
  expect_warning(oldie(), sprintf("deprecated as of version %s", past_ver))
})

test_that("deprecated function signals itself defunct", {
  oldie <- deprecate(rlang_fn, defunct_cycle)
  expect_error(oldie(), "defunct as of version 0.1.0")
})

test_that("deprecation stages are promoted", {
  scoped_options(oldie_verbose_deprecation = TRUE)

  oldie <- deprecate(rlang_fn, deprecated_cycle)
  expect_error(oldie(), sprintf("defunct as of version %s", past_ver))
})

test_that("deprecated function signals a replacement if supplied", {
  oldie <- deprecate(rlang_fn, defunct_cycle, bar)
  expect_error(oldie(), "please use `bar()` instead", fixed = TRUE)
})

test_that("deprecated function fails if not scoped in a namespace", {
  oldie <- function() "return"
  oldie <- deprecate(oldie, "99.9.0")
  expect_error(oldie(), "must be scoped in a namespace")
})
