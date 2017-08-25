context("signal")

past_ver <- as.character(past_rlang_ver())

soft_deprecated_cycle <- c(past_ver, "", "")
deprecated_cycle <- c("", past_ver, "")
defunct_cycle <- c("", "", past_ver)

if (is_true(peek_option("oldie_verbose_deprecation"))) {
  abort("Verbose deprecation should be disabled")
}


# Functions ----------------------------------------------------------

rlang_fn <- set_env(function() "returned", ns_env("rlang"))

test_that("correct deprecation message is built", {
  cycle <- c("1.0.0", "2.0.0", "3.0.0")
  expect_identical(deprecated_function_msg("foo", "1.0.0", "soft-deprecated", "bar"), "`foo()` is soft-deprecated as of version 1.0.0, please use `bar()` instead")
  expect_identical(deprecated_function_msg("foo", "2.0.0", "deprecated"), "`foo()` is deprecated as of version 2.0.0")
})

test_that("deprecated function does not signal if not in deprecation cycle", {
  future_ver <- as.character(future_rlang_ver())
  oldie <- retire(rlang_fn, c(future_ver, "", ""))

  expect_true(is_retired(oldie))
  expect_condition(oldie(), NA)
})

test_that("deprecated function signals soft-deprecation", {
  oldie <- retire(rlang_fn, soft_deprecated_cycle)
  msg <- sprintf("deprecated as of version %s", past_ver)
  expect_condition(oldie(), "deprecated", msg)
})

test_that("deprecated function signals deprecation", {
  oldie <- retire(rlang_fn, deprecated_cycle)
  expect_warning(oldie(), sprintf("deprecated as of version %s", past_ver))
})

test_that("deprecated function signals itself defunct", {
  oldie <- retire(rlang_fn, defunct_cycle)
  expect_error(oldie(), "defunct as of version 0.1.0")
})

test_that("deprecation stages are promoted", {
  scoped_options(oldie_verbose_deprecation = TRUE)

  oldie <- retire(rlang_fn, deprecated_cycle)
  expect_error(oldie(), sprintf("defunct as of version %s", past_ver))

  oldie <- retire(rlang_fn, "99.9.0", bar)
  expect_condition(oldie(), "deprecated", "soft-deprecated as of version `undefined`")
})

test_that("deprecated function signals a replacement if supplied", {
  oldie <- retire(rlang_fn, defunct_cycle, bar)
  expect_error(oldie(), "please use `bar()` instead", fixed = TRUE)
})

test_that("deprecated function fails if not scoped in a namespace", {
  oldie <- function() "return"
  oldie <- retire(oldie, "99.9.0")
  expect_error(oldie(), "must be scoped in a namespace")
})

test_that("can supply namespaced replacement function", {
  oldie <- retire(rlang_fn, defunct_cycle, pkg::bar)
  expect_error(oldie(), "please use `pkg::bar()` instead", fixed = TRUE)
})


# Arguments -----------------------------------------------------------

rlang_fn <- set_env(function(new) "returned", ns_env("rlang"))

test_that("correct argument deprecation message is built", {
  cycle <- c("1.0.0", "2.0.0", "3.0.0")
  expect_identical(
    deprecated_argument_msg("foo", "arg", "1.0.0", "soft-deprecated", "bar"),
    unclass(glue(
      "Argument `arg` of function `foo()` is soft-deprecated as of version 1.0.0
       Please use `bar` instead"
    ))
  )
  expect_identical(
    deprecated_argument_msg("foo", "arg", "2.0.0", "deprecated"),
    "Argument `arg` of function `foo()` is deprecated as of version 2.0.0"
  )
})

test_that("deprecated argument signals soft-deprecation", {
  oldie <- retire(rlang_fn, soft_deprecated_cycle, old = new)
  expect_condition(oldie(old = foo), "deprecated_arg",
    glue(
      "Argument `old` of function `rlang_fn()` is soft-deprecated as of version 0.1.0
       Please use `new` instead"
    ),
    fixed = TRUE
  )
})

test_that("deprecated argument signals deprecation", {
  oldie <- retire(rlang_fn, deprecated_cycle, old = )
  expect_warning(oldie(old = foo),
    "Argument `old` of function `rlang_fn()` is deprecated",
    fixed = TRUE
  )
})

test_that("deprecated argument signals defunction", {
  oldie <- retire(rlang_fn, defunct_cycle, old = )
  expect_error(oldie(old = foo),
    "Argument `old` of function `rlang_fn()` is defunct",
    fixed = TRUE
  )
})

test_that("deprecated argument signals are promoted", {
  scoped_options(oldie_verbose_deprecation = TRUE)
  oldie <- retire(rlang_fn, deprecated_cycle, old = )
  expect_error(oldie(old = foo),
    "Argument `old` of function `rlang_fn()` is defunct",
    fixed = TRUE
  )
})
