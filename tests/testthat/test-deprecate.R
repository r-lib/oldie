context("deprecate")

# Function -----------------------------------------------------------

test_that("can't deprecate function multiple times", {
  foo <- function() "returned"
  foo <- deprecate(foo, "0.1.0")
  expect_error(deprecate(foo, "0.1.0"), "Function `foo` is already deprecated")
})

test_that("replacement function must be supplied as symbol", {
  foo <- function() "returned"
  expect_error(deprecate(foo, "0.1.0", 2), "must be a symbol")
})


# Arguments ----------------------------------------------------------

test_that("replaced arguments must be symbols", {
  foo <- function(foo) "returned"
  expect_error(deprecate(foo, "0.1.0", arg = foo()), "must be symbols")
})

test_that("replaced arguments must be named", {
  foo <- function(foo) "returned"
  expect_error(deprecate(foo, "0.1.0", foo = bar, replaced), "must be named")
})

test_that("replaced arguments must exist in function", {
  foo <- function(foo, bar) "returned"
  expect_error(deprecate(foo, "0.1.0", bar = other), "Can't find replacement")
})

test_that("can't deprecate the same argument twice", {
  foo <- function(foo) "returned"
  foo <- deprecate(foo, "0.1.0", bar = foo)
  expect_error(deprecate(foo, "0.1.0", bar = foo), "`bar` has already been deprecated")

  foo <- deprecate(foo, "0.1.0", baz = foo)
  expect_error(deprecate(foo, "0.1.0", bar = foo, baz = foo), "`bar` and `baz` have already been deprecated")
})

test_that("new arguments are reassigned if old is supplied", {
  foo <- function(new) list(new, old)
  foo <- set_env(foo, ns_env("rlang"))
  foo <- deprecate(foo, "99.9.0", old = new)
  expect_identical(foo(, 2), list(2, 2))
})

test_that("no reassignment when new argument has not been declared", {
  foo <- set_env(function() old, ns_env("rlang"))
  foo <- deprecate(foo, "99.9.0", old = )
  expect_error(foo(), "is missing, with no default")
  expect_identical(foo("arg"), "arg")
})

test_that("deprecated arguments are stored in attributes", {
  foo <- set_env(function(new1, new2, new3) NULL, ns_env("rlang"))
  foo <- deprecate(foo, "0.1.0", old1 = new1, old2 = )
  foo <- deprecate(foo, "0.2.0", old3 = new3)

  depr_args <- deprecated_args(foo)
  expect_named(depr_args, c("old1", "old2", "old3"))

  arg1 <- depr_args[[1]]
  arg2 <- depr_args[[2]]
  arg3 <- depr_args[[3]]
  expect_identical(arg1, list(replacement = "new1", cycle = c("0.1.0", "0.2.0", "0.3.0")))
  expect_identical(arg2, list(replacement = "", cycle = c("0.1.0", "0.2.0", "0.3.0")))
  expect_identical(arg3, list(replacement = "new3", cycle = c("0.2.0", "0.3.0", "0.4.0")))
})

test_that("new arguments are added without defaults", {
  foo <- function(new) NULL
  fmls <- formals(deprecate(foo, "0.1.0", old = ))
  expect_identical(as.list(fmls), alist(new = , old = ))

  replaced_fmls <- formals(deprecate(foo, "0.1.0", old = new))
  expect_identical(as.list(replaced_fmls), alist(new = , old = ))
})

test_that("can add mark existing argument as deprecated", {
  foo <- function(new, old) "foo()"
  expect_error(regexp = NA, deprecate(foo, "0.1.0", old = ))
})
