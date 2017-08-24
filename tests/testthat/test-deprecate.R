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

test_that("replacements must not exist in function", {
  foo <- function(foo, bar) "returned"
  expect_error(deprecate(foo, "0.1.0", bar = foo), "already exists in the function")
})

test_that("replaced arguments must exist in function", {
  foo <- function(foo, bar) "returned"
  expect_error(deprecate(foo, "0.1.0", bar = other), "Can't find successor")
})

test_that("can't deprecate the same argument twice", {
  foo <- function(foo) "returned"
  foo <- deprecate(foo, "0.1.0", bar = foo)
  expect_error(deprecate(foo, "0.1.0", bar = foo), "already exists in the function")
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
