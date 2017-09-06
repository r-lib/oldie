# oldie

[![Build Status](https://travis-ci.org/lionel-/oldie.svg?branch=master)](https://travis-ci.org/tidyverse/oldie)


## Overview

This is an experimental package for deprecation of functions and
arguments. Deprecation is spread over several releases with three
levels of deprecation:

- **Soft-deprecated**: This is the first stage of deprecation. The
  function or argument continues to work normally without any
  warning. Soft-deprecated functions will generally not be documented,
  and should not be used in examples or package code. They are left
  around so that existing code continues to work, but new code should
  not use them.

- **Deprecated**: The function or argument now issues a warning when
  used or supplied. Users should upgrade their code to use the
  suggested replacement, if any.

- **Defunct**: The function or argument now issues an error when used
  or supplied.

The deprecation levels can be promoted by calling
`promote_retirement()`.  The `testthat` and the `strict` packages
will probably automatically promote the deprecation levels to make
sure you are not using any soft-deprecated functions in your code.


## Retirement

### Functions

Let's create an obsolete function to illustrate how to retire it:

```{r}
old_fn <- function() "old"
```

`retire()` is a function operator that takes a function and modifies
it so that it automatically calls `oldie::signal_retired()`. It takes
as second argument a release cycle:

```{r}
old_fn <- retire(old_fn, c("0.1.0", "0.3.0", "0.5.0"))
```

In this release cycle the function is soft-deprecated from version
0.1.0 until 0.3.0. During that period it signals a simple condition.
Between 0.3.0 and 0.5.0 it is deprecated and signals a warning. After
0.5.0 it is defunct and issues an error.

If you supply a single version, oldie completes it on the assumption
that each minor version increases the deprecation level. The following
expressions are thus equivalent:

```{r}
retire(old_fn, "0.1.0")
retire(old_fn, c("0.1.0", "0.2.0", "0.3.0"))
```

If there is a new function that you can suggest as replacement, supply
its name as third argument:

```{r}
retire(old_fn, "0.1.0", "new_fn")

# If it lives in another package make it clear with a namespace:
retire(old_fn, "0.1.0", "pkg::new_fn")
```


### Arguments

When `retire()` is supplied *named arguments* it deprecates the
function arguments rather than the function itself:

```{r}
fn <- function(..., old, new) NULL

# This deprecates the argument `old` with no replacement
retire(fn, "0.1.0", old =)

# This deprecates the argument `old` with replacement `new`
retire(fn, "0.1.0", old = new)
```

If a user supplies the retired argument, two things happen:

- A deprecation signal is sent. The signal follows the same rule as
  for function deprecation.

- The value supplied to the deprecated argument is reassigned to the
  replacement, if there is one.

Note that the retired argument is automatically added to the function
formals if it's not there. This makes the code cleaner because retired
arguments don't need to clutter the function definition:

```{r}
fn <- function(..., new) NULL
retire(fn, "0.1.0", old =)
```

However this might not be appropriate if the deprecated argument
originally appeared before `...`. In that case the argument order
might not be backward compatible.
