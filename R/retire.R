#' Deprecate a function or argument
#'
#' @description
#'
#' `retire()` marks a function or some of its arguments as
#' obsolete. This enables automatic documentation by roxygen, signals
#' a condition when a deprecated function is run or when a deprecated
#' argument is supplied, and checks that the deprecation cycle
#' conforms to tidyverse rules.
#'
#' The conditions are signalled with with `signal_retired()` which
#' has the same interface as `retire()`. It should always be called
#' directly within the deprecated function. Since it is added
#' automatically by `retire()`, you should rarely have to call it
#' yourself.
#'
#' @section Deprecation levels:
#'
#' There are three deprecation levels:
#'
#' - **Soft-deprecated**: This is the first stage of deprecation. The
#'   function or argument continues to work normally without any
#'   warning. Soft-deprecated functions will generally not be documented, and
#'   should not be used in examples or package code. They are left
#'   around so that existing code continues to work, but new code
#'   should not use them.
#'
#'   To make soft-deprecated functions signal an error, see below.
#'
#' - **Deprecated**: The function or argument now issues a warning
#'   when used or supplied. Users should upgrade their code to use the
#'   suggested replacement, if any.
#'
#' - **Defunct**: The function or argument now issues an error when
#'   used or supplied.
#'
#' These levels are defined by a deprecation cycle, see section
#' below. You can promote the current deprecation level by setting the
#' global option `oldie_verbose_retirement` to `TRUE`.
#' Soft-deprecated functions then become deprecated, deprecated
#' functions become defunct, and so on. This is useful to check
#' whether you're relying on any soft-deprecated functions or
#' arguments.
#'
#' @section Deprecation cycle:
#'
#' `.cycle` associates each deprecation stage to a release version of
#' your package. It should be a character vectors of three versions.
#'
#' * `c("0.1.0", "0.3.0", "1.0.0")`: Soft-deprecation at after the
#'   0.1.0 release, deprecation after 0.3.0, and defunct after 1.0.0.
#'
#' * `"0.1.0"`: This is equivalent to `c("0.1.0", "0.2.0", "0.3.0")`.
#'   When a single version is supplied, it is assumed that it marks
#'   the start of a deprecation cycle that is bumped at each minor
#'   version (middle number).
#'
#' * `c("0.1.0", "", "")`: The function is soft-deprecated but is not
#'   planned to ever be deprecated or defunct. This is useful for
#'   discouraging users from using a function without forcing them to
#'   change their code.
#'
#' @param .fn The function to deprecate or whose arguments are to be
#'   deprecated. This should be supplied as a bare name.
#' @param .cycle A character vector defining the deprecation cycle.
#'   See the relevant section.
#' @param ... Replacements, supplied as bare names.
#'
#' * If no replacement is supplied, the function is deprecated with no
#'   replacement.
#'
#' * If a single unnamed replacement is supplied, the function is
#'   deprecated with the replacement. If the replacement function
#'   lives in another package, indicate it with a namespace:
#'   `"pkg::replacement"`.
#'
#' * If one or several named replacements are supplied, the function
#'   is not deprecated. Instead, the supplied arguments are. `old =
#'   new` means that the argument `old` is deprecated with replacement
#'   `new`. `old = ` means that the argument `old` is deprecated
#'   without replacement.
#' @param .msg An alternative deprecation message.
#' @export
#' @examples
#' # Let's create an obsolete function:
#' old_fn <- function() "old"
#'
#' # You can deprecate it without any replacement:
#' retire(old_fn, "0.1.0")
#'
#' # The cycle above specifies only one version. The cycle is
#' # automatically filled and the above expression is thus equivalent to:
#' retire(old_fn, c("0.1.0", "0.2.0", "0.3.0"))
#'
#' # If there is a new function replacing the old one, just supply its
#' # bare name:
#' retire(old_fn, "0.1.0", replacement_fn)
#'
#'
#' # Deprecating an argument is very similar. They are supplied as
#' # key-value pairs where the key is the deprecated argument and the
#' # value, if supplied, is the replacement. This deprecates an
#' # argument without replacement:
#' fn <- function(..., old) NULL
#' retire(fn, "0.1.0", old = )
#'
#' # This deprecates with replacement. The deprecated argument is
#' # automatically reassigned to the replacement:
#' fn <- function(..., new, old) NULL
#' retire(fn, "0.1.0", old = new)
#'
#' # The deprecated argument will be added to the formals if
#' # needed. This way you can omit the deprecated arguments from the
#' # function declaration:
#' fn <- function(..., new) NULL
#' retire(fn, "0.1.0", old = new)
retire <- function(.fn, .cycle, ..., .msg = NULL) {
  nm <- ensym(.fn)
  stopifnot(is_closure(.fn))

  if (is_fn_replacement(...)) {
    fn <- retire_function(.fn, nm, .cycle, ..., .msg = .msg)
  } else {
    fn <- retire_arguments(.fn, nm, .cycle, ..., .msg = .msg)
  }
  new_fn(fn)
}
is_fn_replacement <- function(...) {
  n_dots <- nargs()

  if (!n_dots) {
    return(TRUE)
  }

  n_dots == 1 && names2(exprs(..., .ignore_empty = "none")) == ""
}

retire_function <- function(.fn, .name, .cycle, ..., .msg = NULL) {
  if (is_retired(.fn)) {
    abort(sprintf("Function `%s` is already deprecated", as_string(.name)))
  }

  .cycle <- new_cycle_chr(.cycle)

  if (dots_n(...)) {
    replacement <- expr(...)
    if (!is_symbol(replacement) && !is_namespaced_symbol(replacement)) {
      abort("Replacement function must be a symbol")
    }
    data <- list(.name, .cycle, replacement, .msg = .msg)
  } else {
    data <- list(.name, .cycle, .msg = .msg)
  }

  # Remove NULL arguments for prettier code expansion
  if (is_null(.msg)) {
    data$.msg <- NULL
  }

  body(.fn) <- expr({
    oldie::signal_retired(!!! data)
    !!! body(.fn)
  })

  set_attrs(.fn, retired = TRUE)
}

retire_arguments <- function(.fn, .name, .cycle, ..., .msg = NULL) {
  args <- exprs(..., .ignore_empty = "none")
  if (!every(args, is_symbol)) {
    abort("Replacements must be symbols")
  }

  nms <- names2(args)
  if (any(nms == "")) {
    abort("Replacements must be named")
  }

  already_retired <- nms %in% names(retired_args(.fn))
  if (any(already_retired)) {
    bad <- nms[already_retired]
    has <- pluralise_len(bad, "has", "have")
    abort(glue("{ bad_symbols(bad) } { has } already been deprecated"))
  }

  replacements <- map_chr(args, as_string)

  .fn <- add_retired_formals(.fn, replacements)

  depr_exprs <- map2(nms, replacements, retired_arg_expr, .name, .cycle)
  fn_body(.fn) <- expr({
    !!! depr_exprs

    NULL # Work around quasiquotation bug

    !!! body(.fn)
  })

  .cycle <- new_cycle_chr(.cycle)
  retired_args <- map(set_names(replacements, nms), retired_arg, cycle = .cycle)
  retired_args <- c(retired_args(.fn), retired_args)
  .fn <- set_attrs(.fn, retired_args = retired_args)

  .fn
}
add_retired_formals <- function(fn, replacements) {
  formals <- fn_fmls(fn)
  formals_nms <- names(formals)
  if (!all(replacements %in% c(formals_nms, ""))) {
    abort("Can't find replacement in function arguments")
  }

  nms <- names(replacements)
  existing <- nms %in% formals_nms

  new_args <- set_names(nms[!existing])
  new_args <- map(new_args, function(...) missing_arg())
  fn_fmls(fn) <- c(formals, new_args)

  fn
}

retired_arg_expr <- function(old, new, name, cycle, body) {
  old_sym <- sym(old)
  new_sym <- sym(new)

  if (is_missing(new_sym)) {
    reassign <- NULL
  } else {
    reassign <- expr(UQ(new_sym) <- UQ(old_sym))
  }

  expr(
    if (!missing(!! old_sym)) {
      oldie::signal_retired(!! name, !! cycle, !!! set_names(new, old))
      !!! reassign
    }
  )
}
utils::globalVariables("UQ<-")

retired_arg <- function(replacement, cycle) {
  list(replacement = replacement, cycle = cycle)
}

is_retired <- function(x) {
  is_true(attr(x, "retired"))
}
has_retired_args <- function(x) {
  length(retired_args(x))
}
retired_args <- function(x) {
  attr(x, "retired_args")
}
