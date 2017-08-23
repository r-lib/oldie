
deprecate <- function(.fn, .cycle, ..., .msg = NULL) {
  nm <- as_string(ensym(.fn))
  stopifnot(is_closure(.fn))

  if (is_fn_replacement(...)) {
    deprecate_function(.fn, nm, .cycle, ..., .msg = .msg)
  } else {
    deprecate_arguments(.fn, nm, .cycle, ..., .msg = .msg)
  }
}
is_fn_replacement <- function(...) {
  n_dots <- nargs()

  if (!n_dots) {
    return(TRUE)
  }

  n_dots == 1 && names2(list(...)) == ""
}

deprecate_function <- function(.fn, .name, .cycle, ..., .msg = NULL) {
  if (is_deprecated(.fn)) {
    abort(sprintf("Function `%s` is already deprecated", .name))
  }

  data <- list(.name, .cycle, ..., .msg = .msg)

  # Remove NULL arguments for prettier code expansion
  if (is_null(.msg)) {
    data$.msg <- NULL
  }

  body(.fn) <- expr({
    oldie::signal_deprecation(!!! data)
    !!! body(.fn)
  })

  set_attrs(.fn, deprecated = TRUE)
}
deprecate_arguments <- function(.fn, .name, .cycle, ..., .msg = NULL) {
  abort("TODO")
}

is_deprecated <- function(x) {
  is_true(attr(x, "deprecated"))
}
