
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
  new <- dots_list(...)
  if (!every(new, is_string)) {
    abort("Successors must be strings")
  }

  old <- names2(new)
  if (any(old == "")) {
    abort("Deprecated arguments must be named with their successors")
  }

  new <- flatten_chr(unname(new))
  fmls <- fn_fmls(.fn)
  fmls_nms <- names(fmls)
  if (!all(new %in% fmls_nms)) {
    abort("Can't find successor in function arguments")
  }
  if (any(old %in% fmls_nms)) {
    abort("Can't add deprecated argument since it already exists in the function")
  }

  compat_fmls <- rep_along(old, list(missing_arg()))
  compat_fmls <- set_names(compat_fmls, old)
  fn_fmls(.fn) <- c(fmls, compat_fmls)

  depr_exprs <- map2(old, new, depr_argument_expr, .name, .cycle)
  body(.fn) <- expr({
    !!! depr_exprs

    NULL # Work around quasiquotation bug

    !!! body(.fn)
  })

  .fn
}
depr_argument_expr <- function(old, new, name, cycle, body) {
  old_sym <- sym(old)
  new_sym <- sym(new)

  expr(
    if (!missing(!! old_sym)) {
      oldie::signal_deprecation(!! name, !! cycle, !!! set_names(new, old))
      UQ(new_sym) <- UQ(old_sym)
    }
  )
}

is_deprecated <- function(x) {
  is_true(attr(x, "deprecated"))
}
