
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

  n_dots == 1 && names2(exprs(...)) == ""
}

deprecate_function <- function(.fn, .name, .cycle, ..., .msg = NULL) {
  if (is_deprecated(.fn)) {
    abort(sprintf("Function `%s` is already deprecated", .name))
  }

  if (dots_n(...)) {
    replacement <- expr(...)
    if (!is_symbol(replacement)) {
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
    oldie::signal_deprecation(!!! data)
    !!! body(.fn)
  })

  set_attrs(.fn, deprecated = TRUE)
}

deprecate_arguments <- function(.fn, .name, .cycle, ..., .msg = NULL) {
  args <- exprs(...)
  if (!every(args, is_symbol)) {
    abort("Successors must be symbols")
  }

  nms <- names2(args)
  if (any(nms == "")) {
    abort("Deprecated arguments must be named with their successors")
  }

  args_chr <- map_chr(args, as_string)
  fmls <- fn_fmls(.fn)
  fmls_nms <- names(fmls)
  if (!all(args_chr %in% fmls_nms)) {
    abort("Can't find successor in function arguments")
  }
  if (any(nms %in% fmls_nms)) {
    abort("Can't add deprecated argument since it already exists in the function")
  }

  fn_fmls(.fn) <- c(fmls, args)

  depr_exprs <- map2(nms, args_chr, depr_argument_expr, .name, .cycle)
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
