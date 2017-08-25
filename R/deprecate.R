
deprecate <- function(.fn, .cycle, ..., .msg = NULL) {
  nm <- ensym(.fn)
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

  n_dots == 1 && names2(exprs(..., .ignore_empty = "none")) == ""
}

deprecate_function <- function(.fn, .name, .cycle, ..., .msg = NULL) {
  if (is_deprecated(.fn)) {
    abort(sprintf("Function `%s` is already deprecated", as_string(.name)))
  }

  .cycle <- new_cycle_chr(.cycle)

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
    oldie::signal_deprecated(!!! data)
    !!! body(.fn)
  })

  set_attrs(.fn, deprecated = TRUE)
}

deprecate_arguments <- function(.fn, .name, .cycle, ..., .msg = NULL) {
  args <- exprs(..., .ignore_empty = "none")
  if (!every(args, is_symbol)) {
    abort("Successors must be symbols")
  }

  nms <- names2(args)
  if (any(nms == "")) {
    abort("Deprecated arguments must be named with their successors")
  }

  args_chr <- map_chr(args, as_string)
  formals <- fn_fmls(.fn)
  formals_nms <- names(formals)
  if (!all(args_chr %in% c(formals_nms, ""))) {
    abort("Can't find successor in function arguments")
  }
  if (any(nms %in% formals_nms)) {
    abort("Can't add deprecated argument since it already exists in the function")
  }

  new_args <- map(args, function(...) missing_arg())
  fn_fmls(.fn) <- c(formals, new_args)

  depr_exprs <- map2(nms, args_chr, deprecated_arg_expr, .name, .cycle)
  fn_body(.fn) <- expr({
    !!! depr_exprs

    NULL # Work around quasiquotation bug

    !!! body(.fn)
  })

  .cycle <- new_cycle_chr(.cycle)
  deprecated_args <- map(set_names(args_chr, nms), deprecated_arg, cycle = .cycle)
  deprecated_args <- c(deprecated_args(.fn), deprecated_args)
  .fn <- set_attrs(.fn, deprecated_args = deprecated_args)

  .fn
}
deprecated_arg_expr <- function(old, new, name, cycle, body) {
  old_sym <- sym(old)
  new_sym <- sym(new)

  if (is_missing(new_sym)) {
    reassign <- NULL
  } else {
    reassign <- expr(UQ(new_sym) <- UQ(old_sym))
  }

  expr(
    if (!missing(!! old_sym)) {
      oldie::signal_deprecated(!! name, !! cycle, !!! set_names(new, old))
      !!! reassign
    }
  )
}
deprecated_arg <- function(successor, cycle) {
  list(successor = successor, cycle = cycle)
}

is_deprecated <- function(x) {
  is_true(attr(x, "deprecated"))
}
has_deprecated_args <- function(x) {
  length(deprecated_args(x))
}
deprecated_args <- function(x) {
  attr(x, "deprecated_args")
}
