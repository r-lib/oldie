
slide <- function(.x, .f, ...) {
  n <- length(.x)
  if (n <= 1) {
    abort("Can't slide on a vector that is not at least length 2")
  }

  out <- list_len(n - 1)
  for (i in seq_len(n - 1)) {
    out[[i]] <- .f(.x[[i]], .x[[i + 1]], ...)
  }

  out
}
slide_lgl <- function(.x, .f, ...) {
  flatten_lgl(slide(.x, .f, ...))
}

is_language <- is_lang

bad_symbols <- function(chr) {
  bad <- glue::backtick(chr)
  bad <- glue::collapse(bad, ", ", last = " and ")
  bad
}
pluralise <- function(n, singular, plural) {
  if (n == 1) {
    singular
  } else {
    plural
  }
}
pluralise_len <- function(x, singular, plural) {
  pluralise(length(x), singular, plural)
}

is_namespaced_symbol <- function(x) {
  if (typeof(x) != "language") {
    return(FALSE)
  }

  first <- node_car(x)
  arg <- node_cadr(node_cdr(x))
  identical(first, quote(`::`)) && is_symbol(arg)
}
