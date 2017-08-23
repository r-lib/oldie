
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
