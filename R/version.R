
ver <- function(x) {
  stopifnot(is_string(x))
  as_version(as.numeric_version(x))
}
new_version <- function(x) {
  stopifnot(is_integerish(x))
  ver(paste(x, collapse = "."))
}
as_version <- function(x) {
  if (inherits(x, "numeric_version")) {
    set_attrs(x, class = "version")
  } else {
    abort("Can't convert `x` to a version")
  }
}

is_version <- function(x, n_components = NULL, max_digits = NULL, minor = NULL) {
  if (!inherits(x, "version")) {
    return(FALSE)
  }

  components <- ver_components(x)

  if (!is_null(n_components) && length(components) != n_components) {
    return(FALSE)
  }

  if (!is_null(max_digits)) {
    large <- log10(components) >= max_digits
    if (any(large)) {
      return(FALSE)
    }
  }

  if (!is_null(minor)) {
    is_minor <-  components[[length(components)]] != 0
    if (!identical(minor, is_minor)) {
      return(FALSE)
    }
  }

  TRUE
}

#' @export
`[[.version` <- function(x, i) {
  ver_components(x)[[i]]
}
#' @export
`[[<-.version` <- function(x, i, value) {
  components <- ver_components(x)
  components[[i]] <- value
  new_version(components)
}
#' @export
length.version <- function(x) {
  length(ver_components(x))
}
#' @export
Ops.version <- function(e1, e2) {
  # Ops.numeric_version() assumes the length method is not implemented
  e1 <- as.numeric_version(e1)
  e2 <- as.numeric_version(e2)

  # For some reason NextMethod() throws an error
  eval_bare(lang(.Generic, e1, e2), base_env())
}

#' @export
print.version <- function(x, ...) {
  print(as.numeric_version(x))
}
#' @export
as.character.version <- function(x, ...) {
  as.character(as.numeric_version(x))
}


ver_components <- function(ver) {
  flatten_int(ver)
}

ver_bump <- function(ver, component = c("patch", "minor", "major")) {
  stopifnot(is_version(ver))

  components <- ver_components(ver)
  i <- ver_component_index(component)

  stopifnot(length(components) >= i)
  components[[i]] <- components[[i]] + 1L

  new_version(components)
}
ver_unbump <- function(ver, component = c("patch", "minor", "major")) {
  stopifnot(is_version(ver))

  i <- ver_component_index(component)

  if (i > length(ver)) {
    abort("Version does not have that many components")
  }
  if (i < 0) {
    abort("Can't supply negative version component")
  }

  x <- ver[[i]]

  if (x == 0) {
    ver[[i]] <- 9
    ver_unbump(ver, i - 1)
  } else {
    ver[[i]] <- x - 1
    ver
  }
}
ver_component_index <- function(component) {
  if (is_scalar_integer(component)) {
    return(component)
  }

  component <- arg_match(component, c("patch", "minor", "major"))

  i <- switch(component,
    major = 1L,
    minor = 2L,
    patch = 3L
  )
}

ver_trim <- function(ver, max_components = 3) {
  stopifnot(is_version(ver))

  if (length(ver) <= max_components) {
    return(ver)
  }

  components <- ver_components(ver)

  i <- min(length(components), max_components)
  i <- max(i, max_components)

  components <- components[seq_len(i)]
  new_version(components)
}


pkg_ver <- function(pkg) {
  stopifnot(is_string(pkg))
  as_version(utils::packageVersion(pkg))
}
