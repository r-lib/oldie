
new_cycle <- function(cycle) {
  if (!length(cycle) || length(cycle) > 3) {
    abort("`cycle` must have 1, 2, or 3 components")
  }
  if (is_character(cycle)) {
    cycle <- chr_as_cycle(cycle)
  }

  cycle_check(cycle, n_components = 3, max_digits = 2, minor = FALSE)

  cycle
}
chr_as_cycle <- function(cycle) {
  if (all(cycle == "")) {
    abort("`cycle` can't be empty")
  }

  is_empty <- cycle == ""
  cycle[!is_empty] <- map(cycle[!is_empty], ver)
  cycle[is_empty] <- list(NULL)

  # All cycles must have 3 components
  if (length(cycle) < 3) {
    n <- length(cycle)
    n_missing <- 3 - n

    filler <- list_len(n_missing)
    cycle <- c(cycle, filler)

    for (i in seq_len(n_missing)) {
      cycle[[n + i]] <- ver_bump(cycle[[n + i - 1]], "minor")
    }
  }

  cycle
}

cycle_check <- function(cycle, n_components, max_digits, minor) {
  is_empty <- map_lgl(cycle, is_null)
  trimmed_cycle <- cycle[!is_empty]
  map(trimmed_cycle, ver_check, n_components, max_digits, minor)

  if (length(trimmed_cycle) > 1) {
    if (any(slide_lgl(trimmed_cycle, `>=`))) {
      abort("`cycle` versions must be monotonically increasing")
    }
  }
}
ver_check <- function(ver, n_components = NULL, max_digits = NULL, minor = NULL) {
  if (!is_version(ver)) {
    abort("can't parse version")
  }

  components <- ver_components(ver)

  if (!is_version(ver, n_components = n_components)) {
    msg <- "version must have %s components, not %s"
    msg <- sprintf(msg, n_components, length(components))
    abort(msg)
  }

  if (!is_version(ver, max_digits = max_digits)) {
    msg <- "version can't have components with more than %s digits"
    msg <- sprintf(msg, max_digits)
    abort(msg)
  }

  if (!is_version(ver, minor = minor)) {
    if (minor) {
      abort("version must be a minor update")
    } else {
      abort("version can't be a minor update")
    }
  }

  invisible(TRUE)
}
