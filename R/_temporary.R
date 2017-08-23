#' Signal deprecation of a function or arguments
#'
#' @param .name The name of the function that is deprecated or whose
#'   arguments are deprecated.
#' @param .cycle A character vector defining the deprecation cycle.
#' @param ... Replacements.
#' @param .msg A custom error message.
#'
#' @export
signal_deprecation <- function(.name, .cycle, ..., .msg = NULL) {
  fn <- caller_fn()
  stopifnot(is_namespace(get_env(fn)))

  cycle <- new_cycle(.cycle)
  pkg_version <- pkg_ver(ns_env_name(fn))

  level <- deprecation_level(cycle, pkg_version)
  effective_level <- maybe_promote_deprecation(level)

  # Not deprecated yet
  if (effective_level < 1) {
    return(invisible(NULL))
  }

  signal <- switch(effective_level,
    `1` = cnd_signal,
    `2` = cnd_warn,
    `3` = cnd_abort
  )

  if (is_fn_replacement(...)) {
    if (dots_n(...)) {
      replacement <- string(...)
    } else {
      replacement <- NULL
    }

    version <- as.character(cycle[[level]])
    msg <- deprecated_function_msg(.name, version, level, replacement)

    return(signal("deprecated",
      replacement = replacement,
      version = version,
      .msg = msg
    ))
  }

  browser("TODO")

  return(invisible(NULL))
}

deprecation_level <- function(cycle, pkg_version) {
  due_levels <- map_lgl(cycle, function(ver) !is_null(ver) && ver <= pkg_version)

  if (!any(due_levels)) {
    return(0)
  }

  max(which(due_levels))
}
maybe_promote_deprecation <- function(level) {
  if (level < 3 && is_true(peek_option("oldie_verbose_deprecation"))) {
    level <- level + 1
  }

  level
}

deprecated_function_msg <- function(name, version, level, replacement = NULL) {
  stopifnot(
    is_string(name),
    is_string(version),
    is_null(replacement) || is_string(replacement)
  )

  type <- switch(level,
    `1` = "soft-deprecated",
    `2` = "deprecated",
    `3` = "defunct"
  )

  msg <- sprintf("`%s` is %s as of version %s", name, type, version)
  if (!is_null(replacement)) {
    msg <- sprintf("%s, please use `%s` instead", msg, replacement)
  }

  msg
}


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
