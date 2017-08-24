#' Signal deprecation of a function or arguments
#'
#' @param .fn The function to deprecate or whose arguments are to be
#'   deprecated. This should be supplied as a bare name.
#' @param .cycle A character vector defining the deprecation cycle.
#' @param ... Replacements.
#' @param .msg A custom error message.
#'
#' @export
signal_deprecation <- function(.fn, .cycle, ..., .msg = NULL) {
  name <- as_string(ensym(.fn))
  caller_fn <- caller_fn()
  stopifnot(is_namespace(get_env(caller_fn)))

  cycle <- new_cycle(.cycle)
  pkg_version <- pkg_ver(ns_env_name(caller_fn))

  level <- deprecation_level(cycle, pkg_version)
  effective_level <- maybe_promote_deprecation(level)

  # Return immediately if not yet deprecated
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
      replacement <- as_string(expr(...))
    } else {
      replacement <- NULL
    }

    version <- as.character(cycle[[level]])
    msg <- deprecated_function_msg(name, version, level, replacement)

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

  msg <- sprintf("`%s()` is %s as of version %s", name, type, version)
  if (!is_null(replacement)) {
    msg <- sprintf("%s, please use `%s()` instead", msg, replacement)
  }

  msg
}
