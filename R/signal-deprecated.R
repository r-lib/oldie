#' @rdname deprecate
#' @export
signal_deprecated <- function(.fn, .cycle, ..., .msg = NULL) {
  name <- as_string(ensym(.fn))

  caller_fn <- caller_fn()
  if (!is_namespace(get_env(caller_fn))) {
    abort("Deprecated functions must be scoped in a namespace")
  }

  cycle <- new_cycle(.cycle)
  pkg_version <- pkg_ver(ns_env_name(caller_fn))

  level <- deprecation_level(cycle, pkg_version)
  effective_level <- maybe_promote_deprecation(level)

  if (!level) {
    version <- "`undefined`"
  } else {
    version <- as.character(cycle[[level]])
  }

  # Return immediately if not yet deprecated
  if (effective_level < 1) {
    return(invisible(NULL))
  }

  type <- switch(effective_level,
    `1` = "soft-deprecated",
    `2` = "deprecated",
    `3` = "defunct"
  )
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

    msg <- deprecated_function_msg(name, version, type, replacement)

    signal("deprecated",
      name = name,
      replacement = replacement,
      version = version,
      .msg = msg
    )
    return(invisible(NULL))
  }

  replacements <- dots_list(...)
  args <- names(replacements)

  for (i in seq_along(replacements)) {
    msg <- deprecated_argument_msg(name, args[[i]], version, type, replacements[[i]])

    signal("deprecated_arg",
      name = name,
      argument = args[[i]],
      replacement = replacements[[i]],
      version = version,
      .msg = msg
    )
  }

  invisible(NULL)
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

deprecated_function_msg <- function(name, version, type,
                                    replacement = NULL) {
  stopifnot(
    is_string(name),
    is_null(replacement) || is_string(replacement)
  )

  msg <- sprintf("`%s()` is %s as of version %s", name, type, version)
  if (!is_null(replacement)) {
    msg <- sprintf("%s, please use `%s()` instead", msg, replacement)
  }

  msg
}
deprecated_argument_msg <- function(name, argument, version, type,
                                    replacement = "") {
  stopifnot(
    is_string(name),
    is_string(argument),
    is_string(replacement)
  )

  msg <- sprintf("Argument `%s` of function `%s()` is %s as of version %s",
    argument,
    name,
    type,
    version
  )
  if (!identical(replacement, "")) {
    msg <- sprintf("%s\nPlease use `%s` instead", msg, replacement)
  }

  msg
}
