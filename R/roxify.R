
#' @export
roxify_tether <- function(x, tether = as_tether(x), ...) {
  if(!is.null(roxified <- attr(x, "roxified", TRUE)))
    return(str_normalize_tether(roxified))
  if(!is.null(roxified <- attr(tether, "roxified", TRUE)))
    return(str_normalize_tether(roxified))
  rm(roxified)

  UseMethod("roxify_tether")
}

#' @export
roxify_tether.python.builtin.function <- function(x, tether, ...) {

  doc <- x$`__doc__` |> glue::trim() |> str_normalize_tether() |> str_split_lines()
  roxy <- str_c("#' ", doc)

  fn <- function() {}
  formals(fn) <- formals(x)

  str_normalize_tether(str_flatten_lines(roxy, deparse(fn)))
}

#' @export
roxify_tether.python.builtin.type <- roxify_tether.python.builtin.function

#' @export
roxify_tether.python.builtin.module <- function(x, tether, ...) {
  ""
}

#' @export
roxify_tether.default <- function(x, tether, ...) {
  tether <- str_normalize_tether(tether)
  str_flatten_lines(str_c("#' ", str_split_lines(tether)))
}
