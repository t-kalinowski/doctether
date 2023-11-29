

#' Generate tether
#'
#' @param x an object
#' @param ... passed on to methods
#' @param roxify whether a attribute `"roxified"` should be included. This is
#'   the value that will be used to rebase the roxygen block instead of the
#'   tether itself.
#'
#' @return the tether, a string or character vector (of lines) that will be used
#'   to track changes and rebase the documentation. If `roxify` is `TRUE`, the
#'   returned tether will contain an attribute `attr(tether, "roxified")`, which
#'   will also be a string. If present, `"roxified"` will be used to perform the
#'   rebase.
#' @export
as_tether <- function(x, ..., roxify = TRUE) {
  UseMethod("as_tether")
}

#' @rdname as_tether
#' @export
as_tether.default <- function(x, ..., roxify = TRUE) {
  tether <- str_normalize_tether(as.character(x, ...))
  if(roxify)
    attr(tether, "roxified") <- roxify_tether(x, tether = tether)
  tether
}


#' @rdname as_tether
#' @export
as_tether.connection <- function(x, ..., roxify = TRUE) {
  tether <- readLines(x) |> str_normalize_tether()
  if(roxify)
    attr(tether, "roxified") <- roxify_tether(x, tether = tether)
  tether
}

#' @rdname as_tether
#' @export
as_tether.python.builtin.object <- function(x, ..., roxify = TRUE) {
  py_help <- reticulate::import_builtins()$help
  tether <- reticulate::py_capture_output(py_help(x), type = "stdout") |>
    str_normalize_tether()
  if(roxify)
    attr(tether, "roxified") <- roxify_tether(x, tether = tether)
  tether
}

#' @importFrom stringr str_split str_c str_sort str_replace_all
#' @importFrom fs dir_create
#' @importFrom magrittr %>% %<>%
#' @export
#' @param inherited Whether to include inherited methods and attributes.
#' @rdname as_tether
as_tether.python.builtin.type <- function(x, ..., roxify = TRUE, inherited = FALSE) {

  tether <- as_tether.python.builtin.object(x, roxify = FALSE)

  # default help includes all inherited methods and properties, too much noise
  # for a single tether to track changes from inherited methods,
  if (isFALSE(inherited)) {
    attr_types <- c(
      "Methods",
      "Class methods",
      "Static methods",
      "Readonly properties",
      "Data descriptors",
      "Data and other attributes"
    )

    section_delim <-
    "\n \\|  ----------------------------------------------------------------------\n"
    section_header <- sprintf(" \\|  (%s) inherited from ([a-zA-Z0-9._]+):\n",
                              paste0(attr_types, collapse = "|"))
    pattern <- paste0(section_delim, section_header)
    # each section will either be
    # " | {attr_type} defined here:" or " | {attr_type} inherited from foo:"
    # all the "defined here" sections are first, so the first "inherited from"
    # section is where we cut.
    tether <- str_split1_on_first(tether, pattern)[1]
  }

  # signatures are all on one line, not great for git diffs.
  # split long signatures over multiple lines
  tether <- str_split_lines(tether)

  # match function signatures, which always show up on one line
  sig_line_nums <- grep("^ \\|  [a-zA-Z_][a-zA-Z0-9_]*\\(([^,]*(, [^,])*)*\\)$", tether)

  # the sig_line_nums regex can pick up examples in __doc__
  # quick hack:, filter for a section after __doc__, though we can still error
  # later if __doc__'s of methods have examples with lines like
  # array([1,2,3,4])
  sig_line_nums <- sig_line_nums |>
    (\(.) .[. > match(" |  Method resolution order:", tether, nomatch = 1)])()

  if(length(sig_line_nums)) {
    tether[sig_line_nums] <- tether[sig_line_nums] |>
      map_chr(function(line) {
        x <- line |> str_replace(fixed(" |  "), "") |> str_trim()
        x <- try(format_py_signature(x), silent = TRUE)
        if (inherits(x, "try-error"))
          return(line)
        str_flatten_lines(paste0(" |  ", str_split_lines(x)))
      })
  }

  tether <- str_normalize_tether(tether)

  if(roxify) {
    attr(tether, "roxified") <- roxify_tether(x, tether = tether)
  }

  # doc <- reticulate::import("inspect")$getdoc(x) |> glue::trim()
  # attr(tether, "roxified") <- str_normalize_tether(str_flatten_lines(
  #   str_c("#' ", str_split_lines(doc)),
  #   deparse(as.function.default(c(formals(x), quote({}))))
  # ))
  tether
}

#' @rdname as_tether
#' @export
as_tether.python.builtin.module <- function(x, ..., roxify = TRUE) {
  is_callable <- reticulate::import_builtins()$callable
  out <- map_chr(str_sort(names(x)), function(name) {
    .x <- x[[name]]
    if(is_callable(.x))
      format_py_signature(.x, name = name)
    else
      str_flatten(c(name, ": ", reticulate::py_str(.x)))
  })
  out <- str_normalize_tether(out)
  if(roxify)
    attr(out, "roxified") <- ""
  out
}

#' @rdname as_tether
#' @export
#' @param name the name to present in the Python `__signature__`
as_tether.python.builtin.function <- function(x, ..., name = x$`__name__`, roxify = TRUE) {
  inspect <- reticulate::import("inspect")
  doc <- glue::trim(inspect$getdoc(x))
  out <- str_normalize_tether(str_flatten_lines(
    # "__module__",
    # reticulate::py_to_r(reticulate::py_get_attr(x, "__module__", TRUE)) %||% "",
    "__signature__",
    format_py_signature(x, name = name),
    "__doc__",
    doc
  ))

  if(roxify) {
    attr(out, "roxified") <- str_normalize_tether(str_flatten_lines(
      str_c("#' ", str_split_lines(doc)),
      deparse(as.function.default(c(formals(x), quote({}))))
    ))
  }
  out
}

#' @export
#' @rdname as_tether
as_tether.python.builtin.method <- as_tether.python.builtin.function
# TODO: we can build a better __name__ for methods here...

#' @importFrom stringr str_split_1 str_trim str_flatten str_sub<- str_sub
#'   str_replace fixed
#' @importFrom glue glue as_glue
#' @importFrom rlang is_string
format_py_signature <- function(x = reticulate::py_eval(name), name = NULL) {
  if (!is_string(x)) {
    if (!inherits(x, "inspect.Signature")) {
      inspect <- reticulate::import("inspect")
      x <- inspect$signature(x)
    }
    x <- reticulate::py_str(x)
  }

  # split sigs with 3 or more args over multiple lines, one arg per line,
  # taking care to not split default tuple values (i.e., can't just strsplit(","))
  x2 <- x |>
    str_replace_all("(\\(|, )([a-zA-Z_*][a-zA-Z0-9_*]*=?)", "\\1\n\\2") |>
    str_flatten_lines() |>
    str_replace_all(fixed("=\n"), "=") |>
    str_split_lines() |>
    str_trim()

  # sanity check
  if(!(length(x2) == 1 || x2[1] |> endsWith("("))) {
    stop("error formatting py signature")
  }

  # 3 or more args, do split
  if (length(x2) >= 4) {
    x <- x2
    x[-1] <- str_c("  ", x[-1]) # left pad
    x[length(x)] <- x[length(x)] |> str_replace("\\)$", "\n)")
    x <- x |> str_flatten_lines()
  }

  x <- str_flatten(c(name, x))
  as_glue(x)
}


#' @export
as_tether.function <- function(x, ..., roxify = TRUE) {
  ns <- environment(x)
  ns_name <- environmentName(ns)
  if(!identical(ns, asNamespace(ns_name)))
    stop("Can't locate packge for ", deparse1(x))
  pkgname <- ns_name
  for(funname in getNamespaceExports(ns)) {
    if (identical(x, get(funname, envir = ns, inherits = FALSE)))
      break
  }
  help_ <- utils::help((funname), (pkgname), help_type = 'text')
  tether <- utils::capture.output(tools::Rd2txt(
    asNamespace("utils")$.getHelpFile(help_),
    options = list(code_quote = FALSE,
                   underline_titles = FALSE,
                   itemBullet = "* ",
                   sectionIndent = 0L),
    package = pkgname
  )) |>
    str_normalize_tether()

  fn <- x
  body(fn) <- quote({})

  if(roxify)
    attr(tether, "roxified") <- str_normalize_tether(str_flatten_lines(
      str_c("#' ", str_split_lines(tether)),
      deparse(as.function(c(formals(x), quote({}))))
    ))

  tether
}
