

str_split1_on_first <- function(x, pattern, ...) {
  stopifnot(length(x) == 1, is.character(x))
  regmatches(x, regexpr(pattern, x, ...), invert = TRUE)[[1L]]
}


str_flatten_lines <- function(...) {
  stringi::stri_flatten(unlist(c(...)), collapse = "\n")
}

str_split_lines <- function(...) {
  x <- c(...)
  if(!is.character(x) || length(x) > 1)
    x <- str_flatten_lines(x)
  stringi::stri_split_fixed(x, "\n")[[1L]]
}

str_flatten_and_compact_lines <- function(..., roxygen = FALSE) {
  out <- str_split_lines(...) |>
    stringi::stri_trim_right() |>
    str_flatten_lines() |>
    stringi::stri_replace_all_regex("\n{4,}", "\n\n\n")
  if(roxygen)
    out <- out |> stringi::stri_replace_all_regex("\n(#'\n){3,}", "\n#'\n#'\n")
  out
}


str_normalize_tether <- function(x) {
  str_split_lines(x) |>
    stringi::stri_trim_right() |>
    str_flatten_lines() |>
    stringi::stri_trim_both()
}

map_chr <- function(.x, .f, ...) {
  vapply(X = .x, FUN = .f, FUN.VALUE = "", ..., USE.NAMES = FALSE)
}


map_lgl <- function(.x, .f, ...) {
  vapply(X = .x, FUN = .f, FUN.VALUE = TRUE, ..., USE.NAMES = FALSE)
}


drop_nulls <- function(x, i = NULL) {
  if(is.null(i))
    return(x[!vapply(x, is.null, FALSE, USE.NAMES = FALSE)])

  drop <- logical(length(x))
  names(drop) <- names(x)
  drop[i] <- vapply(x[i], is.null, FALSE, USE.NAMES = FALSE)
  x[!drop]
}



system2t <- function(command, args, ...) {
  message(paste("+", maybe_shQuote(command), paste0(args, collapse = " ")))
  system2(command, args, ...)
}

maybe_shQuote <- function(x)  {
  needs_quote <- !grepl("^[[:alnum:]/._-]+$", x)
  if (any(needs_quote))
    x[needs_quote] <- shQuote(x[needs_quote])
  x
}

`%||%` <- function(x, y) if (is.null(x)) y else x

check_no_unstaged_changes <- function(...) {
  files_w_unstaged_changes <- git("diff --name-only", ..., stdout = TRUE)
  if(length(files_w_unstaged_changes))
    stop("Run `retether()` only without any unstaged changes. ",
         "The following files have changes that must be staged, committed, or stashed:\n",
         paste0("- ", files_w_unstaged_changes, collapse = "\n"))
}

