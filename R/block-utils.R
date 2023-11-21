
get_block_name <- function(block) {
  block$object$topic %||%
    unlist(lapply(block$tags, \(tag) if(tag$tag == "rdname") tag$val)) %||%
    unlist(lapply(block$tags, \(tag) if(tag$tag == "title") tag$val)) %||%
    paste0(c(block$file, block$line), collapse = "-")
}

#' @importFrom utils getSrcLocation
get_block_overlay_line_range <- function(block) {
  ## Returns the line number pair c(<start_of_roxygen_block>, <end_of_fn_signature>)
  ## possibly falling back to <end_of_fn_body> instead of <end_of_fn_signature>
  ## returns NULL if something goes wrong

  # this only contains the start line for each tag
  roxy_line_range <- range(unlist(lapply(block$tags, \(tag) {
    if (is.null(tag$raw)) NULL # skip '<generated>' tags that point to sourcecode
    else tag$line
  })))

  call_start_line <- getSrcLocation(block$object$value, "parse") %||% -1L
  call_end_line <- getSrcLocation(block$object$value, "parse", FALSE) %||% -1L
  call_body_start_line <- tryCatch(
    getSrcLocation(body(block$object$value), "line")[1],
    warning = function(w) NULL,
    error = function(e) NULL
  ) %||% call_end_line

  code_line_range <- c(call_start_line, call_end_line)
  signature_line_range <- c(call_start_line, call_body_start_line)
  overlay_line_range <- range(c(roxy_line_range, signature_line_range))

  if(any(overlay_line_range == -1L))
    NULL
  else
    as.integer(overlay_line_range)
}


check_line_range <- function(line_range, max = Inf) {
  stopifnot(length(line_range) == 2, is.integer(line_range),
            line_range[1] >= 1,
            line_range[2] <= max)
}


get_block_overlay_lines <- function(rdname) {
  # go look for man/{rdname}.Rd
  # line 2, parse out from:  "% Please edit documentation in R/retether.R, R/roclet.R"
  rd <- readLines(glue("man/{rdname}.Rd"))
  file <- sub("% Please edit documentation in ", "", rd[2], fixed = TRUE)
  blocks <- roxygen2::parse_file(file)
  for(block in blocks) {
    if(!identical(get_block_name(block), rdname))
      next

    line_range <- get_block_overlay_line_range(block)
    lines <- readLines(file)[line_range[1]:line_range[2]]
    return(lines)
  }
}
