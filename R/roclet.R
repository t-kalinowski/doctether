

#' @export
#' @importFrom roxygen2 roxy_tag_parse roxy_tag_warning
roxy_tag_parse.roxy_tag_tether <- function(x) x # identity?


tether_parse_tag <- function(x) {
  # job of this function is to set `x$val <- <something>`
  #  where <something> is something convenient for roclet_process() later.
  parse_tag <- getOption('roxysync.tether_tag_parse', function(raw) {
    str_normalize_tether(map_chr(str2expression(x$raw), eval, globalenv()))
  })
  parsed <- tryCatch(parse_tag(x$raw), error = identity)

  if (inherits(parsed, "error")) {
    roxy_tag_warning(x, "error evaluating @tether tag")
    return()
  }

  x$val <- str_normalize_tether(parsed)
  x
}


#' @importFrom roxygen2 roclet_process block_get_tag
tether_process_blocks <- function(blocks, env) {
  # called with a list of blocks
  # returns whatever we want, something convenient for roclet_output() later
  results <- list()
  for (block in blocks) {

    tag <- block_get_tag(block, "tether") %||% next
    tag <- tether_parse_tag(tag)

    name <- get_block_name(block)

    results[[name]] <- list(
      name = name,
      file = block$file,
      line_range =  get_overlay_line_range(block),
      tether =  tag$val
    )
  }

  results
}


tether_output <- function(results) {

  .files <- new.env(parent = emptyenv())
  get_file_lines <- function(file, line_range) {
    file_lines <- .files[[file]] %||% { .files[[file]] <- readLines(file) }
    check_line_range(line_range, max = length(file_lines))
    file_lines[line_range[1]:line_range[2]]
  }

  set_file_lines <- function(file, line_range, new_lines) {
    file_lines <- .files[[file]] %||% { .files[[file]] <- readLines(file) }
    check_line_range(line_range, max = length(file_lines))
    file_lines[line_range[1]:line_range[2]] <- NA
    file_lines[line_range[1]] <- str_normalize_tether(new_lines)
    .files[[file]] <- file_lines
  }

  for(result in results) {
    tether_file <- get_tether_file(result$name)

    if(!fs::file_exists(tether_file)) {
      cat(sprintf("%s: writing out new tether (%s)\n",
                  result$name, tether_file))
      writeLines(result$tether, tether_file)
      next
    }

    old_tether <- str_normalize_tether(readLines(tether_file))
    new_tether <- result$tether

    if(old_tether == new_tether) {
      cat(result$name, ": No change\n", sep = "")
      next
    }

    old_overlaid <- get_file_lines(result$file, result$line_range)
    new_overlaid <- make_updated_overlay(
      old_tether = old_tether,
      old_overlaid = old_overlaid,
      new_tether = new_tether,
      name = result$name
    )

    set_file_lines(result$file, result$line_range, new_overlaid)
    message("writing out new tether: ", tether_file)
    writeLines(new_tether, tether_file)
  }

  for(file in names(.files)) {
    message("Writing out: ", file)
    file_lines <- .files[[file]]
    writeLines(file_lines[!is.na(file_lines)], file)
  }

  invisible(NULL)
}


#' Retether an R package
#'
#' @param base_path path to an R package.
#'
#' @param tag_parser A function taking a single argument, the raw `@tether`
#'   string, returning a tether. If `NULL`, (the default), the tag value is
#'   evaluated with `eval(str2expression(tag$raw), globalenv())`
#' @param resolve_tether_file if `NULL`, the default, tether files are stored
#'   under `"man-src/tether/{block_name}.R"`. If supplied a string, this is
#'   taken as the directory where to store tether files (instead of
#'   `"man-src/tether"`). If a function, it is expected to take a single
#'   argument, the block name, and return a filepath.
#'
#' @export
#' @importFrom roxygen2 env_package parse_package roclet_process roclet_output
#' @importFrom withr local_dir local_options with_options
retether <- function(base_path = ".", tag_parser = NULL, resolve_tether_file = NULL) {

  if(base_path != ".") {
    local_dir(base_path)
    base_path <- "."
  }

  check_no_unstaged_changes("R/", "man-src/")

  local_options(roxysync.tether_tag_parse = tag_parser,
                roxytether.resolve_tether_file = resolve_tether_file)

  with_options(list(keep.source.pkgs = TRUE,
                    keep.parse.data.pkgs = TRUE), {
    env <- env_package(".")
  })

  blocks <- parse_package(env = env)
  results <- tether_process_blocks(blocks, env = env)
  tether_output(results)

}


check_no_unstaged_changes <- function(...) {
  files_w_unstaged_changes <- git("diff --name-only", ..., stdout = TRUE)
  if(length(files_w_unstaged_changes))
    stop("Run `retether()` only without any unstaged changes. The following files have changes that must be staged, committed, or stashed: ",
         paste0("- ", files_w_unstaged_changes, collapse = "\n"))
}
