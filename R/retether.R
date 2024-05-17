
#' Retether an R package
#'
#' @param ... reserved for future use, and to ensure that all arguments are
#'   named
#' @param base_path path to an R package.
#' @param roxy_tag_eval A function which takes a single argument, the string
#'   value of the `@tether` tag, and returns a tether. A tether is coerced and
#'   flatten to a character vector. Pass `NULL` to skip processing roxygen
#'   blocks, and only retether vignettes.
#' @param roxy_tether_file A function which takes a single argument, the roxygen
#'   block name, and returns a file path for where to store (cache) the tether.
#'   Defaults to `".tether/man/{block_name}.txt"`.
#' @param rmd_field_eval A function which takes a single argument, the value
#'   supplied to the `tether: ` field in the frontmatter of an Rmd or Qmd file
#'   in the `vignettes/` or `vignettes-src/` directory, and returns a tether. A
#'   tether is coerced and flatten to a character vector. Pass `NULL` to skip
#'   processing vignettes and only retether roxygen blocks.
#' @param rmd_tether_file A function which takes a single argument, the rmd file
#'   path, and returns a file path for where to store (cache) the tether.
#'   Defaults to `".tether/{relative_rmd_filepath}"`
#'
#' @export
#' @importFrom roxygen2 env_package parse_package roclet_process roclet_output
#' @importFrom withr local_dir local_options with_options
#' @importFrom cli cli_alert_info cli_progress_step cli_progress_update
#'   cli_progress_bar cli_progress_update cli_progress_done cli_alert_success
#'   cli_bullets cli_alert_warning
retether <-
function(...,
         base_path = ".",
         roxy_tag_eval = \(tag_raw) eval(str2lang(tag_raw), globalenv()),
         roxy_tether_file = \(block_name) glue(".tether/man/{block_name}.txt"),
         rmd_field_eval = \(field_raw) readLines(field_raw),
         rmd_tether_file = \(rmd_file) fs::path(".tether/", fs::path_rel(rmd_file))) {

  stopifnot(length(list(...)) == 0)

  if(base_path != ".") {
    local_dir(base_path)
    base_path <- "."
  }

  check_no_unstaged_changes(
    # ".tether/",
    if(!is.null(roxy_tag_eval)) "R/",
    if(!is.null(rmd_field_eval)) c("vignettes-src", "vignettes"))

  if(!is.null(roxy_tag_eval)) {

    local_options(doctether.roxy_tag_eval = roxy_tag_eval,
                  doctether.resolve_tether_file = roxy_tether_file)

    # cli_progress_step("Loading package")
    with_options(list(keep.source.pkgs = TRUE,
                      keep.parse.data.pkgs = TRUE), {
      env <- env_package(".")
    })

    cli_alert_info("Parsing roxygen blocks.")
    blocks <- parse_package(env = env)

    cli_alert_info("Finding @tether tags")
    results <- tether_process_blocks(blocks, env = env)

    cli_alert_info("Checking roxygen tethers for updates")
    tether_output(results)
  }

  # now handle vignettes
  if(!is.null(rmd_field_eval)) {
    rmd_files <- list.files(c("vignettes-src", "vignettes"),
                            pattern = "\\.[qQrR]?md$", recursive = TRUE,
                            full.names = TRUE, all.files = TRUE)
    cli_alert_info("Checking vignette tethers for updates")
    cli_progress_bar("Checking vignettes", total = length(rmd_files))
    for (rmd_file in rmd_files) {
      cli_progress_update()
      tryCatch({
        retether_rmd(rmd_file,
                     eval_tether_field = rmd_field_eval,
                     get_tether_file = rmd_tether_file)
      }, error = warning)
    }
    cli_progress_done()
  }
  cli_alert_success("Finished retethering package!")
}

# TODO: git hooks to make sure someone doesn't accidently check in <<<< ===== >>>> lines.

#' @importFrom fs path path_rel
retether_rmd <-
function(rmd_file,
         eval_tether_field = readLines,
         get_tether_file = \(rmd_file) path(".tether/", path_rel(rmd_file))) {

  fm <- rmarkdown::yaml_front_matter(rmd_file)
  if (is.null(fm$tether)) {
    # message("No tether field: ", rmd_file)
    return(invisible())
  }

  new_tether <- eval_tether_field(fm$tether) |> str_normalize_tether()
  tether_file <- get_tether_file(rmd_file)

  if (!file.exists(tether_file)) {
    dir_create(dirname(tether_file))
    # message("Writing out new tether: ", tether_file)
    cli_alert_info("New tether: {.file {tether_file}}")
    writeLines(new_tether, tether_file)
    fs::file_chmod(tether_file, "444") # read only, same as '-w'
    return(invisible())
  }

  old_tether <- readLines(tether_file) |> str_normalize_tether()

  if(old_tether == new_tether) {
    # message("No change: ", rmd_file)
    return(invisible())
  }

  new_overlaid <- make_updated_overlay(old_tether = old_tether,
                                       old_overlaid = readLines(rmd_file),
                                       new_tether = new_tether)
  cli_alert_info("Updated tether: {.file {tether_file}}")
  # message("Writing out updated tether: ", tether_file)
  unlink(tether_file)
  writeLines(new_tether, tether_file)
  fs::file_chmod(tether_file, "444") # read only, same as '-w'

  if(isTRUE(attr(new_overlaid, "conflict", TRUE))) {
    cli_alert_warning("Updating vignette (with conflicts): {.file {rmd_file}}")
  } else {
    cli_alert_info("Updating vignette: {.file {rmd_file}}")
  }
  # message("Writing out updated rmd: ", rmd_file)
  writeLines(new_overlaid, rmd_file)

  invisible()
}

# TODO: use {cli} instead of message()/cat();
#   print a nice summary output at end of retether() listing
#   - updated blocks, (w/ and w/o conflicts),
#   - new symbols / tethers
#   - n symbols w/ unchanged tethers.

#' Stub tag parse for `@tether` tags
#'
#' This method is exported to allow roxygen2 to ignore the `@tether` tag
#' during the standard `roxygen2::roxygenise()` workflow.
#'
#' @param x The `@tether` tag
#'
#' @return `x`, unmodified
#' @export
#' @keywords internal
roxy_tag_parse.roxy_tag_tether <- function(x) x

#' @importFrom roxygen2 roxy_tag_parse roxy_tag_warning
tether_parse_tag <- function(x) {
  # job of this function is to set `x$val <- <something>`
  #  where <something> is something convenient for roclet_process() later.
  eval_tag <- getOption('doctether.roxy_tag_eval', function(string) {
    eval(str2lang(string), new.env(parent = globalenv()))
  })
  evaled <- tryCatch({
    eval_tag(x$raw)
  }, error = identity)

  if (inherits(evaled, "error")) {
    roxy_tag_warning(x, "error evaluating @tether tag")
    return()
  }

  x$val <- evaled
  x
}


#' @importFrom roxygen2 roclet_process block_get_tag
tether_process_blocks <- function(blocks, env) {
  # called with a list of blocks
  # returns whatever we want, something convenient for roclet_output() later
  results <- list()

  cli_progress_bar(
    format = "Blocks checked {cli::pb_current}/{length(blocks)}, Tags found: {length(results)}.",
    total = length(blocks),
    clear = FALSE
  )

  for (block in blocks) {
    cli_progress_update()

    tag <- block_get_tag(block, "tether") %||% next
    tag <- tether_parse_tag(tag)

    name <- get_block_name(block)

    results[[length(results) + 1L]] <- list(
      name = name,
      file = block$file,
      line_range =  get_block_overlay_line_range(block),
      # tether_obj = tag$val,
      tether = as_tether(tag$val)
    )
  }
  cli_progress_done()

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

  tally <- list(
    no_change = 0L,
    new_tether = 0L,
    updated_tether_w_conflict = 0L,
    updated_tether_wo_conflict = 0L
  )

  cli_progress_bar(
    total = length(results),
    format = paste(
      sep = ", ",
      "{cli::pb_bar}",
      "Processed: {cli::pb_current}/{cli::pb_total}",
      "No change: {tally$no_change}",
      "New: {tally$new_tether}",
      "Updated: (no conflict) {tally$updated_tether_wo_conflict}",
      "Updated: (conflict) {tally$updated_tether_w_conflict}"
    )
  )
  for(result in results) {
    cli_progress_update()

    tether_file <- get_tether_file(result$name)

    if(!fs::file_exists(tether_file)) {
      cli_alert_info("New tether {.topic {result$name}} ({.file {tether_file}})")
      # cat(sprintf("%s: writing out new tether (%s)\n",
      #             result$name, tether_file))
      writeLines(result$tether, tether_file)
      fs::file_chmod(tether_file, "444") # read only, same as '-w'
      add(tally$new_tether) <- 1L
      next
    }

    old_tether <- str_normalize_tether(readLines(tether_file))
    new_tether <- str_normalize_tether(result$tether)

    if(old_tether == new_tether) {
      # cat(result$name, ": No change\n", sep = "")
      add(tally$no_change) <- 1L
      next
    }

    roxified_new_tether <-
      attr(result$tether, "roxified", TRUE) %||%
      roxify_tether.default(tether = new_tether) |>
      str_normalize_tether()

    if(!is.na(roxified_new_tether) && !identical(roxified_new_tether, "")) {

      old_overlaid <-
        get_file_lines(result$file, result$line_range) |>
        str_normalize_tether()

      if (identical(roxified_new_tether, "") || is.na(roxified_new_tether))
        new_overlaid <- old_overlaid
      else
        new_overlaid <- make_updated_overlay(
          old_tether = old_tether,
          old_overlaid = old_overlaid,
          new_tether = roxified_new_tether,
          name = result$name
        )

      if(isTRUE(attr(new_overlaid, "conflict", TRUE))) {
        add(tally$updated_tether_w_conflict) <- 1L
      } else {
        add(tally$updated_tether_wo_conflict) <- 1L
      }

      set_file_lines(result$file, result$line_range, new_overlaid)
    }

    # message("writing out new tether: ", tether_file)
    cli_alert_info("Updated tether {.topic {result$name}} ({.file {tether_file}})")
    unlink(tether_file)
    writeLines(new_tether, tether_file)
    fs::file_chmod(tether_file, "444") # read only, same as '-w'
  }

  cli_progress_done()
  cli_alert_success("Finished processing roxygen tethers")
  x <- character()  # Build up summary bullets
  if (tally$no_change)
    x <- c(x, "v" = "Unchanged tethers: {tally$no_change}")
  if (tally$new_tether)
    x <- c(x, "i" = "New tethers: {tally$new_tether}")
  if (tally$updated_tether_wo_conflict)
    x <- c(x, "!" = "Updated tethers (no conflicts): {tally$updated_tether_wo_conflict}")
  if (tally$updated_tether_w_conflict)
    x <- c(x, "x" = "Updated tethers (with conflicts): {tally$updated_tether_w_conflict}")
  cli_bullets(x)

  for(file in names(.files)) {
    cli_alert_warning("Updating source file: {.file {file}}")
    # message("Writing out: ", file)
    file_lines <- .files[[file]]
    writeLines(file_lines[!is.na(file_lines)], file)
  }

  invisible(NULL)
}


get_tether_file <- function(name) {
  resolve <- getOption("doctether.resolve_tether_file", NULL)
  if(is.null(resolve)) {
    dir <- fs::path("man-src/tether")
    file <- fs::path(dir, paste0(name, ".R"))
  } else if(is.function(resolve)) {
    file <- fs::path_tidy(resolve(name))
    dir <- dirname(file)
  } else if(is.character(resolve)) {
    dir <- resolve
    file <- fs::path(dir, paste0(name, ".R"))
  } else {
    stop("Invalid value supplied to `options(doctether.resolve_tether_file = )`")
  }

  fs::dir_create(dir)
  file
}
