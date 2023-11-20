
make_updated_overlay <- function(old_tether, old_overlaid, new_tether,
                                 name = "roxygen_block") {
  dir <- withr::local_tempdir(paste0(dirname(getwd()), "-roxysync"))
  withr::local_dir(dir)

  git("init --initial-branch=tether") #--quiet
  file <- paste0(name, ".txt")
  writeLines(old_tether, file)
  git("add", file)
  git("commit -m 'previous tether'")

  git("checkout -b 'overlay'")
  writeLines(old_overlaid, file)
  git("add", file)
  git("commit -m 'overlay'")

  git("checkout tether")
  writeLines(new_tether, file)
  git("add", file)
  git("commit -m 'new tether'")

  git("checkout overlay")
  exit_code <- git("rebase tether overlay", valid_exit_codes = c(0L, 1L)) # --empty=keep --keep-empty
  new_overlaid <- readLines(file)
  if(exit_code) {
    new_overlaid[new_overlaid == "<<<<<<< HEAD"] <-
      "<<<<<<< (updated tether)"
    new_overlaid[grep(">>>>>>> [^ ]+ \\(overlay\\)", new_overlaid)] <-
      ">>>>>>> (previous overlay)"
  }

  new_overlaid
}


view_tether_overlay_diff <- function(rdname) {
  # writeout block rd
  tether_file <- fs::path("man-src", "tethers", rdname, ext = "md")
  adaptation_file <- fs::path(tempdir(), "doctether", rdname, ext = "R")
  fs::dir_create(dirname(adaptation_file))
  writeLines(get_block_lines(rdname), adaptation)

  system2("code", c("--diff", tether_file, adaptation_file))
}

get_block_lines <- function(rdname) {
  # go look for man/{rdname}.Rd
  # line 2, parse out from:  "% Please edit documentation in R/retether.R, R/roclet.R"
  rd <- readLines(glue("man/{rdname}.Rd"))
  file <- sub("% Please edit documentation in ", "", rd[2], fixed = TRUE)
  blocks <- roxygen2::parse_file(file)
  for(block in blocks) {
    if(!identical(get_block_name(block), rdname))
      next

    line_range <- get_overlay_line_range(block)
    lines <- readLines(file)[line_range[1]:line_range[2]]
    return(lines)
  }
}
