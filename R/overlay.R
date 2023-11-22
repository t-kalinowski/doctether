
make_updated_overlay <- function(old_tether, old_overlaid, new_tether,
                                 name = "roxygen_block") {
  dir <- withr::local_tempdir(paste0(dirname(getwd()), "-roxysync"))
  withr::local_dir(dir)

  git("init --initial-branch=tether")
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
  exit_code <- git("rebase tether overlay", # --empty=keep --keep-empty
                   valid_exit_codes = c(0L, 1L),
                   stdout = FALSE)
  new_overlaid <- readLines(file)
  if(exit_code) {
    new_overlaid[new_overlaid == "<<<<<<< HEAD"] <-
      "<<<<<<< (updated tether)"
    new_overlaid[grep(">>>>>>> [^ ]+ \\(overlay\\)", new_overlaid)] <-
      ">>>>>>> (previous overlay)"
    attr(new_overlaid, "conflict") <- TRUE
  }

  new_overlaid
}


view_tether_overlay_diff <- function(rdname) {
  # writeout block rd
  tether_file <- fs::path("man-src", "tethers", rdname, ext = "md")
  adaptation_file <- fs::path(tempdir(), "doctether", rdname, ext = "R")
  fs::dir_create(dirname(adaptation_file))
  writeLines(get_block_overlay_lines(rdname), adaptation)

  system2("code", c("--diff", tether_file, adaptation_file))
}
