

git <- function(...,
                stdout = '', warnings = FALSE,
                retries = 4L, valid_exit_codes = 0L) {
  for (i in seq(retries)) {
    res <- if (warnings)
                       system2t("git", c(...), stdout = stdout) else
      suppressWarnings(system2t("git", c(...), stdout = stdout))

    if (isTRUE(stdout)) {
      out <- res
      exit_code <- attr(res, "status", TRUE) %||% 0L
      errmsg <- attr(res, "errmsg", TRUE)
    } else {
      out <- exit_code <- res
      errmsg <- NULL
    }

    if (identical(exit_code, 128L)) {
      # probably .git/index.lock contention with vscode
      Sys.sleep(.1)
      next
    } else if (any(map_lgl(valid_exit_codes, identical, exit_code))) {
      break
    } else {
      cat("res <- ")
      dput(res)
      stop("non-0 exit from git ", str_split1_on_first(..1, " ")[[1]], errmsg)
    }
  }
  out
}
