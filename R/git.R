

git <- function(...,
                stdout = TRUE, stderr = FALSE,
                warnings = FALSE,
                retries = 1L, valid_exit_codes = 0L) {
  for (i in seq(retries)) {
    res <- if (warnings)
                     system2("git", c(...), stdout = stdout, stderr = stderr) else
    suppressWarnings(system2("git", c(...), stdout = stdout, stderr = stderr))

    if (isTRUE(stdout)) {
      out <- res
      exit_code <- attr(res, "status", TRUE) %||% 0L
      errmsg <- attr(res, "errmsg", TRUE)
    } else {
      out <- exit_code <- res
      errmsg <- NULL
    }

    if (identical(exit_code, 128L)) {
      # probably .git/index.lock contention with vscode, let retry
      Sys.sleep(.1)
      next
    } else if (any(map_lgl(valid_exit_codes, identical, exit_code))) {
      break
    } else {
      # cat("res <- "); dput(res)
      stop("non-0 exit from git ", str_split1_on_first(..1, " ")[[1]], errmsg)
    }
  }
  invisible(out)
}
