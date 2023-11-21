
# doctether <a href="https://github.com/t-kalinowski/doctether/"><img src="man/figures/logo.png" align="right" height="138" alt="a drawing of a carabiner and rope in the shape of the letter R" /></a>

<!-- badges: start -->
<!-- badges: end -->

_doctether_ makes it easy to keep R documentation up-to-date
with any upstream sources the documentation is adapted from.
It is a tool for R package maintainers.

## Installation

You can install the development version of doctether like so:

``` r
remotes::install_github("t-kalinowski/doctether")
```

## Example

Start by adding a `@tether` tag to a roxygen block. This tag will be used to
resolve the upstream documentation you want to keep the roxygen block
synchronized with.

For example, say we have in our package a documented R function,
`layer_identity()`, and we would like to keep the function and documentation
synchronized with upstream, the Python module endpoint `keras.layers.Identity`. 
We add to our roxygen block a `@tether keras.layers.Identity` tag like this: 

``` r
#' Identity Layer
#'
#' Use as a placeholder layer when no operation is to be
#' performed.
#'
#' @param object
#' Object to compose the layer with. A tensor, array, or sequential model.
#'
#' @param ...
#' Standard layer args
#'
#' @export
#' @tether keras.layers.Identity
layer_identity <-
function (object, ...)
{
  create_layer(keras$layers$Identity, object, list(...))
}
```

In the standard `devtools::document()` (Ctrl/Cmd + Shift + D) workflow, the `@tether` tag is ignored.

To synchronize docs with tethers, we call `doctether::retether()`, like so:

```r
py_run_string("import keras") # setup python __main__ w/ the latest module version

parse_tether_tag <- function(endpoint) {
  py_obj <- py_eval(endpoint) # e.g., keras.layers.Identity

  roxy <- py_obj$`__doc__` |> glue::trim() |> strsplit("\n") |> _[[1]]

  fn <- function() {}
  formals(fn) <- formals(py_obj)  # just the signature

  paste0(collapse = "\n",
    paste0("#' ", roxy),
    deparse(fn)
  )
}

doctether::retether(parse_tag = parse_tether_tag)
```

Now, when the Python endpoint `__doc__` or `__signature__` changes upstream, you
will see the changes dynamically incorporated into the roxygen block and
function definition in `R/layers.R`, as well as an updated tether file cached in
`man-src/tether/layer_identity.R`. If there are any conflicts encountered while
attempting to rebase the previous roxygen adaption and overlay it on the updated
tether, git-formatted conflict markers are inserted in the roxygen block. All
that's left then to do is to review the changes, resolve any conflicts, and
stage and commit the updates.
