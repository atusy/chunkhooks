#' Prepend title comment to chunks
#'
#' Prepend title comments to each chunks.
#' By default, title is the name of the corresponding chunk engines.
#'
#' @param trigger A name of chunk option triggers the hook.
#' @param default
#'  A string or function as a default value of the `trigger`ing chunk option.
#'  If string, the value is given to `glue::glue`, and evaluates the expression
#'  by setting chunk options as an environment, and then automatically commented out.
#'  If function, it must have `options` parameter which receives chunk options,
#'  and return a string.
#' @param args
#'  A list of arguments given to `glue::glue()` or callable `default`.
#' @param .set
#'  `TRUE` or `FALSE` to set a default value and a hook to the `trigger`.
#'
#' @examples
#' # Set hook which comments engine names of each chunks at the beginning.
#' hook_title_comment()
#'
#' # Customize format
#' # For R chunks, let title comment be something like "chunk-label.R".
#' # Otherwise, fall back to the default behavior.
#' hook_title_comment(
#'   default = function(options) {
#'     if (options$engine == "R") {
#'       return(sprintf("# %s.%s", options$label, options$engine))
#'     }
#'     return(comment_title(options, template = "{engine}"))
#'   }
#' )
#'
#' @seealso [comment_title()], [comment_out()], [comment_syntax()]
#'
#' @export
hook_title_comment <- function(
  trigger = "title",
  default = "{engine}",
  args = list(),
  .set = TRUE
) {
  force(trigger)
  force(args)
  hook <- function(options) {
    title <- options[[trigger]]
    options$code <- c(
      do.call(
        if (is.function(title)) {
          function(...) c(title(options = options, ...), options$code)
        } else {
          function(...) {
            comment <- comment_title(options = options, template = title, ...)
            if (inherits(comment, "AsIs")) {
              warning("Skip adding title which cannot be commented out.")
              return(NULL)
            }
            c(comment, options$code)
          }
        },
        args
      )
    )
    return(options)
  }

  if (.set) {
    set(knitr::opts_chunk, trigger, list(default))
    set(knitr::opts_hooks, trigger, list(hook))
  }

  return(invisible(hook))
}

tidy_comment_syntax <- function(x) {
  do.call(
    c,
    lapply(x, function(x) setNames(rep_len(x[[1L]], length(x[[2L]])), x[[2L]]))
  )
}

#' Comment syntax.
#'
#' A named vector which defines comment syntax. This is internally used by
#' `hook_title_comment()`, `comment_title()`, and `comment_out()`.
#'
#' @export
comment_syntax <- tidy_comment_syntax(list(
  list("# %s", c("awk", "bash", "coffee", "gawk", "octave", "perl", "Rscript", "ruby", "sed", "sh", "zsh", "python", "julia", "R")),
  list("// %s", c("groovy", "node", "scala", "Rcpp", "dot", "asy", "stan", "js", "go")),
  list("-- %s", c("haskell", "mysql", "psql", "sql", "lua")),
  list("%% %s", c("tikz")),
  list("/* %s */", c("sas", "stata", "c", "css", "sass", "scss", "bslib")),
  list("! %s", c("fortran", "fortran95"))
))

#' Comment out codes based on their languages.
#'
#' @param x A character vector
#' @param engine
#'   A string that defines the engine of the `x`.
#'   See `names(comment_syntax)` for the predefined engines.
#' @param extra_syntax
#'   A named character vector which defines extra syntax to comment out.
#'   Each values should contain `%s` which is the placeholder to insert `x`.
#'   If definitions collides among `comment_syntax` and `extra_syntax`,
#'   then the latter has priority.
#'
#' @examples
#' # Default behaviors
#' comment_out("R language", "R")
#' comment_out("Python language", "python")
#'
#' # A customized behavior
#' comment_out(
#'   "Python language",
#'   "python",
#'   extra_syntax = c("python" = '"""%s"""')
#' )
#'
#' @seealso [comment_syntax()]
#' @export
comment_out <- function(x, engine, extra_syntax = character(0L)) {
  if (
    length(extra_syntax > 0L) &&
    (!is.character(extra_syntax) || is.null(names(extra_syntax)))
  ) {
    stop("extra_syntax must be a named character vector.")
  }
  syntax <- c(
    comment_syntax[setdiff(names(comment_syntax), names(extra_syntax))],
    extra_syntax
  )
  if (!any(names(syntax) %in% engine)) {
    warning(
      sprintf("Comment syntax is not registered for %s. ", engine),
      "Consider specifying extra_syntax option. ",
      'For example, `c(R = "# %s")` for R engine. ',
      "Note that %s is the place holder for the text to be commented out."
    )
    return(I(x))
  }
  sprintf(syntax[[engine]], x)
}

#' Comment title to chunk code
#'
#' @param options A list of chunk options.
#' @param template
#'  A template of title processed by `glue::glue()`.
#'  The processed value is automatically commented out by `comment_out()`.
#' @inheritParams comment_out
#' @param ... Arguments passed to `glue::glue()`
#'
#' @examples
#' comment_title(
#'   options = list(engine = "R"),
#'   template = "{engine}"
#' )
#'
#' comment_title(
#'   options = list(engine = "css", label = "mystyle"),
#'   template = "{label}.{engine}"
#' )
#'
#' @seealso [comment_out()]
#' @export
comment_title <- function(
  options = list(engine = "R"),
  template = "{engine}",
  extra_syntax = character(0L),
  ...
) {
  comment_out(
    glue::glue(template, .envir = options, ...),
    options[["engine"]],
    extra_syntax
  )
}
