#' Prepend title comment to chunks 
#'
#' Title of chunks can be determined and prepended based on chunk options.
#' 
#' @param trigger A name of chunk option triggers the hook.
#' @param default
#'  A string or function as a default value of the `trigger`ing chunk option.
#'  If string, the value is given to `glue::glue`, and evaluates the expression
#'  by setting chunk options as an environment.
#'  If function, it must have `options` parameter which receives chunk options,
#'  and return a string.
#' @param args 
#'  A list of arguments given to `glue::glue()` or callable `default`.
#' @param .set
#'  `TRUE` or `FALSE` to set a default value and a hook to the `trigger`.
#'
#' @export
hook_title_comment <- function(
  trigger = "title",
  default = "# {label}.{engine}",
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
          function(...) title(options = options, ...)
        } else {
          function(...) glue::glue(title, .envir = options, ...)
        },
        args
      ),
      options$code
    )
    return(options)
  }

  if (.set) {
    set(knitr::opts_chunk, trigger, list(default))
    set(knitr::opts_hooks, trigger, list(hook))
  }

  return(invisible(hook))
}

