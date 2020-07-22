#' Change the unit of figure size
#'
#' By default, figure size of R Markdown is specified with inches.
#' This function changes the default unit.
#'
#' As a side effect, `fig.retina` is set to `NULL` because of
#' <https://github.com/yihui/knitr/issues/1876>.
#'
#' @param unit A string of an unit (default: "mm"). Available units follow.
#'  `measurements::conv_unit_options$length`
#'
#' @return invisible `NULL`
#'
#' @examples
#' hook_figure_unit('mm')
#'
#' @export
hook_figure_unit <- function(unit = "mm") {
  unit = match.arg(unit, measurements::conv_unit_options$length)
  if (unit == "inch") {
    message('inch is the default unit, and does not require hooks')
    return(invisible(NULL))
  }

  message('fig.retina is set NULL')

  coefficient = measurements::conv_unit(1, unit, "inch")

  hook_height <- function(options) {
    options$fig.height <- options$fig.height * coefficient
    return(options)
  }

  hook_width <- function(options) {
    options$fig.width <- options$fig.width * coefficient
    return(options)
  }

  knitr::opts_chunk$set(
    fig.retina = NULL,
    fig.height = knitr::opts_chunk$get("fig.height") / coefficient,
    fig.width = knitr::opts_chunk$get("fig.width") / coefficient
  )
  knitr::opts_hooks$set(
    fig.height = hook_height,
    fig.width = hook_width
  )

  return(invisible(NULL))
}
