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
#' @param .set `TRUE` or `FALSE` to set the hook.
#'
#' @return invisible hook function
#'
#' @examples
#' hook_figure_unit('mm')
#'
#' @export
hook_figure_unit <- function(unit = "mm", .set = TRUE) {
  unit = match.arg(unit, measurements::conv_unit_options$length)

  coefficient = measurements::conv_unit(1, unit, "inch")

  hook <- function(options) {
    options$fig.width <- options$fig.width * coefficient
    options$fig.height <- options$fig.height * coefficient
    return(options)
  }

  if (.set) {
    knitr::opts_chunk$set(
      fig.retina = NULL,
      fig.height = knitr::opts_chunk$get("fig.height") / coefficient,
      fig.width = knitr::opts_chunk$get("fig.width") / coefficient
    )
    message('fig.retina is set NULL')
    knitr::opts_hooks$set(fig.width = hook)
  }

  return(invisible(hook))
}
