#' Number lines on code blocks
#'
#' Number lines on code blocks created by chunks, i.e. source, output,
#' message, warning, and/or error.
#'
#' @param targets A character vector specifying what kind of code blocks
#'  automatically number lines (default: `"source"`). Choices are
#'  `"source"`, `"output"`, `"message"`, `"warning"`, and/or `"error"`.`
#'  `NULL` disables the automation.
#'
hook_numberLines <- function(targets = "source") {
  targets <- if (!is.null(targets)) {
    match.arg(
      default,
      c("source", "output", "message", "warning", "error"),
      several.ok = TRUE
    )
  }
  hook <- function(options) {
    targets <- paste0("class.", knitr::opts_current$get("numberLines"))
    options[targets] <- lapply(options[targets], append, "numberLines")
    return(options)
  }
  knitr::opts_chunk$set(numberLines = targets)
  knitr::opts_hooks$set(numberLines = hook)
  return(invisible(NULL))
}
