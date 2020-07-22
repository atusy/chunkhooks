#' Benchmark chunks
#'
#' `hook_benchmark` sets a hook to benchmark chunks with the `benchmark=TRUE`
#' option. The name of the trigger chunk option can be changed via the
#' `chunk_option` parameter. The result is printed right after chunk outputs.
#' See examples for the default printing format by `format_benchmark`.
#'
#' `benchmarks` records the results of benchmarks from chunks as a list named by
#' chunk labels. If one requires complex formatting of benchmark results, then
#' suppress automatic formatting by `hook_benchmark(format = NULL)`. Then,
#' retrieve benchmark results from `benchmarks[["chunk-label"]]`. Furthermore,
#' `format` can happen conditionally by utilizing current chunk options via
#' the second argument of the `format`ting function.
#'
#' @param trigger A name of chunk option that triggers benchmark (default:
#'  `"benchmark"`). A value of option must be other than `NULL` (default) in
#'  order to perform a benchmark.
#' @param format A function to format a benchmark result (default:
#'  `format_benchmark`). It must accept two arguments, where the first is the
#'  benchmark result and the second is the list of current chunk options.
#'  `NULL` suppresses printing.
#'
#' @examples
#' # Set a hook that triggers benchmarks if the `time` chunk option is not `NULL`.
#' hook_benchmark("time")
#'
#' # Example of the default output format
#' # Input is sec. Output is prettified.
#' format_benchmark(1234)
#'
#' @name benchmark
#' @export
hook_benchmark <- function(trigger = "benchmark", format = format_benchmark) {
  format <- check_format_benchmark(format)
  hook <- function(before, options, envir) {
    t <- proc.time()['elapsed']
    if (before) {
      assign(options$label, t, envir = benchmarks)
      return(invisible(NULL))
    } else {
      result = t - get(options$label, envir = benchmarks)
      assign(options$label, result, envir = benchmarks)
      return(format(benchmarks[[options$label]], options))
    }
  }

  do.call(knitr::knit_hooks$set, stats::setNames(list(hook), trigger))
  return(invisible(NULL))
}

#' @param result A result of benchmark
#' @param options A list of current chunk options
#' @rdname benchmark
#' @export
format_benchmark <- function(result, options) {
  sprintf('%s: %s', options$label, prettyunits::pretty_sec(result))
}

#' @param
#' @rdname benchmark
#' @export
benchmarks <- new.env()

check_format_benchmark <- function(format) {
  if (is.null(format)) {
    return(function(...) NULL)
  }
  if (!is.function(format)) {
    stop("format must be a function")
  }
  params <- names(formals(format))
  if ((length(params) < 2) && !("..." %in% params)) {
    stop("format must accept at least 2 arguments like `format_benchmark()`")
  }
  format
}
