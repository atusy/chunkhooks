% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/hook_benchmark.R
\docType{data}
\name{benchmark}
\alias{benchmark}
\alias{hook_benchmark}
\alias{format_benchmark}
\alias{benchmarks}
\title{Benchmark chunks}
\format{
An object of class \code{environment} of length 0.
}
\usage{
hook_benchmark(
  trigger = "benchmark",
  default = NULL,
  format = format_benchmark,
  .set = TRUE
)

format_benchmark(result, options)

benchmarks
}
\arguments{
\item{trigger}{A name of chunk option that triggers benchmark (default:
\code{"benchmark"}). In order to trigger benchmark, specify \code{TRUE} as a
value of this option.}

\item{default}{A default value for the chunk option that \code{trigger}s
the hook (default: \code{NULL}).}

\item{format}{A function to format a benchmark result (default:
\code{format_benchmark}). It must accept two arguments, where the first is the
benchmark result and the second is the list of current chunk options.
\code{NULL} suppresses printing.}

\item{.set}{\code{TRUE} or \code{FALSE} to set the hook.}

\item{result}{A result of benchmark}

\item{options}{A list of current chunk options}
}
\value{
invisible hook function
}
\description{
\code{hook_benchmark} sets a hook to benchmark chunks with the \code{benchmark=TRUE}
option. The name of the trigger chunk option can be changed via the
\code{chunk_option} parameter. The result is printed right after chunk outputs.
See examples for the default printing format by \code{format_benchmark}.
}
\details{
\code{benchmarks} records the results of benchmarks from chunks as a list named by
chunk labels. If one requires complex formatting of benchmark results, then
suppress automatic formatting by \code{hook_benchmark(format = NULL)}. Then,
retrieve benchmark results from \code{benchmarks[["chunk-label"]]}. Furthermore,
\code{format} can happen conditionally by utilizing current chunk options via
the second argument of the \code{format}ting function.
}
\examples{
# Set a hook that triggers benchmarks if the `time` chunk option is not `NULL`.
hook_benchmark("time")

# Example of the default output format
# Input is sec. Output is prettified.
format_benchmark(1234, options = list(label = "example-chunk"))

}
\keyword{datasets}
