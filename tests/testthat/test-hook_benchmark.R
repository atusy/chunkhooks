init <- function() {
  knitr::opts_chunk$delete("benchmark")
  knitr::knit_hooks$delete("benchmark")
}

test_that("Set chunk options and hooks", {
  init()
  hook <- hook_benchmark(default = TRUE)
  testthat::expect_type(hook, "closure")
  expect_true(knitr::opts_chunk$get("benchmark"))
  expect_identical(knitr::knit_hooks$get("benchmark"), hook)
})

test_that("Skip set chunk options and hooks", {
  init()
  hook <- hook_benchmark(default = TRUE, .set = FALSE)
  testthat::expect_type(hook, "closure")
  expect_null(knitr::opts_chunk$get("benchmark"))
  expect_null(knitr::knit_hooks$get("benchmark"))
})
