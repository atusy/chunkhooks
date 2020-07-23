test_that("Set chunk options and hooks", {
  hook <- hook_benchmark()
  testthat::expect_type(f, "closure")
  expect_null(knitr::opts_chunk$get("benchmark"))
  expect_identical(knitr::knit_hooks$get("benchmark"), hook)
  knitr::opts_chunk$restore()
  knitr::knit_hooks$restore()
})

test_that("Skip set chunk options and hooks", {
  hook <- hook_benchmark(default = TRUE, .set = FALSE)
  testthat::expect_type(hook, "closure")
  expect_null(knitr::opts_chunk$get("benchmark"))
  expect_null(knitr::knit_hooks$get("benchmark"))
  knitr::opts_chunk$restore()
  knitr::knit_hooks$restore()
})
