test_that("set chunk options", {
  set(knitr::opts_chunk, c("foo", "bar"), list(1L, 2L))
  expect_identical(knitr::opts_chunk$get("foo"), 1L)
  expect_identical(knitr::opts_chunk$get("bar"), 2L)
})
