testthat::context("test package loads")

testthat::test_that("package loads", {
  testthat::expect_success(library("appl"))
})
