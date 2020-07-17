library(testthat)

context("useUnicode")

test_that("useUnicode", {
  expect_error(useUnicode("a"), "Assertion on")
  expect_silent(useUnicode(TRUE))
  expect_true(useUnicode())
  expect_silent(useUnicode(FALSE))
  expect_false(useUnicode())
})
