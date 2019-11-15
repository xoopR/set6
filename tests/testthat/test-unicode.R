library(testthat)

context("use_unicode")

test_that("use_unicode",{
  expect_error(use_unicode("a"), "Assertion on")
  expect_silent(use_unicode(TRUE))
  expect_true(use_unicode())
  expect_silent(use_unicode(FALSE))
  expect_false(use_unicode())
})
