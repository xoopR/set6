library(testthat)

context("DifferenceSet")

test_that("SpecialSets",{
  use_unicode(FALSE)
  x = Reals$new() - Naturals$new()
  expect_equal(x$strprint(), "R \\ N")
  expect_true(x$contains(1.1))
  expect_false(x$contains(1))
  expect_equal(x$elements, NaN)
  expect_equal(x$length, Inf)
  expect_equal(x$addedSet, Reals$new())
  expect_equal(x$subtractedSet, Naturals$new())
  use_unicode(TRUE)
})
