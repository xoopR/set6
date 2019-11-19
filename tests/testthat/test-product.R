library(testthat)

context("product")

test_that("Set",{
  expect_true(product(Set$new(1), Set$new(2))$equals(Set$new(Tuple$new(1, 2))))
  expect_true(product(Set$new(1, 2), Set$new(2))$equals(Set$new(Tuple$new(1, 2), Tuple$new(2, 2))))
  expect_equal(product(Set$new(1, 2), Set$new(2), simplify = FALSE)$strprint(),
               "{1, 2} \u00D7 {2}")
})

test_that("Interval",{
  i = Interval$new(1,2) * Interval$new(3,4)
  expect_equal(i$strprint(), "[1, 2] \u00D7 [3, 4]")
  expect_true(i$lower$equals(Tuple$new(1, 3)))
  expect_true(i$upper$equals(Tuple$new(2, 4)))
  expect_equal(i$contains(list(Tuple$new(1, 4), Tuple$new(1, 3), Tuple$new(3, 1))), c(TRUE, TRUE, FALSE))
  expect_message(expect_equal(Interval$new(1,2) * ConditionalSet$new(function(x) TRUE), Set$new()), "Product of")
  expect_equal(Interval$new(1,2)*Set$new(), Interval$new(1,2))
})

test_that("conditionalset",{
  expect_equal(ConditionalSet$new(function(x) x == 1) * ConditionalSet$new(function(y) y > 1),
               ConditionalSet$new(function(x, y) x == 1 & y > 1))
})

test_that("fuzzy",{
  expect_equal(FuzzySet$new()*FuzzySet$new(), Set$new())
  expect_equal(FuzzySet$new(1, 0.1)*FuzzySet$new(), FuzzySet$new(1, 0.1))
  expect_equal(FuzzySet$new()*FuzzySet$new(1, 0.2), FuzzySet$new(1, 0.2))
  expect_message(expect_equal(FuzzySet$new(1,0.5) * ConditionalSet$new(function(x) TRUE), Set$new()), "Product of")
  expect_true(product(FuzzySet$new(1, 0.1), FuzzySet$new(2, 0.2))$equals(Set$new(FuzzyTuple$new(1, 0.1, 2, 0.2))))
  expect_equal(product(FuzzySet$new(1, 0.1), FuzzySet$new(2, 0.2), simplify = FALSE)$strprint(),
              "{1(0.1)} \u00D7 {2(0.2)}")
})

test_that("ProductSet",{
  i = (Interval$new(1, 2) * Interval$new(3, 4)) * Interval$new(5, 6)
  expect_equal(i$strprint(), "([1, 2] \u00D7 [3, 4]) \u00D7 [5, 6]")
  use_unicode(FALSE)
  expect_equal(i$strprint(), "([1, 2] X [3, 4]) X [5, 6]")
  expect_equal(getR6Class(Interval$new(1, 2) * Interval$new(1, 2)), "ExponentSet")
  use_unicode(TRUE)
  expect_true(i$contains(Tuple$new(Tuple$new(1, 3), 5)))
  expect_false(i$contains(Tuple$new(1, 3, 5)))
  expect_true(i$contains(c(Tuple$new(Tuple$new(1, 3), 5), Tuple$new(Tuple$new(2, 3), 6)), all = TRUE))
})
