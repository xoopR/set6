library(testthat)

context("ProductSet")

test_that("Interval",{
  useUnicode(FALSE)
  expect_equal((Interval$new(1,2) * ConditionalSet$new(function(x) TRUE))$strprint(),
               "[1,2] X {TRUE : x in V}")
  expect_equal(Interval$new(1,2)*Set$new(), Interval$new(1,2))

  i = (Interval$new(1, 2) * Interval$new(3, 4)) * Interval$new(5, 6)
  expect_equal(i$strprint(), "[1,2] X [3,4] X [5,6]")
  i = (Interval$new(5, 6) * Interval$new(1, 2) * Interval$new(3, 4))
  expect_equal(i$strprint(), "[5,6] X [1,2] X [3,4]")
  i = setproduct(Interval$new(1, 2) * Interval$new(3, 4), Interval$new(5, 6), nest = TRUE)
  expect_equal(i$strprint(), "([1,2] X [3,4]) X [5,6]")
  expect_equal(getR6Class(Interval$new(1, 2) * Interval$new(1, 2)), "ExponentSet")
  expect_true(i$contains(Tuple$new(Tuple$new(1, 3), 5)))
  expect_false(i$contains(Tuple$new(1, 3, 5)))
  expect_true(i$contains(c(Tuple$new(Tuple$new(1, 3), 5), Tuple$new(Tuple$new(2, 3), 6)), all = TRUE))
  useUnicode(TRUE)
  expect_equal(i$strprint(), "([1,2] \u00D7 [3,4]) \u00D7 [5,6]")

  expect_equal((Interval$new(1, class = "integer") * Set$new(1,2))$properties$cardinality, "Aleph0")
})

test_that("conditionalset",{
  useUnicode(FALSE)
  expect_equal((ConditionalSet$new(function(x) x == 1) * ConditionalSet$new(function(y) y > 1))$strprint(),
               "{x == 1 : x in V} X {y > 1 : y in V}")
  useUnicode(TRUE)
})

test_that("fuzzy",{
  useUnicode(FALSE)
  expect_equal((FuzzySet$new(1,0.5) * ConditionalSet$new(function(x) TRUE))$strprint(),
               "{1} X {TRUE : x in V}")
  expect_true(setproduct(FuzzySet$new(1, 0.1), FuzzySet$new(2, 0.2), simplify = TRUE)$equals(Set$new(FuzzyTuple$new(1, 0.1, 2, 0.2))))
  expect_equal(setproduct(FuzzySet$new(1, 0.1), FuzzySet$new(2, 0.2), simplify = FALSE)$strprint(),
               "{1(0.1)} X {2(0.2)}")
  useUnicode(TRUE)
})
