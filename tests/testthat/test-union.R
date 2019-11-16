library(testthat)

context("union")

test_that("subsets",{
  expect_equal(Set$new(1,2,3) + Set$new(1), Set$new(1,2,3))
  expect_equal(Set$new(1:3) + Set$new(1:4), Set$new(1:4))
})

test_that("special sets",{
  expect_equal(PosReals$new() + NegReals$new(), Reals$new())
  expect_equal(NegReals$new() + PosReals$new(), Reals$new())
  expect_equal(PosRationals$new() + NegRationals$new(), Rationals$new())
  expect_equal(NegRationals$new() + PosRationals$new(), Rationals$new())
  expect_equal(PosIntegers$new() + NegIntegers$new(), Integers$new())
  expect_equal(NegIntegers$new() + PosIntegers$new(), Integers$new())
  expect_equal(Reals$new() + Set$new(-Inf, Inf), ExtendedReals$new())
  use_unicode(TRUE)
  expect_equal((Reals$new() + Set$new("a"))$strprint(), "{a} \u222A ℝ")
  use_unicode(FALSE)
  expect_equal((Reals$new() + Set$new("a"))$strprint(), "{a} U R")
  use_unicode(TRUE)
})

test_that("set",{
  expect_equal(Set$new(-Inf, Inf) + Reals$new(), ExtendedReals$new())
  expect_true((Set$new(1,2) | Interval$new(3, 4, class = "integer"))$equals(Set$new(1:4)))
  expect_equal(getR6Class(Set$new(1,2) + Interval$new(3, 4)), "UnionSet")
  expect_message(expect_equal(Set$new(1,2) + ConditionalSet$new(function(x) TRUE), Set$new()), "Union of")
  expect_true(union(Set$new(1,2,3), Tuple$new("a", 1i))$equals(Set$new(1, 2, 3, 1i, "a")))
  expect_false(union(Tuple$new(1,2,3), Set$new("a", 1i))$equals(Tuple$new(1, 2, 3, 1i, "a")))
})

test_that("interval",{
  expect_equal(Interval$new(1, 5) + Interval$new(4, 10), Interval$new(1, 10))
  expect_equal(Interval$new(4, 10) + Interval$new(1, 5), Interval$new(1, 10))
})

test_that("fuzzy",{
  expect_equal(FuzzySet$new(1,0.1) + FuzzySet$new(2, 0.3), FuzzySet$new(1,0.1,2,0.3))
  expect_equal(FuzzySet$new(1,0.1) + Set$new(2), FuzzySet$new(1,0.1,2,1))
  expect_equal(FuzzyTuple$new(1,0.1) + FuzzyTuple$new(2, 0.3), FuzzyTuple$new(1,0.1,2,0.3))
  expect_equal(FuzzyTuple$new(1,0.1) + Set$new(2), FuzzyTuple$new(1,0.1,2,1))
})

test_that("conditional",{
  expect_equal(ConditionalSet$new(function(x) x == 0) + ConditionalSet$new(function(y) y > 0),
               ConditionalSet$new(function(x, y) x == 0 | y > 0))
  expect_message(expect_equal(ConditionalSet$new(function(x) x == 0) + Set$new(1), Set$new()),"Union of")
})

test_that("contains",{
  x = Interval$new(1,8) + Interval$new(5, 15, type = "()")
  expect_true(x$contains(c(1,4,6,15), all = TRUE, bound = TRUE))
  expect_false(x$contains(c(1,4,6,15), all = TRUE, bound = FALSE))
  expect_equal(x$contains(c(0,1,4,6,15,18), bound = TRUE), c(FALSE, TRUE, TRUE, TRUE, TRUE, FALSE))
  expect_true(x$contains(8))
})
