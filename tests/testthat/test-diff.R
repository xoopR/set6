library(testthat)

context("difference")

test_that("subsets",{
  expect_equal(Set$new(1:3) - Set$new(1:4), Set$new())
  expect_equal(Set$new(1) - Reals$new(), Set$new())
})

test_that("special sets",{
  expect_equal(Reals$new() - PosReals$new(), NegReals$new())
  expect_equal(Reals$new() - NegReals$new(), PosReals$new())
  expect_equal(Rationals$new() - PosRationals$new(), NegRationals$new())
  expect_equal(Rationals$new() - NegRationals$new(), PosRationals$new())
  expect_equal(Integers$new() - PosIntegers$new(), NegIntegers$new())
  expect_equal(Integers$new() - NegIntegers$new(), PosIntegers$new())
})

test_that("set",{
  expect_equal(Set$new(1:5) - Set$new(3:5), Set$new(1:2))
  expect_equal(Set$new(1:5) - Set$new(6:10), Set$new(1:5))
})

test_that("interval",{
  expect_equal(Interval$new(1,10) - Interval$new(11, 20), Interval$new(1, 10))
  expect_equal(Interval$new(1,10) - Interval$new(5, 10), Interval$new(1, 5, type = "[)"))
})

test_that("fuzzy",{
  expect_equal(FuzzySet$new(1,0.1,2,0.2,3,0.3) - FuzzySet$new(3,0.3,4,0.4), FuzzySet$new(1,0.1,2,0.2))
})

test_that("conditional",{
  expect_equal(ConditionalSet$new(function(x) x == 0) - ConditionalSet$new(function(y) y > 0),
               ConditionalSet$new(function(x, y) x == 0 & !(y > 0)))
})

test_that("contains",{
  x = Interval$new(1,8) - Interval$new(5, 15, type = "()")
  expect_false(x$contains(0))
  expect_true(x$contains(1))
  expect_false(x$contains(6))
  expect_false(x$contains(9))
  expect_false(x$contains(16))
  expect_equal(x$contains(c(0,1,6,9,16)), c(FALSE, TRUE, FALSE, FALSE, FALSE))
})
