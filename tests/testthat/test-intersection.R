library(testthat)

context("intersection")

test_that("SetXSet",{
  expect_equal(intersection(Set$new(1,2,3), Set$new(3:5)), Set$new(3L))
  expect_equal(Set$new(1) & Set$new(), Set$new())
  expect_equal(Set$new(1,2,3) & Set$new(1), Set$new(1))
  expect_equal(Tuple$new(1, "a", 2L) & Set$new(letters), Set$new("a"))
  expect_equal(Set$new(1) & Set$new(1,2), Set$new(1))
})

test_that("conditionalset",{
  expect_equal(ConditionalSet$new(function(x) x == 1) & ConditionalSet$new(function(y) y > 1),
               ConditionalSet$new(function(x, y) x == 1 & y > 1))
  expect_equal(ConditionalSet$new(function(x) x == 1) & ConditionalSet$new(function(y) y == 1),
               ConditionalSet$new(function(x) x == 1))
  expect_true((ConditionalSet$new(function(x) x == 0) & Set$new(-2:2))$equals(Set$new(0)))
})

test_that("fuzzy",{
  expect_equal(FuzzySet$new(1,0.1,2,0.3) & Set$new(2:5), Set$new(2L))
})

test_that("interval",{
  expect_equal(Interval$new(1,5) & Interval$new(7, 10), Set$new())
  expect_equal(Interval$new(1,5) & Interval$new(2, 10), Interval$new(2, 5))
  expect_equal(Interval$new(1,7) & Interval$new(3, 10), Interval$new(3, 7))
  expect_equal(Interval$new(3, 10) & Interval$new(1,7), Interval$new(3, 7))
})

test_that("mixed",{
  expect_equal(Set$new(1:2) & ConditionalSet$new(function(x) TRUE), Set$new())
  expect_equal(Set$new("a",2) & Interval$new(1,10), Set$new(2))
  expect_equal(Interval$new(1,10) & Set$new("a",2), Set$new(2))
})
