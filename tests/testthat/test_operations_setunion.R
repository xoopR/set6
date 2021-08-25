library(testthat)

context("setunion")

test_that("subsets", {
  expect_equal(Set$new(1, 2, 3) + Set$new(1), Set$new(1, 2, 3))
  expect_equal(Set$new(elements = 1:3) + Set$new(elements = 1:4), Set$new(elements = 1:4))
})

test_that("special sets", {
  expect_equal(PosReals$new() + NegReals$new(), Reals$new())
  expect_equal(NegReals$new() + PosReals$new(), Reals$new())
  expect_equal(PosRationals$new() + NegRationals$new(), Rationals$new())
  expect_equal(NegRationals$new() + PosRationals$new(), Rationals$new())
  expect_equal(PosIntegers$new() + NegIntegers$new(), Integers$new())
  expect_equal(NegIntegers$new() + PosIntegers$new(), Integers$new())
  useUnicode(FALSE)
  expect_equal((Reals$new() + Set$new("a"))$strprint(), "{a} U R")
  useUnicode(TRUE)
})

test_that("set", {
  expect_true((Set$new(-Inf, Inf) + Reals$new())$equals(ExtendedReals$new()))
  expect_true((Set$new(1, 2) + Set$new(1, 2))$equals(Set$new(1, 2)))
  expect_true((Set$new(1, 2) | Interval$new(3, 4, class = "integer"))$equals(Set$new(1:4)))
  expect_equal(object_class(Set$new(1, 2) + Interval$new(3, 4)), "UnionSet")
  useUnicode(FALSE)
  expect_equal((Set$new(1, 2) + ConditionalSet$new(function(x) TRUE))$strprint(), "{1, 2} U {x in V}")
  expect_true(setunion(Set$new(1, 2, 3), Tuple$new("a", 1i))$equals(Set$new(1, 2, 3, 1i, "a")))
  expect_true(setunion(Set$new(1, 2, 3), Multiset$new("a", 1i))$equals(Set$new(1, 2, 3, 1i, "a")))
  expect_true(setunion(Tuple$new(1, 2, 3), Multiset$new("a", 1i))$equals(Set$new(1, 2, 3, 1i, "a")))
  expect_equal(Set$new(1, 2) + Set$new(5, 7) + Set$new(1, 10), Set$new(1, 2, 5, 7, 10))
  expect_equal(setunion(Set$new()), Set$new())
  expect_equal(setunion(), Set$new())
  expect_true((Set$new(1, 2, 3) + Set$new(4, 5))$equals(Set$new(1:5)))
  expect_true(setunion(Set$new(1, 2, 3), Set$new(4, 5))$equals(Set$new(1:5)))
  expect_equal(setunion(Set$new(1, 2, 3), Set$new(4, 5), simplify = FALSE)$strprint(), "{1, 2, 3} U {4, 5}")
  useUnicode(TRUE)
})

test_that("interval", {
  expect_equal(Interval$new(1, 5) + Interval$new(4, 10), Interval$new(1, 10))
  expect_equal(Interval$new(4, 10) + Interval$new(1, 5), Interval$new(1, 10))
  expect_equal(Interval$new(1, 5, type = "()") + Interval$new(5, 5), Interval$new(1, 5, type = "(]"))
})

test_that("fuzzy", {
  expect_equal(FuzzySet$new(1, 0.1) + FuzzySet$new(2, 0.3), FuzzySet$new(1, 0.1, 2, 0.3))
  expect_equal(FuzzySet$new(1, 0.1) + Set$new(2), Set$new(1, 2))
  expect_equal(FuzzyTuple$new(1, 0.1) + FuzzyTuple$new(2, 0.3), FuzzyTuple$new(1, 0.1, 2, 0.3))
  expect_equal(FuzzyMultiset$new(1, 0.1) + FuzzyMultiset$new(2, 0.3), FuzzyMultiset$new(1, 0.1, 2, 0.3))
  expect_equal(FuzzyTuple$new(1, 0.1) + Set$new(2), Set$new(1, 2))
  expect_equal(FuzzyMultiset$new(1, 0.1) + Set$new(2), Set$new(1, 2))
  expect_equal(FuzzyTuple$new(1, 0.1) + Tuple$new(2), Tuple$new(1, 2))
  expect_equal(FuzzyMultiset$new(1, 0.1) + Multiset$new(2), Multiset$new(1, 2))
})

test_that("conditional", {
  useUnicode(FALSE)
  expect_equal(
    (ConditionalSet$new(function(x) x == 0) + ConditionalSet$new(function(y) y > 0))$strprint(),
    "{x in V, y in V : x == 0 | y > 0}"
  )
  expect_equal(
    setunion(
      ConditionalSet$new(function(x) x == 0), ConditionalSet$new(function(y) y > 0),
      ConditionalSet$new(function(z) z == 2)
    )$strprint(),
    "{x in V, y in V, z in V : x == 0 | y > 0 | z == 2}"
  )
  expect_equal(
    (ConditionalSet$new(function(x) x == 0) + ConditionalSet$new(function(y) y > 0) +
      ConditionalSet$new(function(z) z == 2))$strprint(),
    "{x in V, y in V, z in V : x == 0 | y > 0 | z == 2}"
  )
  useUnicode(TRUE)
})

test_that("contains", {
  x <- Interval$new(1, 8) + Interval$new(5, 15, type = "()")
  expect_true(x$contains(c(1, 4, 6, 15), all = TRUE, bound = TRUE))
  expect_false(x$contains(c(1, 4, 6, 15), all = TRUE, bound = FALSE))
  expect_equal(x$contains(c(0, 1, 4, 6, 15, 18), bound = TRUE), c(FALSE, TRUE, TRUE, TRUE, TRUE, FALSE))
  expect_true(x$contains(8))
})
