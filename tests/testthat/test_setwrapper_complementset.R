library(testthat)

context("ComplementSet")

test_that("SpecialSets",{
  useUnicode(FALSE)
  x = Reals$new() - Naturals$new()
  expect_equal(x$strprint(), "R \\ N0")
  expect_true(x$contains(1.1))
  expect_false(x$contains(1))
  expect_equal(x$elements, NaN)
  expect_equal(x$length, Inf)
  expect_equal(x$addedSet, Reals$new())
  expect_equal(x$subtractedSet, Naturals$new())
  useUnicode(TRUE)
})

test_that("constructor",{
  expect_silent(ComplementSet$new(Set$new(1:10), Set$new(5, 7)))
  expect_equal(ComplementSet$new(Interval$new(1, 3), Interval$new(1, 2))$lower, 1 + .Machine$double.xmin)
  expect_equal(ComplementSet$new(Interval$new(1, 3, class = "integer"), Interval$new(1, 2))$lower, 2)
  expect_equal(ComplementSet$new(Interval$new(1, 3), Interval$new(2, 3))$upper, 3 - .Machine$double.xmin)
  expect_equal(ComplementSet$new(Interval$new(1, 3, class = "integer"), Interval$new(2,3))$upper, 2)
  expect_equal(ComplementSet$new(Interval$new(1, 3, class = "integer"), Interval$new(1, 2))$upper, 3)
  expect_equal(ComplementSet$new(Interval$new(1, 3, class = "integer"), Interval$new(2, 3))$lower, 1)
  # expect_equal(ComplementSet$new(Set$new(1, 3), Set$new(1))$lower, 3)
  # expect_equal(ComplementSet$new(Set$new(1, 2, 3), Set$new(1,2))$lower, 3)
  # expect_equal(ComplementSet$new(Set$new(1, 3), Set$new(3))$upper, 1)
  # expect_equal(ComplementSet$new(Set$new(1, 2, 3), Set$new(2,3))$upper, 1)
})

test_that("fields",{
  d = ComplementSet$new(Set$new(1:10), Set$new(5, 10))
  expect_equal(d$length, 8)
  # expect_equal(d$lower, 1)
  # expect_equal(d$upper, 9)
  expect_equal(d$elements, as.list(c(1:4, 6:9)))
  expect_equal(ComplementSet$new(Set$new(1:10), Interval$new(5, 10))$elements, NaN)
  expect_equal(d$type, "{}")
})

test_that("strprint",{
  d = ComplementSet$new(Set$new(1,2,3,4,5) + Interval$new(5,7), Set$new(5, 10) + Interval$new(10,15))
  expect_equal(d$strprint(n = 1), "({1,...,5} ∪ [5, 7]) \\ ({5, 10} ∪ [10, 15])")
})
