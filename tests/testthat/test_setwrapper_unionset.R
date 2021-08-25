library(testthat)

context("unionset")

test_that("constructor", {
  expect_silent(UnionSet$new(list(Set$new(1, 2, 3), Interval$new(4, 9))))
  expect_silent(UnionSet$new(list(Set$new(1, 2, 3), Set$new(elements = letters[1:5]))))
  expect_error(UnionSet$new(Set$new(1, 2, 3), Interval$new(4, 9)), "Assertion on")
})

test_that("as.character", {
  u <- UnionSet$new(list(Set$new(1, 2, 3), Set$new(elements = letters[1:5])))
  useUnicode(TRUE)
  expect_equal(as.character(u), "{1, 2, 3} \u222A {a, b,...,d, e}")
  useUnicode(FALSE)
  expect_equal(as.character(u), "{1, 2, 3} U {a, b,...,d, e}")
  expect_equal(
    as.character(UnionSet$new(list(
      Set$new(1) * Interval$new(1, 2),
      Set$new(elements = letters[1:2])
    ))),
    "({1} X [1,2]) U {a, b}"
  )
})

test_that("fields", {
  u <- UnionSet$new(list(Set$new(1, 2, 3), Set$new(elements = letters[1:5])))
  expect_equal(u$lower, "1")
  expect_equal(u$upper, "e")
  expect_equal(u$elements, c(1, 2, 3, letters[1:5]))
  expect_equal(UnionSet$new(list(Set$new(1), Set$new(2)))$elements, 1:2)
  expect_equal(
    UnionSet$new(list(Interval$new(1, 2), Set$new(elements = letters[1:5])))$elements,
    NA
  )
})

test_that("contains", {
  u <- UnionSet$new(list(Set$new(1, 2, 3), Set$new(elements = letters[1:5])))
  expect_true(u$contains(1, "a", all = TRUE))
  expect_equal(u$contains(c(1, "b", 6)), c(TRUE, TRUE, FALSE))
})

test_that("length", {
  expect_equal(UnionSet$new(list(Set$new(1, 2, 3), Set$new(elements = letters[1:5])))$length, 8)
  expect_equal(UnionSet$new(list(Set$new(1, 2, 3), Interval$new()))$length, Inf)
})

test_that("cardinality", {
  expect_equal(setunion(Set$new(1, 2) + Set$new(3, 4))$properties$cardinality, 4)
  expect_equal(setunion(Set$new(1, 2) + Set$new(2, 3, 4))$properties$cardinality, 4)
  expect_equal(UnionSet$new(list(Set$new(1, 2), Universal$new()))$properties$cardinality, Inf)
})
