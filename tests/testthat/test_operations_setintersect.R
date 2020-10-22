library(testthat)

context("setintersect")

test_that("SetXSet", {
  expect_true(setintersect(Set$new(1, 2, 3), Set$new(3:5)) == (Set$new(3)))
  expect_equal(Set$new(1) & Set$new(), Set$new())
  expect_equal(Set$new(1, 2, 3) & Set$new(1), Set$new(1))
  expect_equal(Set$new(1) & Set$new(1, 2, 3), Set$new(1))
  expect_equal(Tuple$new(1, "a", 2L) & Set$new(elements = letters), Set$new("a"))
  expect_equal(Set$new(1) & Set$new(1, 2), Set$new(1))
  expect_equal(Set$new() & Set$new(), Set$new())
})

test_that("conditionalset", {
  useUnicode(FALSE)
  expect_equal(
    (ConditionalSet$new(function(x) x == 1) & ConditionalSet$new(function(y) y > 1))$strprint(),
    "{x in V, y in V : x == 1 & y > 1}"
  )
  expect_equal(
    ConditionalSet$new(function(x) x == 1) & ConditionalSet$new(function(y) y == 1),
    ConditionalSet$new(function(x) x == 1)
  )
  expect_true((ConditionalSet$new(function(x) x == 0) &
                 Set$new(elements = -2:2))$equals(Set$new(0)))
  useUnicode(TRUE)
})

test_that("fuzzy", {
  expect_true((FuzzySet$new(1, 0.1, 2, 0.3) & Set$new(elements = 2:5)) == Set$new(2))
})

test_that("interval", {
  expect_equal(Interval$new(1, 5) & Interval$new(7, 10), Set$new())
  expect_equal(Interval$new(1, 5) & Interval$new(2, 10), Interval$new(2, 5))
  expect_equal(Interval$new(1, 7) & Interval$new(3, 10), Interval$new(3, 7))
  expect_equal(Interval$new(3, 10) & Interval$new(1, 7), Interval$new(3, 7))
})

test_that("mixed", {
  expect_equal(Set$new(elements = 1:2) & ConditionalSet$new(function(x) x == 3), Set$new())
  expect_equal(Set$new("a", 2) & Interval$new(1, 10), Set$new(2))
  expect_equal(Interval$new(1, 10) & Set$new("a", 2), Set$new(2))
  expect_equal(Interval$new() & Set$new(), Set$new())
  expect_equal(Interval$new(1, 5) & Interval$new(2, 3), Interval$new(2, 3))
  expect_equal(Interval$new(2, 3) & Interval$new(1, 5), Interval$new(2, 3))
})

test_that("UnionSet", {
  expect_equal(
    setintersect(
      UnionSet$new(list(Set$new(1, 2, 5), Set$new(2, "a", Tuple$new(2)))),
      Set$new(1, "a", Tuple$new(2), 7)
    ),
    Set$new(1, "a", Tuple$new(2))
  )
  expect_true(setintersect(
    UnionSet$new(list(Set$new(1, 2, 5), Set$new(4, 3))),
    UnionSet$new(list(Set$new(1, 2, 5), Set$new(4, 3)))
  )$equals(Set$new(1:5)))
})

test_that("ComplementSet", {
  expect_equal(
    ComplementSet$new(Reals$new(), Integers$new()) & Set$new(1.1, 2, 4.5, "a"),
    Set$new(1.1, 4.5)
  )
  expect_equal(ComplementSet$new(Reals$new(), Integers$new()) &
    ComplementSet$new(Set$new(1.1, 2.3), Set$new(2)), Set$new(1.1, 2.3))
  expect_true(setintersect(
    ComplementSet$new(Set$new(1, 2, 5), Set$new(4, 3)),
    UnionSet$new(list(Set$new(1), Set$new(2, 5)))
  )$equals(Set$new(1, 2, 5)))
})

test_that("ProductSet", {
  expect_equal(
    setproduct(Set$new(1, 2), Set$new(3, 4), simplify = TRUE) & Set$new(Tuple$new(1, 4)),
    Set$new(Tuple$new(1, 4))
  )
  expect_equal(
    (Set$new(1, 2) * Set$new(3, 4)) & Set$new(Tuple$new(1, 4), Tuple$new(5, 6)),
    Set$new(Tuple$new(1, 4))
  )
  expect_equal((Set$new(1, 2) * Set$new(3, 4)) & Tuple$new(1, 4), Set$new())
  expect_message((Set$new(1, 2) * Set$new(3, 4)) & (Set$new(1, 2) * Set$new(3, 4)), "currently not implemented")
})
