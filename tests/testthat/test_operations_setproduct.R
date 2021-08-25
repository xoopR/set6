test_that("redundant", {
  expect_equal(setproduct(), Set$new())
  expect_equal(setproduct(Set$new(1)), Set$new(1))
})
test_that("Set", {
  useUnicode(FALSE)
  expect_equal(setproduct(Set$new(1), Set$new(2))$strprint(), "{1} X {2}")
  expect_true(setproduct(Set$new(1), Set$new(2), simplify = TRUE)$equals(Set$new(Tuple$new(1, 2))))
  expect_true(setproduct(Set$new(1, 2), Set$new(2), simplify = TRUE)$equals(Set$new(Tuple$new(1, 2), Tuple$new(2, 2))))
  expect_equal(
    setproduct(Set$new(1, 2), Set$new(2), simplify = FALSE)$strprint(),
    "{1, 2} X {2}"
  )
  expect_equal(
    setproduct(Set$new(1, 2) * Set$new(3, 4), Set$new(5, 6), simplify = T, nest = T)$strprint(),
    "({1, 2} X {3, 4}) X {5, 6}"
  )
  expect_equal(
    setproduct(Set$new(1, 2), Set$new(3, 4), Set$new(5, 6), simplify = T, nest = T)$strprint(),
    "{((1, 3), 5), ((1, 3), 6),...,((2, 4), 5), ((2, 4), 6)}"
  )
  useUnicode(TRUE)
})

test_that("Interval", {
  useUnicode(FALSE)
  i <- Interval$new(1, 2) * Interval$new(3, 4)
  expect_equal(i$strprint(), "[1,2] X [3,4]")
  expect_true(i$lower$equals(Tuple$new(1, 3)))
  expect_true(i$upper$equals(Tuple$new(2, 4)))
  expect_equal(i$contains(list(Tuple$new(1, 4), Tuple$new(1, 3), Tuple$new(3, 1))), c(TRUE, TRUE, FALSE))
  expect_equal(
    (Interval$new(1, 2) * ConditionalSet$new(function(x) TRUE))$strprint(),
    "[1,2] X {x in V}"
  )
  expect_equal(Interval$new(1, 2) * Set$new(), Interval$new(1, 2))
  useUnicode(TRUE)
})

test_that("conditionalset", {
  useUnicode(FALSE)
  expect_equal(
    (ConditionalSet$new(function(x) x == 1) * ConditionalSet$new(function(y) y > 1))$strprint(),
    "{x in V : x == 1} X {y in V : y > 1}"
  )
  useUnicode(TRUE)
})

test_that("fuzzy", {
  useUnicode(FALSE)
  expect_equal(FuzzySet$new() * FuzzySet$new(), Set$new())
  expect_equal(FuzzySet$new(1, 0.1) * FuzzySet$new(), FuzzySet$new(1, 0.1))
  expect_equal(FuzzySet$new() * FuzzySet$new(1, 0.2), FuzzySet$new(1, 0.2))
  expect_equal(
    (FuzzySet$new(1, 0.5) * ConditionalSet$new(function(x) TRUE))$strprint(),
    "{1(0.5)} X {x in V}"
  )
  expect_true(setproduct(FuzzySet$new(1, 0.1), FuzzySet$new(2, 0.2), simplify = TRUE)$equals(Set$new(FuzzyTuple$new(1, 0.1, 2, 0.2))))
  expect_equal(
    setproduct(FuzzySet$new(1, 0.1), FuzzySet$new(2, 0.2), simplify = FALSE)$strprint(),
    "{1(0.1)} X {2(0.2)}"
  )
  useUnicode(TRUE)
})

test_that("productSet", {
  useUnicode(FALSE)
  i <- (Interval$new(1, 2) * Interval$new(3, 4)) * Interval$new(5, 6)
  expect_equal(i$strprint(), "[1,2] X [3,4] X [5,6]")
  i <- (Interval$new(5, 6) * Interval$new(1, 2) * Interval$new(3, 4))
  expect_equal(i$strprint(), "[5,6] X [1,2] X [3,4]")
  i <- setproduct(Interval$new(1, 2) * Interval$new(3, 4), Interval$new(5, 6), nest = TRUE)
  expect_equal(i$strprint(), "([1,2] X [3,4]) X [5,6]")
  expect_equal(object_class(Interval$new(1, 2) * Interval$new(1, 2)), "ExponentSet")
  expect_true(i$contains(Tuple$new(Tuple$new(1, 3), 5)))
  expect_false(i$contains(Tuple$new(1, 3, 5)))
  expect_true(i$contains(c(Tuple$new(Tuple$new(1, 3), 5), Tuple$new(Tuple$new(2, 3), 6)), all = TRUE))
  useUnicode(TRUE)
  expect_equal(i$strprint(), "([1,2] \u00D7 [3,4]) \u00D7 [5,6]")
})
