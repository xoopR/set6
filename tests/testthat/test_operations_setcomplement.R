test_that("subsets", {
  expect_equal(Set$new(elements = 1:3) - Set$new(elements = 1:4), Set$new())
  expect_equal(Set$new(1) - Reals$new(), Set$new())
  expect_equal(Set$new(1) - Set$new(), Set$new(1))
})

test_that("special sets", {
  expect_equal(Reals$new() - PosReals$new(), NegReals$new())
  expect_equal(Reals$new() - NegReals$new(), PosReals$new())
  expect_equal(Rationals$new() - PosRationals$new(), NegRationals$new())
  expect_equal(Rationals$new() - NegRationals$new(), PosRationals$new())
  expect_equal(Rationals$new() - Interval$new(5, 10), Interval$new(-Inf, 5, type = "()") +
    Interval$new(10, Inf, type = "()"))
  expect_equal(Integers$new() - PosIntegers$new(), NegIntegers$new())
  expect_equal(Integers$new() - NegIntegers$new(), PosIntegers$new())
  expect_equal(Integers$new() - Interval$new(5, 10), Interval$new(-Inf, 5, type = "()", class = "integer") +
    Interval$new(10, Inf, type = "()", class = "integer"))
})

test_that("set", {
  expect_equal(Set$new(elements = 1:5) - Set$new(elements = 3:5), Set$new(elements = 1:2))
  expect_equal(Set$new(elements = 1:5) - Set$new(elements = 6:10), Set$new(elements = 1:5))
  expect_equal(Tuple$new(elements = 1:5) - Set$new(elements = 6:10), Tuple$new(elements = 1:5))
  expect_equal(Set$new(elements = 1:5) - Interval$new(4, 15), Set$new(elements = 1:3))
  expect_equal(Tuple$new(elements = 1:5) - Interval$new(4, 15), Tuple$new(elements = 1:3))
  expect_equal(setcomplement(Set$new(1, 2, universe = Set$new(1, 2, 3))), Set$new(3))
})

test_that("interval", {
  expect_equal(Interval$new(1, 10) - Interval$new(11, 20), Interval$new(1, 10))
  expect_equal(Interval$new(1, 10) - Set$new(1), Interval$new(1, 10, type = "(]"))
  expect_equal(Interval$new(1, 10) - Set$new(10), Interval$new(1, 10, type = "[)"))
  expect_equal(Interval$new(1, 10) - Interval$new(5, 10), Interval$new(1, 5, type = "[)"))
  expect_true((Interval$new(1, 10, class = "integer") - Set$new(elements = 5:15))$equals(Set$new(elements = 1:4)))
  expect_equal(
    Interval$new(1, 10) - Set$new(1, 3, 5),
    Interval$new(1, 3, type = "()") + Interval$new(3, 5, type = "()") +
      Interval$new(5, 10, type = "(]")
  )
  expect_equal(
    setcomplement(Interval$new(1, 10), Set$new(1, 3, 5),
      simplify = FALSE
    )$strprint(),
    "[1,10] \\ {1, 3, 5}"
  )
  expect_equal(
    Interval$new(1, 10) - Set$new(2, 3, 4),
    Interval$new(1, 2, type = "[)") + Interval$new(2, 3, type = "()") +
      Interval$new(3, 4, type = "()") + Interval$new(4, 10, type = "(]")
  )
  expect_equal(
    Interval$new(1, 10) - Interval$new(2, 4, class = "integer"),
    Interval$new(1, 2, type = "[)") + Interval$new(2, 3, type = "()") +
      Interval$new(3, 4, type = "()") + Interval$new(4, 10, type = "(]")
  )
  expect_equal(
    setcomplement(Interval$new(1, 10), Interval$new(2, 4, class = "integer"),
      simplify = FALSE
    )$strprint(),
    "[1,10] \\ {2,...,4}"
  )
  expect_equal(Interval$new(1, 5) - Set$new(3, 5), Interval$new(1, 3, type = "[)") + Interval$new(3, 5, type = "()"))
})

test_that("fuzzy", {
  expect_equal(FuzzySet$new(1, 0.1, 2, 0.2, 3, 0.3) - FuzzySet$new(3, 0.3, 4, 0.4), FuzzySet$new(1, 0.1, 2, 0.2))
  expect_equal(FuzzyTuple$new(1, 0.1, 2, 0.2, 3, 0.3) - FuzzyTuple$new(3, 0.3, 4, 0.4), FuzzyTuple$new(1, 0.1, 2, 0.2))
  expect_equal(Tuple$new(2) - FuzzyTuple$new(2, 0.1), Tuple$new())
  expect_equal(Tuple$new(2) - FuzzySet$new(1, 0.1), Tuple$new(2))
})

test_that("conditional", {
  useUnicode(FALSE)
  expect_equal(
    (ConditionalSet$new(function(x) x == 0) - ConditionalSet$new(function(y) y > 0))$strprint(),
    "{x in V, y in V : x == 0 & !(y > 0)}"
  )

  expect_equal(
    (ConditionalSet$new(function(x) TRUE) - Set$new(1))$strprint(),
    "{x in V : TRUE} \\ {1}"
  )
  useUnicode(TRUE)
})

test_that("wrappers", {
  expect_equal((Reals$new() - Integers$new()) - Set$new(1, 2), Reals$new() - Integers$new())
  useUnicode(FALSE)
  expect_equal(
    ((Reals$new() * Integers$new()) - Set$new(1, 2))$strprint(),
    "(R X Z) \\ {1, 2}"
  )
  useUnicode(TRUE)
})
