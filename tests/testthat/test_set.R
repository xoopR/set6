test_that("construction", {
  expect_silent(Set$new(1, 2, 3))
  expect_equal(Set$new(1, 2, 3), Set$new(1:3))
  expect_silent(Set$new(1, 2, 3, 1))
  expect_silent(Set$new("A", TRUE, as.factor("a")))
  expect_silent(Set$new(1 + 0i, 2, 2L, 5.67))
  expect_silent(Set$new(1:10))
  expect_silent(Set$new(list(a = 1)))
  expect_silent(Set$new(Set$new(2), Interval$new(1, 5)))
  expect_error(Set$new(1, 2, universe = c(1, 2, 3)))
  expect_silent(Set$new(1, 2, universe = Set$new(1, 2, 3)))
  expect_equal(Set$new(1, 2)$type, "{}")
})

test_that("elements", {
  expect_equal(Set$new(1, 2, 3)$elements, list(1, 2, 3))
  expect_equal(Set$new(1, 2, 3, 1, 2)$elements, list(1, 2, 3))
  # expect_equal(Set$new("A", TRUE, as.factor("a"))$elements, list(as.factor("a"), "A", TRUE))
  expect_equal(Set$new(1 + 0i, 2, 5.67)$elements, list(1 + 0i, 2, 5.67))
  expect_equal(Set$new(Set$new(2), Interval$new(1, 5))$elements, c(Interval$new(1, 5), Set$new(2)))
})

test_that("numerics", {
  expect_equal(Set$new(1)$lower, 1)
  expect_equal(Set$new(1)$min, NaN)
  expect_equal(Set$new(1)$upper, 1)
  expect_equal(Set$new(1)$max, NaN)
  expect_equal(Set$new(1)$range, NaN)
})

test_that("length", {
  expect_equal(Set$new(1, 2, 3)$length, 3)
  expect_equal(Set$new(1, 2, 3, 2, 1)$length, 3)
  expect_equal(Set$new(list(a = 1))$length, 1)
  expect_equal(Set$new(Set$new(2), Interval$new(1, 5))$length, 2)
})

test_that("contains", {
  x <- Set$new(1, 2, 3)
  y <- c(2, 3, 4)
  expect_equal(x$contains(y, all = F), c(TRUE, TRUE, FALSE))
  expect_false(x$contains(y, all = T))
  expect_true(x$contains(c(2, 3), all = T))
  expect_true(Set$new(1, 2, Set$new())$contains(c(2, Set$new()), all = TRUE))
})

test_that("equals", {
  expect_true(Set$new(1, 2, 3)$equals(Set$new(1, 2, 3)))
  expect_true(Set$new(1, 2, 3) == Set$new(1, 2, 3))
  expect_false(Set$new(1, 2, 3) != Set$new(1, 2, 3))
  expect_true(Set$new(1, 2, 3)$equals(Set$new(2, 1, 3)))
  expect_true(Set$new(1, 2, 3)$equals(Set$new(2, 1, 3, 1, 2, 2, 2)))
  expect_true(Set$new("a", Set$new(1), 3)$equals(Set$new(3, "a", Set$new(1))))
  expect_false(Set$new(1, 2, 3)$equals(Set$new(1, 2)))
  expect_false(Set$new(1, 2, 3) == Set$new(1, 2))
  expect_true(Set$new(1, 2, 3) != Set$new(1, 2))
  expect_false(Set$new(1, 2, 3)$equals(Set$new(1, 2, 3, 4)))
  expect_true(Set$new(1, 2, 3)$equals(FuzzySet$new(elements = 1:3)))
  expect_false(Set$new(1, 2, 3)$equals(FuzzySet$new(elements = 1:3, membership = 0.1)))
  expect_false(Set$new(1, 2, 3) == Interval$new(1, 2))
  expect_false(Set$new(1, 2, 3) == 1)
  expect_true(Set$new(1, 2, 3) == Interval$new(1, 3, class = "integer"))
  expect_false(Set$new(Tuple$new(1), Tuple$new(2, 3), Tuple$new(1, 4)) == Set$new(Tuple$new(1, 4)))
})

test_that("equals empty set", {
  a <- Set$new()
  b <- Set$new("a")
  a$equals(a)
  a$equals(b)
  b$equals(a)
})

test_that("strprint", {
  expect_equal(Set$new(1)$strprint(), "{1}")
  expect_equal(Set$new(1, "a")$strprint(), "{1, a}")
  expect_equal(Set$new(1, 2, 3)$strprint(n = 1), "{1,...,3}")
})

test_that("summary", {
  expect_output(Set$new(1, 2, 3)$summary())
  expect_output(Set$new(1)$summary(), "Singleton")
  expect_output(Set$new()$summary(), "Empty")
  expect_output(summary(Set$new(1, 2, 3)))
})

test_that("isSubset", {
  x <- Set$new(1, 2, 3)
  expect_false(x$isSubset(Set$new(1, 2, 3), proper = T))
  expect_false(x$isSubset(Set$new(1, 2, 3, 4), proper = T))
  expect_true(x$isSubset(Set$new(1, 2), proper = T))
  expect_true(x$isSubset(Set$new(1, 2, 3), proper = F))
  expect_true(x$isSubset(Set$new(1, 2), proper = F))
  expect_false(x$isSubset(Set$new(1, 2, 3, 4), proper = F))

  expect_false(Set$new(1, 2, 3) < Set$new(1, 2, 3))
  expect_true(Set$new(1, 2) < Set$new(1, 2, 3))
  expect_true(Set$new(1, 2, 3) <= Set$new(1, 2, 3))

  expect_false(Set$new(1, 2, 3) > Set$new(1, 2, 3))
  expect_true(Set$new(1, 2, 3, 4) > Set$new(1, 2, 3))
  expect_true(Set$new(1, 2, 3) >= Set$new(1, 2, 3))

  expect_equal(c(Set$new(1), Set$new(2, 4), Set$new(5), Set$new(1, 2, 3, 4)) < Set$new(1, 2, 3, 4), c(TRUE, TRUE, FALSE, FALSE))
  expect_equal(c(Set$new(1), Set$new(2, 4), Set$new(5), Set$new(1, 2, 3, 4)) <= Set$new(1, 2, 3, 4), c(TRUE, TRUE, FALSE, TRUE))
  expect_false(Set$new(1, 2, 3, 4)$isSubset(c(Set$new(1), Set$new(2, 4), Set$new(5)), all = TRUE))

  expect_false(Set$new(1, 2, 3)$isSubset(FuzzySet$new(elements = 1:3, membership = 0.1)))
  expect_true(Set$new(1, 2, 3)$isSubset(FuzzySet$new(elements = 1:3)))
  expect_false(Set$new(1, 2, 3)$isSubset(Interval$new(1, 2)))
  expect_false(Set$new(1, 2, 3)$isSubset(1))
  expect_true(Set$new(1, 2, 3)$isSubset(Interval$new(1, 3, class = "integer")))
})

test_that("as.Set", {
  expect_equal(as.Set(c(1, 2)), Set$new(1, 2, class = "numeric"))
  expect_equal(as.Set(list(1, 2)), list(Set$new(1), Set$new(2)))
  expect_equal(as.Set(matrix(c(1, 2, 3, 4), nrow = 2)), list(Set$new(1, 2), Set$new(3, 4)))
  expect_error(expect_equal(as.Set(Interval$new()), Interval$new()), "Interval cannot be")
  expect_error(expect_equal(as.Set(ConditionalSet$new(function(x) TRUE)), ConditionalSet$new(function(x) TRUE)), "ConditionalSet cannot be")
})

test_that("as.double", {
  expect_equal(as.numeric(Set$new(elements = 1:10, class = "numeric")), 1:10)
  expect_equal(as.numeric(FuzzySet$new(1, 0, 2, 0.1, 3, 1)), 2:3)
})

test_that("typed set", {
  expect_equal(Set$new(1.5, class = "integer")$elements, list(1L))
  expect_equal(Set$new(1.5, class = "integer")$class, "integer")
  expect_equal(Set$new(1:5, class = "numeric")$lower, 1)
  expect_equal(Set$new(1:5, class = "numeric")$min, 1)
  expect_equal(Set$new(1:5, class = "numeric")$upper, 5)
  expect_equal(Set$new(1:5, class = "numeric")$max, 5)
  expect_equal(Set$new(1:2, class = "complex")$upper, 2 + 0i)
  expect_equal(Set$new(1:2, class = "complex")$lower, 1 + 0i)
})

test_that("add", {
  expect_equal(Set$new()$add(1), Set$new(1))
  expect_equal(Set$new(1, 2, 3)$add(4), Set$new(1, 2, 3, 4))
  expect_equal(Set$new(1, 2, 3)$add(4, 5), Set$new(1, 2, 3, 4, 5))
  expect_equal(Set$new(1, 2, class = "complex")$add(3), Set$new(1:3, class = "complex"))
  expect_equal(Set$new(1, 2, 3)$add(Interval$new(5, 10)), Set$new(1, 2, 3, Interval$new(5, 10)))
  expect_silent(Set$new(1, 2, 3, universe = Interval$new(1, 4))$add(4))
  expect_error(Set$new(1, 2, 3, universe = Interval$new(1, 4))$add(5), "some added")
  expect_equal(Set$new(1, 2)$add(3)$elements, list(1, 2, 3))
  expect_equal(Set$new(2, class = "numeric")$add(1)$lower, 1)
  expect_equal(Set$new(2, class = "numeric")$add(3)$upper, 3)
  expect_equal(Set$new(2)$add(1)$properties$cardinality, 2)
  expect_equal(Interval$new(1, 2, type = "()")$add(2)$type, "(]")
})

test_that("remove", {
  expect_equal(Set$new(1, 2)$remove(2), Set$new(1))
  expect_equal(Set$new(1, 2)$remove(1, 2), Set$new())
  expect_equal(Interval$new(1, 5)$remove(5), Interval$new(1, 5, type = "[)"))
  expect_equal(Interval$new(1, 5)$remove(2), Interval$new(1, 2, type = "[)") + Interval$new(2, 5, type = "(]"))
  expect_equal(Set$new(1, 2)$remove(2)$elements, list(1))
  expect_equal(Set$new(1, 2, 3, class = "numeric")$remove(1)$lower, 2)
  expect_equal(Set$new(1, 2, 3, class = "numeric")$remove(3)$upper, 2)
  expect_equal(Set$new(1, 2)$remove(1)$properties$cardinality, 1)
  expect_equal(Interval$new(1, 2, type = "[]")$remove(2)$type, "[)")
})
