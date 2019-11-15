library(testthat)

context("ConditionalSet")

test_that("construction",{
  expect_silent(ConditionalSet$new(function(x) x == 0))
  expect_error(ConditionalSet$new(function(x) x), "'condition' should result")
  expect_error(ConditionalSet$new(1), "'condition' must be")
  expect_silent(ConditionalSet$new(function(x, y) x + y == 0))
  expect_silent(ConditionalSet$new(function(x) TRUE, list(x = Reals$new())))
  expect_error(ConditionalSet$new(function(x) TRUE, list(x = Reals)))
})

test_that("contains",{
  c = ConditionalSet$new(function(x, y) x + y == 0)
  expect_true(c$contains(Set$new(2, -2)))
  expect_true(c$contains(Tuple$new(0, 0)))
  expect_false(c$contains(Set$new(1, 2)))
  expect_error(c$contains(Set$new(1)), "Set is of length")
  expect_equal(c$contains(list(Set$new(0, 1), Set$new(-1, 1))), c(FALSE, TRUE))
  expect_false(c$contains(list(Set$new(0, 1), Set$new(-1, 1)), all = TRUE))
  expect_true(c$contains(list(Set$new(2, -2), Set$new(-1, 1)), all = TRUE))
  expect_error(c$contains(list(Set$new(1), Set$new(1,1))))
  expect_true(ConditionalSet$new(function(x) x == 0)$contains(0))
  expect_false(ConditionalSet$new(function(x) x == 0)$contains(1))
})

test_that("equals",{
  c1 = ConditionalSet$new(function(x, y) x + y == 0)
  c2 = ConditionalSet$new(function(z, q) z + q == 0)
  c3 = ConditionalSet$new(function(x, y) x + y == 0)
  c4 = ConditionalSet$new(function(x, y) x == 0)
  c5 = ConditionalSet$new(function(x, y) x + y == 0, argclass = list(x = Complex$new()))
  expect_true(c1 == c2)
  expect_true(c1 == c3)
  expect_true(c1 != c4)
  expect_true(c1 != c5)
  expect_false(c1 == Set$new())
})

test_that("strprint",{
  expect_equal(ConditionalSet$new(function(x) TRUE)$strprint(), paste0("{TRUE : x", " \u03B5 ", setSymbol(Reals), "}"))
  use_unicode(FALSE)
  expect_equal(ConditionalSet$new(function(x) TRUE)$strprint(), paste0("{TRUE : x in ", setSymbol(Reals), "}"))
  use_unicode(TRUE)
})

test_that("summary",{
  expect_output(expect_equal(ConditionalSet$new(function(x) TRUE)$summary(), ConditionalSet$new(function(x) TRUE)$print()))
})

test_that("isSubset",{
  expect_false(ConditionalSet$new(function(x) TRUE)$isSubset(Set$new(1)))
  expect_false(ConditionalSet$new(function(x) TRUE)$isSubset(1))
})

test_that("fields",{
  c = ConditionalSet$new(function(x) TRUE)
  expect_equal(c$condition, function(x) TRUE)
  expect_equal(c$class, list(x = Reals$new()))
  expect_equal(c$elements, NaN)
})
