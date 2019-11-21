library(testthat)

context("powerset")

test_that("Set",{
  expect_equal(powerset(Set$new(1,2), simplify = TRUE),Set$new(Set$new(), Set$new(1),Set$new(2),Set$new(1,2)))
  expect_equal(getR6Class(powerset(Set$new(1,2),FALSE)), "PowersetSet")
  expect_equal(powerset(Set$new(1,2), FALSE)$strprint(), paste0("\U2118(", "{1, 2}", ")"))
  use_unicode(FALSE)
  expect_equal(powerset(Set$new(1,2), FALSE)$strprint(), "2^{1, 2}")
  use_unicode(TRUE)
})

test_that("contains Set",{
  ps = powerset(Set$new(1,2,3,5))
  expect_true(ps$contains(Set$new(1,2)))
  expect_false(Set$new(2, 8) %inset% ps)
  expect_true(ps$contains(Set$new(1,2), Set$new(3,5), all = TRUE))
})

test_that("contains Interval",{
  ps = powerset(Interval$new(1, 5))
  expect_true(ps$contains(Interval$new(2, 4)))
  expect_false(ps$contains(Interval$new(2, 8)))
})

test_that("isSubset",{
  ps = powerset(Set$new(1,2,3,5))
  expect_true(ps <= ps)
  expect_false(ps < ps)
  expect_false(Set$new(1) < ps)
  expect_true(Set$new(Set$new(1)) < ps)
})

test_that("isSubset Interval",{
  ps = powerset(Interval$new(1, 5))
  expect_true(ps$isSubset(Set$new(Interval$new(2, 4))))
  expect_false(ps$isSubset(Set$new(Interval$new(2, 8))))
})
