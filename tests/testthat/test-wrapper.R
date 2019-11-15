library(testthat)

context("SetWrapper")

test_that("construction",{
  expect_error(SetWrapper$new(), "SetWrapper is an abstract")
})

test_that("accessors",{
  u = Set$new(1) + Interval$new(3, 4)
  expect_equal(u$class, "numeric")
})

test_that("equals",{
  u = Set$new(1) + Interval$new(3, 4)
  expect_true(u$equals(Set$new(1) + Interval$new(3, 4)))
  expect_false(u$equals(Set$new(2) + Interval$new(3, 4)))
  expect_false(u$equals(Set$new(1) * Interval$new(3, 4)))
})
