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
  expect_false((Set$new(1,2)*Interval$new(1,2))$equals((Set$new(1,2)*Interval$new(1,2))*Interval$new(5,6)))
  expect_false(u$equals(Set$new(1)*Interval$new(2)))
})

test_that("subset/complement",{
  u = Set$new(1) + Interval$new(3, 4)
  expect_message(u$isSubset(Set$new(1)))
  expect_message(u$absComplement())
})
