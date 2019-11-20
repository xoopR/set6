library(testthat)

context("powerset")

test_that("Set",{
  expect_equal(power_set(Set$new(1,2)),Set$new(Set$new(), Set$new(1),Set$new(2),Set$new(1,2)))
  expect_equal(getR6Class(power_set(Set$new(1,2),FALSE)), "Powerset")
  use_unicode(FALSE)
  expect_equal(power_set(Set$new(1,2), FALSE)$strprint(), "2^{1, 2}")
  use_unicode(TRUE)
})
