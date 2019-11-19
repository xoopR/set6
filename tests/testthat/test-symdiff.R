library(testthat)

context("symdiff")

test_that("subsets",{
  expect_equal(Set$new(1) %-% Reals$new(), Reals$new() - Set$new(1))
  expect_equal(Reals$new() %-% Set$new(1,2,3), Reals$new() - Set$new(1,2,3))
})

test_that("symdiff",{
  expect_equal(Interval$new(1,3,class="integer") %-% Interval$new(3,4,class="integer"),
               Set$new(1,2,4))
})
