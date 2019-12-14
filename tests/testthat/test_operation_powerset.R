library(testthat)

context("powerset")

test_that("Set",{
  expect_equal(powerset(Set$new()), Set$new(Set$new()))
  expect_equal(powerset(Set$new(1,2), simplify = TRUE),Set$new(Set$new(), Set$new(1),Set$new(2),Set$new(1,2)))
  expect_equal(powerset(Set$new(1,Set$new(2)), simplify = TRUE),Set$new(Set$new(), Set$new(1),Set$new(Set$new(2)),Set$new(1,Set$new(2))))
})

test_that("Tuple",{
  expect_equal(powerset(Tuple$new(1,2), simplify = TRUE),Set$new(Set$new(), Set$new(1),Set$new(2),Set$new(1,2)))
  expect_equal(powerset(Tuple$new(1,Set$new(2)), simplify = TRUE),Set$new(Set$new(), Set$new(1),Set$new(Set$new(2)),Set$new(1,Set$new(2))))
})

test_that("FuzzySet",{
  expect_equal(powerset(FuzzySet$new(1,0.1,2,0.2), simplify = TRUE),
               Set$new(Set$new(), FuzzySet$new(1,0.1),FuzzySet$new(2,0.2), FuzzySet$new(1,0.1,2,0.2)))
  expect_true(powerset(FuzzySet$new(1,0.1,"a",0.2), simplify = TRUE)$equals(
    Set$new(Set$new(),FuzzySet$new(1,0.1),FuzzySet$new("a",0.2), FuzzySet$new(1,0.1,"a",0.2))))
  expect_equal(powerset(FuzzySet$new(1,0.1,Set$new(1),0.2), simplify = TRUE),
               Set$new(Set$new(), FuzzySet$new(1,0.1), FuzzySet$new(Set$new(1),0.2), FuzzySet$new(1,0.1,Set$new(1),0.2)))
})

test_that("FuzzyTuple",{
  expect_equal(powerset(FuzzyTuple$new(1,0.1,2,0.2), simplify = TRUE),
               Set$new(Set$new(), FuzzyTuple$new(1,0.1),FuzzyTuple$new(2,0.2), FuzzyTuple$new(1,0.1,2,0.2)))
  expect_equal(powerset(FuzzyTuple$new(1,0.1,Set$new(1),0.2), simplify = TRUE),
               Set$new(Set$new(), FuzzyTuple$new(1,0.1), FuzzyTuple$new(Set$new(1),0.2), FuzzyTuple$new(1,0.1,Set$new(1),0.2)))
})


