library(testthat)

context("listSpecialSets")

test_that("lists",{
  expect_silent(listSpecialSets(T))
  expect_silent(listSpecialSets(F))
})
