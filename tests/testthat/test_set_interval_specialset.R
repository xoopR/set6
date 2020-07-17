library(testthat)

context("Special sets")

test_that("test abstract", {
  expect_error(SpecialSet$new())
})

test_that("special constructors", {
  expect_silent(Complex$new())
  expect_silent(Integers$new())
  expect_silent(Naturals$new())
  expect_silent(NegIntegers$new(zero = F))
  expect_silent(NegIntegers$new(zero = T))
  expect_silent(PosIntegers$new(zero = T))
  expect_silent(PosIntegers$new(zero = F))
  expect_silent(PosNaturals$new())
  expect_silent(Rationals$new())
  expect_silent(Reals$new())
  expect_silent(ExtendedReals$new())
  expect_silent(NegRationals$new(zero = F))
  expect_silent(NegRationals$new(zero = T))
  expect_silent(NegReals$new(zero = T))
  expect_silent(NegReals$new(zero = F))
  expect_silent(PosRationals$new(zero = T))
  expect_silent(PosRationals$new(zero = F))
  expect_silent(PosReals$new(zero = T))
  expect_silent(PosReals$new(zero = F))
})

test_that("complex contains", {
  c <- Complex$new()
  expect_false(c$contains(1))
  expect_true(c$contains(1i))
  expect_true(c$contains(c(1i, 2i), all = TRUE))
  expect_false(c$contains(list(1i, 2), all = TRUE))
  expect_equal(c$contains(list(1, 2i)), c(FALSE, TRUE))
})
