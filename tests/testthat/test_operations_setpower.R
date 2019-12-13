library(testthat)

context("setpower")

test_that("Set",{
  expect_equal(setpower(Set$new(1, 2), 2, simplify = FALSE)$strprint(), "{1, 2}^2")
  expect_true(setpower(Set$new(1, 2), 2, simplify = TRUE)$equals(Set$new(Tuple$new(1,1),Tuple$new(2,1),Tuple$new(1,2),Tuple$new(2,2))))
  expect_equal(Set$new(1,2)^1, Set$new(1,2))
  })

test_that("conditionalset",{
  useUnicode(FALSE)
  expect_equal((ConditionalSet$new(function(x) x == 1)^2)$strprint(), "{x == 1 : x in V}^2")
  useUnicode(TRUE)
})

test_that("interval",{
  i = Interval$new(1,2)^3
  expect_equal(i$strprint(),"[1, 2]^3")
  expect_equal(i$power, 3)
})

test_that("exponent",{
  expect_equal((Interval$new(1,2)^2)^3, Interval$new(1,2)^6)
})

test_that("setwrapper",{
  useUnicode(FALSE)
  expect_equal(((Interval$new(1,2) + Interval$new(3,4))^2)$strprint(),
               "([1, 2] U [3, 4])^2")
  useUnicode(TRUE)
})

