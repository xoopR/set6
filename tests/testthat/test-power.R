library(testthat)

context("power")

test_that("Set",{
  expect_equal(power(Set$new(1, 2), 2, simplify = FALSE)$strprint(), "{1, 2}^2")
  expect_true((Set$new(1,2)^2)$equals(Set$new(Tuple$new(1,1),Tuple$new(2,1),Tuple$new(1,2),Tuple$new(2,2))))
  expect_equal(intersect(Set$new(1,2,3), Set$new(3:5)), Set$new(3L))
  expect_equal(Set$new(1) & Set$new(), Set$new())
  expect_equal(Set$new(1,2,3) & Set$new(1), Set$new(1))
  expect_equal(Tuple$new(1, "a", 2L) & Set$new(letters), Set$new("a"))
})

test_that("conditionalset",{
  expect_true((ConditionalSet$new(function(x) x == 1)^2)$equals(ConditionalSet$new(function(x) x == 1)))
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
  use_unicode(FALSE)
  expect_equal(((Interval$new(1,2) + Interval$new(3,4))^2)$strprint(),
               "([1, 2] U [3, 4])^2")
  use_unicode(TRUE)
})

