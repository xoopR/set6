library(testthat)

context("Interval")

test_that("construction",{
  expect_silent(Interval$new())
  expect_silent(Interval$new(lower = 1, upper = 3))
  expect_silent(Interval$new(type = "()"))
  expect_silent(Interval$new(type = "[]"))
  expect_silent(Interval$new(class = "integer"))
  expect_error(Interval$new(lower = 0, upper = -10))
  expect_error(Interval$new(type = "{}"))
  expect_error(Interval$new(class = "character"))
})

test_that("inherited_methods",{
  expect_equal(Interval$new(1,2)$type,"[]")
  expect_equal(Interval$new(1,2)$max,2)
  expect_equal(Interval$new(1,2)$min,1)
  expect_equal(Interval$new(1,2, type = "()")$max,2-1.1e-15)
  expect_equal(Interval$new(1,2, type = "()")$min,1+1.1e-15)
  expect_equal(Interval$new(1,2)$upper,2)
  expect_equal(Interval$new(1,2)$lower,1)
  expect_equal(Interval$new(1,2)$class, "numeric")
  expect_equal(Interval$new(1,3)$range,2)
  expect_silent(Interval$new(1,10)$complement())
  expect_equal(Interval$new(1,10,universe = Interval$new(1,15))$complement(),
               Interval$new(10,15, type = "(]"))
})

test_that("elements",{
  expect_equal(Interval$new(1,10,class="numeric")$elements, NaN)
  expect_equal(Interval$new(1, class = "integer")$elements, NaN)
  expect_equal(Interval$new(1,10,class="integer")$elements, 1:10)
})

test_that("length",{
  expect_equal(Interval$new(1,10,class="numeric")$length, Inf)
  expect_equal(Interval$new(1,Inf,class="integer")$length, Inf)
  expect_equal(Interval$new(1,10,class="integer")$length, 10)
})

test_that("equals",{
  expect_true(Interval$new(1,4)$equals(Interval$new(1,4)))
  expect_false(Interval$new(1,4, class = "numeric")$equals(Interval$new(1,4, class = "integer")))
  expect_false(Interval$new(1,4)$equals(Interval$new(1,4, type = "(]")))
  expect_false(Interval$new(1,4) != Interval$new(1,4))
})

test_that("strprint",{
  expect_equal(Interval$new()$strprint(),"[-\u221E, +\u221E]")
  expect_equal(Interval$new(1,10,type="(]")$strprint(),"(1, 10]")
  expect_equal(Interval$new(1,3)$strprint(),"[1, 3]")
})

test_that("liesInSetInterval",{
  x <- Interval$new(1,10,type="[]")
  y <- Interval$new(1,10,type="()")
  expect_equal(x$liesInSet(c(1,2.5,10,11)), c(TRUE, TRUE, TRUE, FALSE))
  expect_equal(y$liesInSet(c(1,2.5,10,11)), c(FALSE, TRUE, FALSE, FALSE))
  expect_equal(y$liesInSet(c(1,2.5,10,11), bound = TRUE), c(TRUE, TRUE, TRUE, FALSE))
  expect_false(x$liesInSet(c(1,2.5,10,11), all = T))
  expect_true(x$liesInSet(c(1.1,9.99), all = T))
})

