library(testthat)

context("Set")

test_that("construction",{
  expect_silent(Set$new(1,2,3))
  expect_silent(Set$new(1,2,3,1))
  expect_silent(Set$new("A",TRUE,function(x) x^2, as.factor("a")))
  expect_silent(Set$new(1+0i,2,2L,5.67))
  expect_silent(Set$new(1:10))
  expect_silent(Set$new(list(a=1)))
  expect_silent(Set$new(Set$new(2), Interval$new(1,5)))
  expect_error(Set$new(1,2,universe = c(1,2,3)))
  expect_silent(Set$new(1,2,universe = Set$new(1,2,3)))
})

test_that("inherited_methods",{
  expect_equal(Set$new(1,2)$type,"{}")
  expect_equal(Set$new(1,2)$max,2)
  expect_equal(Set$new(1,2)$min,1)
  expect_equal(Set$new(1,2)$upper,2)
  expect_equal(Set$new(1,2)$lower,1)
  expect_equal(Set$new("a",2)$max, numeric(0))
  expect_equal(Set$new("a",2)$min, numeric(0))
  expect_equal(Set$new("a",2)$upper, numeric(0))
  expect_equal(Set$new("a",2)$lower, numeric(0))
  expect_equal(Set$new(1,2)$class, "numeric")
  expect_equal(Set$new(Set$new(1),Set$new())$class, "Set")
  expect_equal(Set$new(1, "a")$class, "multiple")
  expect_equal(Set$new(1,2,3)$range,2)
  expect_equal(Set$new(1,"a")$range, numeric(0))
  expect_silent(Set$new(1,"a")$complement())
  expect_equal(Set$new(1,2,universe = Set$new(1,2,3))$complement(),Set$new(3))
})

test_that("elements",{
  expect_equal(Set$new(1,2,3)$elements, 1:3)
  expect_equal(Set$new(1,2,3,1,2)$elements, 1:3)
  expect_equal(Set$new("A",TRUE,function(x) x^2, as.factor("a"))$elements, list("A",TRUE,function(x) x^2, as.factor("a")))
  expect_equal(Set$new(1+0i,2,2L,5.67)$elements,list(1+0i,2,2L,5.67))
  expect_equal(Set$new(1:10)$elements, 1:10)
  expect_equal(Set$new(list(1))$elements, 1)
  expect_equal(Set$new(Set$new(2), Interval$new(1,5))$elements, c(Set$new(2),Interval$new(1,5)))
})

test_that("length",{
  expect_equal(Set$new(1,2,3)$length, 3)
  expect_equal(Set$new(1,2,3,2,1)$length, 3)
  expect_equal(Set$new(list(a=1))$length, 1)
  expect_equal(Set$new(Set$new(2), Interval$new(1,5))$length, 2)
})

test_that("contains",{
  x <- Set$new(1,2,3)
  y <- c(2,3,4)
  expect_equal(x$contains(y, all = F), c(TRUE, TRUE, FALSE))
  expect_false(x$contains(y, all = T))
  expect_true(x$contains(c(2,3), all = T))
  expect_true(Set$new(1,2,Set$new())$contains(c(2,Set$new()), all = TRUE))
})

test_that("equals",{
  expect_true(Set$new(1,2,3)$equals(Set$new(1,2,3)))
  expect_true(Set$new(1,2,3) == Set$new(1,2,3))
  expect_false(Set$new(1,2,3) != Set$new(1,2,3))
  expect_true(Set$new(1,2,3)$equals(Set$new(2,1,3)))
  expect_true(Set$new(1,2,3)$equals(Set$new(2,1,3,1,2,2,2)))
  expect_true(Set$new("a",Set$new(1),3)$equals(Set$new(3, "a", Set$new(1))))
  expect_false(Set$new(1,2,3)$equals(Set$new(1,2)))
  expect_false(Set$new(1,2,3) == Set$new(1,2))
  expect_true(Set$new(1,2,3) != Set$new(1,2))
  expect_false(Set$new(1,2,3)$equals(Set$new(1,2,3,4)))
})

test_that("strprint",{
  expect_equal(Set$new(1)$strprint(),"{1}")
  expect_equal(Set$new(1,"a")$strprint(),"{1, a}")
  expect_equal(Set$new(1,2,3)$strprint(n = 1),"{1,...,3}")
})

test_that("powerset",{
  expect_equal(Set$new(1,2)$powerset(),Set$new(Set$new(), Set$new(1),Set$new(2),Set$new(1,2)))
})

test_that("isSubset",{
  x <- Set$new(1,2,3)
  expect_false(x$isSubset(Set$new(1,2,3), proper = T))
  expect_false(x$isSubset(Set$new(1,2,3,4), proper = T))
  expect_true(x$isSubset(Set$new(1,2), proper = T))
  expect_true(x$isSubset(Set$new(1,2,3), proper = F))
  expect_true(x$isSubset(Set$new(1,2), proper = F))
  expect_false(x$isSubset(Set$new(1,2,3,4), proper = F))

  expect_false(Set$new(1,2,3) < Set$new(1,2,3))
  expect_true(Set$new(1,2) < Set$new(1,2,3))
  expect_true(Set$new(1,2,3) <= Set$new(1,2,3))

  expect_false(Set$new(1,2,3) > Set$new(1,2,3))
  expect_true(Set$new(1,2,3,4) > Set$new(1,2,3))
  expect_true(Set$new(1,2,3) >= Set$new(1,2,3))
})

test_that("as.Set",{
  expect_equal(as.Set(c(1,2)), Set$new(1,2))
  expect_equal(as.Set(list(1,2)), Set$new(1,2))
  expect_equal(as.Set(matrix(c(1,2,3,4),nrow = 2)), list(Set$new(1,2),Set$new(3,4)))
})
