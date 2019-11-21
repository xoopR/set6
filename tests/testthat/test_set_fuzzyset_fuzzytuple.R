library(testthat)

context("FuzzyTuple")

test_that("construction",{
  expect_error(FuzzyTuple$new(1,2,3))
  expect_warning(expect_error(FuzzyTuple$new(0.1,1,0.9,"a")))
  expect_silent(FuzzyTuple$new(1,0.1,"a",0.9))
  expect_silent(FuzzyTuple$new(1,0.1,"a",0.9))
  expect_silent(FuzzyTuple$new("A",0,TRUE,1,function(x){x^2},0.4,as.factor("a"), 0.6))
  expect_equal(FuzzyTuple$new(1,0.1,2,0.8),FuzzyTuple$new(elements = c(1,2),
                                                      membership = c(0.1,0.8)))
  expect_error(FuzzyTuple$new(1,0.1,2))
  expect_silent(expect_equal(FuzzyTuple$new(1,0.1,1,0.9),FuzzyTuple$new(1,0.1,1,0.9)))
})

test_that("inherited_methods",{
  expect_equal(FuzzyTuple$new(1,0.3)$type,"()")
  expect_equal(FuzzyTuple$new(1,0.4,2,0.9)$max,2)
  expect_equal(FuzzyTuple$new(1,0.2,2,0.8)$min,1)
  expect_equal(FuzzyTuple$new(1,0.2,2,0.1)$upper,2)
  expect_equal(FuzzyTuple$new(1,0,2,1)$lower,1)
  expect_equal(FuzzyTuple$new("a",0.1)$max,NaN)
  expect_equal(FuzzyTuple$new("a",0.1)$min,NaN)
  expect_equal(FuzzyTuple$new("a",0.1)$upper,NaN)
  expect_equal(FuzzyTuple$new("a",0.1)$lower,NaN)
  expect_equal(FuzzyTuple$new(1,0.3)$class, "numeric")
  expect_equal(FuzzyTuple$new(Set$new(1),0.1,Set$new(),0.7)$class, "Set")
  expect_equal(FuzzyTuple$new(1,0.1, "a",0.3)$class, "multiple")
  expect_equal(FuzzyTuple$new(1,0.1,2,0.3,3,0.9)$range,2)
  expect_equal(FuzzyTuple$new(1,0.2,"a",0.3)$range, numeric(0))
  expect_equal(FuzzyTuple$new(1,0.1,2,0.3,3,0.9)$elements,1:3)
  expect_equal(FuzzyTuple$new(1,0.1,2,0.3,3,0.9)$length,3)
  expect_true(FuzzyTuple$new(1,0.1,2,0.3,3,0.9)$contains(1))
  expect_false(FuzzyTuple$new(1,0.1,2,0.3,3,0.9)$contains(5))
})
f <- FuzzyTuple$new(elements = c(1,2,3), membership = c(0.1,0.2,0.3))
test_that("membership",{
  expect_equal(f$membership(), c(0.1,0.2,0.3))
  expect_equal(f$membership(1), 0.1)
  expect_message(expect_equal(f$membership(5), NA))
})
test_that("strprint",{
  expect_equal(f$strprint(),"(1(0.1), 2(0.2), 3(0.3))")
})
test_that("alphaCut",{
  expect_equal(f$alphaCut(0.15), 2:3)
  expect_equal(f$alphaCut(0.2, strong = FALSE), 2:3)
  expect_equal(f$alphaCut(0.2, strong = TRUE), 3)
  expect_true(f$alphaCut(0.2, strong = TRUE, create = TRUE)$equals(Tuple$new(3)))
  expect_equal(getR6Class(f$alphaCut(0.2, strong = TRUE, create = TRUE)), "Tuple")
})
test_that("support",{
  expect_equal(f$support(), 1:3)
  expect_equal(FuzzyTuple$new(1,0.2,3,0.1,2,0)$support(), c(1,3))
  expect_null(FuzzyTuple$new()$support())
  expect_equal(FuzzyTuple$new()$support(T),Set$new())
})
test_that("core",{
  expect_null(f$core())
  expect_equal(f$core(T), Set$new())
  expect_equal(FuzzyTuple$new(1,1,2,0.3,3,1,4.2,0.1)$core(), c(1,3))
})
test_that("inclusion",{
  expect_equal(FuzzyTuple$new(1,0,2,0.4,3,1)$inclusion(1),"Not Included")
  expect_equal(FuzzyTuple$new(1,0,2,0.4,3,1)$inclusion(4),"Not Included")
  expect_equal(FuzzyTuple$new(1,0,2,0.4,3,1)$inclusion(2),"Partially Included")
  expect_equal(FuzzyTuple$new(1,0,2,0.4,3,1)$inclusion(3),"Fully Included")
})
test_that("equals",{
  expect_true(FuzzyTuple$new(1,0.1,2,0.1,3,0.1)$equals(FuzzyTuple$new(elements = 1:3, membership = rep(0.1,3))))
  expect_false(FuzzyTuple$new(1,0.1,2,0.2,3,0.1)$equals(FuzzyTuple$new(elements = 1:3, membership = rep(0.1,3))))
  expect_false(FuzzyTuple$new(1,0.1,2,0.1,3,0.1,4,0.1)$equals(FuzzyTuple$new(elements = 1:3, membership = rep(0.1,3))))
  expect_true(FuzzyTuple$new(1,0.1,2,0.1) != FuzzyTuple$new(2,0.1,1,0.1))
  expect_true(FuzzyTuple$new(2,0.1,2,0.1) != FuzzyTuple$new(2,0.1))
  expect_true(FuzzyTuple$new(elements = 1:3)$equals(Set$new(1:3)))
  expect_false(FuzzyTuple$new(1, 0.3, 2, 0.5)$equals(Interval$new(1, 3)))
})
test_that("complement",{
  expect_equal(FuzzyTuple$new(1,0.1,2,0.8)$complement(),FuzzyTuple$new(1,0.9,2,0.2))
})
test_that("powerset",{
  expect_equal(FuzzyTuple$new(1,0.1,2,0.2)$powerset(), Set$new(Set$new(), FuzzyTuple$new(1,0.1),FuzzyTuple$new(2,0.2),
                                                  FuzzyTuple$new(1,0.1,2,0.2)))
  expect_equal(FuzzyTuple$new(1,0.1,"a",0.2)$powerset(), Set$new(Set$new(), FuzzyTuple$new(1,0.1),FuzzyTuple$new("a",0.2),
                                                               FuzzyTuple$new(1,0.1,"a",0.2)))
})

test_that("isSubset",{
  expect_true(f$isSubset(f, proper = FALSE))
  expect_false(f$isSubset(f, proper = TRUE))
  expect_true(f$isSubset(FuzzyTuple$new(2,0.2), proper = FALSE))
  expect_true(f$isSubset(FuzzyTuple$new(2,0.2), proper = TRUE))
  expect_false(f$isSubset(FuzzyTuple$new(2,0.1), proper = TRUE))
  expect_true(Set$new(1) < FuzzySet$new(elements = 1:3))
  expect_false(FuzzyTuple$new(elements = 1:5, membership = 0.1) < f)
  expect_false(f$isSubset(FuzzyTuple$new(2,0.2,1,0.1,3,0.3), proper = TRUE))
  expect_true(f$isSubset(FuzzyTuple$new(1,0.1,2,0.2)))
  expect_false(f$isSubset(FuzzyTuple$new(2,0.2,1,0.1)))
  expect_true(FuzzyTuple$new(elements = 1:5, membership = 1) > Tuple$new(1,2))
})

test_that("as.FuzzyTuple",{
  expect_equal(as.FuzzyTuple(c('0.1'=1,'0.2'=2,'0.3'=3)), f)
  expect_equal(as.FuzzyTuple(list('0.1'=1,'0.2'=2,'0.3'=3)), f)
  expect_equal(as.FuzzyTuple(matrix(c(1,2,3,0.1,0.2,0.3),ncol=2)), f)
  expect_equal(as.FuzzyTuple(data.frame(1:3, c(0.1,0.2,0.3))), f)
  expect_equal(as.FuzzyTuple(FuzzySet$new(1,0.1,2,0.2)), FuzzyTuple$new(1,0.1,2,0.2))
})
