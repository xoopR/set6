library(testthat)

context("setsymdiff")

test_that("subsets",{
  expect_equal(Set$new(1) %-% Reals$new(), Reals$new() - Set$new(1))
  expect_equal(Reals$new() %-% Set$new(1,2,3), Reals$new() - Set$new(1,2,3))
})

test_that("simplify",{
  expect_equal(Interval$new(1,3,class="integer") %-% Interval$new(3,4,class="integer"),
               Set$new(1,2,4))
  expect_equal(setsymdiff(Interval$new(1,3,class="integer"), Interval$new(3,4,class="integer")),
               Set$new(1,2,4))
})

test_that("!simplify",{
  useUnicode(FALSE)
  expect_equal(setsymdiff(Interval$new(1,3,class="integer"), Interval$new(3,4,class="integer"),
                          simplify = FALSE)$strprint(), "({1, 2, 3} ∪ {3, 4}) \\ {3}")
  useUnicode(TRUE)
})
