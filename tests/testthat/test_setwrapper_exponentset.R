library(testthat)

test_that("contains", {
  e <- Set$new(1, 2)^2
  expect_true(e$contains(Tuple$new(1, 1)))
  expect_false(e$contains(Tuple$new(1, 1, 1)))
  expect_false(e$contains(1))
  expect_equal(e$contains(c(Tuple$new(1, 1), Tuple$new(2, 3))), c(TRUE, FALSE))
  expect_false(e$contains(c(Tuple$new(1, 1), Tuple$new(2, 3)), all = TRUE))
})
