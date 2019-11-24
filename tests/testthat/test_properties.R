library(testthat)

context("Properties")

test_that("construction",{
  expect_error(Properties$new(cardinality = "c"), "Cardinality must either")
  expect_equal(Properties$new(cardinality = "a0")$cardinality, "Aleph0")
  expect_equal(Properties$new(cardinality = "aleph0")$cardinality, "Aleph0")
  expect_equal(Properties$new(cardinality = "beth0")$cardinality, "Beth0")
  expect_equal(Properties$new(cardinality = "b20")$cardinality, "Beth20")
  expect_error(Properties$new(cardinality = "b20a"))

  expect_error(Properties$new(closure = "not closed"))
})

test_that("strprint/print",{
  expect_equal(class(Properties$new("closed", 5)$strprint()), "list")
  expect_output(print(Properties$new("closed", 5)))

  useUnicode(TRUE)
  expect_equal(Properties$new("closed", 5)$strprint()$cardinality, 5)
  expect_equal(Properties$new("closed", "a0")$strprint()$cardinality, "\u2135\u2080")
  expect_equal(Properties$new("closed", "b15")$strprint()$cardinality, "\u2136\u2081\u2085")
  useUnicode(FALSE)
  expect_equal(Properties$new("closed", 5)$strprint()$cardinality, 5)
  expect_equal(Properties$new("closed", "a0")$strprint()$cardinality, "Aleph0")
  expect_equal(Properties$new("closed", "b15")$strprint()$cardinality, "Beth15")
})
