test_that("construction", {
  expect_silent(Logicals$new())
  l <- Logicals$new()
  expect_equal(l$elements, list(TRUE, FALSE))
  expect_equal(l$min, NaN)
  expect_equal(l$max, NaN)
  expect_equal(l$lower, TRUE)
  expect_equal(l$upper, FALSE)
})
