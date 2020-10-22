test_that("construction", {
  expect_silent(Universal$new())
  expect_error(Universal$new(12))
})

test_that("strprint", {
  expect_silent(expect_equal(Universal$new()$strprint(), "V"))
})

v <- Universal$new()

test_that("contains", {
  expect_true(v$contains(list(letters, 1, 2, "a"), all = TRUE))
  expect_equal(v$contains(list(FALSE, 1, 2, "a")), rep(TRUE, 4))
  expect_silent(expect_equal(Universal$new()$strprint(), "V"))
})

test_that("subset", {
  expect_false(v$isSubset("a"))
  expect_false(v$isSubset(1))
  expect_true(Set$new(1) < v)
  expect_true(Set$new(1) <= v)
  expect_false(Set$new(1) > v)
  expect_true(v <= v)
  expect_false(v < v)
})

test_that("equals", {
  expect_true(v == v)
  expect_false(v == Set$new(1))
})

test_that("operations", {
  expect_equal(setunion(Set$new(1, 2, 3), Interval$new(), v), v)
  expect_equal(Set$new(1, 2, 3) - v, Set$new())
  expect_equal(setcomplement(Set$new(1, 2, 3))$contains(1:5), c(rep(F, 3), rep(T, 2)))
  expect_equal(Set$new(letters) & v, Set$new(letters))
  expect_equal(powerset(v), v)
  expect_equal(v^8, v)
  expect_true((v * Set$new(1, 2, 3))$contains(Tuple$new("blueberries", 2)))
})
