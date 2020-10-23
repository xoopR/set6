test_that("construction", {
  expect_error(FuzzySet$new(1, 2, 3))
  expect_warning(expect_error(FuzzySet$new(0.1, 1, 0.9, "a")))
  expect_silent(FuzzySet$new(1, 0.1, "a", 0.9))
  expect_silent(FuzzySet$new(1, 0.1, "a", 0.9))
  expect_silent(FuzzySet$new("A", 0, TRUE, 1, as.factor("a"), 0.6))
  expect_equal(FuzzySet$new(1, 0.1, 2, 0.8), FuzzySet$new(
    elements = c(1, 2),
    membership = c(0.1, 0.8)
  ))
  expect_error(FuzzySet$new(1, 0.1, 2))
  expect_message(expect_equal(FuzzySet$new(1, 0.1, 1, 0.9), FuzzySet$new(1, 0.1)))
})

test_that("inherited_methods", {
  expect_equal(FuzzySet$new(1, 0.3)$type, "{}")
  expect_equal(FuzzySet$new(1, 0.1, 2, 0.3, 3, 0.9)$elements, as.list(1:3))
  expect_equal(FuzzySet$new(1, 0.1, 2, 0.3, 3, 0.9)$length, 3)
  expect_true(FuzzySet$new(1, 0.1, 2, 0.3, 3, 0.9)$contains(1))
  expect_false(FuzzySet$new(1, 0.1, 2, 0.3, 3, 0.9)$contains(5))
})
f <- FuzzySet$new(elements = c(1, 2, 3), membership = c(0.1, 0.2, 0.3))
test_that("membership", {
  expect_equal(f$membership(), list("1" = 0.1, "2" = 0.2, "3" = 0.3))
  expect_equal(f$membership(1), 0.1)
  expect_equal(f$membership(c(1, 5)), list("1" = 0.1, "5" = 0))
})
test_that("strprint", {
  useUnicode(TRUE)
  expect_equal(FuzzySet$new()$strprint(), "\u2205")
  useUnicode(FALSE)
  expect_equal(FuzzySet$new()$strprint(), "{}")
  expect_equal(f$strprint(), "{1(0.1), 2(0.2), 3(0.3)}")
  expect_equal(f$strprint(1), "{1(0.1),...,3(0.3)}")
  # expect_equal(
  #   FuzzySet$new(Set$new(1), 0.2, 2, 0.5)$strprint(),
  #   "{{1}(0.2), 2(0.5)}"
  # )
  useUnicode(TRUE)
})
test_that("alphaCut", {
  expect_equal(f$alphaCut(0.15), as.list(2:3))
  expect_equal(f$alphaCut(0.2, strong = FALSE), as.list(2:3))
  expect_equal(f$alphaCut(0.2, strong = TRUE), list(3))
  expect_equal(f$alphaCut(0.2, create = TRUE), Set$new(2, 3))
})
test_that("support", {
  expect_equal(f$support(), as.list(1:3))
  expect_equal(FuzzySet$new(1, 0.2, 3, 0.1, 2, 0)$support(), list(1, 3))
  expect_null(FuzzySet$new()$support())
  expect_equal(FuzzySet$new()$support(T), Set$new())
})
test_that("core", {
  expect_null(f$core())
  expect_equal(f$core(T), Set$new())
  expect_equal(FuzzySet$new(1, 1, 2, 0.3, 3, 1, 4.2, 0.1)$core(), list(1, 3))
})
test_that("inclusion", {
  expect_equal(FuzzySet$new(1, 0, 2, 0.4, 3, 1)$inclusion(1), "Not Included")
  expect_equal(FuzzySet$new(1, 0, 2, 0.4, 3, 1)$inclusion(4), "Not Included")
  expect_equal(FuzzySet$new(1, 0, 2, 0.4, 3, 1)$inclusion(2), "Partially Included")
  expect_equal(FuzzySet$new(1, 0, 2, 0.4, 3, 1)$inclusion(3), "Fully Included")
  expect_equal(FuzzySet$new(1, 0, 2, 0.4, 3, 1)$inclusion(c(2, 5)),
               list("2" = "Partially Included", "5" = "Not Included"))
})
test_that("equals", {
  expect_true(FuzzySet$new(1, 0.1, 2, 0.2)$equals(FuzzySet$new(1, 0.1, 2, 0.2)))
  expect_true(FuzzySet$new(1, 0.1, 2, 0.2)$equals(FuzzySet$new(2, 0.2, 1, 0.1)))
  expect_true(FuzzySet$new(1, 1, 2, 1)$equals(Set$new(1:2)))
  expect_false(FuzzySet$new(1, 0.1, 2, 0.1)$equals(Set$new(1:2)))
  expect_true(FuzzySet$new(1, 0.1, 2, 0.1, 3, 0.1)$equals(FuzzySet$new(elements = 1:3, membership = rep(0.1, 3))))
  expect_false(FuzzySet$new(1, 0.1, 2, 0.2, 3, 0.1)$equals(FuzzySet$new(elements = 1:3, membership = rep(0.1, 3))))
  expect_false(FuzzySet$new(1, 0.1, 2, 0.1, 3, 0.1, 4, 0.1)$equals(FuzzySet$new(elements = 1:3, membership = rep(0.1, 3))))
  expect_true(FuzzySet$new(1, 0.1, 2, 0.2, "a", 0.3) == FuzzySet$new(elements = list(1, 2, "a"), membership = c(0.1, 0.2, 0.3)))
  expect_true(FuzzySet$new(2, 0.1, 1, 0.2, "a", 0.3) ==
    FuzzySet$new(elements = list(1, 2, "a"), membership = c(0.2, 0.1, 0.3)))
})
test_that("absolute complement", {
  expect_equal(setcomplement(FuzzySet$new(1, 0.1, 2, 0.8)), FuzzySet$new(1, 0.9, 2, 0.2))
})

test_that("isSubset", {
  expect_true(FuzzySet$new(1, 1, 2, 1, 3, 1)$isSubset(Set$new(1:2)))
  expect_true(f$isSubset(f, proper = FALSE))
  expect_false(f$isSubset(f, proper = TRUE))
  expect_true(f$isSubset(FuzzySet$new(2, 0.2), proper = FALSE))
  expect_true(f$isSubset(FuzzySet$new(2, 0.2), proper = TRUE))
  expect_false(f$isSubset(FuzzySet$new(2, 0.1), proper = TRUE))
})

test_that("as.FuzzySet", {
  expect_equal(as.FuzzySet(c(1, 0.1, 2, 0.2, 3, 0.3)), f)
  expect_equal(as.FuzzySet(matrix(c(1, 2, 3, 0.1, 0.2, 0.3), ncol = 2)), f)
  expect_equal(as.FuzzySet(data.frame(elements = c(1, 2, 3), membership = c(0.1, 0.2, 0.3))), f)
  expect_equal(as.FuzzySet(list(elements = c(1, 2, 3), membership = c(0.1, 0.2, 0.3))), f)
  expect_equal(as.FuzzySet(data.frame(c(1, 2, 3), c(0.1, 0.2, 0.3))), f)
  expect_true(as.FuzzySet(Interval$new(1, 5, class = "integer")) == FuzzySet$new(elements = 1:5))
  expect_error(expect_equal(as.FuzzySet(Interval$new()), Interval$new()), "Interval cannot be")
  expect_error(expect_equal(as.FuzzySet(ConditionalSet$new(function(x) TRUE)), ConditionalSet$new(function(x) TRUE)), "ConditionalSet cannot be")
})

