test_that("assertThat", {
  expect_silent(assertThat(Set$new()$length, Set$new()$length == "0", "Not True"))
  expect_error(assertThat(Set$new()$length, Set$new()$length == "2", "Not True"))
})

test_that("checkThat", {
  expect_true(checkThat(Set$new()$length == "0", "Not True"))
  expect_equal(checkThat(Set$new()$length == "2", "Not True"), "Not True")
})

test_that("testThat", {
  expect_true(testThat(Set$new()$length == "0"))
  expect_false(testThat(Set$new()$length == "2"))
})

test_that("isThat", {
  expect_true(isThat(Set$new()$length == "0"))
  expect_false(isThat(Set$new()$length == "2"))
})

test_that("makeChecks", {
  expect_silent(makeChecks("Test", 1 == 1, "Error"))
})


test_that("stopwarn", {
  expect_warning(expect_null(stopwarn(error = "warn", "Warning")))
  expect_error(stopwarn(error = "stop", "Warning"))
})

test_that("testmessage", {
  expect_true(testMessage(message("Hi")))
  expect_warning(expect_false(testMessage(warning("Hi"))))
})

test_that("ifnerror", {
  expect_equal(ifnerror(stop("Error"), noerror = "Success", error = "Failure", silent = T), "Failure")
  expect_equal(ifnerror("Nerror", noerror = "Success", error = "Failure", silent = T), "Success")
  expect_warning(ifnerror(stop("Error"), noerror = "Success", stopwarn = "warn", silent = T))
  expect_error(ifnerror(stop("Error"), noerror = "Success", stopwarn = "stop", silent = T))
})

test_that("modal", {
  expect_equal(modal(c(1, 2, 2, 4, 5, 6, 7, 2, 4, 4, 2, 4, 2)), 2)
  expect_equal(modal(c(1, 2, 2, 4, 5, 6, 7, 2, 4, 4, 2, 4, 2, 4)), c(2, 4))
})

test_that("toproper", {
  expect_equal(toproper("PROPER CaSe"), "Proper Case")
})


test_that("crispify", {
  expect_equal(crispify(Set$new(1)), Set$new(1))
  expect_equal(crispify(FuzzySet$new(1, 1)), Set$new(1))
  expect_equal(crispify(FuzzyTuple$new(1, 1)), Tuple$new(1))
})

test_that("fuzzify", {
  expect_equal(fuzzify(Set$new(1)), FuzzySet$new(1, 1))
  expect_equal(fuzzify(FuzzySet$new(1, 1)), FuzzySet$new(1, 1))
  expect_equal(fuzzify(Tuple$new(1)), FuzzyTuple$new(1, 1))
  expect_error(fuzzify(Interval$new(1, 4)), "cannot be fuzzified")
})

test_that("setSymbol", {
  useUnicode(FALSE)
  expect_equal(setSymbol("Reals", FALSE), "R")
  useUnicode(TRUE)
})

test_that("returner", {
  expect_equal(returner(list(TRUE), TRUE), TRUE)
})
