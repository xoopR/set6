#' @title assert/check/test/SetInterval
#' @description Validation checks to test if a given object is an R6 SetInterval.
#' @param object object to test
#' @return If check passes then \code{assert} returns invisibly and \code{test}/\code{check}
#'   return \code{TRUE}. If check fails, \code{assert} stops code with error, \code{check} returns
#'   an error message as string, \code{test} returns \code{FALSE}.
#'
#' @examples
#' testSetInterval(5) # FALSE
#' testSetInterval(Set$new(2)) # TRUE
#'
#' @export
testSetInterval <- function(){}
#' @rdname testSetInterval
#' @export
checkSetInterval <- function(){}
#' @rdname testSetInterval
#' @export
assertSetInterval <- function(){}
makeChecks(assertionName = "SetInterval",
           cond = inherits(object, "SetInterval"),
           errormsg = "This is not an R6 SetInterval object",
           pos = environment())

#' @title assert/check/test/Set
#' @description Validation checks to test if a given object is an R6 Set.
#' @param object object to test
#' @return If check passes then \code{assert} returns invisibly and \code{test}/\code{check}
#'   return \code{TRUE}. If check fails, \code{assert} stops code with error, \code{check} returns
#'   an error message as string, \code{test} returns \code{FALSE}.
#'
#' @examples
#' testSet(Interval$new()) # FALSE
#' testSet(Set$new(2)) # TRUE
#'
#' @export
testSet <- function(){}
#' @rdname testSet
#' @export
checkSet <- function(){}
#' @rdname testSet
#' @export
assertSet <- function(){}
makeChecks(assertionName = "Set",
           cond = inherits(object, "Set"),
           errormsg = "This is not an R6 Set object",
           pos = environment())

#' @title assert/check/test/Tuple
#' @description Validation checks to test if a given object is an R6 Tuple.
#' @param object object to test
#' @return If check passes then \code{assert} returns invisibly and \code{test}/\code{check}
#'   return \code{TRUE}. If check fails, \code{assert} stops code with error, \code{check} returns
#'   an error message as string, \code{test} returns \code{FALSE}.
#'
#' @examples
#' testTuple(Set$new(2,3)) # FALSE
#' testTuple(Tuple$new(2,3)) # TRUE
#'
#' @export
testTuple <- function(){}
#' @rdname testTuple
#' @export
checkTuple <- function(){}
#' @rdname testTuple
#' @export
assertTuple <- function(){}
makeChecks(assertionName = "Tuple",
           cond = inherits(object, "Tuple"),
           errormsg = "This is not an R6 Tuple object",
           pos = environment())

#' @title assert/check/test/FuzzySet
#' @description Validation checks to test if a given object is an R6 FuzzySet.
#' @param object object to test
#' @return If check passes then \code{assert} returns invisibly and \code{test}/\code{check}
#'   return \code{TRUE}. If check fails, \code{assert} stops code with error, \code{check} returns
#'   an error message as string, \code{test} returns \code{FALSE}.
#'
#' @examples
#' testFuzzySet(Set$new(2,3)) # FALSE
#' testFuzzySet(FuzzySet$new(2,0.1,3,0.6)) # TRUE
#'
#' @export
testFuzzySet <- function(){}
#' @rdname testFuzzySet
#' @export
checkFuzzySet <- function(){}
#' @rdname testFuzzySet
#' @export
assertFuzzySet <- function(){}
makeChecks(assertionName = "FuzzySet",
           cond = inherits(object, "FuzzySet"),
           errormsg = "This is not an R6 FuzzySet object",
           pos = environment())

#' @title assert/check/test/FuzzyTuple
#' @description Validation checks to test if a given object is an R6 FuzzyTuple.
#' @param object object to test
#' @return If check passes then \code{assert} returns invisibly and \code{test}/\code{check}
#'   return \code{TRUE}. If check fails, \code{assert} stops code with error, \code{check} returns
#'   an error message as string, \code{test} returns \code{FALSE}.
#'
#' @examples
#' testFuzzyTuple(Set$new(2,3)) # FALSE
#' testFuzzyTuple(FuzzyTuple$new(2,0.1,3,0.6)) # TRUE
#'
#' @export
testFuzzyTuple <- function(){}
#' @rdname testFuzzyTuple
#' @export
checkFuzzyTuple <- function(){}
#' @rdname testFuzzyTuple
#' @export
assertFuzzyTuple <- function(){}
makeChecks(assertionName = "FuzzyTuple",
           cond = inherits(object, "FuzzyTuple"),
           errormsg = "This is not an R6 FuzzyTuple object",
           pos = environment())

#' @title assert/check/test/Interval
#' @description Validation checks to test if a given object is an R6 Interval.
#' @param object object to test
#' @return If check passes then \code{assert} returns invisibly and \code{test}/\code{check}
#'   return \code{TRUE}. If check fails, \code{assert} stops code with error, \code{check} returns
#'   an error message as string, \code{test} returns \code{FALSE}.
#'
#' @examples
#' testInterval(Set$new(2,3)) # FALSE
#' testInterval(Interval$new(1, 4)) # TRUE
#'
#' @export
testInterval <- function(){}
#' @rdname testInterval
#' @export
checkInterval <- function(){}
#' @rdname testInterval
#' @export
assertInterval <- function(){}
makeChecks(assertionName = "Interval",
           cond = inherits(object, "Interval"),
           errormsg = "This is not an R6 Interval object",
           pos = environment())

#' @title assert/check/test/BoundedAbove
#' @description Validation checks to test if a given object is BoundedAbove.
#' @param object object to test
#' @return If check passes then \code{assert} returns invisibly and \code{test}/\code{check}
#'   return \code{TRUE}. If check fails, \code{assert} stops code with error, \code{check} returns
#'   an error message as string, \code{test} returns \code{FALSE}.
#'
#' @examples
#' testBoundedAbove(Interval$new(2,3,type='()')) # FALSE
#' testBoundedAbove(Interval$new(2,3,type='[]')) # TRUE
#'
#' @export
testBoundedAbove <- function(){}
#' @rdname testBoundedAbove
#' @export
checkBoundedAbove <- function(){}
#' @rdname testBoundedAbove
#' @export
assertBoundedAbove <- function(){}
makeChecks(assertionName = "BoundedAbove",
           cond = testSet(object) |  substr(object$type(),2,2) == "]",
           errormsg = "This is not bounded above",
           pos = environment())

#' @title assert/check/test/BoundedBelow
#' @description Validation checks to test if a given object is BoundedBelow.
#' @param object object to test
#' @return If check passes then \code{assert} returns invisibly and \code{test}/\code{check}
#'   return \code{TRUE}. If check fails, \code{assert} stops code with error, \code{check} returns
#'   an error message as string, \code{test} returns \code{FALSE}.
#'
#' @examples
#' testBoundedBelow(Interval$new(2,3,type='()')) # FALSE
#' testBoundedBelow(Interval$new(2,3,type='[]')) # TRUE
#'
#' @export
testBoundedBelow <- function(){}
#' @rdname testBoundedBelow
#' @export
checkBoundedBelow <- function(){}
#' @rdname testBoundedBelow
#' @export
assertBoundedBelow <- function(){}
makeChecks(assertionName = "BoundedBelow",
           cond = testSet(object) |  substr(object$type(),1,1) == "[",
           errormsg = "This is not bounded below",
           pos = environment())

#' @title assert/check/test/Bounded
#' @description Validation checks to test if a given object is Bounded.
#' @param object object to test
#' @return If check passes then \code{assert} returns invisibly and \code{test}/\code{check}
#'   return \code{TRUE}. If check fails, \code{assert} stops code with error, \code{check} returns
#'   an error message as string, \code{test} returns \code{FALSE}.
#'
#' @examples
#' testBounded(Interval$new(2,3,type='()')) # FALSE
#' testBounded(Interval$new(2,3,type='[]')) # TRUE
#'
#' @export
testBounded <- function(){}
#' @rdname testBounded
#' @export
checkBounded <- function(){}
#' @rdname testBounded
#' @export
assertBounded <- function(){}
makeChecks(assertionName = "Bounded",
           cond = testBoundedBelow(object) &  testBoundedAbove(object),
           errormsg = "This is not bounded",
           pos = environment())

#' @title assert/check/test/Finite
#' @description Validation checks to test if a given object is Finite.
#' @param object object to test
#' @return If check passes then \code{assert} returns invisibly and \code{test}/\code{check}
#'   return \code{TRUE}. If check fails, \code{assert} stops code with error, \code{check} returns
#'   an error message as string, \code{test} returns \code{FALSE}.
#'
#' @examples
#' testFinite(Interval$new(2,3,class="integer")) # TRUE
#' testFinite(Interval$new()) # FALSE
#'
#' @export
testFinite <- function(){}
#' @rdname testFinite
#' @export
checkFinite <- function(){}
#' @rdname testFinite
#' @export
assertFinite <- function(){}
makeChecks(assertionName = "Finite",
           cond = object$inf()!=-Inf &  object$sup()!=Inf,
           errormsg = "This is not finite",
           pos = environment())
