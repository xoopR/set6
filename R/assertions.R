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

#' @title assert/check/test/SetList
#' @description Validation checks to test if a given object is a list of R6 Sets.
#' @param object object to test
#' @return If check passes then \code{assert} returns invisibly and \code{test}/\code{check}
#'   return \code{TRUE}. If check fails, \code{assert} stops code with error, \code{check} returns
#'   an error message as string, \code{test} returns \code{FALSE}.
#'
#' @examples
#' testSetList(list(Set$new(5),5)) # FALSE
#' testSetList(list(Set$new(),Interval$new())) # TRUE
#'
#' @export
testSetList <- function(){}
#' @rdname testSetList
#' @export
checkSetList <- function(){}
#' @rdname testSetList
#' @export
assertSetList <- function(){}

makeChecks(assertionName =  "SetList",
           cond = all(unlist(lapply(object, inherits,"Set"))),
           errormsg = "One or more items in the list are not Sets",
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

#' @title assert/check/test/ClosedAbove
#' @description Validation checks to test if a given object is ClosedAbove.
#' @param object object to test
#' @return If check passes then \code{assert} returns invisibly and \code{test}/\code{check}
#'   return \code{TRUE}. If check fails, \code{assert} stops code with error, \code{check} returns
#'   an error message as string, \code{test} returns \code{FALSE}.
#'
#' @examples
#' testClosedAbove(Interval$new(2,3,type='()')) # FALSE
#' testClosedAbove(Interval$new(2,3,type='[]')) # TRUE
#'
#' @export
testClosedAbove <- function(){}
#' @rdname testClosedAbove
#' @export
checkClosedAbove <- function(){}
#' @rdname testClosedAbove
#' @export
assertClosedAbove <- function(){}
makeChecks(assertionName = "ClosedAbove",
           cond = testSet(object) &  substr(object$type,2,2) == "]",
           errormsg = "This is not a set closed above",
           pos = environment())

#' @title assert/check/test/ClosedBelow
#' @description Validation checks to test if a given object is ClosedBelow.
#' @param object object to test
#' @return If check passes then \code{assert} returns invisibly and \code{test}/\code{check}
#'   return \code{TRUE}. If check fails, \code{assert} stops code with error, \code{check} returns
#'   an error message as string, \code{test} returns \code{FALSE}.
#'
#' @examples
#' testClosedBelow(Interval$new(2,3,type='()')) # FALSE
#' testClosedBelow(Interval$new(2,3,type='[]')) # TRUE
#'
#' @export
testClosedBelow <- function(){}
#' @rdname testClosedBelow
#' @export
checkClosedBelow <- function(){}
#' @rdname testClosedBelow
#' @export
assertClosedBelow <- function(){}
makeChecks(assertionName = "ClosedBelow",
           cond = testSet(object) &  substr(object$type,1,1) == "[",
           errormsg = "This is not a set closed below",
           pos = environment())

#' @title assert/check/test/Closed
#' @description Validation checks to test if a given object is Closed.
#' @param object object to test
#' @return If check passes then \code{assert} returns invisibly and \code{test}/\code{check}
#'   return \code{TRUE}. If check fails, \code{assert} stops code with error, \code{check} returns
#'   an error message as string, \code{test} returns \code{FALSE}.
#'
#' @examples
#' testClosed(Interval$new(2,3,type='()')) # FALSE
#' testClosed(Interval$new(2,3,type='[]')) # TRUE
#'
#' @export
testClosed <- function(){}
#' @rdname testClosed
#' @export
checkClosed <- function(){}
#' @rdname testClosed
#' @export
assertClosed <- function(){}
makeChecks(assertionName = "Closed",
           cond = object$properties$closure == "closed",
           errormsg = "This is not a closed set",
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
           cond = object$lower!=-Inf &  object$upper!=Inf,
           errormsg = "This is not finite",
           pos = environment())

#' @title assert/check/test/Fuzzy
#' @description Validation checks to test if a given set/tuple is fuzzy.
#' @param object object to test
#' @return If check passes then \code{assert} returns invisibly and \code{test}/\code{check}
#'   return \code{TRUE}. If check fails, \code{assert} stops code with error, \code{check} returns
#'   an error message as string, \code{test} returns \code{FALSE}.
#'
#' @examples
#' testFuzzy(FuzzySet$new(1, 0.2)) # TRUE
#' testFuzzy(Set$new(1, 0.2)) # FALSE
#'
#' @export
testFuzzy <- function(){}
#' @rdname testFuzzy
#' @export
checkFuzzy <- function(){}
#' @rdname testFuzzy
#' @export
assertFuzzy <- function(){}
makeChecks(assertionName = "Fuzzy",
           cond = grepl("Fuzzy", getR6Class(object)),
           errormsg = "This is not fuzzy.",
           pos = environment())

#' @title assert/check/test/Crisp
#' @description Validation checks to test if a given set/tuple is crisp.
#' @param object object to test
#' @return If check passes then \code{assert} returns invisibly and \code{test}/\code{check}
#'   return \code{TRUE}. If check fails, \code{assert} stops code with error, \code{check} returns
#'   an error message as string, \code{test} returns \code{FALSE}.
#'
#' @examples
#' testCrisp(Set$new(1, 0.2)) # TRUE
#' testCrisp(FuzzySet$new(1, 0.2)) # FALSE
#'
#' @export
testCrisp <- function(){}
#' @rdname testCrisp
#' @export
checkCrisp <- function(){}
#' @rdname testCrisp
#' @export
assertCrisp <- function(){}
makeChecks(assertionName = "Crisp",
           cond = object$traits$crisp,
           errormsg = "This is not crisp.",
           pos = environment())

#' @title assert/check/test/ConditionalSet
#' @description Validation checks to test if a given object is an R6 ConditionalSet.
#' @param object object to test
#' @return If check passes then \code{assert} returns invisibly and \code{test}/\code{check}
#'   return \code{TRUE}. If check fails, \code{assert} stops code with error, \code{check} returns
#'   an error message as string, \code{test} returns \code{FALSE}.
#'
#' @examples
#' testConditionalSet(Set$new(2)) # FALSE
#' testConditionalSet(ConditionalSet$new(function(x,y) x + y < 2)) # TRUE
#'
#' @export
testConditionalSet <- function(){}
#' @rdname testConditionalSet
#' @export
checkConditionalSet <- function(){}
#' @rdname testConditionalSet
#' @export
assertConditionalSet <- function(){}
makeChecks(assertionName = "ConditionalSet",
           cond = inherits(object, "ConditionalSet"),
           errormsg = "This is not an R6 ConditionalSet object",
           pos = environment())

#' @title assert/check/test/Empty
#' @description Validation checks to test if a given set is empty.
#' @param object object to test
#' @return If check passes then \code{assert} returns invisibly and \code{test}/\code{check}
#'   return \code{TRUE}. If check fails, \code{assert} stops code with error, \code{check} returns
#'   an error message as string, \code{test} returns \code{FALSE}.
#'
#' @examples
#' testEmpty(Set$new(2)) # FALSE
#' testEmpty(Set$new()) # TRUE
#'
#' @export
testEmpty <- function(){}
#' @rdname testEmpty
#' @export
checkEmpty <- function(){}
#' @rdname testEmpty
#' @export
assertEmpty <- function(){}
makeChecks(assertionName = "Empty",
           cond = object$properties$empty,
           errormsg = "This is not an R6 Empty object",
           pos = environment())
