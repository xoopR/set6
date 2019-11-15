#' @include helpers.R

#' @template assertions_class
#' @templateVar class Set
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

#' @template assertions_class
#' @templateVar class SetList
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

#' @template assertions_class
#' @templateVar class Tuple
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

#' @template assertions_class
#' @templateVar class FuzzySet
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

#' @template assertions_class
#' @templateVar class FuzzyTuple
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

#' @template assertions_class
#' @templateVar class Interval
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

#' @template assertions_class
#' @templateVar class ConditionalSet
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

#' @template assertions_properties
#' @templateVar property ClosedAbove
#' @templateVar test1 Interval$new(1, 10, type = "[]")
#' @templateVar test2 Interval$new(1, 10, type = "[)")
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

#' @template assertions_properties
#' @templateVar property ClosedBelow
#' @templateVar test1 Interval$new(1, 10, type = "[]")
#' @templateVar test2 Interval$new(1, 10, type = "(]")
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

#' @template assertions_properties
#' @templateVar property Closed
#' @templateVar test1 Interval$new(1, 10, type = "[]")
#' @templateVar test2 Interval$new(1, 10, type = "(]")
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

#' @template assertions_properties
#' @templateVar property Finite
#' @templateVar test1 Interval$new(1, 10, class = "integer")
#' @templateVar test2 Interval$new(1, 10, class = "numeric")
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

#' @template assertions_properties
#' @templateVar property Fuzzy
#' @templateVar test1 FuzzySet$new(1, 0.5)
#' @templateVar test2 Set$new(1)
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

#' @template assertions_properties
#' @templateVar property Crisp
#' @templateVar test1 Set$new(1)
#' @templateVar test2 FuzzySet$new(1, 0.5)
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

#' @template assertions_properties
#' @templateVar property Empty
#' @templateVar test1 Set$new()
#' @templateVar test2 Set$new(1)
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
           errormsg = "This is not an empty set",
           pos = environment())
