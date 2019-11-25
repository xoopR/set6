#' @include Set.R setSymbol.R
SpecialSet <- R6::R6Class("SpecialSet", inherit = Interval)
SpecialSet$set("public","initialize",function(lower = -Inf, upper = Inf, type = "()", class = "numeric"){
  if(getR6Class(self, pos = environment()) == "SpecialSet")
    stop(paste(getR6Class(self, pos = environment()), "is an abstract class that can't be initialized."))

  super$initialize(lower = lower, upper = upper, type = type, class = class, universe = NULL)

  invisible(self)
})
SpecialSet$set("public","strprint",function(...){
  setSymbol(getR6Class(self))
})

#' @name Naturals
#' @template SpecialSetcon
#' @templateVar class Naturals
#' @templateVar set natural numbers
#' @templateVar def the counting numbers
#' @templateVar latexeqn  \\{0, 1, 2,...\\}
#' @templateVar roxeqn {0, 1, 2,...}
#' @templateVar conargs lower = 0
#' @templateVar arg1 \code{lower} \tab integer \tab Where to start the set.
#' @templateVar constructorDets Generally the `lower` argument should be ignored, its primary use-case is for the `PosNaturals` child-class.
#' @export
NULL
Naturals <- R6::R6Class("Naturals",inherit = SpecialSet)
Naturals$set("public", "initialize", function(lower = 0){
  super$initialize(lower = lower, upper = Inf, type = "[)", class = "integer")
})

#' @name PosNaturals
#' @template SpecialSet
#' @templateVar class PosNaturals
#' @templateVar set positive natural numbers
#' @templateVar def the positive counting numbers
#' @templateVar latexeqn \\{1, 2, 3,...\\}
#' @templateVar roxeqn {1, 2, 3,...}
#' @templateVar conargs ...
#' @templateVar arg1 \code{...} \tab ANY \tab Ignored, added for consistency.
#' @export
NULL
PosNaturals <- R6::R6Class("PosNaturals",inherit = Naturals)
PosNaturals$set("public", "initialize", function(){
  super$initialize(lower = 1)
})

#' @name Integers
#' @template SpecialSetcon
#' @templateVar class Integers
#' @templateVar set integers
#' @templateVar def the set of whole numbers
#' @templateVar latexeqn  \\{...,-3, -2, -1, 0, 1, 2, 3,...\\}
#' @templateVar roxeqn {...,-3, -2, -1, 0, 1, 2, 3,...}
#' @templateVar conargs ...
#' @templateVar arg1 \code{...} \tab ANY \tab Additional arguments.
#' @templateVar constructorDets Generally the \code{...} argument should be ignored, its primary use-case is for the child-classes.
#' @export
NULL
Integers <- R6::R6Class("Integers",inherit = SpecialSet)
Integers$set("public", "initialize", function(lower = -Inf, upper = Inf, type = "()"){
  super$initialize(lower = lower, upper = upper, type = type, class = "integer")
})

#' @name PosIntegers
#' @template SpecialSet
#' @templateVar class PosIntegers
#' @templateVar set positive integers
#' @templateVar def the set of positive whole numbers
#' @templateVar latexeqn  \\{0, 1, 2, 3,...\\}
#' @templateVar roxeqn {0, 1, 2, 3,...}
#' @templateVar conargs zero = FALSE
#' @templateVar arg1 \code{zero = FALSE} \tab logical \tab If TRUE, zero is included in the set.
#' @export
NULL
PosIntegers <- R6::R6Class("PosIntegers",inherit = Integers)
PosIntegers$set("public", "initialize", function(zero = FALSE){
  super$initialize(lower = ifelse(zero, 0, 1), type = "[)")
})

#' @name NegIntegers
#' @template SpecialSet
#' @templateVar class NegIntegers
#' @templateVar set negative integers
#' @templateVar def the set of negative whole numbers
#' @templateVar latexeqn  \\{...,-3, -2, -1, 0\\}
#' @templateVar roxeqn {...,-3, -2, -1, 0}
#' @templateVar conargs zero = FALSE
#' @templateVar arg1 \code{zero = FALSE} \tab logical \tab If TRUE, zero is included in the set.
#' @export
NULL
NegIntegers <- R6::R6Class("NegIntegers",inherit = Integers)
NegIntegers$set("public", "initialize", function(zero = FALSE){
  super$initialize(upper = ifelse(zero, 0, -1), type = "(]")
})

#' @name Rationals
#' @template SpecialSetcon
#' @templateVar class Rationals
#' @templateVar set rational numbers
#' @templateVar def the set of numbers that can be written as a fraction of two integers
#' @templateVar latexeqn  \\{\frac{p}{q} \ : \ p,q \ \in \ Z, \ q \ne 0 \\}
#' @templateVar roxeqn {p/q : p,q \epsilon Z, q != 0}
#' @templateVar support where \eqn{Z} is the set of integers.
#' @templateVar deets The [contains] method does not work for the set of Rationals as it is notoriously difficult/impossible to find an algorithm for determining if any given number is rational or not. Furthermore, computers must truncate all irrational numbers to rational numbers.
#' @templateVar conargs ...
#' @templateVar arg1 \code{...} \tab ANY \tab Additional arguments.
#' @templateVar constructorDets Generally the \code{...} argument should be ignored, its primary use-case is for the child-classes.
#' @export
NULL
Rationals <- R6::R6Class("Rationals",inherit = SpecialSet)
Rationals$set("public", "initialize", function(lower = -Inf, upper = Inf, type = "()"){
  super$initialize(lower = lower, upper = upper, type = type, class = "numeric")
})

#' @name PosRationals
#' @template SpecialSetcon
#' @templateVar class PosRationals
#' @templateVar set positive rational numbers
#' @templateVar def the set of numbers that can be written as a fraction of two integers and are non-negative
#' @templateVar latexeqn \\{\frac{p}{q} \ : \ p,q \ \in \ Z, \ p/q \ge 0, \ q \ne 0\\}
#' @templateVar roxeqn {p/q : p,q \epsilon Z, p/q \ge 0, q != 0}
#' @templateVar support where \eqn{Z} is the set of integers.
#' @templateVar deets The [contains] method does not work for the set of Rationals as it is notoriously difficult/impossible to find an algorithm for determining if any given number is rational or not. Furthermore, computers must truncate all irrational numbers to rational numbers.
#' @templateVar conargs zero = FALSE
#' @templateVar arg1 \code{zero = FALSE} \tab logical \tab If TRUE, zero is included in the set. \cr
#' @templateVar constructorDets Generally the \code{...} argument should be ignored, its primary use-case is for the child-classes.
#' @export
NULL
PosRationals <- R6::R6Class("PosRationals",inherit = Rationals)
PosRationals$set("public", "initialize", function(zero = FALSE){
  super$initialize(lower = 0, type = ifelse(zero, "[)", "()"))
})

#' @name NegRationals
#' @template SpecialSetcon
#' @templateVar class NegRationals
#' @templateVar set negative rational numbers
#' @templateVar def the set of numbers that can be written as a fraction of two integers and are non-positive
#' @templateVar latexeqn \\{\frac{p}{q} \ : \ p,q \ \in \ Z, \ p/q \le 0, \ q \ne 0\\}
#' @templateVar roxeqn {p/q : p,q \epsilon Z, p/q \le 0, q != 0}
#' @templateVar support where \eqn{Z} is the set of integers.
#' @templateVar deets The [contains] method does not work for the set of Rationals as it is notoriously difficult/impossible to find an algorithm for determining if any given number is rational or not. Furthermore, computers must truncate all irrational numbers to rational numbers.
#' @templateVar conargs zero = FALSE
#' @templateVar arg1 \code{zero = FALSE} \tab logical \tab If TRUE, zero is included in the set. \cr
#' @templateVar constructorDets Generally the \code{...} argument should be ignored, its primary use-case is for the child-classes.
#' @export
NULL
NegRationals <- R6::R6Class("NegRationals",inherit = Rationals)
NegRationals$set("public", "initialize", function(zero = FALSE){
  super$initialize(upper = 0, type = ifelse(zero, "(]", "()"))
})

#' @name Reals
#' @template SpecialSetcon
#' @templateVar class Reals
#' @templateVar set real numbers
#' @templateVar def the union of the set of rationals and irrationals
#' @templateVar latexeqn  I \cup Q
#' @templateVar roxeqn I U Q
#' @templateVar support where \eqn{I} is the set of irrationals and \eqn{Q} is the set of rationals.
#' @templateVar conargs ...
#' @templateVar arg1 \code{...} \tab ANY \tab Additional arguments.
#' @templateVar constructorDets Generally the \code{...} argument should be ignored, its primary use-case is for the child-classes.
#' @export
NULL
Reals <- R6::R6Class("Reals",inherit = SpecialSet)
Reals$set("public", "initialize", function(lower = -Inf, upper = Inf, type = "()"){
  super$initialize(lower = lower, upper = upper, type = type, class = "numeric")
})

#' @name PosReals
#' @template SpecialSet
#' @templateVar class PosReals
#' @templateVar set positive real numbers
#' @templateVar def the union of the set of positive rationals and positive irrationals
#' @templateVar latexeqn I^+ \cup Q^+
#' @templateVar roxeqn I+ U Q+
#' @templateVar support where \eqn{I^+}{I+} is the set of positive irrationals and \eqn{Q^+}{Q+} is the set of positive rationals.
#' @templateVar conargs zero = FALSE
#' @templateVar arg1 \code{zero = FALSE} \tab logical \tab If TRUE, zero is included in the set. \cr
#' @export
NULL
PosReals <- R6::R6Class("PosReals",inherit = Reals)
PosReals$set("public", "initialize", function(zero = FALSE){
  super$initialize(lower = 0, type = ifelse(zero, "[)", "()"))
})

#' @name NegReals
#' @template SpecialSet
#' @templateVar class NegReals
#' @templateVar set negative real numbers
#' @templateVar def the union of the set of negative rationals and negative irrationals
#' @templateVar latexeqn I^- \cup Q^-
#' @templateVar roxeqn I- U Q-
#' @templateVar support where \eqn{I^-}{I-} is the set of negative irrationals and \eqn{Q^-}{Q-} is the set of negative rationals.
#' @templateVar conargs zero = FALSE
#' @templateVar arg1 \code{zero = FALSE} \tab logical \tab If TRUE, zero is included in the set. \cr
#' @export
NULL
NegReals <- R6::R6Class("NegReals",inherit = Reals)
NegReals$set("public", "initialize", function(zero = FALSE){
  super$initialize(upper = 0, type = ifelse(zero, "(]", "()"))
})

#' @name ExtendedReals
#' @template SpecialSet
#' @templateVar class ExtendedReals
#' @templateVar set extended real numbers
#' @templateVar def the union of the set of reals with \eqn{\pm\infty}{±\infty}
#' @templateVar latexeqn R \cup \\{-\infty, \infty\\}
#' @templateVar roxeqn R U {-\infty, \infty}
#' @templateVar conargs ...
#' @templateVar arg1 \code{...} \tab ANY \tab Ignored, added for consistency.
#' @templateVar support where \eqn{R} is the set of reals.
#' @export
NULL
ExtendedReals <- R6::R6Class("ExtendedReals",inherit = Reals)
ExtendedReals$set("public", "initialize", function(){
  super$initialize(type = "[]")
})

#' @name Complex
#' @template SpecialSet
#' @templateVar class Complex
#' @templateVar set complex numbers
#' @templateVar def the set of reals with possibly imaginary components
#' @templateVar latexeqn \\{a + bi \\ : \\ a,b \in R\\}
#' @templateVar roxeqn {a + bi : a,b \epsilon R}
#' @templateVar conargs lower = -Inf+0i, upper = Inf+0i
#' @templateVar arg1 `lower` \tab complex \tab Lower limit of interval. \cr
#' @templateVar arg2 `upper` \tab complex \tab Upper limit of interval.
#' @templateVar support where \eqn{R} is the set of reals.
#' @export
NULL
Complex <- R6::R6Class("Complex",inherit = SpecialSet)
Complex$set("public", "initialize", function(lower = -Inf+0i, upper = Inf+0i){
  super$initialize(lower = as.complex(lower), upper = as.complex(upper), type = "()", class = "numeric")
})
Complex$set("public","contains",function(x, all = FALSE, bound = NULL){
  ret <- sapply(x, function(y) inherits(y, "complex"))
  returner(ret, all)
})
