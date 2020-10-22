#' @include Set.R setSymbol.R

#' @name SpecialSet
#' @rdname SpecialSet
#' @title Abstract Class for Special Sets
#' @description The 'special sets' are the group of sets that are commonly used in mathematics
#' and are thus given their own names.
#' @details This is an abstract class and should not be constructed directly. Use [listSpecialSets]
#' to see the list of implemented special sets.
SpecialSet <- R6Class("SpecialSet",
  inherit = Interval,
  public = list(
    #' @description `SpecialSet` is an abstract class, the constructor cannot be used directly.
    #' @param lower defines the lower bound of the interval.
    #' @param upper defines the upper bound of the interval.
    #' @param type defines the interval closure type.
    #' @param class defines the interval class.
    initialize = function(lower = -Inf, upper = Inf, type = "()", class = "numeric") {
      if (getR6Class(self, pos = environment()) == "SpecialSet") {
        stop(paste(getR6Class(self, pos = environment()), "is an abstract class that can't be initialized."))
      }

      super$initialize(lower = lower, upper = upper, type = type, class = class, universe = Universal$new())

      invisible(self)
    },

    #' @description Creates a printable representation of the object.
    #' @param n ignored, added for consistency.
    #' @return A character string representing the object.
    strprint = function(n = NULL) {
      setSymbol(getR6Class(self), private$.zero)
    }
  ),

  private = list(
    .zero = FALSE
  )
)

#' @name Naturals
#' @title Set of Natural Numbers
#' @description The mathematical set of natural numbers, defined as the counting numbers. i.e.
#' \deqn{\\{0, 1, 2,...\\}}{0, 1, 2,...}
#' @family special sets
#' @export
Naturals <- R6Class("Naturals",
  inherit = SpecialSet,
  public = list(
    #' @description Create a new `Naturals` object.
    #' @return A new `Naturals` object.
    #' @param lower numeric. Where to start the set. Advised to ignore, used by child-classes.
    initialize = function(lower = 0) {
      super$initialize(lower = lower, upper = Inf, type = "[)", class = "integer")
    }
  )
)

#' @name PosNaturals
#' @title Set of Positive Natural Numbers
#' @description The mathematical set of positive natural numbers, defined as the positive counting numbers. i.e.
#' \deqn{\\{1, 2, 3,...\\}}{0, 1, 2, 3,...}
#' @family special sets
#' @export
PosNaturals <- R6Class("PosNaturals",
  inherit = Naturals,
  public = list(
    #' @description Create a new `PosNaturals` object.
    #' @return A new `PosNaturals` object.
    initialize = function() {
      super$initialize(lower = 1)
    }
  )
)


#' @name Integers
#' @title Set of Integers
#' @description The mathematical set of integers, defined as the set of whole numbers. i.e.
#' \deqn{\\{...,-3, -2, -1, 0, 1, 2, 3,...\\}}{...,-3, -2, -1, 0, 1, 2, 3,...}
#' @family special sets
#' @export
Integers <- R6Class("Integers",
  inherit = SpecialSet,
  public = list(
    #' @description Create a new `Integers` object.
    #' @return A new `Integers` object.
    #' @param lower numeric. Where to start the set. Advised to ignore, used by child-classes.
    #' @param upper numeric. Where to end the set. Advised to ignore, used by child-classes.
    #' @param type character Set closure type. Advised to ignore, used by child-classes.
    initialize = function(lower = -Inf, upper = Inf, type = "()") {
      super$initialize(lower = lower, upper = upper, type = type, class = "integer")
    }
  )
)

#' @name PosIntegers
#' @title Set of Positive Integers
#' @description The mathematical set of positive integers, defined as the set of positive whole numbers. i.e.
#' \deqn{\\{0, 1, 2, 3,...\\}}{0, 1, 2, 3,...}
#' @family special sets
#' @export
PosIntegers <- R6Class("PosIntegers",
  inherit = Integers,
  public = list(
    #' @description Create a new `PosIntegers` object.
    #' @return A new `PosIntegers` object.
    #' @param zero logical. If TRUE, zero is included in the set.
    initialize = function(zero = FALSE) {
      if (zero) private$.zero <- TRUE
      super$initialize(lower = ifelse(zero, 0, 1), type = "[)")
    }
  )
)

#' @name NegIntegers
#' @title Set of Negative Integers
#' @description The mathematical set of negative integers, defined as the set of negative whole numbers. i.e.
#' \deqn{\\{...,-3, -2, -1, 0\\}}{...,-3, -2, -1, 0}
#' @family special sets
#' @export
NegIntegers <- R6Class("NegIntegers",
  inherit = Integers,
  public = list(
    #' @description Create a new `NegIntegers` object.
    #' @return A new `NegIntegers` object.
    #' @param zero logical. If TRUE, zero is included in the set.
    initialize = function(zero = FALSE) {
      if (zero) private$.zero <- TRUE
      super$initialize(upper = ifelse(zero, 0, -1), type = "(]")
    }
  )
)

#' @name Rationals
#' @title Set of Rational Numbers
#' @description The mathematical set of rational numbers,
#' defined as the set of numbers that can be written as a fraction of two integers. i.e.
#' \deqn{\\{\frac{p}{q} \ : \ p,q \ \in \ Z, \ q \ne 0 \\}}{p/q : p,q \epsilon Z, q != 0}
#' where \eqn{Z} is the set of integers.
#' @details The `$contains` method does not work for the set of Rationals as it is notoriously
#' difficult/impossible to find an algorithm for determining if any given number is rational or not.
#' Furthermore, computers must truncate all irrational numbers to rational numbers.
#' @family special sets
#' @export
Rationals <- R6Class("Rationals",
  inherit = SpecialSet,
  public = list(
    #' @description Create a new `Rationals` object.
    #' @return A new `Rationals` object.
    #' @param lower numeric. Where to start the set. Advised to ignore, used by child-classes.
    #' @param upper numeric. Where to end the set. Advised to ignore, used by child-classes.
    #' @param type character Set closure type. Advised to ignore, used by child-classes.
    initialize = function(lower = -Inf, upper = Inf, type = "()") {
      super$initialize(lower = lower, upper = upper, type = type, class = "numeric")
    }
  )
)

#' @name PosRationals
#' @title Set of Positive Rational Numbers
#' @description The mathematical set of positive rational numbers,
#' defined as the set of numbers that can be written as a fraction of two integers and are non-negative. i.e.
#' \deqn{\\{\frac{p}{q} \ : \ p,q \ \in \ Z, \ p/q \ge 0, \ q \ne 0\\}}{p/q : p,q \epsilon Z, p/q \ge 0, q != 0}
#' where \eqn{Z} is the set of integers.
#' @details The `$contains` method does not work for the set of Rationals as it is notoriously
#' difficult/impossible to find an algorithm for determining if any given number is rational or not.
#' Furthermore, computers must truncate all irrational numbers to rational numbers.
#' @family special sets
#' @export
PosRationals <- R6Class("PosRationals",
  inherit = Rationals,
  public = list(
    #' @description Create a new `PosRationals` object.
    #' @return A new `PosRationals` object.
    #' @param zero logical. If TRUE, zero is included in the set.
    initialize = function(zero = FALSE) {
      if (zero) private$.zero <- TRUE
      super$initialize(lower = 0, type = ifelse(zero, "[)", "()"))
    }
  )
)

#' @name NegRationals
#' @title Set of Negative Rational Numbers
#' @description The mathematical set of negative rational numbers,
#' defined as the set of numbers that can be written as a fraction of two integers and are non-positive. i.e.
#' \deqn{\\{\frac{p}{q} \ : \ p,q \ \in \ Z, \ p/q \le 0, \ q \ne 0\\}}{p/q : p,q \epsilon Z, p/q \le 0, q != 0}
#' where \eqn{Z} is the set of integers.
#' @details The `$contains` method does not work for the set of Rationals as it is notoriously
#' difficult/impossible to find an algorithm for determining if any given number is rational or not.
#' Furthermore, computers must truncate all irrational numbers to rational numbers.
#' @family special sets
#' @export
NegRationals <- R6Class("NegRationals",
  inherit = Rationals,
  public = list(
    #' @description Create a new `NegRationals` object.
    #' @return A new `NegRationals` object.
    #' @param zero logical. If TRUE, zero is included in the set.
    initialize = function(zero = FALSE) {
      if (zero) private$.zero <- TRUE
      super$initialize(upper = 0, type = ifelse(zero, "(]", "()"))
    }
  )
)

#' @name Reals
#' @title Set of Real Numbers
#' @description The mathematical set of real numbers,
#' defined as the union of the set of rationals and irrationals. i.e.
#' \deqn{I \cup Q}{I U Q}
#' where \eqn{I} is the set of irrationals and \eqn{Q} is the set of rationals.
#' @family special sets
#' @export
Reals <- R6Class("Reals",
  inherit = SpecialSet,
  public = list(
    #' @description Create a new `Reals` object.
    #' @return A new `Reals` object.
    #' @param lower numeric. Where to start the set. Advised to ignore, used by child-classes.
    #' @param upper numeric. Where to end the set. Advised to ignore, used by child-classes.
    #' @param type character Set closure type. Advised to ignore, used by child-classes.
    initialize = function(lower = -Inf, upper = Inf, type = "()") {
      super$initialize(lower = lower, upper = upper, type = type, class = "numeric")
    }
  )
)

#' @name PosReals
#' @title Set of Positive Real Numbers
#' @description The mathematical set of positive real numbers,
#' defined as the union of the set of positive rationals and positive irrationals. i.e.
#' \deqn{I^+ \cup Q^+}{I+ U Q+}
#' where \eqn{I^+}{I+} is the set of positive irrationals and \eqn{Q^+}{Q+} is the set of positive rationals.
#' @family special sets
#' @export
PosReals <- R6Class("PosReals",
  inherit = Reals,
  public = list(
    #' @description Create a new `PosReals` object.
    #' @return A new `PosReals` object.
    #' @param zero logical. If TRUE, zero is included in the set.
    initialize = function(zero = FALSE) {
      if (zero) private$.zero <- TRUE
      super$initialize(lower = 0, type = ifelse(zero, "[)", "()"))
    }
  )
)

#' @name NegReals
#' @title Set of Negative Real Numbers
#' @description The mathematical set of negative real numbers,
#' defined as the union of the set of negative rationals and negative irrationals. i.e.
#' \deqn{I^- \cup Q^-}{I- U Q-}
#' where \eqn{I^-}{I-} is the set of negative irrationals and \eqn{Q^-}{Q-} is the set of negative rationals.
#' @family special sets
#' @export
NegReals <- R6Class("NegReals",
  inherit = Reals,
  public = list(
    #' @description Create a new `NegReals` object.
    #' @return A new `NegReals` object.
    #' @param zero logical. If TRUE, zero is included in the set.
    initialize = function(zero = FALSE) {
      if (zero) private$.zero <- TRUE
      super$initialize(upper = 0, type = ifelse(zero, "(]", "()"))
    }
  )
)


#' @name ExtendedReals
#' @title Set of Extended Real Numbers
#' @description The mathematical set of extended real numbers,
#' defined as the union of the set of reals with \eqn{\pm\infty}{±\infty}. i.e.
#' \deqn{R \cup \\{-\infty, \infty\\}}{R U {-\infty, \infty}}
#' where \eqn{R} is the set of reals.
#' @family special sets
#' @export
ExtendedReals <- R6Class("ExtendedReals",
  inherit = Reals,
  public = list(
    #' @description Create a new `ExtendedReals` object.
    #' @return A new `ExtendedReals` object.
    initialize = function() {
      super$initialize(type = "[]")
    }
  )
)
