#' @name Complex
#' @title Set of Complex Numbers
#' @description The mathematical set of complex numbers,
#' defined as the the set of reals with possibly imaginary components. i.e.
#' \deqn{\\{a + bi \\ : \\ a,b \in R\\}}{{a + bi : a,b \epsilon R}}
#' where \eqn{R} is the set of reals.
#'
#' @details There is no inherent ordering in the set of complex numbers, hence only the `contains`
#' method is implemented here.
#'
#' @family special sets
#' @export
Complex <- R6Class("Complex",
  inherit = Set,
  public = list(
    #' @description Create a new `Complex` object.
    #' @return A new `Complex` object.
    initialize = function() {
      private$.properties <- Properties$new(closure = "open", cardinality = Inf)
      invisible(self)
    },

    #' @description Tests to see if \code{x} is contained in the Set.
    #'
    #' @param x any. Object or vector of objects to test.
    #' @param all logical. If `FALSE` tests each `x` separately. Otherwise returns `TRUE` only if all `x` pass test.
    #' @param bound logical.
    #'
    #' @details \code{x} can be of any type, including a Set itself. \code{x} should be a tuple if
    #' checking to see if it lies within a set of dimension greater than one. To test for multiple \code{x}
    #' at the same time, then provide these as a list.
    #'
    #' If `all = TRUE` then returns `TRUE` if all `x` are contained in the `Set`, otherwise
    #' returns a vector of logicals. For [Interval]s, `bound` is used to specify if elements lying on the
    #' (possibly open) boundary of the interval are considered contained (`bound = TRUE`) or not (`bound = FALSE`).
    #'
    #' @return If \code{all} is `TRUE` then returns `TRUE` if all elements of \code{x} are contained in the `Set`, otherwise
    #' `FALSE.` If \code{all} is `FALSE` then returns a vector of logicals corresponding to each individual
    #' element of \code{x}.
    #'
    #' The infix operator `%inset%` is available to test if `x` is an element in the `Set`,
    #' see examples.
    contains = function(x, all = FALSE, bound = NULL) {
      returner(
        x = sapply(x, is.complex),
        all = all
      )
    },

    #' @description Tests if two sets are equal.
    #' @param x [Set] or vector of [Set]s.
    #' @param all logical. If `FALSE` tests each `x` separately. Otherwise returns `TRUE` only if all `x` pass test.
    #' @return If `all` is `TRUE` then returns `TRUE` if all `x` are equal to the Set, otherwise
    #' `FALSE`. If `all` is `FALSE` then returns a vector of logicals corresponding to each individual
    #' element of `x`.
    #'
    #' Infix operators can be used for:
    #' \tabular{ll}{
    #' Equal \tab `==` \cr
    #' Not equal \tab `!=` \cr
    #' }
    #'
    #' @examples
    #' # Equals
    #' Set$new(1,2)$equals(Set$new(5,6))
    #' Set$new(1,2)$equals(Interval$new(1,2))
    #' Set$new(1,2) == Interval$new(1,2, class = "integer")
    #'
    #' # Not equal
    #' !Set$new(1,2)$equals(Set$new(1,2))
    #' Set$new(1,2) != Set$new(1,5)
    equals = function(x, all = FALSE) {
      ret <- sapply(listify(x), getR6Class) %in% "Complex"
      returner(ret, all)
    },

    #' @description  Test if one set is a (proper) subset of another
    #' @param x any. Object or vector of objects to test.
    #' @param proper logical. If `TRUE` tests for proper subsets.
    #' @param all logical. If `FALSE` tests each `x` separately. Otherwise returns `TRUE` only if all `x` pass test.
    #' @details If using the method directly, and not via one of the operators then the additional boolean
    #' argument `proper` can be used to specify testing of subsets or proper subsets. A Set is a proper
    #' subset of another if it is fully contained by the other Set (i.e. not equal to) whereas a Set is a
    #' (non-proper) subset if it is fully contained by, or equal to, the other Set.
    #'
    #' When calling `$isSubset` on objects inheriting from [Interval], the method treats the interval as if
    #' it is a [Set], i.e. ordering and class are ignored. Use `$isSubinterval` to test if one interval
    #' is a subinterval of another.
    #'
    #' Infix operators can be used for:
    #' \tabular{ll}{
    #' Subset \tab `<` \cr
    #' Proper Subset \tab `<=` \cr
    #' Superset \tab `>` \cr
    #' Proper Superset \tab `>=`
    #' }
    #'
    #' Every `Set` is a subset of a `Universal`. No `Set` is a super set of a `Universal`,
    #' and only a `Universal` is not a proper subset of a `Universal`.
    #'
    #' @return If `all` is `TRUE` then returns `TRUE` if all `x` are subsets of the Set, otherwise
    #' `FALSE`. If `all` is `FALSE` then returns a vector of logicals corresponding to each individual
    #' element of `x`.
    #' @examples
    #' Set$new(1,2,3)$isSubset(Set$new(1,2), proper = TRUE)
    #' Set$new(1,2) < Set$new(1,2,3) # proper subset
    #'
    #' c(Set$new(1,2,3), Set$new(1)) < Set$new(1,2,3) # not proper
    #' Set$new(1,2,3) <= Set$new(1,2,3) # proper
    isSubset = function(x, proper = FALSE, all = FALSE) {
      stop("Not implemented for complex sets.")
    },

    #' @description Creates a printable representation of the object.
    #' @param n numeric. Number of elements to display on either side of ellipsis when printing.
    #' @return A character string representing the object.
    strprint = function(n = 2) {
      setSymbol(getR6Class(self))
    }
  )
)
