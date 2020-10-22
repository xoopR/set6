#' @name UniversalSet
#' @title Mathematical Universal Set
#' @description The `UniversalSet` is defined as the [Set] containing all possible elements.
#'
#' @details
#' The Universal set is the default universe to all sets, and is the largest possible set.
#' The Universal set contains every single possible element. We denote the Universal set with `V`
#' instead of `U` to avoid confusion with the union symbol. The Universal set cardinality is set to
#' `Inf` where we assume `Inf` is greater than any `Aleph` or `Beth` numbers. The Universal set is
#' also responsible for a few set paradoxes, to resolve these we use the following results:
#'
#' Let \eqn{V} be the universal set, \eqn{S} be any non-universal set, and \eqn{0} the empty set, then
#'
#' \deqn{V \cup S = V}{V or S = V}
#' \deqn{V \cap S = S}{V and S = S}
#' \deqn{S - V = 0}
#' \deqn{V^n = V}
#' \deqn{P(V) = V}
#'
#' @examples
#' u <- UniversalSet$new()
#' print(u)
#' u$contains(c(1, letters, TRUE, Set$new()), all = TRUE)
#' @export
UniversalSet <- R6::R6Class("UniversalSet",
  inherit = Set,
  public = list(
    #' @description Create a new `UniversalSet` object.
    #' @details The Universal set is the set containing every possible element.
    #' @return A new `UniversalSet` object.
    initialize = function() {
      warning("Deprecated. In the future please use Universal$new(). This will be removed in v0.4.0.")
      private$.properties <- Properties$new(closure = "open", cardinality = Inf)
      invisible(self)
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
      ret <- sapply(listify(x), getR6Class) %in% "UniversalSet"
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
    #' Every `Set` is a subset of a `UniversalSet`. No `Set` is a super set of a `UniversalSet`,
    #' and only a `UniversalSet` is not a proper subset of a `UniversalSet`.
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
      x <- listify(x)
      returner(
        x = sapply(x, inherits, what = "Set") & sapply(x, getR6Class) != "UniversalSet" |
          sapply(x, getR6Class) == "UniversalSet" & !proper,
        all = all
      )
    },

    #' @description Tests to see if \code{x} is contained in the Set.
    #'
    #' @param x any. Object or vector of objects to test.
    #' @param all logical. If `FALSE` tests each `x` separately. Otherwise returns `TRUE` only if all `x` pass test.
    #' @param bound ignored.
    #'
    #' @details \code{x} can be of any type, including a Set itself. \code{x} should be a tuple if
    #' checking to see if it lies within a set of dimension greater than one. To test for multiple \code{x}
    #' at the same time, then provide these as a list.
    #'
    #' If using the method directly, and not via one of the operators then the additional boolean
    #' arguments `all` and `bound`. If `all = TRUE` then returns `TRUE` if all `x` are contained in the `Set`, otherwise
    #' returns a vector of logicals. For [Interval]s, `bound` is used to specify if elements lying on the
    #' (possibly open) boundary of the interval are considered contained (`bound = TRUE`) or not (`bound = FALSE`).
    #'
    #' @return If \code{all} is `TRUE` then returns `TRUE` if all elements of \code{x} are contained in the `Set`, otherwise
    #' `FALSE.` If \code{all} is `FALSE` then returns a vector of logicals corresponding to each individual
    #' element of \code{x}.
    #'
    #' The infix operator `%inset%` is available to test if `x` is an element in the `Set`,
    #' see examples.
    #'
    #' Every element is contained within the Universal set.
    #'
    #' @examples
    #' s = Set$new(1:5)
    #'
    #' # Simplest case
    #' s$contains(4)
    #' 8 %inset% s
    #'
    #' # Test if multiple elements lie in the set
    #' s$contains(4:6, all = FALSE)
    #' s$contains(4:6, all = TRUE)
    #'
    #' # Check if a tuple lies in a Set of higher dimension
    #' s2 = s * s
    #' s2$contains(Tuple$new(2,1))
    #' c(Tuple$new(2,1), Tuple$new(1,7), 2) %inset% s2
    contains = function(x, all = FALSE, bound = NULL) {
      returner(
        x = rep(TRUE, length(listify(x))),
        all = all
      )
    },

    #' @description Creates a printable representation of the object.
    #' @param n numeric. Number of elements to display on either side of ellipsis when printing.
    #' @return A character string representing the object.
    strprint = function(n = NULL) {
      setSymbol("universal")
    }
  ),

  private = list(
    .elements = NA
  )
)
