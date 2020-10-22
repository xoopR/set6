#' @name Multiset
#' @title Mathematical Multiset
#' @family sets
#'
#' @description A general Multiset object for mathematical Multisets, inheriting from `Set`.
#'
#' @details
#' Multisets are generalisations of sets that allow an element to be repeated. They can be thought
#' of as [Tuple]s without ordering.
#'
#' @examples
#' # Multiset of integers
#' Multiset$new(1:5)
#'
#' # Multiset of multiple types
#' Multiset$new("a", 5, Set$new(1), Multiset$new(2))
#'
#' # Each Multiset has properties and traits
#' t <- Multiset$new(1, 2, 3)
#' t$traits
#' t$properties
#'
#' # Elements can be duplicated
#' Multiset$new(2, 2) != Multiset$new(2)
#'
#' # Ordering does not matter
#' Multiset$new(1, 2) == Multiset$new(2, 1)
#' @export
Multiset <- R6Class("Multiset",
  inherit = Set,
  public = list(
    #' @description Tests if two sets are equal.
    #' @param x [Set] or vector of [Set]s.
    #' @param all logical. If `FALSE` tests each `x` separately. Otherwise returns `TRUE` only if all `x` pass test.
    #' @details An object is equal to a Multiset if it contains all the same elements, and in the same order.
    #' Infix operators can be used for:
    #' \tabular{ll}{
    #' Equal \tab `==` \cr
    #' Not equal \tab `!=` \cr
    #' }
    #'
    #' @return If `all` is `TRUE` then returns `TRUE` if all `x` are equal to the Set, otherwise
    #' `FALSE`. If `all` is `FALSE` then returns a vector of logicals corresponding to each individual
    #' element of `x`.
    #'
    #' @examples
    #' Multiset$new(1,2) ==  Multiset$new(1,2)
    #' Multiset$new(1,2) != Multiset$new(1,2)
    #' Multiset$new(1,1) != Set$new(1,1)
    equals = function(x, all = FALSE) {
      x <- listify(x)

      ret <- sapply(x, function(el) {
        if (!inherits(el, "R6")) {
          return(FALSE)
        }

        if (testFuzzy(el)) {
          if (all(el$membership() == 1)) {
            el <- as.Multiset(el)
          }
        }

        if (testInterval(el) & class(try(as.Multiset(el), silent = TRUE))[1] != "try-error") {
          el <- as.Multiset(el)
        } else if (testConditionalSet(el)) {
          return(FALSE)
        }

        if (el$length != self$length) {
          return(FALSE)
        }

        ifelse(all.equal(el$multiplicity(), self$multiplicity()) == TRUE, TRUE, FALSE)
      })

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
    #' An object is a (proper) subset of a Multiset if it contains all (some) of the same elements,
    #' and in the same order.
    #'
    #' @return If `all` is `TRUE` then returns `TRUE` if all `x` are subsets of the Set, otherwise
    #' `FALSE`. If `all` is `FALSE` then returns a vector of logicals corresponding to each individual
    #' element of `x`.
    #' @examples
    #' Multiset$new(1,2,3) < Multiset$new(1,2,3,4)
    #' Multiset$new(1,3,2) < Multiset$new(1,2,3,4)
    #' Multiset$new(1,3,2,4) <= Multiset$new(1,2,3,4)
    isSubset = function(x, proper = FALSE, all = FALSE) {
      x <- listify(x)

      ret <- sapply(x, function(el) {
        if (!inherits(el, "R6")) {
          return(FALSE)
        }

        if (testFuzzy(el)) {
          if (all(el$membership() == 1)) {
            el <- as.Multiset(el)
          }
        }

        if (testInterval(el) & class(try(as.Multiset(el), silent = TRUE))[1] != "try-error") {
          el <- as.Multiset(el)
        }

        if (!testSet(el) | testFuzzy(el) | testConditionalSet(el) | testInterval(el)) {
          return(FALSE)
        }

        if (el$length > self$length) {
          return(FALSE)
        } else if (el$length == self$length) {
          if (!proper & el$equals(self)) {
            return(TRUE)
          } else {
            return(FALSE)
          }
        } else {
          mtc <- match(el$elements, self$elements)
          if (all(is.na(mtc))) {
            return(FALSE)
          } else {
            return(TRUE)
          }
        }
      })

      returner(ret, all)
    }
  ),

  private = list(
    .type = "{}"
  )
)
