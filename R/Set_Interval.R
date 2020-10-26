#' @name Interval
#' @title Mathematical Finite or Infinite Interval
#' @description A general Interval object for mathematical intervals, inheriting from [Set]. Intervals
#' may be open, closed, or half-open; as well as bounded above, below, or not at all.
#' @family sets
#'
#' @details
#' The Interval class can be used for finite or infinite intervals, but often Sets will be preferred for
#' integer intervals over a finite continuous range.
#'
#' @examples
#' # Set of Reals
#' Interval$new()
#'
#' # Set of Integers
#' Interval$new(class = "integer")
#'
#' # Half-open interval
#' i <- Interval$new(1, 10, "(]")
#' i$contains(c(1, 10))
#' i$contains(c(1, 10), bound = TRUE)
#'
#' # Equivalent Set and Interval
#' Set$new(1:5) == Interval$new(1, 5, class = "integer")
#'
#' # SpecialSets can provide more efficient implementation
#' Interval$new() == ExtendedReals$new()
#' Interval$new(class = "integer", type = "()") == Integers$new()
#' @export
Interval <- R6Class("Interval",
  inherit = Set,
  public = list(
    #' @description Create a new `Interval` object.
    #' @details `Interval`s are constructed by specifying the `Interval` limits, the boundary type,
    #' the class, and the possible universe. The `universe` differs from `class` as it is primarily used
    #' for the [setcomplement] method. Whereas `class` specifies if the interval takes integers or
    #' numerics, the `universe` specifies what range the interval could take.
    #' @param lower numeric. Lower limit of the interval.
    #' @param upper numeric. Upper limit of the interval.
    #' @param type character. One of: '()', '(]', '[)', '[]', which specifies if interval is open, left-open, right-open, or closed.
    #' @param class character. One of: 'numeric', 'integer', which specifies if interval is over the Reals or Integers.
    #' @param universe Set. Universe that the interval lives in, default [Reals].
    #' @return A new `Interval` object.
    initialize = function(lower = -Inf, upper = Inf, type = c("[]", "(]", "[)", "()"),
                          class = "numeric", universe = ExtendedReals$new()) {

      type <- match.arg(type)
      checkmate::assert(lower <= upper)
      checkmate::assertChoice(class, c("numeric", "integer"))

      if (getR6Class(self) != "SpecialSet") {
        assertSet(universe)
        private$.universe <- universe
      }


      if (lower == upper) {
        private$.class <- "integer"
        private$.type <- "[]"
      } else {
        private$.class <- class
        private$.type <- type
      }

      private$.lower <- lower
      private$.upper <- upper

      if (private$.class == "numeric") {
        cardinality <- "b1"
      } else {
        if (lower == -Inf | upper == Inf) {
          cardinality <- "a0"
        } else {
          cardinality <- length(seq.int(lower, upper, 1))
        }
      }

      closure <- switch(type,
        "[]" = "closed",
        "()" = "open",
        "half-open"
      )

      private$.properties <- Properties$new(closure, cardinality)

      assertSubset(universe, self, errormsg = "interval is not contained in the given universe")
      invisible(self)
    },

    #' @description Creates a printable representation of the object.
    #' @param ... ignored, added for consistency.
    #' @return A character string representing the object.
    strprint = function(...) {

      inf <- ifelse(self$lower == -Inf & useUnicode(), "-\u221E", self$lower)
      sup <- ifelse(self$upper == Inf & useUnicode(), "+\u221E", self$upper)

      if (self$class == "integer") {
        return(paste0("{", inf, ",...,", sup, "}"))
      } else {
        return(paste0(substr(self$type, 1, 1), inf, ",", sup, substr(self$type, 2, 2)))
      }
    },

    #' @description Tests if two sets are equal.
    #' @param x [Set] or vector of [Set]s.
    #' @param all logical. If `FALSE` tests each `x` separately. Otherwise returns `TRUE` only if all `x` pass test.
    #' @details Two `Interval`s are equal if they have the same: class, type, and bounds.
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
    #' Interval$new(1,5) == Interval$new(1,5)
    #' Interval$new(1,5, class = "integer") != Interval$new(1,5,class="numeric")
    equals = function(x, all = FALSE) {
      if (class(try(as.Set(self), silent = TRUE))[1] != "try-error") {
        return(super$equals(x, all))
      }

      x <- listify(x)

      ret <- sapply(x, function(el) {
        if (!testInterval(el)) {
          return(FALSE)
        }

        if (el$lower == self$lower & el$upper == self$upper & el$type == self$type & el$class == self$class) {
          return(TRUE)
        } else {
          return(FALSE)
        }
      })

      returner(ret, all)
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
    contains = function(x, all = FALSE, bound = FALSE) {
      if (is.null(x)) {
        return(TRUE)
      }

      x <- sapply(listify(x), function(y) ifelse(inherits(y, c("numeric", "integer")), y, NaN))

      if (all & any(is.nan(x))) {
        return(FALSE)
      } else if (all(is.nan(x))) {
        return(rep(FALSE, length(x)))
      }

      if (all) {
        IntervalContainsAll(
          x = x, inf = self$lower, sup = self$upper,
          min = self$min, max = self$max,
          bound = bound, class_str = self$class
        )
      } else {
        IntervalContains(
          x = x, inf = self$lower, sup = self$upper,
          min = self$min, max = self$max,
          bound = bound, class_str = self$class
        )
      }
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
    #' When calling `isSubset` on objects inheriting from `Interval`, the method treats the interval as if
    #' it is a [Set], i.e. ordering and class are ignored. Use `isSubinterval` to test if one interval
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
    #' @return If `all` is `TRUE` then returns `TRUE` if all `x` are subsets of the Set, otherwise
    #' `FALSE`. If `all` is `FALSE` then returns a vector of logicals corresponding to each individual
    #' element of `x`.
    #'
    #' @examples
    #' Interval$new(1,3) < Interval$new(1,5)
    #' Set$new(1,3) < Interval$new(0,5)
    isSubset = function(x, proper = FALSE, all = FALSE) {
      # if this Interval can be coerced to a Set then uses Set method instead
      if (class(try(as.Set(self), silent = TRUE))[1] != "try-error") {
        return(super$isSubset(x, proper, all))
      }

      x <- listify(x)
      ret <- logical(length(x))
      for (i in seq_along(x)) {
        if (testSet(x[[i]])) {
          if (testEmpty(x[[i]])) {
            ret[i] <- TRUE
          } else if (!testInterval(x[[i]])) {
            ret[i] <- self$contains(x[[i]]$elements, all = TRUE, bound = FALSE)
          } else if (!(self$class == "integer" & x[[i]]$class == "numeric")) {
            if (proper) {
              ret[i] <- (x[[i]]$min > self$min & x[[i]]$max <= self$max) |
                (x[[i]]$min >= self$min & x[[i]]$max < self$max) |
                (x[[i]]$min >= self$min & x[[i]]$max <= self$max & x[[i]]$class == "integer" & self$class == "numeric")
            } else {
              ret[i] <- x[[i]]$min >= self$min & x[[i]]$max <= self$max
            }
          }
        }
      }

      returner(ret, all)
    },

    #' @description  Test if one interval is a (proper) subinterval of another
    #' @param x `Set` or `list`
    #' @param proper If `TRUE` then tests if `x` is a proper subinterval (i.e. subinterval and not equal to)
    #' of `self`, otherwise `FALSE` tests if `x` is a (non-proper) subinterval.
    #' @param all If `TRUE` then returns `TRUE` if all `x` are subintervals, otherwise returns a vector of logicals.
    #' @details If `x` is a [Set] then will be coerced to an [Interval] if possible. `$isSubinterval` differs
    #' from `$isSubset` in that ordering and class are respected in `$isSubinterval`. See examples for
    #' a clearer illustration of the difference.
    #' @return If `all` is `TRUE` then returns `TRUE` if all `x` are subsets of the Set, otherwise
    #' `FALSE`. If `all` is `FALSE` then returns a vector of logicals corresponding to each individual
    #' element of `x`.
    #' @examples
    #' Interval$new(1,3)$isSubset(Set$new(1,2)) # TRUE
    #' Interval$new(1,3)$isSubset(Set$new(2, 1)) # TRUE
    #' Interval$new(1,3, class = "integer")$isSubinterval(Set$new(1, 2)) # TRUE
    #' Interval$new(1,3)$isSubinterval(Set$new(1, 2)) # FALSE
    #' Interval$new(1,3)$isSubinterval(Set$new(2, 1)) # FALSE
    #'
    #' Reals$new()$isSubset(Integers$new()) # TRUE
    #' Reals$new()$isSubinterval(Integers$new()) # FALSE
    isSubinterval = function(x, proper = FALSE, all = FALSE) {
      if (class(try(as.Tuple(self), silent = TRUE))[1] != "try-error") {
        return(as.Tuple(self)$isSubset(x, proper, all))
      }

      x <- listify(x)

      ret <- sapply(x, function(el) {
        if (!testSet(el)) {
          return(FALSE)
        }

        if (el$properties$empty) {
          return(TRUE)
        }

        if (testFuzzy(el) | testConditionalSet(el)) {
          return(FALSE)
        }

        el <- try(as.Interval(el), silent = TRUE)

        if (class(el)[1] == "try-error") {
          return(FALSE)
        }

        if (el$class != self$class) {
          return(FALSE)
        }

        if (proper) {
          if ((el$lower > self$lower & el$upper <= self$upper) | (el$lower >= self$lower & el$upper < self$upper)) {
            return(TRUE)
          } else {
            return(FALSE)
          }
        } else {
          if (el$lower >= self$lower & el$upper <= self$upper) {
            return(TRUE)
          } else {
            return(FALSE)
          }
        }
      })

      returner(ret, all)
    }
  ),

  active = list(
    #' @field length
    #' If the `Interval` is countably finite then returns the number of elements in the `Interval`,
    #' otherwise `Inf`. See the cardinality property for the type of infinity.
    length = function() {
      if (self$properties$countability == "countably finite") {
        return(length(self$elements))
      } else {
        return(Inf)
      }
    },

    #' @field elements
    #' If the `Interval` is finite then returns all elements in the `Interval`, otherwise `NA`.
    elements = function() {
      if (self$properties$countability == "countably finite") {
        return(seq.int(self$min, self$max, 1))
      } else {
        return(NA)
      }
    }
  ),

  private = list(
    .elements = NA
  )
)
