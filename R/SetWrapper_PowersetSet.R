#' @name PowersetSet
#' @template SetWrapper
#' @templateVar operation powerset
#' @templateVar class PowersetSet
#' @export
PowersetSet <- R6Class("PowersetSet",
  inherit = ProductSet,
  public = list(
    #' @description Create a new `PowersetSet` object. It is not recommended to construct this class directly.
    #' @param set [Set] to wrap.
    #' @return A new `PowersetSet` object.
    initialize = function(set) {

      cardinality <- set$properties$cardinality

      if (class(cardinality) != "character") {
        cardinality <- 2^cardinality
      } else if (cardinality == "Aleph0") {
        cardinality <- "Beth1"
      } else {
        cardinality <- paste0("Beth", as.numeric(substr(cardinality, 5, 100)) + 1)
      }

      super$initialize(
        setlist = list(set), lower = Set$new(), upper = set, type = "{}",
        cardinality = cardinality
      )
    },

    #' @template param_strprint
    #' @description Creates a printable representation of the object.
    #' @return A character string representing the object.
    strprint = function(n = 2) {
      if (useUnicode()) {
        paste0("\U2118(", self$wrappedSets[[1]]$strprint(n), ")")
      } else {
        paste0("2^", self$wrappedSets[[1]]$strprint(n))
      }
    },

    #' @description Tests if elements `x` are contained in `self`.
    #' @template param_xall
    #' @param bound logical
    #' @return If `all == TRUE` then returns `TRUE` if all `x` are contained in `self`, otherwise `FALSE`.
    #' If `all == FALSE` returns a vector of logicals corresponding to the length of `x`, representing
    #' if each is contained in `self`. If `bound == TRUE` then an element is contained in `self` if it
    #' is on or within the (possibly-open) bounds of `self`, otherwise `TRUE` only if the element is within
    #' `self` or the bounds are closed.
    contains = function(x, all = FALSE, bound = NULL) {
      self$wrappedSets[[1]]$isSubset(x, proper = FALSE, all = all)
    },

    #' @description Tests if `x` is a (proper) subset of `self`.
    #' @template param_xall
    #' @param proper logical. If `TRUE` tests for proper subsets.
    #' @return If `all == TRUE` then returns `TRUE` if all `x` are (proper) subsets of `self`, otherwise `FALSE`.
    #' If `all == FALSE` returns a vector of logicals corresponding to the length of `x`, representing
    #' if each is a (proper) subset of `self`.
    isSubset = function(x, proper = FALSE, all = FALSE) {
      x <- listify(x)

      ret <- sapply(x, function(el) {
        if (!inherits(el, "R6")) {
          return(FALSE)
        }

        if (self$equals(el)) {
          if (proper) {
            return(FALSE)
          } else {
            return(TRUE)
          }
        }

        if (getR6Class(el) == "PowersetSet") {
          return(self$wrappedSets[[1]]$isSubset(el$wrappedSets[[1]], proper = proper))
        }

        if (!(getR6Class(el) %in% c("Tuple", "Set", "Multiset"))) {
          return(FALSE)
        }

        if (!testSet(el$elements[[1]])) {
          return(FALSE)
        }

        all(self$wrappedSets[[1]]$isSubset(el$elements[[1]], proper = FALSE))
      })

      returner(ret, all)
    }
  )
)
