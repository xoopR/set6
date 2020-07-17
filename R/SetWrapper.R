#' @name SetWrapper
#' @rdname SetWrapper
#' @title Abstract SetWrapper Class
#' @description This class should not be constructed directly. Parent class to `SetWrapper`s.
SetWrapper <- R6Class("SetWrapper",
  inherit = Set,
  public = list(
    #' @description Create a new `SetWrapper` object. It is not recommended to construct this class directly.
    #' @param setlist List of [Set]s to wrap.
    #' @param lower [Set]. Lower bound of wrapper.
    #' @param upper [Set]. Upper bound of wrapper.
    #' @param type character. Closure type of wrapper.
    #' @param class character. Ignored.
    #' @param cardinality character or integer. Cardinality of wrapper.
    #' @return A new `SetWrapper` object.
    initialize = function(setlist, lower = NULL, upper = NULL, type = NULL,
                          class = NULL, cardinality) {
      if (getR6Class(self) == "SetWrapper") {
        stop(paste(getR6Class(self), "is an abstract class that can't be initialized."))
      }

      private$.wrappedSets <- assertSetList(setlist)

      if (!is.null(lower)) private$.lower <- lower
      if (!is.null(upper)) private$.upper <- upper
      if (!is.null(type)) private$.type <- type

      class <- rsapply(setlist, class, active = TRUE)
      if (length(unique(class)) == 1) {
        private$.class <- unique(class)
      }

      private$.properties <- Properties$new(closure = "closed", cardinality)

      invisible(self)
    },

    #' @description Tests if `x` is equal to `self`.
    #' @template param_xall
    #' @return If `all == TRUE` then returns `TRUE` if all `x` are equal to `self`, otherwise `FALSE`.
    #' If `all == FALSE` returns a vector of logicals corresponding to the length of `x`, representing
    #' if each is equal to `self`.
    equals = function(x, all = FALSE) {
      x <- listify(x)

      ret <- sapply(x, function(el) {
        if (getR6Class(el) != getR6Class(self)) {
          return(FALSE)
        }

        if (length(self$wrappedSets) != length(el$wrappedSets)) {
          return(FALSE)
        }

        ret <- TRUE
        for (i in 1:length(self$wrappedSets)) {
          if (self$wrappedSets[[i]] != el$wrappedSets[[i]]) {
            ret <- FALSE
            break()
          }
        }

        return(ret)
      })

      returner(ret, all)
    },

    #' @description Tests if `x` is a (proper) subset of `self`.
    #' @template param_subset
    #' @return If `all == TRUE` then returns `TRUE` if all `x` are (proper) subsets of `self`, otherwise `FALSE`.
    #' If `all == FALSE` returns a vector of logicals corresponding to the length of `x`, representing
    #' if each is a (proper) subset of `self`.
    isSubset = function(x, proper = FALSE, all = FALSE) {
      message("isSubset currently not implemented for this wrapper.")
    }
  ),

  active = list(
    #' @field wrappedSets
    #' Returns the list of `Set`s that are wrapped in the given wrapper.
    wrappedSets = function() {
      return(private$.wrappedSets)
    }
  ),

  private = list(
    .wrappedSets = list()
  )
)
