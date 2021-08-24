#' @name ConditionalSet
#' @title Mathematical Set of Conditions
#' @description A mathematical set defined by one or more logical conditions.
#' @family sets
#'
#' @details
#' Conditional sets are a useful tool for symbolically defining possibly infinite sets. They can be combined
#' using standard 'and', `&`, and 'or', `|`, operators.
#'
#' @examples
#' # Set of Positive Naturals
#' s <- ConditionalSet$new(function(x) TRUE, argclass = list(x = PosNaturals$new()))
#' @export
ConditionalSet <- R6Class("ConditionalSet",
  inherit = Set,
  public = list(
    #' @description Create a new `ConditionalSet` object.
    #' @return A new `ConditionalSet` object.
    #' @param condition function. Defines the set, see details.
    #' @param argclass list. Optional list of sets that the function arguments live in, see details.
    #' @details The `condition` should be given as a function that when evaluated returns
    #' either `TRUE` or `FALSE`. Further constraints can be given by providing the universe of the
    #' function arguments as [Set]s, if these are not given then [Universal] is assumed.
    #' See examples. Defaults construct the Universal set.
    initialize = function(condition = function(x) TRUE, argclass = NULL) {
      if (!is.function(condition)) {
        stop("'condition' must be a function.")
      } else {
        lst <- as.list(seq_along(formals(condition)))
        names(lst) <- names(formals(condition))
        if (!checkmate::testLogical(do.call(condition, lst))) {
          stop("'condition' should result in a logical.")
        }
      }

      private$.condition <- condition
      private$.dimension <- length(formals(condition))

      if (!is.null(argclass)) {
        assertSetList(argclass)
      } else {
        argclass <- rep(list(Universal$new()), private$.dimension)
        names(argclass) <- names(formals(condition))
      }

      private$.argclass <- argclass

      private$.properties <- Properties$new()

      invisible(self)
    },

    #' @description Tests to see if \code{x} is contained in the Set.
    #'
    #' @param x any. Object or vector of objects to test.
    #' @param all logical. If `FALSE` tests each `x` separately. Otherwise returns `TRUE` only if all `x` pass test.
    #' @param bound ignored, added for consistency.
    #'
    #' @details \code{x} can be of any type, including a Set itself. \code{x} should be a tuple if
    #' checking to see if it lies within a set of dimension greater than one. To test for multiple \code{x}
    #' at the same time, then provide these as a list.
    #'
    #' If `all = TRUE` then returns `TRUE` if all `x` are contained in the `Set`, otherwise
    #' returns a vector of logicals.
    #'
    #' An element is contained in a `ConditionalSet` if it returns `TRUE` as an argument in the defining function.
    #' For sets that are defined with a function that takes multiple arguments, a [Tuple] should be
    #' passed to `x`.
    #'
    #' @return If \code{all} is `TRUE` then returns `TRUE` if all elements of \code{x} are contained in the `Set`, otherwise
    #' `FALSE.` If \code{all} is `FALSE` then returns a vector of logicals corresponding to each individual
    #' element of \code{x}.
    #'
    #' The infix operator `%inset%` is available to test if `x` is an element in the `Set`,
    #' see examples.
    #'
    #' @examples
    #' # Set of positives
    #' s = ConditionalSet$new(function(x) x > 0)
    #' s$contains(list(1,-1))
    #'
    #' # Set via equality
    #' s = ConditionalSet$new(function(x, y) x + y == 2)
    #' s$contains(list(Set$new(2, 0), Set$new(0, 2)))
    #'
    #' # Tuples are recommended when using contains as they allow non-unique elements
    #' s = ConditionalSet$new(function(x, y) x + y == 4)
    #' \dontrun{
    #' s$contains(Set$new(2, 2)) # Errors as Set$new(2,2) == Set$new(2)
    #' }
    #'
    #' # Set of Positive Naturals
    #' s = ConditionalSet$new(function(x) TRUE, argclass = list(x = PosNaturals$new()))
    #' s$contains(list(-2, 2))
    contains = function(x, all = FALSE, bound = NULL) {
      x <- listify(x)
      if (!testSetList(x)) {
        x <- as.Set(x)
      }
      # x <- assertSetList(listify(x),
      #                    "`x` should be a Set, Tuple, or list of Sets/Tuples.")

      ret <- sapply(seq_along(x), function(i) {
        els <- as.list(x[[i]]$elements)
        if (length(els) != length(self$class)) {
          stop(sprintf("Set is of length %s, length %s expected.", length(els), length(self$class)))
        }
        names(els) <- names(self$class)
        ret <- do.call(self$condition, els) & all(mapply(function(x, y) x$contains(y), self$class, els))
      })

      returner(ret, all)
    },

    #' @description Tests if two sets are equal.
    #' @details Two sets are equal if they contain the same elements. Infix operators can be used for:
    #' \tabular{ll}{
    #' Equal \tab `==` \cr
    #' Not equal \tab `!=` \cr
    #' }
    #' @param x [Set] or vector of [Set]s.
    #' @param all logical. If `FALSE` tests each `x` separately. Otherwise returns `TRUE` only if all `x` pass test.
    #' @return If `all` is `TRUE` then returns `TRUE` if all `x` are equal to the Set, otherwise
    #' `FALSE`. If `all` is `FALSE` then returns a vector of logicals corresponding to each individual
    #' element of `x`.
    equals = function(x, all = FALSE) {
      x <- listify(x)

      ret <- sapply(x, function(el) {
        if (!testConditionalSet(el)) {
          return(FALSE)
        }

        if (all(names(formals(el$condition)) == names(formals(self$condition))) &
          all(body(el$condition) == body(self$condition)) &
          all(unlist(lapply(el$class, getR6Class)) == unlist(lapply(self$class, getR6Class)))) {
          return(TRUE)
        }


        if (!all(rsapply(self$class, "strprint") == rsapply(el$class, "strprint"))) {
          return(FALSE)
        } else {
          sclass <- self$class
          elclass <- el$class
          if (length(sclass) < length(elclass)) {
            sclass <- rep(sclass, length(elclass))[seq_along(elclass)]
          }
          if (length(elclass) < length(sclass)) {
            elclass <- rep(elclass, length(sclass))[seq_along(sclass)]
          }

          elcond <- body(el$condition)
          if (!all(names(sclass) == names(elclass))) {
            for (i in seq_along(names(elclass))) {
              elcond <- gsub(names(elclass)[[i]], names(sclass)[[i]], elcond, fixed = TRUE)
            }
          }
          if (all(elcond == as.character(body(self$condition)))) {
            return(TRUE)
          } else {
            return(FALSE)
          }
        }
      })

      returner(ret, all)
    },

    #' @description
    #' Creates a printable representation of the object.
    #' @param n ignored, added for consistency.
    #' @return A character string representing the object.
    strprint = function(n = NULL) {
      if (useUnicode()) {
        sep <- " \u2208 "
      } else {
        sep <- " in "
      }

      if (body(self$condition) == TRUE) {
        paste0("{",
          paste(names(self$class), sapply(self$class, function(x) x$strprint()),
                sep = sep, collapse = ", "), "}")
      } else {
        paste0("{", paste0(
          paste(names(self$class), sapply(self$class, function(x) x$strprint()),
                sep = sep, collapse = ", "), " : ",
          paste0(deparse(body(self$condition), width.cutoff = 500L), collapse = ""), "}"))
      }

    },

    #' @description See `strprint`.
    #' @param n ignored, added for consistency.
    summary = function(n = NULL) {
      self$print()
    },

    #' @description Currently undefined for `ConditionalSet`s.
    #' @param x ignored, added for consistency.
    #' @param proper ignored, added for consistency.
    #' @param all ignored, added for consistency.
    isSubset = function(x, proper = FALSE, all = FALSE) {
      message("isSubset is currently undefined for conditional sets.")
      return(FALSE)
    }
  ),

  active = list(
    #' @field condition
    #' Returns the condition defining the ConditionalSet.
    condition = function() {
      return(private$.condition)
    },

    #' @field class
    #' Returns `argclass`, see `$new`.
    class = function() {
      return(private$.argclass)
    },

    #' @field elements
    #' Returns `NA`.
    elements = function() {
      return(NA)
    }
  ),

  private = list(
    .condition = NULL,
    .argclass = NULL,
    .lower = NA,
    .upper = NA
  )
)
