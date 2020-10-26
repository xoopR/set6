#' @name FuzzySet
#' @title Mathematical Fuzzy Set
#' @family sets
#' @description A general FuzzySet object for mathematical fuzzy sets, inheriting from `Set`.
#'
#' @details
#' Fuzzy sets generalise standard mathematical sets to allow for fuzzy relationships. Whereas a
#' standard, or crisp, set assumes that an element is either in a set or not, a fuzzy set allows
#' an element to be in a set to a particular degree, known as the membership function, which
#' quantifies the inclusion of an element by a number in \[0, 1\]. Thus a (crisp) set is a
#' fuzzy set where all elements have a membership equal to \eqn{1}. Similarly to [Set]s, elements
#' must be unique and the ordering does not matter, to establish order and non-unique elements,
#' [FuzzyTuple]s can be used.
#'
#' @examples
#' # Different constructors
#' FuzzySet$new(1, 0.5, 2, 1, 3, 0)
#' FuzzySet$new(elements = 1:3, membership = c(0.5, 1, 0))
#'
#' # Crisp sets are a special case FuzzySet
#' # Note membership defaults to full membership
#' FuzzySet$new(elements = 1:5) == Set$new(1:5)
#'
#' f <- FuzzySet$new(1, 0.2, 2, 1, 3, 0)
#' f$membership()
#' f$alphaCut(0.3)
#' f$core()
#' f$inclusion(0)
#' f$membership(0)
#' f$membership(1)
#' @export
FuzzySet <- R6Class("FuzzySet",
  inherit = Set,
  public = list(
    #' @description Create a new `FuzzySet` object.
    #' @return A new `FuzzySet` object.
    #' @param ... Alternating elements and membership, see details.
    #' @param elements Elements in the set, see details.
    #' @param membership Corresponding membership of the elements, see details.
    #' @param class Optional string naming a class that if supplied gives the set the `typed` property.
    #' @details
    #' `FuzzySet`s can be constructed in one of two ways, either by supplying the elements and their
    #' membership in alternate order, or by providing a list of elements to `elements` and a list of
    #' respective memberships to `membership`, see examples. If the `class` argument is non-`NULL`,
    #' then all elements will be coerced to the given class in construction, and if elements of a
    #' different class are added these will either be rejected or coerced.
    initialize = function(..., elements = NULL, membership = rep(1, length(elements)), class = NULL) {
      if (!is.null(elements) & !is.null(membership)) {
        elements <- listify(elements)
        membership <- as.numeric(membership)
        if (length(membership) == 1) {
          membership <- rep(membership, length(elements))
        }
      } else if (length(list(...)) != 0) {
        dots <- list(...)
        if (length(dots) %% 2) {
          stop("Every element needs a corresponding membership.")
        }
        elements <- dots[seq.int(1, length(dots), 2)]
        membership <- as.numeric(dots[seq.int(2, length(dots), 2)])
      }

      if (any(duplicated(elements)) & !testFuzzyTuple(self) & !testFuzzyMultiset(self)) {
        message("Duplicated elements dedicated, only the first element-membership pair is included.")
        membership <- membership[!duplicated(elements)]
        elements <- elements[!duplicated(elements)]
      }

      checkmate::assertNumeric(membership, lower = 0, upper = 1, any.missing = FALSE)

      super$initialize(elements = elements, class = class)

      if (self$length) {
        private$.membership <- as.list(membership)

        if (!testFuzzyTuple(self)) {
          private$.membership <- private$.membership[match(lapply(elements, as.character),
                                                           private$.str_elements)]
        }

        names(private$.membership) <- private$.str_elements
      }

      invisible(self)
    },

    #' @description
    #' Creates a printable representation of the object.
    #' @param n numeric. Number of elements to display on either side of ellipsis when printing.
    #' @return A character string representing the object.
    strprint = function(n = 2) {
      if (self$properties$empty) {
        if (useUnicode()) {
          return("\u2205")
        } else {
          return("{}")
        }
      } else {
        elements <- self$membership()

        if (self$length <= n * 2) {
          return(paste0(
            substr(self$type, 1, 1), paste0(names(elements), "(", elements, ")", collapse = ", "),
            substr(self$type, 2, 2)
          ))
        } else {
          return(paste0(substr(self$type, 1, 1), paste0(names(elements)[1:n], "(", elements[1:n], ")", collapse = ", "), ",...,",
            paste0(names(elements)[(self$length - n + 1):self$length], "(",
                   elements[(self$length - n + 1):self$length], ")",
              collapse = ", "
            ),
            substr(self$type, 2, 2),
            collapse = ", "
          ))
        }
      }
    },

    #' @description Returns the membership, i.e. value in \[0, 1\], of either the given element(s)
    #' or all elements in the fuzzy set.
    #' @param element element or list of element in the `set`, if `NULL` returns membership of all elements
    #' @details For `FuzzySet`s this is straightforward and returns the membership of the given element(s),
    #' however in `FuzzyTuple`s and `FuzzyMultiset`s when an element may be duplicated, the function returns the membership of
    #' all instances of the element.
    #' @return Value, or list of values, in \[0, 1\].
    #' @examples
    #' f = FuzzySet$new(1, 0.1, 2, 0.5, 3, 1)
    #' f$membership()
    #' f$membership(2)
    #' f$membership(list(1, 2))
    membership = function(element = NULL) {

      if (is.null(element)) {
        return(private$.membership)
      }

      element <- lapply(listify(element), as.character)
      ret <- numeric(length(element))
      mem <- private$.membership
      mtc <- match(element, names(mem))
      ret[!is.na(mtc)] <- mem[mtc][!is.na(mtc)]
      names(ret) <- element

      if (length(ret) == 1) ret <- ret[[1]]
      return(ret)
    },

    #' @description The alpha-cut of a fuzzy set is defined as the set
    #' \deqn{A_\alpha = \{x \epsilon F | m \ge \alpha\}}{A_\alpha = {x \epsilon F | m \ge \alpha}}
    #' where \eqn{x} is an element in the fuzzy set, \eqn{F}, and \eqn{m} is the corresponding membership.
    #' @param alpha numeric in \[0, 1\] to determine which elements to return
    #' @param strong logical, if `FALSE` (default) then includes elements greater than or equal to alpha, otherwise only strictly greater than
    #' @param create logical, if `FALSE` (default) returns the elements in the alpha cut, otherwise returns a crisp set of the elements
    #' @return Elements in [FuzzySet] or a [Set] of the elements.
    #' @examples
    #' f = FuzzySet$new(1, 0.1, 2, 0.5, 3, 1)
    #' # Alpha-cut
    #' f$alphaCut(0.5)
    #'
    #' # Strong alpha-cut
    #' f$alphaCut(0.5, strong = TRUE)
    #'
    #' # Create a set from the alpha-cut
    #' f$alphaCut(0.5, create = TRUE)
    alphaCut = function(alpha, strong = FALSE, create = FALSE) {

      els <- self$elements
      mem <- self$membership()

      if (strong) {
        mtc <- match(names(mem[mem > alpha]), private$.str_elements)
      } else {
        mtc <- match(names(mem[mem >= alpha]), private$.str_elements)
      }

      els <- els[mtc][!is.na(mtc)]


      if (create) {
        if (length(els) == 0) {
          return(Set$new())
        } else {
          return(Set$new(elements = els))
        }
      } else {
        if (length(els) == 0) {
          return(NULL)
        } else {
          return(unname(els))
        }
      }
    },

    #' @description The support of a fuzzy set is defined as the set of elements whose membership
    #' is greater than zero, or the strong alpha-cut with \eqn{\alpha = 0},
    #' \deqn{A_\alpha = \{x \epsilon F | m > 0\}}{A_\alpha = {x \epsilon F | m > 0}}
    #' where \eqn{x} is an element in the fuzzy set, \eqn{F}, and \eqn{m} is the corresponding
    #' membership.
    #' @param create logical, if `FALSE` (default) returns the support elements, otherwise returns a [Set] of the support elements
    #' @return Support elements in fuzzy set or a [Set] of the support elements.
    #' @examples
    #' f = FuzzySet$new(0.1, 0, 1, 0.1, 2, 0.5, 3, 1)
    #' f$support()
    #' f$support(TRUE)
    support = function(create = FALSE) {
      self$alphaCut(0, TRUE, create)
    },

    #' @description The core of a fuzzy set is defined as the set of elements whose membership is equal to one,
    #' or the alpha-cut with \eqn{\alpha = 1},
    #' \deqn{A_\alpha = \{x \epsilon F \ : \ m \ge 1\}}{A_\alpha = {x \epsilon F : m \ge 1}}
    #' where \eqn{x} is an element in the fuzzy set, \eqn{F}, and \eqn{m} is the corresponding membership.
    #' @param create logical, if `FALSE` (default) returns the core elements, otherwise returns a [Set] of the core elements
    #' @return Core elements in [FuzzySet] or a [Set] of the core elements.
    #' @examples
    #' f = FuzzySet$new(0.1, 0, 1, 0.1, 2, 0.5, 3, 1)
    #' f$core()
    #' f$core(TRUE)
    core = function(create = FALSE) {
      self$alphaCut(1, FALSE, create)
    },

    #' @description An element in a fuzzy set, with corresponding membership \eqn{m}, is:
    #' * Included - If \eqn{m = 1}
    #' * Partially Included - If \eqn{0 < m < 1}
    #' * Not Included - If \eqn{m = 0}
    #' @param element element or list of elements in fuzzy set for which to get the inclusion level
    #' @return One of: "Included", "Partially Included", "Not Included"
    #' @details For [FuzzySet]s this is straightforward and returns the inclusion level of the given element(s),
    #' however in [FuzzyTuple]s and [FuzzyMultiset]s when an element may be duplicated, the function returns the inclusion level of
    #' all instances of the element.
    #' @examples
    #' f = FuzzySet$new(0.1, 0, 1, 0.1, 2, 0.5, 3, 1)
    #' f$inclusion(0.1)
    #' f$inclusion(1)
    #' f$inclusion(3)
    inclusion = function(element) {

      member <- self$membership(element)
      ret <- as.list(rep("Partially Included", length(member)))

      ret[member %in% 0] <- "Not Included"
      ret[member %in% 1] <- "Fully Included"

      names(ret) <- names(member)
      if (length(ret) == 1) ret <- ret[[1]]
      return(ret)
    },

    #' @description Tests if two sets are equal.
    #' @details Two fuzzy sets are equal if they contain the same elements with the same memberships.
    #' Infix operators can be used for:
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
      if (all(self$membership() == 1)) {
        return(self$core(create = T)$equals(x))
      }

      x <- listify(x)

      ret <- sapply(x, function(el) {

        if (!testFuzzySet(el)) {
          return(FALSE)
        }

        elel <- unlist(lapply(el$elements, function(x) ifelse(testSet(x), x$strprint(), x)))
        selel <- unlist(lapply(self$elements, function(x) ifelse(testSet(x), x$strprint(), x)))

        el_mat <- matrix(c(elel, el$membership()), ncol = 2)[order(elel), ]
        self_mat <- matrix(c(selel, self$membership()), ncol = 2)[order(selel), ]

        ifelse(all(all.equal(el_mat, self_mat) == TRUE), TRUE, FALSE)
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
    isSubset = function(x, proper = FALSE, all = FALSE) {
      if (all(self$membership() == 1)) {
        return(self$core(create = T)$isSubset(x, proper = proper, all = all))
      }

      x <- listify(x)

      ret <- rep(FALSE, length(x))
      ind <- sapply(x, testFuzzySet)

      ret[ind] <- sapply(x[ind], function(el) {
        self_comp <- paste(self$elements, self$membership(), sep = ";")
        el_comp <- paste(el$elements, el$membership(), sep = ";")

        if (proper) {
          if (all(el_comp %in% self_comp) & !all(self_comp %in% el_comp)) {
            return(TRUE)
          } else {
            return(FALSE)
          }
        } else {
          if (all(el_comp %in% self_comp)) {
            return(TRUE)
          } else {
            return(FALSE)
          }
        }
      })

      returner(ret, all)
    }
  ),

  private = list(
    .type = "{}",
    .membership = 0,
    .traits = list(crisp = FALSE)
  )
)
