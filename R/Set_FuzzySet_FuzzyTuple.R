#' @name FuzzyTuple
#' @title Mathematical Fuzzy Tuple
#' @description A general FuzzyTuple object for mathematical fuzzy tuples, inheriting from `FuzzySet`.
#' @family sets
#'
#' @details
#' Fuzzy tuples generalise standard mathematical tuples to allow for fuzzy relationships. Whereas a
#' standard, or crisp, tuple assumes that an element is either in a tuple or not, a fuzzy tuple allows
#' an element to be in a tuple to a particular degree, known as the membership function, which
#' quantifies the inclusion of an element by a number in \[0, 1\]. Thus a (crisp) tuple is a
#' fuzzy tuple where all elements have a membership equal to \eqn{1}. Similarly to [Tuple]s, elements
#' do not need to be unique and the ordering does matter, [FuzzySet]s are special cases where the ordering
#' does not matter and elements must be unique.
#'
#' @examples
#' # Different constructors
#' FuzzyTuple$new(1, 0.5, 2, 1, 3, 0)
#' FuzzyTuple$new(elements = 1:3, membership = c(0.5, 1, 0))
#'
#' # Crisp sets are a special case FuzzyTuple
#' # Note membership defaults to full membership
#' FuzzyTuple$new(elements = 1:5) == Tuple$new(1:5)
#'
#' f = FuzzyTuple$new(1, 0.2, 2, 1, 3, 0)
#' f$membership()
#' f$alphaCut(0.3)
#' f$core()
#' f$inclusion(0)
#' f$membership(0)
#' f$membership(1)
#'
#' # Elements can be duplicated, and with different memberships,
#' #  although this is not necessarily sensible.
#' FuzzyTuple$new(1, 0.1, 1, 1)
#'
#' # More important is ordering.
#' FuzzyTuple$new(1, 0.1, 2, 0.2) != FuzzyTuple$new(2, 0.2, 1, 0.1)
#' FuzzySet$new(1, 0.1, 2, 0.2) == FuzzySet$new(2, 0.2, 1, 0.1)
#'
#' @export
FuzzyTuple <- R6Class("FuzzyTuple", inherit = FuzzySet,
  public  = list(
    #' @description Tests if two sets are equal.
    #' @details Two fuzzy sets are equal if they contain the same elements with the same memberships and
    #' in the same order. Infix operators can be used for:
    #' \tabular{ll}{
    #' Equal \tab `==` \cr
    #' Not equal \tab `!=` \cr
    #' }
    #' @param x [Set] or vector of [Set]s.
    #' @param all logical. If `FALSE` tests each `x` separately. Otherwise returns `TRUE` only if all `x` pass test.
    #' @return If `all` is `TRUE` then returns `TRUE` if all `x` are equal to the Set, otherwise
    #' `FALSE`. If `all` is `FALSE` then returns a vector of logicals corresponding to each individual
    #' element of `x`.
    equals = function(x, all = FALSE){
      if(all(self$membership() == 1))
        return(self$core(create = T)$equals(x))

      x <- listify(x)

      ret = sapply(x, function(el){
        if(!testFuzzySet(el))
          return(FALSE)

        if(el$length != self$length)
          return(FALSE)

        if(class(el$elements) == "list" | class(self$elements) == "list"){
          elel = unlist(lapply(el$elements, function(x) ifelse(testSet(x), x$strprint(), x)))
          selel = unlist(lapply(self$elements, function(x) ifelse(testSet(x), x$strprint(), x)))
        } else {
          elel = el$elements
          selel = self$elements
        }

        return(suppressWarnings(all(elel == selel) & all(el$membership() == self$membership())))
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
    isSubset = function(x, proper = FALSE, all = FALSE){
      if(all(self$membership() == 1))
        return(self$core(create = T)$isSubset(x, proper = proper, all = all))

      x <- listify(x)

      ret = rep(FALSE, length(x))
      ind = sapply(x, testFuzzyTuple)

      ret[ind] = sapply(x[ind], function(el){
        self_comp <- paste(self$elements, self$membership(), sep=";")
        el_comp <- paste(el$elements, el$membership(), sep=";")

        if(el$length > self$length)
          return(FALSE)
        else if(el$length == self$length){
          if(!proper & el$equals(self))
            return(TRUE)
          else
            return(FALSE)
        } else{
          mtc <- match(el_comp, self_comp)
          if(all(is.na(mtc)))
            return(FALSE)

          if(all(order(mtc) == (1:length(el$elements))))
            return(TRUE)
          else
            return(FALSE)
        }
      })

      returner(ret, all)
    },

    #' @description The alpha-cut of a fuzzy set is defined as the set
    #' \deqn{A_\alpha = \{x \epsilon F | m \ge \alpha\}}{A_\alpha = {x \epsilon F | m \ge \alpha}}
    #' where \eqn{x} is an element in the fuzzy set, \eqn{F}, and \eqn{m} is the corresponding membership.
    #' @param alpha numeric in \[0, 1\] to determine which elements to return
    #' @param strong logical, if `FALSE` (default) then includes elements greater than or equal to alpha, otherwise only strictly greater than
    #' @param create logical, if `FALSE` (default) returns the elements in the alpha cut, otherwise returns a crisp set of the elements
    #' @return Elements in [FuzzyTuple] or a [Set] of the elements.
    alphaCut = function(alpha, strong = FALSE, create = FALSE){
      if(strong)
        els <- self$elements[self$membership() > alpha]
      else
        els <- self$elements[self$membership() >= alpha]

      if(create){
        if(length(els) == 0)
          return(Set$new())
        else
          return(Tuple$new(els))
      } else{
        if(length(els) == 0)
          return(NULL)
        else
          return(els)
      }
    }
  ),

  private = list(
    .type = "()"
  )
)
