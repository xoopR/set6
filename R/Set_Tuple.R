#' @name Tuple
#' @title Mathematical Tuple
#' @family sets
#'
#' @description A general Tuple object for mathematical tuples, inheriting from `Set`.
#'
#' @details
#' Tuples are similar to sets, except that they drop the constraint for elements to be unique, and
#' ordering in a tuple does matter. Tuples are useful for methods including [contains] that may
#' require non-unique elements. They are also the return type of the product of sets. See examples.
#'
#' @examples
#' # Tuple of integers
#' Tuple$new(1:5)
#'
#' # Tuple of multiple types
#' Tuple$new("a", 5, Set$new(1), Tuple$new(2))
#'
#' # Each Tuple has properties and traits
#' t = Tuple$new(1,2,3)
#' t$traits
#' t$properties
#'
#' # Elements can be duplicated
#' Tuple$new(2, 2) != Tuple$new(2)
#'
#' # Ordering does matter
#' Tuple$new(1, 2) != Tuple$new(2, 1)
#'
#' @export
Tuple <- R6Class("Tuple", inherit = Set,
  public = list(
    #' @description Tests if two sets are equal.
    #' @param x [Set] or vector of [Set]s.
    #' @param all logical. If `FALSE` tests each `x` separately. Otherwise returns `TRUE` only if all `x` pass test.
    #' @details An object is equal to a Tuple if it contains all the same elements, and in the same order.
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
    #' Tuple$new(1,2) ==  Tuple$new(1,2)
    #' Tuple$new(1,2) != Tuple$new(1,2)
    #' Tuple$new(1,1) != Set$new(1,1)
    equals = function(x, all = FALSE){
      x <- listify(x)

      ret = sapply(x, function(el){
        if(!inherits(el, "R6"))
          return(FALSE)

        if(testFuzzy(el)){
          if(all(el$membership() == 1))
            el = as.Tuple(el)
        }

        if(testInterval(el) & class(try(as.Tuple(el), silent = TRUE))[1] != "try-error")
          el = as.Tuple(el)
        else if(testConditionalSet(el))
          return(FALSE)

        if(el$length != self$length)
          return(FALSE)

        if(class(el$elements) == "list" | class(self$elements) == "list"){
          ret = TRUE
          for(i in 1:el$length){
            elel = el$elements[[i]]
            selel = self$elements[[i]]

            if(testSet(elel))
              elel = elel$strprint()
            if(testSet(selel))
              selel = selel$strprint()

            if(elel != selel){
              ret = FALSE
              break()
            }
          }
        } else
          ret = suppressWarnings(all(el$elements == self$elements))

        return(ret)
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
    #' When calling [isSubset] on objects inheriting from [Interval], the method treats the interval as if
    #' it is a [Set], i.e. ordering and class are ignored. Use [isSubinterval] to test if one interval
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
    #' An object is a (proper) subset of a Tuple if it contains all (some) of the same elements,
    #' and in the same order.
    #'
    #' @return If `all` is `TRUE` then returns `TRUE` if all `x` are subsets of the Set, otherwise
    #' `FALSE`. If `all` is `FALSE` then returns a vector of logicals corresponding to each individual
    #' element of `x`.
    #' @examples
    #' Tuple$new(1,2,3) < Tuple$new(1,2,3,4)
    #' Tuple$new(1,3,2) < Tuple$new(1,2,3,4)
    isSubset = function(x, proper = FALSE, all = FALSE){
      x <- listify(x)

      ret = sapply(x, function(el){
        if(!inherits(el, "R6"))
          return(FALSE)

        if(testFuzzy(el)){
          if(all(el$membership() == 1))
            el = as.Tuple(el)
        }

        if(testInterval(el) & class(try(as.Tuple(el), silent = TRUE))[1] != "try-error")
          el = as.Tuple(el)

        if(!testSet(el) | testFuzzy(el) | testConditionalSet(el) | testInterval(el))
          return(FALSE)

        if(el$length > self$length)
          return(FALSE)
        else if(el$length == self$length){
          if(!proper & el$equals(self))
            return(TRUE)
          else
            return(FALSE)
        } else{
          mtc <- match(el$elements, self$elements)
          if(all(is.na(mtc)))
            return(FALSE)

          if(all(order(mtc) == (1:length(el$elements))))
            return(TRUE)
          else
            return(FALSE)
        }
      })

      returner(ret, all)
    }
  ),

  private = list(
    .type = "()"
  )
)
