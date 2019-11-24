#---------------------------------------------
# Documentation
#---------------------------------------------
#' @name Tuple
#' @title Mathematical Tuple
#'
#' @description A general Tuple object for mathematical tuples, inheriting from `Set`.
#' @return R6 object of class Tuple inheriting from [Set].
#' @template Set
#' @templateVar constructor Tuple$new(..., universe = NULL)
#' @templateVar arg1 `...` \tab ANY \tab Elements in the tuple. \cr
#' @templateVar arg2 `universe` \tab Set \tab Optional universe that the Tuple lives in.
#' @templateVar constructorDets Tuples are constructed by elements of any types (including R6 classes). The optional `universe` argument is useful for taking the complement of the `Tuple`. If a universe isn't given then [Reals] is assumed.
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
NULL
#---------------------------------------------
# Definition and Construction
#---------------------------------------------
Tuple <- R6::R6Class("Tuple", inherit = Set)

#---------------------------------------------
# Public Methods
#---------------------------------------------
Tuple$set("public","equals",function(x, all = FALSE){
  x <- listify(x)

  ret = sapply(x, function(el){
    if(!inherits(el, "R6"))
      return(FALSE)

    if(testFuzzy(el)){
      if(all(el$membership() == 1))
        el = as.Tuple(el)
    }

    if(testInterval(el) & !testMessage(as.Tuple(el)))
      el = as.Tuple(el)
    else if(testConditionalSet(el))
      return(FALSE)

    if(el$length != self$length)
      return(FALSE)

    if(class(el$elements) == "list" | class(self$elements) == "list"){
      ret = TRUE
      for(i in el$length){
        if(el$elements[[i]] != self$elements[[i]]){
          ret = FALSE
          break()
        }
      }
    } else
      ret = suppressWarnings(all(el$elements == self$elements))

    return(ret)
  })

  returner(ret, all)
})

Tuple$set("public","isSubset",function(x, proper = FALSE, all = FALSE){
  x <- listify(x)

  ret = sapply(x, function(el){
    if(!inherits(el, "R6"))
      return(FALSE)

    if(testFuzzy(el)){
      if(all(el$membership() == 1))
        el = as.Tuple(el)
    }

    if(testInterval(el) & !testMessage(as.Tuple(el)))
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
})

#---------------------------------------------
# Private Fields
#---------------------------------------------
Tuple$set("private",".type","()")

#---------------------------------------------
# Coercions
#---------------------------------------------
#' @title Coercion to R6 Tuple
#' @description Coerces objects to R6 Tuples
#' @param object object to coerce
#' @export
as.Tuple <- function(object){
  UseMethod("as.Tuple",object)
}
#' @rdname as.Tuple
#' @export
as.Tuple.numeric <- function(object){
  Tuple$new(object)
}
#' @rdname as.Tuple
#' @export
as.Tuple.list <- function(object){
  return(Tuple$new(unlist(object)))
}
#' @rdname as.Tuple
#' @export
as.Tuple.matrix <- function(object){
  return(apply(object,2,function(x) Tuple$new(x)))
}
#' @rdname as.Tuple
#' @export
as.Tuple.FuzzySet <- function(object){
  return(Tuple$new(object$support()))
}
#' @rdname as.Tuple
#' @export
as.Tuple.Set <- function(object){
  return(Tuple$new(object$elements))
}
#' @rdname as.Tuple
#' @export
as.Tuple.Interval <- function(object){
  if(any(is.nan(object$elements))){
    message("Interval cannot be coerced to Tuple.")
    return(object)
  } else {
    return(Tuple$new(object$elements))
  }
}
