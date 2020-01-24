#---------------------------------------------
# Documentation
#---------------------------------------------
#' @name Tuple
#' @title Mathematical Tuple
#'
#' @description A general Tuple object for mathematical tuples, inheriting from `Set`.
#' @return R6 object of class Tuple inheriting from [Set].
#' @template Set
#' @templateVar constructor Tuple$new(..., universe = UniversalSet$new())
#' @templateVar arg1 `...` \tab ANY \tab Elements in the tuple. \cr
#' @templateVar arg2 `universe` \tab Set \tab Universe that the Tuple lives in, default [UniversalSet]. \cr
#' @templateVar arg3 `elements` \tab list \tab Alternative constructor that may be more efficienct if passing objects of multiple classes. \cr
#' @templateVar arg4 `class` \tab character \tab Optional string naming a class that if supplied gives the set the `typed` property. \cr
#' @templateVar constructorDets Tuples are constructed by elements of any types (including R6 classes). The `universe` argument is useful for taking the absolute complement of the `Tuple`. If a universe isn't given then [UniversalSet] is assumed. If the `class` argument is non-NULL, then all elements will be coerced to the given class in construction, and if elements of a different class are added these will either be rejected or coerced.
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
Tuple <- R6Class("Tuple", inherit = Set)

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
})

#---------------------------------------------
# Private Fields
#---------------------------------------------
Tuple$set("private",".type","()")

#---------------------------------------------
# Coercions
#---------------------------------------------
#' @rdname as.Set
#' @aliases as.Tuple
#' @export
as.Tuple <- function(object){
  UseMethod("as.Tuple",object)
}
#' @rdname as.Set
#' @export
as.Tuple.numeric <- function(object){
  Tuple$new(object)
}
#' @rdname as.Set
#' @export
as.Tuple.list <- function(object){
  return(lapply(object, function(x) Tuple$new(x)))
}
#' @rdname as.Set
#' @export
as.Tuple.matrix <- function(object){
  return(apply(object,2,function(x) Tuple$new(x)))
}
#' @rdname as.Set
#' @export
as.Tuple.data.frame <- as.Tuple.matrix
#' @rdname as.Set
#' @export
as.Tuple.FuzzySet <- function(object){
  return(Tuple$new(elements = object$support()))
}
#' @rdname as.Set
#' @export
as.Tuple.Set <- function(object){
  return(Tuple$new(elements = object$elements))
}
#' @rdname as.Set
#' @export
as.Tuple.Interval <- function(object){
  if(any(is.nan(object$elements))){
    stop("Interval cannot be coerced to Tuple.")
  } else {
    return(Tuple$new(object$elements))
  }
}
#' @rdname as.Set
#' @export
as.Tuple.ConditionalSet <- function(object){
  stop("ConditionalSet cannot be coerced to Tuple.")
}
