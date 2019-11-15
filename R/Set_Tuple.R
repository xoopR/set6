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
#' @seealso
#' [Set]
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

Tuple$set("public","equals",function(x){
  assertSet(x)

  if(x$length != self$length)
    return(FALSE)

  if(class(x$elements) == "list" | class(self$elements) == "list"){
    ret = TRUE
    for(i in 1:length(x)){
      if(x$elements[[i]] != self$elements[[i]]){
        ret = FALSE
        break()
      }
    }
  } else
    ret = suppressWarnings(all(x$elements == self$elements))

  return(ret)
})

Tuple$set("public","powerset",function(){
  elements <- self$elements
  y = Vectorize(function(m) combn(elements, m),vectorize.args = c("m"))(1:(self$length-1))
  return(Set$new(Tuple$new(), unlist(lapply(y, as.Tuple)), self))
})

Tuple$set("public","isSubset",function(x, proper = FALSE){
  if(!testTuple(x))
    return(FALSE)

  if(x$length > self$length)
    return(FALSE)
  else if(x$length == self$length){
    if(!proper & x$equals(self))
      return(TRUE)
    else
      return(FALSE)
  } else{
    mtc <- match(x$elements, self$elements)
    if(all(is.na(mtc)))
      return(FALSE)

    if(all(order(mtc) == (1:length(x$elements))))
      return(TRUE)
    else
      return(FALSE)
  }
})

Tuple$set("private",".type","()")
Tuple$set("private",".properties",list(crisp = TRUE))

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
