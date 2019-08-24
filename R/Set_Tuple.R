#' @name Tuple
#' @title Tuple
#' @description Tuple Object
NULL
#' @export
Tuple <- R6::R6Class("Tuple", inherit = Set)

Tuple$set("public","equals",function(x){
  assertSet(x)

  if(suppressWarnings(all(x$elements == self$elements)))
    return(TRUE)
  else
    return(FALSE)
})

Tuple$set("public","powerSet",function(){
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
  return(Tuple$new(object$support))
}
#' @rdname as.Tuple
#' @export
as.Tuple.Set <- function(object){
  return(Tuple$new(object$elements))
}
