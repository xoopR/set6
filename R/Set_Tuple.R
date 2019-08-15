#' @name Tuple
#' @title Tuple
#' @description Tuple Object
NULL
#' @export
Tuple <- R6::R6Class("Tuple", inherit = Set)

Tuple$set("public","equals",function(x){
  assertSet(x)

  if(suppressWarnings(all(x$elements() == self$elements())))
    return(TRUE)
  else
    return(FALSE)
})

Tuple$set("public","powerSet",function(){
  elements <- self$elements()
  y = Vectorize(function(m) combn(elements, m),vectorize.args = c("m"))(1:(self$length()-1))
  return(Set$new(Tuple$new(), unlist(lapply(y, as.Tuple)), self))
})

Tuple$set("public","isSubset",function(x, proper = FALSE){
  assertTuple(x)

  self_comp <- paste(self$elements(), 1:self$length(), sep = ";")
  x_comp <- paste(x$elements(), 1:x$length(), sep = ";")

  if(proper){
    if(all(x_comp %in% self_comp) & !all(self_comp %in% x_comp))
      return(TRUE)
    else
      return(FALSE)
  }else{
    if(all(x_comp %in% self_comp))
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
