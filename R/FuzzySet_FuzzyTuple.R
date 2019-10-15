#' @name FuzzyTuple
#' @title FuzzyTuple
#' @description FuzzyTuple Object
NULL
#' @export
FuzzyTuple <- R6::R6Class("FuzzyTuple", inherit = FuzzySet)

FuzzyTuple$set("public","powerSet",function(){
  y = Vectorize(function(m) combn(self$elements, m),vectorize.args = c("m"))(1:(self$length-1))
  if(checkmate::testList(y))
    y = lapply(y, function(z) apply(z, 2, function(x){
      FuzzyTuple$new(elements = x, membership = self$membership(x))
    }))
  else
    y = apply(y, 1, function(x){
      FuzzyTuple$new(elements = x, membership = self$membership(x))
    })
  return(Set$new(Tuple$new(), y, self))
})
FuzzyTuple$set("public","equals",function(x){
  if(!testFuzzyTuple(x))
    return(FALSE)

  if(suppressWarnings(all(x$elements == self$elements) &
                      all(x$membership() == self$membership())))
    return(TRUE)
  else
    return(FALSE)
})
FuzzyTuple$set("public","isSubset",function(x, proper = FALSE){
  if(!testFuzzyTuple(x))
    return(FALSE)

  self_comp <- paste(self$elements, self$membership(), sep=";")
  x_comp <- paste(x$elements, x$membership(), sep=";")

  if(x$length > self$length)
    return(FALSE)
  else if(x$length == self$length){
    if(!proper & x$equals(self))
      return(TRUE)
    else
      return(FALSE)
  } else{
    mtc <- match(x_comp, self_comp)
    if(all(is.na(mtc)))
      return(FALSE)

    if(all(order(mtc) == (1:length(x$elements))))
      return(TRUE)
    else
      return(FALSE)
  }
})
FuzzyTuple$set("public","alphaCut",function(alpha, strong = FALSE, create = FALSE){
  if(strong)
    els <- self$elements[self$membership() > alpha]
  else
    els <- self$elements[self$membership() >= alpha]

  if(create){
    if(length(els) == 0)
      return(Empty$new())
    else
      return(Tuple$new(els))
  } else{
    if(length(els) == 0)
      return(NULL)
    else
      return(els)
  }
})

FuzzyTuple$set("private",".type","()")
FuzzyTuple$set("private",".membership", 0)
FuzzyTuple$set("private",".properties",list(crisp = FALSE))

#' @title Coercion to R6 FuzzyTuple
#' @description Coerces objects to R6 FuzzyTuple
#' @param object object to coerce
#' @export
as.FuzzyTuple <- function(object){
  UseMethod("as.FuzzyTuple",object)
}
#' @rdname as.FuzzyTuple
#' @export
as.FuzzyTuple.numeric <- function(object){
  return(FuzzyTuple$new(elements = as.numeric(object), membership = names(object)))
}
#' @rdname as.FuzzyTuple
#' @export
as.FuzzyTuple.list <- function(object){
  return(FuzzyTuple$new(elements = unlist(object, use.names = FALSE), membership = names(object)))
}
#' @rdname as.FuzzyTuple
#' @export
as.FuzzyTuple.matrix <- function(object){
  return(FuzzyTuple$new(elements = object[,1], membership = object[,2]))
}
#' @rdname as.FuzzyTuple
#' @export
as.FuzzyTuple.data.table <- function(object){
  return(as.FuzzyTuple(as.matrix(object)))
}
#' @rdname as.FuzzyTuple
#' @export
as.FuzzyTuple.data.frame <- function(object){
  return(as.FuzzyTuple(as.matrix(object)))
}
#' @rdname as.FuzzyTuple
#' @export
as.FuzzyTuple.Set <- function(object){
  return(FuzzyTuple$new(elements = object$elements))
}
#' @rdname as.FuzzyTuple
#' @export
as.FuzzyTuple.FuzzyTuple <- function(object){
  return(object)
}
#' @rdname as.FuzzyTuple
#' @export
as.FuzzyTuple.FuzzySet <- function(object){
  return(FuzzyTuple$new(elements = object$elements, membership = object$membership()))
}

