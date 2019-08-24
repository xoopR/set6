#' @name FuzzySet
#' @title FuzzySet
#' @description FuzzySet Object
NULL
#' @export
FuzzySet <- R6::R6Class("FuzzySet", inherit = Set)
FuzzySet$set("public","initialize",function(..., elements = NULL, membership = rep(1, length(elements)), dimension = 1, universe = NULL){
  if(!is.null(elements) & !is.null(membership)){
    membership <- as.numeric(membership)
  } else if(length(list(...)) != 0){
    dots <- list(...)
    if(length(dots) == 1 & is.list(dots))
      dots <- dots[[1]]
    if(length(dots)%%2)
      stop("Every element needs a corresponding membership.")
    elements <- dots[seq.int(1,length(dots),2)]
    membership <- as.numeric(dots[seq.int(2,length(dots),2)])
  }

  if(any(duplicated(elements))){
    message("Duplicated elements dedicated, only the first element-membership pair is included.")
    membership <- membership[!duplicated(elements)]
    elements <- elements[!duplicated(elements)]
  }

  checkmate::assertNumeric(membership, lower = 0, upper = 1, any.missing = FALSE)
  private$.membership <- membership

  super$initialize(elements, dimension = dimension, universe = universe)
  invisible(self)
})

FuzzySet$set("public","strprint",function(){
  return(paste0("{",paste0(self$elements,"(",self$membership(),")", collapse = ", "),"}"))
})
FuzzySet$set("public","membership",function(element = NULL){
  if(is.null(element))
    return(private$.membership)
  else{
    x <- match(element, self$elements)
    if(is.na(x)){
      message(sprintf("%s is not in this fuzzy set.", element))
      return(NA)
    }else
      return(private$.membership[match(element, self$elements)])
  }
})
FuzzySet$set("public","isEmpty",function(){
  if(all(self$membership() == 0))
    return(TRUE)
  else
    return(FALSE)
})
FuzzySet$set("public","alphaCut",function(alpha, strong = FALSE, create = FALSE){
  if(strong)
    els <- self$elements[self$membership() > alpha]
  else
    els <- self$elements[self$membership() >= alpha]

  if(create){
    if(length(els) == 0)
      return(Empty$new())
    else
      return(Set$new(els))
  } else{
    if(length(els) == 0)
      return(NULL)
    else
      return(els)
  }
})
FuzzySet$set("public","support",function(create = FALSE){
  self$alphaCut(0, TRUE, create)
})
FuzzySet$set("public","core",function(create = FALSE){
  self$alphaCut(1, FALSE, create)
})
FuzzySet$set("public","inclusion",function(element){
  member <- self$membership()[self$elements %in% element]
  if(length(member) == 0)
    return("Not Included")

  if(member == 1)
    return("Fully Included")
  else if(member == 0)
    return("Not Included")
  else
    return("Partially Included")
})
FuzzySet$set("public","equals",function(x){
  if(!testFuzzySet(x))
    return(FALSE)

  x_mat = matrix(c(x$elements,x$membership()),ncol=2)[order(x$elements),]
  self_mat = matrix(c(self$elements,self$membership()),ncol=2)[order(self$elements),]

  if(any(dim(x_mat) != dim(self_mat)))
    return(FALSE)

  if(all(x_mat == self_mat))
    return(TRUE)
  else
    return(FALSE)
})
FuzzySet$set("public","complement",function(){
  private$.membership <- 1 - self$membership()
  slf <- self$clone(deep = TRUE)
  private$.membership <- 1 - self$membership()
  return(slf)
})
FuzzySet$set("public","powerSet",function(){
  y = Vectorize(function(m) combn(self$elements, m),vectorize.args = c("m"))(1:(self$length-1))
  if(checkmate::testList(y))
    y = lapply(y, function(z) apply(z, 2, function(x){
      FuzzySet$new(elements = x, membership = self$membership(x))
    }))
  else
    y = apply(y, 1, function(x){
      FuzzySet$new(elements = x, membership = self$membership(x))
    })
  return(Set$new(Set$new(), y, self))
})
FuzzySet$set("public","isSubset",function(x, proper = FALSE){
  if(!testFuzzySet(x))
    return(FALSE)

  self_comp <- paste(self$elements, self$membership(), sep=";")
  x_comp <- paste(x$elements, x$membership(), sep=";")

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

FuzzySet$set("private",".type","{}")
FuzzySet$set("private",".membership", 0)


#' @title Coercion to R6 FuzzySet
#' @description Coerces objects to R6 FuzzySet
#' @param object object to coerce
#' @export
as.FuzzySet <- function(object){
  UseMethod("as.FuzzySet",object)
}
#' @rdname as.FuzzySet
#' @export
as.FuzzySet.numeric <- function(object){
  return(FuzzySet$new(elements = as.numeric(object), membership = names(object)))
}
#' @rdname as.FuzzySet
#' @export
as.FuzzySet.list <- function(object){
  return(FuzzySet$new(elements = unlist(object, use.names = FALSE), membership = names(object)))
}
#' @rdname as.FuzzySet
#' @export
as.FuzzySet.matrix <- function(object){
  return(FuzzySet$new(elements = object[,1], membership = object[,2]))
}
#' @rdname as.FuzzySet
#' @export
as.FuzzySet.data.table <- function(object){
  return(as.FuzzySet(as.matrix(object)))
}
#' @rdname as.FuzzySet
#' @export
as.FuzzySet.data.frame <- function(object){
  return(as.FuzzySet(as.matrix(object)))
}
