#---------------------------------------------
# Documentation
#---------------------------------------------
#' @name FuzzyTuple
#' @title Mathematical Fuzzy Tuple
#' @description A general FuzzyTuple object for mathematical fuzzy tuples, inheriting from `FuzzySet`.
#' @return R6 object of class FuzzyTuple inheriting from [FuzzySet].
#' @template Set
#' @templateVar constructor FuzzyTuple$new(..., elements = NULL, membership = rep(1, length(elements)))
#' @templateVar arg1 `...` \tab ANY \tab Alternating elements and membership, see constructor details. \cr
#' @templateVar arg2 `elements` \tab ANY \tab Elements in the set, see constructor details. \cr
#' @templateVar arg3 `membership` \tab numeric \tab Corresponding membership of the elements, see constructor details. \cr
#' @templateVar constructorDets `FuzzyTuple`s can be constructed in one of two ways, either by supplying the elements and their membership in alternate order, or by providing a list of elements to `elements` and a list of respective memberships to `membership`, see examples.
#' @templateVar meth1 **Fuzzy Methods** \tab **Link** \cr
#' @templateVar meth2 `membership(element = NULL)` \tab [membership] \cr
#' @templateVar meth3 `alphaCut(alpha, strong = FALSE, create = FALSE)` \tab [alphaCut] \cr
#' @templateVar meth4 `support(create = FALSE)` \tab [support] \cr
#' @templateVar meth5 `core(create = FALSE)` \tab [core] \cr
#' @templateVar meth6 `inclusion(element)` \tab [inclusion] \cr
#' @templateVar meth7  \tab \cr \tab \cr \tab \cr
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
#' @seealso
#' [Tuple], [FuzzySet]
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
NULL
#---------------------------------------------
# Definition and Construction
#---------------------------------------------
FuzzyTuple <- R6::R6Class("FuzzyTuple", inherit = FuzzySet)

FuzzyTuple$set("public","powerset",function(){
  y = Vectorize(function(m) combn(self$elements, m),vectorize.args = c("m"))(1:(self$length-1))
  if(checkmate::testList(y))
    y = lapply(y, function(z) apply(z, 2, function(x){
      FuzzyTuple$new(elements = x, membership = self$membership(x))
    }))
  else
    y = apply(y, 1, function(x){
      FuzzyTuple$new(elements = x, membership = self$membership(x))
    })
  return(Set$new(Set$new(), y, self))
})
FuzzyTuple$set("public","equals",function(x){
  if(all(self$membership() == 1))
    return(self$core(create = T)$equals(x))

  if(!testFuzzyTuple(x))
    return(FALSE)

  if(x$length != self$length)
    return(FALSE)

  if(suppressWarnings(all(x$elements == self$elements) &
                      all(x$membership() == self$membership())))
    return(TRUE)
  else
    return(FALSE)
})
FuzzyTuple$set("public","isSubset",function(x, proper = FALSE){
  if(all(self$membership() == 1))
    return(self$core(create = T)$isSubset(x, proper = proper))

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
      return(Set$new())
    else
      return(Tuple$new(els))
  } else{
    if(length(els) == 0)
      return(NULL)
    else
      return(els)
  }
})
FuzzyTuple$set("public","complement",function(){
  FuzzyTuple$new(elements = self$elements, membership = 1 - self$membership())
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

