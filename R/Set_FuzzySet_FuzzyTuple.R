#---------------------------------------------
# Documentation
#---------------------------------------------
#' @name FuzzyTuple
#' @title Mathematical Fuzzy Tuple
#' @description A general FuzzyTuple object for mathematical fuzzy tuples, inheriting from `FuzzySet`.
#' @return R6 object of class FuzzyTuple inheriting from [FuzzySet].
#' @template Set
#' @templateVar constructor FuzzyTuple$new(..., elements = NULL, membership = rep(1, length(elements)), class = NULL)
#' @templateVar arg1 `...` \tab ANY \tab Alternating elements and membership, see constructor details. \cr
#' @templateVar arg2 `elements` \tab ANY \tab Elements in the set, see constructor details. \cr
#' @templateVar arg3 `membership` \tab numeric \tab Corresponding membership of the elements, see constructor details. \cr
#' @templateVar arg4 `class` \tab character \tab Optional string naming a class that if supplied gives the set the `typed` property. \cr
#' @templateVar constructorDets `FuzzyTuple`s can be constructed in one of two ways, either by supplying the elements and their membership in alternate order, or by providing a list of elements to `elements` and a list of respective memberships to `membership`, see examples. If the `class` argument is non-NULL, then all elements will be coerced to the given class in construction, and if elements of a different class are added these will either be rejected or coerced.
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

#---------------------------------------------
# Public Methods
#---------------------------------------------
FuzzyTuple$set("public","equals",function(x, all = FALSE){
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
})
FuzzyTuple$set("public","isSubset",function(x, proper = FALSE, all = FALSE){
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

#---------------------------------------------
# Private Fields
#---------------------------------------------
FuzzyTuple$set("private",".type","()")

#---------------------------------------------
# Coercions
#---------------------------------------------
#' @rdname as.FuzzySet
#' @aliases as.FuzzyTuple
#' @export
as.FuzzyTuple <- function(object){
  UseMethod("as.FuzzyTuple",object)
}
#' @rdname as.FuzzySet
#' @export
as.FuzzyTuple.numeric <- function(object){
  FuzzyTuple$new(elements = object[seq.int(1,length(object),2)],
               membership = as.numeric(object[seq.int(2,length(object),2)]))
}
#' @rdname as.FuzzySet
#' @export
as.FuzzyTuple.list <- function(object){
  return(FuzzyTuple$new(elements = object$elements, membership = object$membership))
}
#' @rdname as.FuzzySet
#' @export
as.FuzzyTuple.matrix <- function(object){
  return(FuzzyTuple$new(elements = object[,1], membership = object[,2]))
}
#' @rdname as.FuzzySet
#' @export
as.FuzzyTuple.data.frame <- function(object){
  if(all(c("elements", "membership") %in% colnames(object)))
    return(FuzzyTuple$new(elements = object$elements, membership = object$membership))
  else
    return(FuzzyTuple$new(elements = object[,1], membership = object[,2]))
}
#' @rdname as.FuzzySet
#' @export
as.FuzzyTuple.Set <- function(object){
  return(FuzzyTuple$new(elements = object$elements))
}
#' @rdname as.FuzzySet
#' @export
as.FuzzyTuple.FuzzySet <- function(object){
  return(FuzzyTuple$new(elements = object$elements, membership = object$membership()))
}
#' @rdname as.FuzzySet
#' @export
as.FuzzyTuple.Interval <- function(object){
  ifnerror(as.Set.Interval(object), error = "stop", errormsg = "Interval cannot be coerced to FuzzyTuple.")
}
#' @rdname as.FuzzySet
#' @export
as.FuzzyTuple.ConditionalSet <- function(object){
  stop("ConditionalSet cannot be coerced to FuzzyTuple.")
}
