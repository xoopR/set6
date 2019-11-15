#' @template SetWrapper
#' @templateVar operation union
#' @templateVar class UnionSet
#' @templateVar constructor UnionSet$new(setlist, lower = NULL, upper = NULL, type = NULL)
#' @templateVar arg1 `setlist` \tab list \tab List of sets to wrap. \cr
#'
#' @export
NULL
UnionSet <- R6::R6Class("UnionSet", inherit = SetWrapper)
UnionSet$set("public", "initialize", function(setlist, lower = NULL, upper = NULL, type = NULL){
  if(is.null(lower)) lower = min(unlist(sapply(setlist, function(x) x$lower)))
  if(is.null(upper)) upper = max(unlist(sapply(setlist, function(x) x$upper)))
  if(is.null(type)) type = "{}"
  super$initialize(setlist = setlist, lower = lower, upper = upper, type = type)
})
UnionSet$set("public","strprint",function(n = 2){
  if(use_unicode())
    collapse = " \u222A "
  else
    collapse = " U "
  paste0("{",paste(lapply(c(self$wrappedSets, self$wrappedIntervals), function(x) x$strprint(n)),
                   collapse = collapse),"}")
})
UnionSet$set("active","elements",function(){
  els = unique(unlist(sapply(self$wrappedSets, function(x) x$elements)))
  if(any(is.nan(els)))
    return(NaN)
  else
    return(els)
})
UnionSet$set("public","liesInSet",function(x, all = FALSE, bound = FALSE){
  apply(do.call(rbind,
                lapply(self$wrappedSets, function(y) y$liesInSet(x, all = all, bound = bound))),
        2, any)
})


#' @name union
#' @param x,y Set
#' @title Union of Two Sets
#' @return An object inheriting from `Set` containing the union of elements in both `x` and `y`.
#' @description Returns the union of two objects inheriting from class `Set`.
#' @details The union of two sets, \eqn{X, Y}, is defined as the set of elements that exist
#' in one or both sets,
#' \deqn{U = \{x, y : x \epsilon X or y \epsilon Y\}}{U = {x, y : x \epsilon X or y \epsilon Y}}
#'
#' The union of two [ConditionalSet]s is defined by combining their defining functions by an
#' 'or', `|`, operator. See examples.
#'
#' @examples
#' # union of two sets
#'
#' Set$new(-2:4) + Set$new(2:5)
#' union(Set$new(1,4,"a"), Set$new("a", 6))
#'
#' # union of two intervals
#'
#' Interval$new(1, 10) + Interval$new(5, 15)
#' Interval$new(1, 2, type = "()") + Interval$new(2, 3, type = "(]")
#' Interval$new(1, 5, class = "integer") +
#'     Interval$new(2, 7, class = "integer")
#'
#' # union of mixed set types
#'
#' Set$new(1:10) + Interval$new(5, 15)
#' Set$new(1:10) + Interval$new(5, 15, class = "integer")
#' Set$new(5,7) | Tuple$new(6, 8, 7)
#'
#' # union of FuzzySet
#' FuzzySet$new(1, 0.1, 2, 0.5) + Set$new(2:5)
#' # not the same when the order is reversed
#' Set$new(2:5) + FuzzySet$new(1, 0.1, 2, 0.5)
#'
#' # union of conditional sets
#'
#' ConditionalSet$new(function(x, y) x >= y) +
#'     ConditionalSet$new(function(x, y) x == y)
#' ConditionalSet$new(function(x) x == 2) +
#'     ConditionalSet$new(function(y) y == 3)
#'
#' # union of special sets
#' PosReals$new() + NegReals$new()
#' Set$new(-Inf, Inf) + Reals$new()
#'
#' @export
union <- function(x, y){
  if (x$isSubset(y))
    return(x)
  else if (y$isSubset(x))
    return(y)

  UseMethod("union", x)
}

#' @rdname union
#' @export
union.PosReals <- function(x, y){
  if(getR6Class(y) == "NegReals")
    return(Reals$new())
  else
    return(union.Interval(x, y))
}
#' @rdname union
#' @export
union.NegReals <- function(x, y){
  if(getR6Class(y) == "PosReals")
    return(Reals$new())
  else
    return(union.Interval(x, y))
}
#' @rdname union
#' @export
union.PosRationals <- function(x, y){
  if(getR6Class(y) == "NegRationals")
    return(Rationals$new())
  else
    return(union.Interval(x, y))
}
#' @rdname union
#' @export
union.NegRationals <- function(x, y){
  if(getR6Class(y) == "PosRationals")
    return(Rationals$new())
  else
    return(union.Interval(x, y))
}
#' @rdname union
#' @export
union.PosIntegers <- function(x, y){
  if(getR6Class(y) == "NegIntegers")
    return(Integers$new())
  else
    return(union.Interval(x, y))
}
#' @rdname union
#' @export
union.NegIntegers <- function(x, y){
  if(getR6Class(y) == "PosIntegers")
    return(Integers$new())
  else
    return(union.Interval(x, y))
}
#' @rdname union
#' @export
union.Reals <- function(x, y){
  if(y$equals(Set$new(-Inf, Inf)))
    return(ExtendedReals$new())
  else
    return(union.Interval(x, y))
}
#' @rdname union
#' @export
union.Set <- function(x, y){
  if (inherits(y, "Interval")){
    if (x$equals(Set$new(-Inf, Inf)) & getR6Class(y) == "Reals")
      return(ExtendedReals$new())

    if (testMessage(as.Set(y)))
      return(UnionSet$new(list(x, y)))
    else
      return(union(x, as.Set(y)))
  } else if (inherits(y, "ConditionalSet")) {
    message(sprintf("Union of %s and %s is not compatible.", x$strprint(), y$strprint()))
    return(Set$new())
  } else if (inherits(x, "Tuple"))
    return(Tuple$new(x$elements, y$elements))
  else
    return(Set$new(x$elements, y$elements))
}
#' @rdname union
#' @export
union.Interval <- function(x, y){
  if (inherits(y, "ConditionalSet")) {
    message(sprintf("Union of %s and %s is not compatible.", x$strprint(), y$strprint()))
    return(Set$new())
  } else if(inherits(y, "Interval")){
    if (x$upper > y$lower & x$lower < y$lower)
      return(Interval$new(x$lower, y$upper,
                          type = paste0(substr(x$type,1,1),substr(y$type,2,2))))
    else if(y$upper > x$lower & y$lower < x$lower)
      return(Interval$new(y$lower, x$upper,
                          type = paste0(substr(y$type,1,1),substr(x$type,2,2))))
    else if(x$upper < y$lower)
      return(UnionSet$new(setlist = list(x,y), lower = x$lower, upper = y$upper,
                       type = paste0(substr(x$type,1,1),substr(y$type,2,2))))
    else if(y$upper < x$lower)
      return(UnionSet$new(setlist = list(y,x), lower = y$lower, upper = x$upper,
                       type = paste0(substr(y$type,1,1),substr(x$type,2,2))))
    else if(y$upper == x$lower){
      if(!testClosedAbove(y) & !testClosedBelow(x))
        return(UnionSet$new(setlist = list(x,y), lower = y$lower, upper = x$upper,
                         type = paste0(substr(y$type,1,1),substr(x$type,2,2))))
      else
        return(Interval$new(y$lower, x$upper,
                            type = paste0(substr(y$type,1,1),substr(x$type,2,2))))
    } else if (x$upper == y$lower){
      if(!testClosedAbove(x) & !testClosedBelow(y))
        return(UnionSet$new(setlist = list(x,y), lower = x$lower, upper = y$upper,
                         type = paste0(substr(x$type,1,1),substr(y$type,2,2))))
      else
        return(Interval$new(x$lower, y$upper,
                            type = paste0(substr(x$type,1,1),substr(y$type,2,2))))
    }
  } else
    return(union(y, x))
}
#' @rdname union
#' @export
union.FuzzySet <- function(x, y){
  if(inherits(y, "ConditionalSet")){
    message(sprintf("Union of %s and %s is not compatible.", x$strprint(), y$strprint()))
    return(Set$new())
  } else{
    y = as.FuzzySet(y)
    return(FuzzySet$new(elements = c(x$elements, y$elements),
                        membership = c(x$membership(), y$membership())))
  }
}
#' @rdname union
#' @export
union.FuzzyTuple <- function(x, y){
  if(inherits(y, "ConditionalSet")){
    message(sprintf("Union of %s and %s is not compatible.", x$strprint(), y$strprint()))
    return(Set$new())
  } else{
    y = as.FuzzyTuple(y)
    return(FuzzyTuple$new(elements = c(x$elements, y$elements),
                        membership = c(x$membership(), y$membership())))
  }
}
#' @rdname union
#' @export
union.ConditionalSet <- function(x, y){
  if(!inherits(y, "ConditionalSet")){
    message(sprintf("Union of %s and %s is not compatible.", x$strprint(), y$strprint()))
    return(Set$new())
  } else {
    condition = function(){}
    names = unique(names(c(formals(x$condition), formals(y$condition))))
    formals <- rep(list(bquote()), length(names))
    names(formals) = names
    formals(condition) = formals
        body(condition) = substitute(bx | by,
                                     list(bx = body(x$condition),
                                          by = body(y$condition)))
        # in future updates we can change this so the union of the argument classes is kept
        # not just the argclass of x
        class = c(x$class, y$class)[!duplicated(names(c(x$class, y$class)))]
        return(ConditionalSet$new(condition = condition, argclass = class))
  }
}
#' @rdname union
#' @export
union.union <- function(x, y){
  UnionSet$new(c(x$wrappedSets, list(y)))
}

#' @rdname union
#' @export
`+.Set` <- function(x, y){
  union(x, y)
}

#' @rdname union
#' @export
'|.Set' <- function(x, y){
  union(x, y)
}
