#' @include Set.R Set_SpecialSet.R Set_Interval.R Set_ConditionalSet.R
#'
#' @name Union
#' @title Union
#' @export
NULL
Union <- R6::R6Class("Union", inherit = SetWrapper)
Union$set("public", "initialize", function(setlist, lower = NULL, upper = NULL, type = NULL,
                                           dimension = NULL){
  if(is.null(lower)) lower = min(sapply(setlist, function(x) x$lower))
  if(is.null(upper)) upper = max(sapply(setlist, function(x) x$upper))
  if(is.null(type)) type = "{}"
  if(is.null(dimension)) dimension = setlist[[1]]$dimension
  super$initialize(setlist = setlist, lower = lower, upper = upper, type = type,
                   dimension = dimension)
})
Union$set("public","strprint",function(n = 2){
  paste0("{",paste(lapply(c(self$wrappedSets, self$wrappedIntervals), function(x) x$strprint(n)), collapse = " \u222A "),"}")
})
Union$set("active","elements",function(){
  els = unique(unlist(sapply(self$wrappedSets, function(x) x$elements)))
  if(any(is.nan(els)))
    return(NaN)
  else
    return(els)
})

union <- function(x, y){
  if (x$isSubset(y))
    return(x)
  else if (y$isSubset(x))
    return(y)

  UseMethod("union", x)
}

union.PosReals <- function(x, y){
  if(class(y) == "NegReals")
    return(Reals$new())
  else
    return(union.interval(x, y))
}
union.NegReals <- function(x, y){
  if(class(y) == "PosReals")
    return(Reals$new())
  else
    return(union.interval(x, y))
}
union.PosRationals <- function(x, y){
  if(class(y) == "NegRationals")
    return(Rationals$new())
  else
    return(union.interval(x, y))
}
union.NegRationals <- function(x, y){
  if(class(y) == "PosRationals")
    return(Rationals$new())
  else
    return(union.interval(x, y))
}
union.PosIntegers <- function(x, y){
  if(class(y) == "NegIntegers")
    return(Integers$new())
  else
    return(union.interval(x, y))
}
union.NegIntegers <- function(x, y){
  if(class(y) == "PosIntegers")
    return(Integers$new())
  else
    return(union.interval(x, y))
}
union.Reals <- function(x, y){
  if(y$equals(Set$new(-Inf, Inf)))
    return(ExtendedReals$new())
  else
    return(union.Interval(x, y))
}

union.Set <- function(x, y){
  if (inherits(y, "Interval")){
    if (x$equals(Set$new(-Inf, Inf)) & class(y) == "Reals")
      return(ExtendedReals$new())

    if (testMessage(as.Set(y)))
      return(Union$new(list(x, y)))
    else
      return(union(x, as.Set(y)))
  } else if (inherits(y, "ConditionalSet")) {
    message(sprintf("Union of %s and %s is not compatible.", x$strprint(), y$strprint()))
    return(Set$new())
  } else if (inherits(y, "Tuple"))
    return(Tuple$new(x$elements, y$elements))
  else
    return(Set$new(x$elements, y$elements))
}
union.Interval <- function(x, y){
  if (inherits(y, "ConditionalSet")) {
    message(sprintf("Union of %s and %s is not compatible.", x$strprint(), y$strprint()))
    return(Set$new())
  } else if(inherits(y, "Interval")){
    if (x$upper > y$lower & x$lower < y$lower)
      return(Interval$new(x$lower, y$upper,
                          type = paste0(substr(x$type,1,1),substr(y$type,2,2)),
                          dimension = x$dimension))
    else if(y$upper > x$lower & y$lower < x$lower)
      return(Interval$new(y$lower, x$upper,
                          type = paste0(substr(y$type,1,1),substr(x$type,2,2)),
                          dimension = x$dimension))
    else if(x$upper < y$lower)
      return(Union$new(setlist = list(x,y), lower = x$lower, upper = y$upper,
                       type = paste0(substr(x$type,1,1),substr(y$type,2,2)),
                       dimension = x$dimension))
    else if(y$upper < x$lower)
      return(Union$new(setlist = list(y,x), lower = y$lower, upper = x$upper,
                       type = paste0(substr(y$type,1,1),substr(x$type,2,2)),
                       dimension = x$dimension))
    else if(y$upper == x$lower){
      if(!testClosedAbove(y) & !testClosedBelow(x))
        return(Union$new(setlist = list(x,y), lower = y$lower, upper = x$upper,
                         type = paste0(substr(y$type,1,1),substr(x$type,2,2)),
                         dimension = y$dimension))
      else
        return(Interval$new(y$lower, x$upper,
                            type = paste0(substr(y$type,1,1),substr(x$type,2,2)),
                            dimension = x$dimension))
    } else if (x$upper == y$lower){
      if(!testClosedAbove(x) & !testClosedBelow(y))
        return(Union$new(setlist = list(x,y), lower = x$lower, upper = y$upper,
                         type = paste0(substr(x$type,1,1),substr(y$type,2,2)),
                         dimension = x$dimension))
      else
        return(Interval$new(x$lower, y$upper,
                            type = paste0(substr(x$type,1,1),substr(y$type,2,2)),
                            dimension = x$dimension))
    }
  } else
    return(union(y, x))
}
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

union.ConditionalSet <- function(x, y){
  if(!inherits(y, "ConditionalSet")){
    message(sprintf("Union of %s and %s is not compatible.", x$strprint(), y$strprint()))
    return(Set$new())
  } else {
      if(all(names(formals(x$condition)) == names(formals(y$condition)))){
        condition = function(){}
        formals(condition) = formals(x$condition)
        body(condition) = substitute(bx | by,
                                     list(bx = body(x$condition),
                                          by = body(y$condition)))
        # in future updates we can change this so the union of the argument classes is kept
        # not just the argclass of x
        return(ConditionalSet$new(condition = condition, argclass = x$class))
      } else
        stop("Conditional set conditions must have the same formal arguments.")
  }
}
union.Union <- function(x, y){
  Union$new(c(x$wrappedSets, list(y)))
}

#' @rdname Union
#' @usage \method{+}{Set}(x, y)
#' @param x Set
#' @param y Set
#' @export
`+.Set` <- function(x, y){
  union(x, y)
}

#' @rdname Union
#' @usage \method{|}{Set}(x, y)
#' @param x Set
#' @param y Set
'|.Set' <- function(x, y){
  union(x, y)
}
