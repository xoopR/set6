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
  paste0("{",paste(lapply(self$wrappedSets, function(x) x$strprint(n)), collapse = " \u222A "),"}")
})
Union$set("active","elements",function(){
  els = unlist(sapply(self$wrappedSets, function(x) x$elements))
  if(any(is.nan(els)))
    return(NaN)
  else
    return(els)
})

union <- function(object, x, y){
  UseMethod("union", object)
}
# Union of Set/Tuple with y
union.Set <- function(x, y){
  if(inherits(y, "SpecialSet"))
    return(union.SpecialSet(y, x))

  if (x$isSubset(y))
    return(x)
  else if (y$isSubset(x))
    return(y)

  if (testInterval(y)){
    if(testMessage(as.Set(y)))
      return(Union$new(list(x, y)))
    else
      return(union(x, as.Set(y)))
  } else if (!testConditionalSet(y)) {
    if(testTuple(x))
      return(Tuple$new(x$elements, y$elements))
    else
      return(Set$new(x$elements, y$elements))
  } else{
    message(sprintf("Union of %s and %s not compatible.", x$strprint(), y$strprint()))
    return(Set$new())
  }
}
# Union of Interval with y
union.Interval <- function(x, y){
  if(testSet(y) & testFuzzy(y) & !testInterval(y))
    return(union.FuzzySet(y, x))
  else if(testSet(y) & !testFuzzy(y) & !testInterval(y))
    return(union.Set(y, x))

  if (x$isSubset(y))
    return(x)
  else if (y$isSubset(x))
    return(y)

  if(testInterval(y)){
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
  } else{
    message(sprintf("Union of %s and %s not compatible.", x$strprint(), y$strprint()))
    return(Set$new())
  }
}
# Union of SpecialSet with y
union.SpecialSet <- function(x, y){
  if (getR6Class(x) == "PosReals" & getR6Class(y) == "NegReals" |
     getR6Class(y) == "PosReals" & getR6Class(x) == "NegReals")
    return(Reals$new())
  else if (getR6Class(x) == "PosRationals" & getR6Class(y) == "NegRationals" |
     getR6Class(y) == "PosRationals" & getR6Class(x) == "NegRationals")
    return(Rationals$new())
  else if (getR6Class(x) == "PosIntegers" & getR6Class(y) == "NegIntegers" |
     getR6Class(y) == "PosIntegers" & getR6Class(x) == "NegIntegers")
    return(Integers$new())
  else if(getR6Class(x) == "Reals" & y$equals(Set$new(-Inf, Inf)))
    return(ExtendedReals$new())
  else
    return(union.Interval(x, y))
}
# Union of FuzzySet/FuzzyTuple with y
union.FuzzySet <- function(x, y){
  if(testInterval(y) | testConditionalSet(y)){
    message(sprintf("Union of %s and %s not compatible.", x$strprint(), y$strprint()))
    return(Set$new())
  }

  if (x$isSubset(y))
    return(x)
  else if (y$isSubset(x))
    return(y)

  if (testFuzzyTuple(x)){
    y = as.FuzzyTuple(y)
    return(FuzzyTuple$new(elements = c(x$elements, y$elements),
                          membership = c(x$membership(), y$membership())))
  } else {
    y = as.FuzzySet(y)
    return(FuzzySet$new(elements = c(x$elements, y$elements),
                          membership = c(x$membership(), y$membership())))
  }
}
# Union of two ConditionalSets
union.ConditionalSet <- function(x, y){
  if(!testConditionalSet(y)){
    message(sprintf("Union of %s and %s not compatible.", x$strprint(), y$strprint()))
    return(Set$new())
  } else{
    if(x$equals(y))
      return(x)
    else{
      if(all(names(formals(x$condition)) == names(formals(y$condition)))){
        condition = function(){}
        formals(condition) = formals(x$condition)
        body(condition) = substitute(bx | by,
                                     list(bx = body(x$condition),
                                          by = body(y$condition)))
        argclass = unique(c(x$class, y$class))
        names(argclass) = names(formals(condition))
        return(ConditionalSet$new(condition = condition, argclass = argclass))
      } else
        stop("Conditional set conditions must have the same formal arguments.")

    }
  }
}
# Union with Union
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
