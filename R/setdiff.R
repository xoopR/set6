#' @name SetDiff
#' @title SetDiff
#' @export
NULL
SetDiff <- R6::R6Class("SetDiff", inherit = SetWrapper)
SetDiff$set("public", "initialize", function(addlist, subtractlist, lower = NULL, upper = NULL, type = NULL,
                                           dimension = NULL){
  if(is.null(lower)) lower = min(sapply(setlist, function(x) x$lower))
  if(is.null(upper)) upper = max(sapply(setlist, function(x) x$upper))
  if(is.null(type)) type = "{}"
  if(is.null(dimension)) dimension = setlist[[1]]$dimension
  private$.addedSets = addlist
  private$.subtractedSets = subtractlist
  super$initialize(setlist = c(addlist, subtractlist), lower = lower, upper = upper, type = type,
                   dimension = dimension)
}) # to do
SetDiff$set("public","strprint",function(n = 2){
 add = paste(lapply(self$addedSets, function(x) x$strprint(n)), collapse = " \u222A ")
 subtract = paste(lapply(self$subtractedSets, function(x) x$strprint(n)), collapse = " \u222A ")
 paste("{", add, subtract, "}", sep = " \ ")
})
SetDiff$set("active","elements",function(){
  els = unique(unlist(sapply(self$wrappedSets, function(x) x$elements)))
  if(any(is.nan(els)))
    return(NaN)
  else
    return(els)
}) # to do

SetDiff$set("active","addedSets", function() return(private$.addedSets))
SetDiff$set("active","subtractedSets",function() return(private$.subtractedSets))
SetDiff$set("private",".addedSets",list())
SetDiff$set("private",".subtractedSets",list())


setdiff <- function(object, x, y){
  UseMethod("setdiff", object)
}
# Difference of Set/Tuple from y
setdiff.Set <- function(x, y){
  # if x is a subset of y then return the Empty set
  if(y >= x)
    return(Set$new())

  # difference of two sets
  if(getR6Class(y) %in% c("Set", "Tuple")){
    if(testTuple(x))
      return(Tuple$new(x$elements[!(x$elements %in% y$elements)]))
    else
      return(Set$new(x$elements[!(x$elements %in% y$elements)]))
  # difference of set and interval
  } else if(testInterval(y)){
    if(testTuple(x))
      return(Tuple$new(x$elements[!y$liesInSet(x$elements)]))
    else
      return(Set$new(x$elements[!y$liesInSet(x$elements)]))
  # otherwise incompatible
  } else {
    message(sprintf("Difference of %s and %s is not compatible.", x$strprint(), y$strprint()))
    return(x)
  }
}
# Difference of Interval from y
setdiff.Interval <- function(x, y){
  # if(testSet(y) & testFuzzy(y) & !testInterval(y))
  #   return(setdiff.FuzzySet(y, x))
  # else if(testSet(y) & !testFuzzy(y) & !testInterval(y))
  #   return(setdiff.Set(y, x))

  # if possible convert x to a Set
  if(!testMessage(as.Set(x)))
    return(setdiff(as.Set(x), y))

  if(testFuzzy(y) | testConditionalSet(y)){
    message(sprintf("Difference of %s and %s is not compatible.", x$strprint(), y$strprint()))
    return(x)
  }

  # if x is a (proper) subset of y then return the Empty set
  if(y >= x)
    return(Set$new())

  # convert to interval if possible
  if(!testMessage(as.Interval(y)))
    y <- as.Interval(y)

  # difference of interval from interval
  if(testInterval(y)){
    if(y$upper >= x$upper & y$lower <= x$lower)
      return(Set$new())
    else if(y$lower > x$upper | y$upper < x$lower)
      return(x)
    else if(y$upper >= x$upper & y$lower > x$lower & y$lower <= x$upper)
      return(Interval$new(lower = x$lower, upper = y$lower, type = paste0(substr(x$type,1,1),")"),
                          class = x$class))
    else if(y$upper < x$upper & y$lower <= x$lower & y$upper >= x$lower)
      return(Interval$new(lower = y$upper, upper = x$upper, type = paste0("(",substr(x$type,2,2)),
                          class = x$class))
    else if(y$upper <= x$upper & y$lower >= x$lower)
      return(union(Interval$new(x$lower,y$lower,type=paste0(substr(x$type,1,1),")"),class = x$class),
                       Interval$new(y$upper,x$upper,type=paste0("(",substr(x$type,2,2)),class = x$class)))
  } else if (getR6Class(y) == "Set") # to do
    return(SetDiff$new(list(x), list(y), lower = x$lower, upper = x$upper,
                     type = paste0(substr(x$type,1,1),substr(y$type,2,2)),
                     dimension = x$dimension))
  else {
    message(sprintf("Difference of %s and %s is not compatible.", x$strprint(), y$strprint()))
    return(x)
  }
}

#' @rdname setdiff
#' @usage \method{-}{Set}(x, y)
#' @param x Set
#' @param y Set
#' @export
`-.Set` <- function(x, y){
  setdiff(x, y)
}
