intersection <- function(x, y){
  if(x$length == 0 | y$length == 0)
    return(Set$new())

  if (y$isSubset(x, proper = FALSE))
    return(x)
  else if (x$isSubset(y, proper = FALSE))
    return(y)

  if(testMessage(as.Set(x)))
    x = suppressMessages(as.Interval(x))
  else
    x = as.Set(x)

  if(testMessage(as.Set(y)))
    y = suppressMessages(as.Interval(y))
  else
    y = as.Set(y)

  if(any(is.nan(x$elements)) | any(is.nan(y$elements)))
    UseMethod("intersection")
  else
    Set$new(intersect(x$elements, y$elements))
}
intersection.Set <- function(x, y){
  if (inherits(y, "ConditionalSet"))
    return(Set$new())
  else
    return(Set$new(x$elements[y$liesInSet(x)]))
}
intersection.Interval <- function(x, y){
  if(inherits(y, "Interval")){
    if (x$lower > y$upper | y$lower > x$upper)
      return(Set$new())
    else if(y$lower <= x$upper & y$lower >= x$lower)
      return(Interval$new(y$lower, x$upper))
    else
      return(Interval$new(x$lower, y$upper))
  } else
    return(Set$new(y$elements[x$liesInSet(y)]))
}
intersection.ConditionalSet <- function(x, y){
  if(!inherits(y, "ConditionalSet"))
    return(Set$new())
  else {
    if(x$equals(y))
      return(x)
    else{
      if(all(names(formals(x$condition)) == names(formals(y$condition)))){
        condition = function(){}
        formals(condition) = formals(x$condition)
        body(condition) = substitute(bx & by,
                                     list(bx = body(x$condition),
                                          by = body(y$condition)))
        # in future updates we can change this so the intersection of the argument classes is kept
        # not just the argclass of x
        return(ConditionalSet$new(condition = condition, argclass = x$class))
      } else
        stop("Conditional set conditions must have the same formal arguments.")
    }
  }
}

#' @rdname intersection
#' @export
'&.Set' <- function(x, y){
  intersection(x, y)
}
