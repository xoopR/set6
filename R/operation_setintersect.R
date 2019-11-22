#' @name setintersect
#' @rdname setintersect
#' @param x,y Set
#' @title Intersection of Two Sets
#' @return A `Set` consisting of elements in both `x` and `y`.
#' @description Returns the intersection of two objects inheriting from class `Set`.
#' @details The intersection of two sets, \eqn{X, Y}, is defined as the set of elements that exist
#' in both sets,
#' \deqn{X \cap Y = \{z : z \epsilon X and `` \epsilon Y\}}{{z : z \epsilon X and z \epsilon Y}}
#' In the case where no elements are common to either set, then the empty set is returned.
#'
#' The intersection of two [ConditionalSet]s is defined by combining their defining functions by an
#' 'and', `&`, operator. See examples.
#'
#' @family operators
#' @examples
#' # intersection of two sets
#'
#' Set$new(-2:4) & Set$new(2:5)
#' setintersect(Set$new(1,4,"a"), Set$new("a", 6))
#' Set$new(1:4) & Set$new(5:7)
#'
#' # intersection of two intervals
#'
#' Interval$new(1, 10) & Interval$new(5, 15)
#' Interval$new(1, 2) & Interval$new(2, 3)
#' Interval$new(1, 5, class = "integer") &
#'     Interval$new(2, 7, class = "integer")
#'
#' # intersection of mixed set types
#'
#' Set$new(1:10) & Interval$new(5, 15)
#' Set$new(5,7) & Tuple$new(6, 8, 7)
#'
#' # Ignores membership of FuzzySet
#'
#' FuzzySet$new(1, 0.1, 2, 0.5) & Set$new(2:5)
#'
#' # intersection of conditional sets
#'
#' ConditionalSet$new(function(x, y) x >= y) &
#'     ConditionalSet$new(function(x, y) x == y)
#' ConditionalSet$new(function(x) x == 2) &
#'     ConditionalSet$new(function(y) y == 3)
#'
#' # But be careful not to make an empty set
#'
#' ConditionalSet$new(function(x) x == 2) &
#'     ConditionalSet$new(function(x) x == 3)
#'
#' @export
setintersect <- function(x, y){

  if(x$length == 0 | y$length == 0)
    return(Set$new())

  if(inherits(y, "SetWrapper") & !inherits(x, "SetWrapper"))
    return(setintersect(y, x))
  if(inherits(x, "SetWrapper") | inherits(y, "SetWrapper"))
    UseMethod("setintersect")

  if(testConditionalSet(x))
    UseMethod("setintersect")
  else if (testConditionalSet(y))
    return(Set$new(x$elements[y$contains(x$elements)]))

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

  if(testInterval(x))
    UseMethod("setintersect")

  return(Set$new(x$elements[y$contains(x$elements)]))
}
# #' rdname setintersect
# #' export
# setintersect.Set <- function(x, y){
#   if (inherits(y, "ConditionalSet"))
#     return(Set$new())
#   else
#     return(Set$new(x$elements[y$contains(x$elements)]))
# }
#' @rdname setintersect
#' @export
setintersect.Interval <- function(x, y){
  if(inherits(y, "Interval")){
    if (x$lower > y$upper | y$lower > x$upper)
      return(Set$new())
    else if(y$lower <= x$upper & y$lower >= x$lower)
      return(Interval$new(y$lower, x$upper,
                          type = paste0(substr(y$type,1,1),substr(x$type,2,2))))
    else
      return(Interval$new(x$lower, y$upper,
                          type = paste0(substr(x$type,1,1),substr(y$type,2,2))))
  } else
    return(Set$new(y$elements[x$contains(y$elements)]))
}
#' @rdname setintersect
#' @export
setintersect.ConditionalSet <- function(x, y){
  if(!inherits(y, "ConditionalSet"))
    return(Set$new(y$elements[x$contains(sapply(y$elements, as.Set))]))
  else {
    if(x$equals(y))
      return(x)
    else{
        condition = function(){}
        names = unique(names(c(formals(x$condition), formals(y$condition))))
        formals <- rep(list(bquote()), length(names))
        names(formals) = names
        formals(condition) = formals
        body(condition) = substitute(bx & by,
                                     list(bx = body(x$condition),
                                          by = body(y$condition)))
        # in future updates we can change this so the intersect of the argument classes is kept
        # not just the argclass of x
        class = c(x$class, y$class)[!duplicated(names(c(x$class, y$class)))]
        return(ConditionalSet$new(condition = condition, argclass = class))
    }
  }
}
#' @rdname setintersect
#' @export
setintersect.UnionSet <- function(x, y){
  if(!inherits(y, "SetWrapper"))
    return(Set$new(unlist(y$elements[x$contains(y$elements)])))
  else {
    int = Set$new()
    sets = sapply(x$wrappedSets, function(set) setintersect(set, y))
    for(i in 1:length(sets))
      int = int + sets[[i]]

    return(int)
  }
}
#' @rdname setintersect
#' @export
setintersect.ComplementSet <- function(x, y){
  if(!inherits(y, "SetWrapper"))
    return(Set$new(unlist(y$elements[x$contains(y$elements)])))
  else if(inherits(y, "ComplementSet")){
    return((x$addedSet & y$addedSet) - (x$subtractedSet + y$subtractedSet))
  } else {
    add_int = setintersect(x$addedSet, y)
    sub_int = setintersect(x$subtractedSet, y)

    return(add_int - sub_int)
  }
}
#' @rdname setintersect
#' @export
setintersect.ProductSet <- function(x, y){
  if (!inherits(y, "SetWrapper"))
    return(Set$new(unlist(y$elements[x$contains(y$elements)])))
  else
    message("setIntersect of ProductSet & SetWrapper currently not implemented.")
}
#' @rdname setintersect
#' @export
'&.Set' <- function(x, y){
  setintersect(x, y)
}
