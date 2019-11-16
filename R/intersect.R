#' @name intersect
#' @rdname intersect
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
#' [base::intersect] is called if `x` does not inherit from an R6 class, thus avoiding masking.
#'
#' @examples
#' # intersection of two sets
#'
#' Set$new(-2:4) & Set$new(2:5)
#' intersect(Set$new(1,4,"a"), Set$new("a", 6))
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
intersect <- function(x, y){
  if(!inherits(x, "R6"))
    return(base::intersect(x, y))

  if(inherits(y, "SetWrapper"))
    return(intersect.SetWrapper(y, x))

  if(x$length == 0 | y$length == 0)
    return(Set$new())

  if (y$isSubset(x, proper = FALSE))
    return(x)
  else if (x$isSubset(y, proper = FALSE))
    return(y)

  if(testConditionalSet(x) | testConditionalSet(y))
    UseMethod("intersect")

  if(testMessage(as.Set(x)))
    x = suppressMessages(as.Interval(x))
  else
    x = as.Set(x)

  if(testMessage(as.Set(y)))
    y = suppressMessages(as.Interval(y))
  else
    y = as.Set(y)

  if(any(grepl("NaN", x$elements)) | any(grepl("NaN", y$elements)))
    UseMethod("intersect")
  else
    Set$new(intersect(x$elements, y$elements))
}
#' @rdname intersect
#' @export
intersect.Set <- function(x, y){
  if (inherits(y, "ConditionalSet"))
    return(Set$new())
  else
    return(Set$new(x$elements[y$contains(x$elements)]))
}
#' @rdname intersect
#' @export
intersect.Interval <- function(x, y){
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
#' @rdname intersect
#' @export
intersect.ConditionalSet <- function(x, y){
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
#' @rdname intersect
#' @export
intersect.SetWrapper <- function(x, y){
  if(inherits(x, "UnionSet")){
    int = Set$new()
    sets = sapply(x$wrappedSets, function(set) intersect(set, y))
    for(i in 1:length(sets))
      int = int + sets[[i]]
    return(int)
  }
}
#' @rdname intersect
#' @export
'&.Set' <- function(x, y){
  intersect(x, y)
}
