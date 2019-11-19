#' @name setdiff
#' @param x,y Set
#' @title Difference of Two Sets
#' @return An object inheriting from `Set` containing the set difference of elements in both `x` and `y`.
#' @description Returns the set difference of two objects inheriting from class `Set`.
#' @details The difference of two sets, \eqn{X, Y}, is defined as the set of elements that exist
#' in set \eqn{X} and not \eqn{Y},
#' \deqn{X-Y = \{z : z \epsilon X and !(z \epsilon Y)\}}{X-Y = {z : z \epsilon X and !(z \epsilon Y)}}
#'
#' The set difference of two [ConditionalSet]s is defined by combining their defining functions by a negated
#' 'and', `!&`, operator. See examples. The set difference of fuzzy sets and tuples only looks at the
#' elements and ignores the memberships.
#'
#' [base::setdiff] is called if `x` does not inherit from an R6 class, thus avoiding masking.
#'
#' @examples
#' # setdiff of two sets
#'
#' Set$new(-2:4) - Set$new(2:5)
#' setdiff(Set$new(1,4,"a"), Set$new("a", 6))
#'
#' # setdiff of two intervals
#'
#' Interval$new(1, 10) - Interval$new(5, 15)
#' Interval$new(1, 10) - Interval$new(-15, 15)
#' Interval$new(1, 10) - Interval$new(-1, 2)
#'
#' # setdiff of mixed set types
#'
#' Set$new(1:10) - Interval$new(5, 15)
#' Set$new(5,7) - Tuple$new(6, 8, 7)
#'
#' # FuzzySet-Set returns a FuzzySet
#' FuzzySet$new(1, 0.1, 2, 0.5) - Set$new(2:5)
#' # Set-FuzzySet returns a Set
#' Set$new(2:5) - FuzzySet$new(1, 0.1, 2, 0.5)
#'
#' # setdiff of conditional sets
#'
#' ConditionalSet$new(function(x, y) x >= y) -
#'     ConditionalSet$new(function(x, y) x == y)
#'
#' # setdiff of special sets
#' Reals$new() - NegReals$new()
#' Rationals$new() - PosRationals$new()
#' Integers$new() - PosIntegers$new()
#'
#' @export
setdiff <- function(x, y){
  if(!inherits(x, "R6"))
    return(base::setdiff(x, y))

  if((testConditionalSet(x) & !testConditionalSet(y)) | (testConditionalSet(y) & !testConditionalSet(x)))
    return(DifferenceSet$new(x, y))

  if(testCrisp(x) & testFuzzyTuple(y))
    y = as.Tuple(y)
  else if(testCrisp(x) & testFuzzySet(y))
    y = as.Set(y)

  # if possible convert Interval to Set
  if(!testMessage(as.Set(x)) & testInterval(x))
    x = as.Set(x)

  if(x == y)
    return(Set$new())

  if(inherits(x, "SetWrapper") | inherits(y, "SetWrapper"))
    return(DifferenceSet$new(x, y))

  if(x <= y)
    return(Set$new())

  if(y$length == 0)
    return(x)

  UseMethod("setdiff")
}
#' @rdname setdiff
#' @export
setdiff.Set <- function(x, y){
  # if x is a subset of y then return the Empty set
  if(y >= x)
    return(Set$new())

  # difference of two sets
  if(getR6Class(y) %in% c("Set", "Tuple","FuzzySet","FuzzyTuple")){
    if(testTuple(x))
      return(Tuple$new(x$elements[!(x$elements %in% y$elements)]))
    else
      return(Set$new(x$elements[!(x$elements %in% y$elements)]))
  # difference of set and interval
  } else if(testInterval(y)){
    if(testTuple(x))
      return(Tuple$new(x$elements[!y$contains(x$elements)]))
    else
      return(Set$new(x$elements[!y$contains(x$elements)]))
  # otherwise incompatible
  } else {
    message(sprintf("Difference of %s and %s is not compatible.", x$strprint(), y$strprint()))
    return(x)
  }
}
#' @rdname setdiff
#' @export
setdiff.Interval <- function(x, y){

  # if x is a (proper) subset of y then return the Empty set
  if(y >= x)
    return(Set$new())

  # convert to interval if possible
  if(!testMessage(as.Interval(y)) & !testInterval(y))
    y <- as.Interval(y)

  if(x$class == "numeric" & y$class == "integer")
    return(DifferenceSet$new(x, y, lower = x$lower, upper = x$upper,
                             type = paste0(substr(x$type,1,1),substr(y$type,2,2))))

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
  } else if (getR6Class(y) == "Set") {
    y = Set$new(y$elements[x$contains(y)])
    int = levels(cut(x$lower:x$upper, unique(c(x$lower, y$elements, x$upper))))
    lower = as.numeric(sapply(int, function(x) substr(x, 2, gregexpr(",", x, fixed = TRUE)[[1]][[1]]-1)))
    upper = as.numeric(sapply(int, function(x) substr(x, gregexpr(",", x, fixed = TRUE)[[1]][[1]]+1,
                                                      gregexpr("]", x, fixed = TRUE)[[1]][[1]]-1)))
    u = Set$new()
    for (i in 1:length(lower)) {
      if(diff(c(lower[[i]], upper[[i]])) == 1 & i != 1)
        next()

      if(i == 1)
        type = paste0(substr(x$type,1,1), ")")
      else if (i == length(lower))
        type = paste0("(", substr(x$type,2,2))
      else
        type = "()"

      u = u + Interval$new(lower[[i]], upper[[i]], type = type)
    }

    return(u)
    # return(DifferenceSet$new(x, y, lower = x$lower, upper = x$upper,
    #                  type = paste0(substr(x$type,1,1),substr(y$type,2,2))))
  } else {
    message(sprintf("Difference of %s and %s is not compatible.", x$strprint(), y$strprint()))
    return(x)
  }
}
#' @rdname setdiff
#' @export
setdiff.FuzzySet <- function(x, y){
  if(inherits(y, "ConditionalSet")){
    message(sprintf("Set difference of %s and %s is not compatible.", x$strprint(), y$strprint()))
  } else{
    y = as.FuzzySet(y)
    ind = !(x$elements %in% y$elements)
    return(FuzzySet$new(elements = x$elements[ind], membership = x$membership()[ind]))
  }
}
#' @rdname setdiff
#' @export
setdiff.FuzzyTuple <- function(x, y){
  if(inherits(y, "ConditionalSet")){
    message(sprintf("Set difference of %s and %s is not compatible.", x$strprint(), y$strprint()))
  } else{
    y = as.FuzzyTuple(y)
    ind = !(x$elements %in% y$elements)
    return(FuzzyTuple$new(elements = x$elements[ind], membership = x$membership()[ind]))
  }
}
#' @rdname setdiff
#' @export
setdiff.ConditionalSet <- function(x, y){
  if(!inherits(y, "ConditionalSet"))
    message(sprintf("Product of %s and %s not compatible.", x$strprint(), y$strprint()))
  else {
    if(x$equals(y))
      return(x)
    else{
      condition = function(){}
      names = unique(names(c(formals(x$condition), formals(y$condition))))
      formals <- rep(list(bquote()), length(names))
      names(formals) = names
      formals(condition) = formals
      body(condition) = substitute(bx & !(by),
                                   list(bx = body(x$condition),
                                        by = body(y$condition)))
      # in future updates we can change this so the intersection of the argument classes is kept
      # not just the argclass of x
      class = c(x$class, y$class)[!duplicated(names(c(x$class, y$class)))]
      return(ConditionalSet$new(condition = condition, argclass = class))
    }
  }
}
#' @rdname setdiff
#' @export
setdiff.Reals <- function(x, y){
  if(getR6Class(y) == "PosReals")
    return(NegReals$new())
  else if(getR6Class(y) == "NegReals")
    return(PosReals$new())
  else
    return(setdiff.Interval(x, y))
}
#' @rdname setdiff
#' @export
setdiff.Rationals <- function(x, y){
  if(getR6Class(y) == "PosRationals")
    return(NegRationals$new())
  else if(getR6Class(y) == "NegRationals")
    return(PosRationals$new())
  else
    return(setdiff.Interval(x, y))
}
#' @rdname setdiff
#' @export
setdiff.Integers <- function(x, y){
  if(getR6Class(y) == "PosIntegers")
    return(NegIntegers$new())
  else if(getR6Class(y) == "NegIntegers")
    return(PosIntegers$new())
  else
    return(setdiff.Interval(x, y))
}
#' @rdname setdiff
#' @export
`-.Set` <- function(x, y){
  setdiff(x, y)
}
