#' @name setcomplement
#' @param x,y Set
#' @param simplify logical, if `TRUE` (default) returns the result in its simplest form, usually a `Set` or
#' [UnionSet], otherwise a `ComplementSet`.
#' @title Complement of Two Sets
#' @return An object inheriting from `Set` containing the set difference of elements in `x` and `y`.
#' @description Returns the set difference of two objects inheriting from class `Set`. If `y` is missing
#' then the complement of `x` from its universe is returned.
#' @details The difference of two sets, \eqn{X, Y}, is defined as the set of elements that exist
#' in set \eqn{X} and not \eqn{Y},
#' \deqn{X-Y = \{z : z \epsilon X \quad and \quad \neg(z \epsilon Y)\}}{X-Y = {z : z \epsilon X and !(z \epsilon Y)}}
#'
#' The set difference of two [ConditionalSet]s is defined by combining their defining functions by a negated
#' 'and', `!&`, operator. See examples.
#'
#' The complement of fuzzy and crisp sets first coerces fuzzy sets to crisp sets by finding their [support].
#'
#' @family operators
#' @examples
#' # absolute complement
#' setcomplement(Set$new(1,2,3, universe = Reals$new()))
#' setcomplement(Set$new(1,2, universe = Set$new(1,2,3,4,5)))
#'
#' # complement of two sets
#'
#' Set$new(-2:4) - Set$new(2:5)
#' setcomplement(Set$new(1,4,"a"), Set$new("a", 6))
#'
#' # complement of two intervals
#'
#' Interval$new(1, 10) - Interval$new(5, 15)
#' Interval$new(1, 10) - Interval$new(-15, 15)
#' Interval$new(1, 10) - Interval$new(-1, 2)
#'
#' # complement of mixed set types
#'
#' Set$new(1:10) - Interval$new(5, 15)
#' Set$new(5,7) - Tuple$new(6, 8, 7)
#'
#' # FuzzySet-Set returns a FuzzySet
#' FuzzySet$new(1, 0.1, 2, 0.5) - Set$new(2:5)
#' # Set-FuzzySet returns a Set
#' Set$new(2:5) - FuzzySet$new(1, 0.1, 2, 0.5)
#'
#' # complement of conditional sets
#'
#' ConditionalSet$new(function(x, y, simplify = TRUE) x >= y) -
#'     ConditionalSet$new(function(x, y, simplify = TRUE) x == y)
#'
#' # complement of special sets
#' Reals$new() - NegReals$new()
#' Rationals$new() - PosRationals$new()
#' Integers$new() - PosIntegers$new()
#'
#' @export
setcomplement <- function(x, y, simplify = TRUE){

  if (missing(y)) {
    if(getR6Class(x) == "FuzzySet")
      return(FuzzySet$new(elements = x$elements, membership = 1 - x$membership()))
    else if(getR6Class(x) == "FuzzyTuple")
      return(FuzzyTuple$new(elements = x$elements, membership = 1 - x$membership()))
    else if (is.null(x$universe))
      stop("Set y is missing and x does not have a universe for absolute complement.")
    else
      return(setcomplement(x$universe, x))
  }

  if((testConditionalSet(x) & !testConditionalSet(y)) |
     (testConditionalSet(y) & !testConditionalSet(x)) |
     !simplify)
    return(ComplementSet$new(x, y))

  if(testCrisp(x) & testFuzzyTuple(y))
    y = as.Tuple(y)
  else if(testCrisp(x) & testFuzzySet(y))
    y = as.Set(y)

  if(x == y)
    return(Set$new())

  if(inherits(x, "ComplementSet"))
    UseMethod("setcomplement")
  else if(inherits(x, "SetWrapper") | inherits(y, "SetWrapper"))
    return(ComplementSet$new(x, y))

  if(testConditionalSet(x))
    UseMethod("setcomplement")

  if(x <= y)
    return(Set$new())

  if(y$length == 0)
    return(x)

  UseMethod("setcomplement")
}
#' @rdname setcomplement
#' @export
setcomplement.Set <- function(x, y, simplify = TRUE){
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
  }
}
#' @rdname setcomplement
#' @export
setcomplement.Interval <- function(x, y, simplify = TRUE){
  # if possible convert Interval to Set
  if(!testMessage(as.Set(x)))
    return(setcomplement.Set(as.Set(x), y))
  if(!testMessage(as.Set(y)))
    y = as.Set(y)

  # difference of interval from interval
  if(testInterval(y)){
    if(x$class == "numeric" & y$class == "integer")
      return(ComplementSet$new(x, y))

    if(y$lower > x$upper | y$upper < x$lower)
      return(x)
    else if(y$upper >= x$upper & y$lower > x$lower & y$lower <= x$upper)
      return(Interval$new(lower = x$lower, upper = y$lower, type = paste0(substr(x$type,1,1),")"),
                          class = x$class))
    else if(y$upper < x$upper & y$lower <= x$lower & y$upper >= x$lower)
      return(Interval$new(lower = y$upper, upper = x$upper, type = paste0("(",substr(x$type,2,2)),
                          class = x$class))
    else if(y$upper <= x$upper & y$lower >= x$lower)
      return(setunion(Interval$new(x$lower,y$lower,type=paste0(substr(x$type,1,1),")"),class = x$class),
                       Interval$new(y$upper,x$upper,type=paste0("(",substr(x$type,2,2)),class = x$class)))
  } else if (getR6Class(y) == "Set") {
    y = Set$new(y$elements[x$contains(y$elements)])
    if(y$length == 1){
      if(y$elements == x$lower)
        return(Interval$new(x$lower, x$upper, type = paste0("(",substr(x$type,2,2))))
      else if(y$elements == x$upper)
        return(Interval$new(x$lower, x$upper, type = paste0(substr(x$type,1,1),")")))
      else
        return(Interval$new(x$lower, y$elements, type = paste0(substr(x$type,1,1),")")) +
                 Interval$new(y$elements, x$upper, type = paste0("(", substr(x$type,2,2))))
    }
    u = Set$new()
    for(i in 1:y$length){
      if (i == y$length & y$elements[[i]] == x$upper)
        break()

      if(i == 1 & y$elements[[1]] != x$lower)
        u = u + Interval$new(x$lower, y$elements[[1]], type = paste0(substr(x$type,1,1),")"))

      if((i == 1 & y$elements[[1]] != x$lower) | (i != 1))
        lower = y$elements[[i]]
      else
        lower = x$lower

      if((i == (y$length - 1) & y$elements[[y$length]] == x$upper) | (i == y$length))
        upper = x$upper
      else
        upper = y$elements[[i + 1]]

      if(i == y$length)
        type = paste0("(", substr(x$type,2,2))
      else
        type = "()"

      u = u + Interval$new(lower, upper, type)
    }
    return(u)
  }
}
#' @rdname setcomplement
#' @export
setcomplement.FuzzySet <- function(x, y, simplify = TRUE){
  y = as.FuzzySet(y)
  ind = !(x$elements %in% y$elements)
  return(FuzzySet$new(elements = x$elements[ind], membership = x$membership()[ind]))
}
#' @rdname setcomplement
#' @export
setcomplement.FuzzyTuple <- function(x, y, simplify = TRUE){
  y = as.FuzzyTuple(y)
  ind = !(x$elements %in% y$elements)
  return(FuzzyTuple$new(elements = x$elements[ind], membership = x$membership()[ind]))
}
#' @rdname setcomplement
#' @export
setcomplement.ConditionalSet <- function(x, y, simplify = TRUE){
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

  ConditionalSet$new(condition = condition, argclass = class)
}
#' @rdname setcomplement
#' @export
setcomplement.Reals <- function(x, y, simplify = TRUE){
  if(getR6Class(y) == "PosReals")
    return(NegReals$new())
  else if(getR6Class(y) == "NegReals")
    return(PosReals$new())
  else
    return(setcomplement.Interval(x, y))
}
#' @rdname setcomplement
#' @export
setcomplement.Rationals <- function(x, y, simplify = TRUE){
  if(getR6Class(y) == "PosRationals")
    return(NegRationals$new())
  else if(getR6Class(y) == "NegRationals")
    return(PosRationals$new())
  else
    return(setcomplement.Interval(x, y))
}
#' @rdname setcomplement
#' @export
setcomplement.Integers <- function(x, y, simplify = TRUE){
  if(getR6Class(y) == "PosIntegers")
    return(NegIntegers$new())
  else if(getR6Class(y) == "NegIntegers")
    return(PosIntegers$new())
  else
    return(setcomplement.Interval(x, y))
}
#' @rdname setcomplement
#' @export
setcomplement.ComplementSet <- function(x, y, simplify = TRUE){
  x$addedSet - (x$subtractedSet + y)
}

#' @rdname setcomplement
#' @export
`-.Set` <- function(x, y){
  setcomplement(x, y)
}

