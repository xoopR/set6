#' @name power
#' @rdname power
#' @param x,e1 Set
#' @param power,e2 power to raise set to
#' @param simplify logical, if `TRUE` returns the result in its simplest (unwrapped) form, usually a `Set`
#' otherwise a `ExponentSet`.
#' @param nest logical, if `FALSE` (default) then will treat any [ProductSet]s passed to `...` as unwrapped
#' [Set]s. See details and examples.
#' @param ... additional arguments
#' @title Power of a Set
#' @return An R6 object of class `Set` or ExponentSet` inheriting from `ProductSet`.
#' @description A convenience wrapper for the cartesian product of a `Set` by itself, possibly multiple times.
#'
#' @family operators
#' @examples
#' # Power of a Set
#' power(Set$new(1, 2), 3, simplify = FALSE)
#' power(Set$new(1, 2), 3, simplify = TRUE)
#' Set$new(1,2)^3
#'
#' # Power of an interval
#' Interval$new(2, 5)^5
#' Reals$new()^3
#'
#' # Use tuples for contains
#' (PosNaturals$new()^3)$contains(Tuple$new(1, 2, 3))
#'
#' # Power of ConditionalSet is meaningless
#' ConditionalSet$new(function(x) TRUE)^2
#'
#' # Power of FuzzySet
#' FuzzySet$new(1,0.1,2,0.5)^2
#'
#' @export
power <- function(x, power, simplify = FALSE, nest = FALSE, ...){
  if(power == 1)
    return(x)

  if (getR6Class(x) %in% c("Set", "FuzzySet", "Tuple", "FuzzyTuple") & simplify){
    x = rep(list(x), power)
    return(do.call(product, c(x, list(nest = nest, simplify = TRUE))))
  } else if (inherits(x, "ExponentSet"))
    return(ExponentSet$new(x$wrappedSets[[1]], x$power * power))
  else
    return(ExponentSet$new(x, power))
}

#' @rdname power
#' @export
`^.Set` <- function(e1, e2){
  power(e1, e2)
}
