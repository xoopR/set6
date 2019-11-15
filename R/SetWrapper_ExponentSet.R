#' @template SetWrapper
#' @templateVar operation exponentiation
#' @templateVar class ExponentSet
#' @templateVar constructor ExponentSet$new(setlist, lower = NULL, upper = NULL, type = NULL)
#' @templateVar arg1 `setlist` \tab list \tab List of sets to wrap. \cr
#'
#' @export
NULL
ExponentSet <- R6::R6Class("ExponentSet", inherit = ProductSet)
ExponentSet$set("private", ".power", 1)
ExponentSet$set("active", "power", function() return(private$.power))
ExponentSet$set("public", "initialize", function(set, power){
  lower = Tuple$new(rep(set$lower, power))
  upper = Tuple$new(rep(set$upper, power))
  type = "{}"
  setlist = rep(list(set), power)
  private$.power = power
  super$initialize(setlist = setlist, lower = lower, upper = upper, type = type)
})
ExponentSet$set("public", "strprint", function(n = 2){
  paste(self$wrappedSets[[1]]$strprint(n=n), self$power, sep = "^")
})

#' @name power
#' @rdname power
#' @param x,e1 Set
#' @param power,e2 power to raise set to
#' @param simplify logical, if `TRUE` (default) returns the result in its simplest form, usually a `Set`
#' otherwise a `ExponentSet`
#' @param ... additional arguments
#' @title Power of a Set
#' @return An R6 object of class `Set` or ExponentSet` inheriting from `ProductSet`.
#' @description A convenience wrapper for the cartesian product of a `Set` by itself, possibly multiple times.
#'
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
power <- function(x, power, ...){
  UseMethod("power")
}
#' @rdname power
#' @export
power.Set <- function(x, power, simplify = TRUE, ...){
  if(simplify){
    y = x
    for (i in 1:(power-1))
      y = product(x, y)
    return(y)
  } else {
    ExponentSet$new(x, power)
  }
}
#' @rdname power
#' @export
power.Interval <- function(x, power, ...){
  ExponentSet$new(x, power)
}
#' @rdname power
#' @export
power.ConditionalSet <- function(x, power, ...){
  return(x)
}
#' @rdname power
#' @export
power.ExponentSet <- function(x, power, ...){
  ExponentSet$new(x$wrappedSets[[1]], x$power * power)
}
#' @rdname power
#' @export
`^.Set` <- function(e1, e2){
  power(e1, e2)
}
