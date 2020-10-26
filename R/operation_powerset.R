#' @name powerset
#' @rdname powerset
#' @title Calculate a Set's Powerset
#' @description Calculates and returns the powerset of a Set.
#' @details A powerset of a set, S, is defined as the set of all subsets of S, including S itself
#' and the empty set.
#' @param x [Set]
#' @param simplify logical, if `TRUE` then tries to simplify the result to a `Set` otherwise
#' creates an object of class [PowersetSet].
#' @return [Set]
#' @family operators
#' @examples
#' # simplify = FALSE is default
#' powerset(Set$new(1, 2))
#' powerset(Set$new(1, 2), simplify = TRUE)
#'
#' # powerset of intervals
#' powerset(Interval$new())
#'
#' # powerset of powersets
#' powerset(powerset(Reals$new()))
#' powerset(powerset(Reals$new()))$properties$cardinality
#' @export
powerset <- function(x, simplify = FALSE) {
  if (getR6Class(x) == "Universal") {
    return(x)
  }

  if (x$properties$empty) {
    return(Set$new(Set$new()))
  }

  if (simplify & testFuzzy(x)) {
    return(.powerset_fuzzy(x))
  } else if (simplify & testCrisp(x)) {
    return(.powerset_crisp(x))
  } else {
    return(PowersetSet$new(x))
  }
}

.powerset_fuzzy <- function(x) {
  y <- Vectorize(function(m) utils::combn(x$elements, m, simplify = FALSE),
                 vectorize.args = c("m"))(1:(x$length - 1))
  y <- lapply(unlist(y), function(el) getR6Class(x, FALSE)$new(elements = el,
                                                               membership = x$membership(el)))
  return(Set$new(elements = c(Set$new(), y, x)))
}

.powerset_crisp <- function(x) {
  elements <- x$elements
  y <- Vectorize(function(m) utils::combn(elements, m, simplify = FALSE), vectorize.args = c("m"),
                 SIMPLIFY = FALSE)(1:(x$length - 1))

  try(
    {return(Set$new(elements = c(Set$new(), unlist(apply(y, 2, get(paste0("as.", getR6Class(x))))), x)))},
    silent = TRUE
  )

  return(Set$new(elements = c(Set$new(), unlist(lapply(y, get(paste0("as.", getR6Class(x))))), x)))
}
