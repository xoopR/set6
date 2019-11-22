#' @name powerset
#' @rdname powerset
#' @title Calculate a Set's Powerset
#' @description Calculates and returns the powerset of a Set.
#' @details A powerset of a set, S, is defined as the set of all subsets of S, including S itself and
#' the empty set.
#' @param x [Set]
#' @param simplify logical, if `TRUE` then tries to simplify the result to a `Set` otherwise
#' creates an object of class [PowersetSet].
#' @return [Set]
#' @family operators
#' @export
powerset <- function(x, simplify = FALSE){
  if(x$properties$empty)
    return(Set$new(Set$new()))

  if(simplify & testFuzzyTuple(x))
    return(.powerset_fuzzytuple(x))
  else if(simplify & testFuzzy(x))
    return(.powerset_fuzzyset(x))
  else if(simplify & testTuple(x))
    return(.powerset_tuple(x))
  else if(simplify & getR6Class(x) == "Set")
    return(.powerset_set(x))
  else
    return(PowersetSet$new(x))
}

.powerset_fuzzytuple <- function(x){
  y = Vectorize(function(m) combn(x$elements, m, simplify = FALSE),vectorize.args = c("m"))(1:(x$length-1))
  # if(class(y[1,1]) == "list")
    y = lapply(unlist(y), function(el) FuzzyTuple$new(elements = el, membership = x$membership(el)))
  # else
  #   y = apply(y, 1, function(el){
  #     FuzzyTuple$new(elements = el, membership = x$membership(el))
  #   })
  return(Set$new(Set$new(), y, x))
}
.powerset_fuzzyset <- function(x){
  y = Vectorize(function(m) combn(x$elements, m, simplify = FALSE),vectorize.args = c("m"))(1:(x$length-1))
  # if(class(y[1,1]) == "list")
    y = lapply(unlist(y), function(el) FuzzySet$new(elements = el, membership = x$membership(el)))
  # else
  #   y = apply(y, 1, function(el){
  #     FuzzySet$new(elements = el, membership = x$membership(el))
  #   })
  return(Set$new(Set$new(), y, x))
}
.powerset_tuple <- function(x){
  elements <- x$elements
  y = Vectorize(function(m) combn(elements, m, simplify = FALSE),vectorize.args = c("m"))(1:(x$length-1))
  return(Set$new(Set$new(), unlist(lapply(y, as.Tuple)), x))
}
.powerset_set <- function(x){
  elements <- x$elements
  y = Vectorize(function(m) utils::combn(elements, m, simplify = FALSE),vectorize.args = c("m"))(1:(x$length-1))
  return(Set$new(Set$new(), unlist(lapply(y, as.Set)), x))
}
