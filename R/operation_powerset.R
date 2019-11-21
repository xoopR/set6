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
  else{
    if(simplify & !testInterval(x)){
      elements <- x$elements
      y = Vectorize(function(m) utils::combn(elements, m),vectorize.args = c("m"))(1:(x$length-1))
      return(Set$new(Set$new(), unlist(lapply(y, as.Set)), x))
    } else {
      PowersetSet$new(x)
    }
  }
}
