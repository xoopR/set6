#' @name symdiff
#' @param x,y Set
#' @title Symmetric Difference of Two Sets
#' @return An object inheriting from `Set` containing the symmetric difference of elements in both `x` and `y`.
#' @description Returns the symmetric difference of two objects inheriting from class `Set`.
#' @details The symmetric difference, aka disjunctive union, of two sets, \eqn{X, Y}, is defined as the set
#' of elements that exist in set \eqn{X} or in \eqn{Y} but not both,
#' \deqn{\{z : (z \epsilon X \cup z \epsilon Y) \cap !(z \epsilon X \cap z \epsilon Y)\}}{\{z : (z \epsilon X or z \epsilon Y) and !(z \epsilon X and z \epsilon Y)\}}
#'
#' The symmetric difference can also be expressed as the union of two sets minus the intersection.
#' Therefore `symdiff` is written as a thin wrapper over these operations, so for two sets, `A,B`: \cr
#' `A %-% B = (A | B) - (A & B)`
#'
#' @examples
#' # symdiff compared to union and intersection
#' Set$new(1,2,3) %-% Set$new(3, 4)
#' (Set$new(1,2,3) | Set$new(3, 4)) - (Set$new(1,2,3) & Set$new(3, 4))
#'
#' # ConditionalSets demonstrate the internal logic
#' ConditionalSet$new(function(x) x > 0) %-%
#'   ConditionalSet$new(function(y) y == 0)
#'
#' @export
symdiff <- function(x, y){
  if(x <= y)
    return(y - x)
  else if(y <= x)
    return(x - y)
  else
    return((x + y) - (x & y))
}
#' @rdname symdiff
#' @export
`%-%` <- function(x, y){
  symdiff(x, y)
}
