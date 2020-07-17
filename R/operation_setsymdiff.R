#' @name setsymdiff
#' @param x,y Set
#' @param simplify logical, if `TRUE` (default) returns the result in its simplest form, usually a `Set` or
#' [UnionSet], otherwise a `ComplementSet`.
#' @title Symmetric Difference of Two Sets
#' @return An object inheriting from `Set` containing the symmetric difference of elements in both `x` and `y`.
#' @description Returns the symmetric difference of two objects inheriting from class `Set`.
#' @details The symmetric difference, aka disjunctive union, of two sets, \eqn{X, Y}, is defined as the set
#' of elements that exist in set \eqn{X} or in \eqn{Y} but not both,
#' \deqn{\{z : (z \epsilon X \cup z \epsilon Y) \\ \cap \\ \neg(z \epsilon X \cap z \epsilon Y)\}}{\{z : (z \epsilon X or z \epsilon Y) and !(z \epsilon X and z \epsilon Y)\}}
#'
#' The symmetric difference can also be expressed as the union of two sets minus the intersection.
#' Therefore `setsymdiff` is written as a thin wrapper over these operations, so for two sets, `A,B`: \cr
#' `A %-% B = (A | B) - (A & B)`.
#'
#' The symmetric difference of fuzzy and crisp sets first coerces fuzzy sets to crisp sets by finding their support.
#'
#' @family operators
#' @examples
#' # symmetrical difference compared to union and intersection
#' Set$new(1, 2, 3) %-% Set$new(3, 4)
#' (Set$new(1, 2, 3) | Set$new(3, 4)) - (Set$new(1, 2, 3) & Set$new(3, 4))
#'
#' # ConditionalSets demonstrate the internal logic
#' ConditionalSet$new(function(x) x > 0) %-%
#'   ConditionalSet$new(function(y) y == 0)
#' @export
setsymdiff <- function(x, y, simplify = TRUE) {
  if (x <= y) {
    return(setcomplement(y, x, simplify = simplify))
  } else if (y <= x) {
    return(setcomplement(x, y, simplify = simplify))
  } else {
    return(setcomplement(setunion(x, y, simplify = simplify), setintersect(x, y), simplify = simplify))
  }
}
#' @rdname setsymdiff
#' @export
`%-%` <- function(x, y) {
  setsymdiff(x, y)
}
