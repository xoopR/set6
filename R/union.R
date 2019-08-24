#' @title Symbolic Unions for Set
#'
#' @description Makes a symbolic representation for the union of sets/intervals.
#'
#' @name union.Set
#'
#' @usage union.Set(..., dim = 1)
#'
#' @param ... Sets to take the union of.
#' @param dim dimension of new Set.
#'
#' @details This does not calculate the union of the arguments but
#'   is just a symbolic representation using unicode.
#'
#' @seealso \code{\link{product.Set}}, \code{\link{setdiff}},
#' \code{\link{power.Set}}
#'
#' @examples
#' PosNaturals$new() + Reals$new()
#' union.Set(PosNaturals$new(), Reals$new())
#'
#' @export
union.Set <- function(..., dim = 1){
  dots = list(...)

  if(length(dots) == 1)
    return(dots[[1]])

  class = lapply(dots, getR6Class)
  parClass = lapply(class, function(x) get(x)$inherit)
  dots = dots[!(parClass %in% class)]

  if(length(unique(unlist(lapply(dots,function(y) y$strprint())))) != 1){
    lower = min(sapply(dots, function(y) y$lower))
    upper = max(sapply(dots, function(y) y$upper))
    setOperation("\u222A", sets = dots, dim = dim, lower = lower, upper = upper)
  }else
    return(dots[[1]])
}

#' @usage \method{+}{Set}(x, y)
#' @rdname union.Set
#' @param x Set
#' @param y Set
#' @export
`+.Set` <- function(x, y){
  union.Set(x, y)
}
