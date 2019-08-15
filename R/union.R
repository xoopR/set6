#' @title Symbolic Unions for SetInterval
#'
#' @description Makes a symbolic representation for the union of sets/intervals.
#'
#' @name union.SetInterval
#'
#' @usage union.SetInterval(..., dim = 1)
#'
#' @param ... SetIntervals to take the union of.
#' @param dim dimension of new SetInterval.
#'
#' @details This does not calculate the union of the arguments but
#'   is just a symbolic representation using unicode.
#'
#' @seealso \code{\link{product.SetInterval}}, \code{\link{setdiff}},
#' \code{\link{power.SetInterval}}
#'
#' @examples
#' PosNaturals$new() * Reals$new()
#' product.SetInterval(PosNaturals$new(), Reals$new())
#'
#' @export
union.SetInterval <- function(..., dim = 1){
  dots = list(...)

  if(length(dots) == 1)
    return(dots[[1]])

  class = lapply(dots, getR6Class)
  parClass = lapply(class, function(x) get(x)$inherit)
  dots = dots[!(parClass %in% class)]

  if(length(unique(unlist(lapply(dots,function(y) y$strprint())))) != 1){
    lower = min(sapply(dots, function(y) y$inf()))
    upper = max(sapply(dots, function(y) y$sup()))
    setOperation("\u222A", sets = dots, dim = dim, lower = lower, upper = upper)
  }else
    return(dots[[1]])
}

#' @usage \method{+}{SetInterval}(x, y)
#' @rdname union.SetInterval
#' @param x SetInterval
#' @param y SetInterval
#' @export
`+.SetInterval` <- function(x, y){
  union.SetInterval(x, y)
}
