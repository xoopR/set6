#' @title Symbolic Cartesian Product for SetInterval
#'
#' @description Makes a symbolic representation for the cartesian product of sets/intervals.
#'
#' @usage product.SetInterval(...)
#'
#' @param ... SetIntervals to take the cartesian product of.
#'
#' @details This does not calculate the cartesian product of the arguments but
#'   is just a symbolic representation using unicode.
#'
#' @seealso \code{\link{union.SetInterval}}, \code{\link{complement.SetInterval}},
#' \code{\link{power.SetInterval}}
#'
#' @examples
#' PosNaturals$new() * Reals$new()
#' product.SetInterval(PosNaturals$new(), Reals$new())
#'
#' @export
product.SetInterval <- function(...){
  dots = list(...)
  if(length(unique(sapply(dots,function(x) x$strprint()))) == 1 & length(dots)>1)
    return(power.SetInterval(dots[[1]], length(dots)))
  else
    return(setOperation("\u00D7", sets = dots))
}

#' @usage \method{*}{SetInterval}(x, y)
#' @rdname product.SetInterval
#' @param x SetInterval
#' @param y SetInterval
#' @export
`*.SetInterval` <- function(x, y){
  product.SetInterval(x, y)
}
