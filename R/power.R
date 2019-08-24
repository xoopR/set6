
#' @title Symbolic Exponentiation for Set
#'
#' @description Makes a symbolic representation for the exponentiation of a given set/interval.
#'
#' @name power.Set
#'
#' @usage power.Set(x, power)
#'
#' @param x Set
#' @param power power to raise Set to
#'
#' @details This does not calculate the exponentiation but
#'   is just a symbolic representation using unicode.
#'
#' @seealso \code{\link{product.Set}}, \code{\link{union.Set}},
#' \code{\link{setdiff}}
#'
#' @examples
#' PosNaturals$new() ^ 2
#' power.Set(Reals$new(), 3)
#'
#' @export
power.Set <- function(x, power){
  symbol = paste0(x$strprint(),"^",power)
  lower = rep(x$lower(),power)
  upper = rep(x$upper(),power)

  Set$new(symbol = symbol, type = x$type(), lower = lower,
                  upper = upper, dimension = power)
}

#' @usage \method{^}{Set}(x, power)
#' @rdname power.Set
#' @export
`^.Set` <- function(x, power){
  power.Set(x, power)
}
