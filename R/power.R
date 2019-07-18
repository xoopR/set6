
#' @title Symbolic Exponentiation for SetInterval
#'
#' @description Makes a symbolic representation for the exponentiation of a given set/interval.
#'
#' @name power.SetInterval
#'
#' @usage power.SetInterval(x, power)
#'
#' @param x SetInterval
#' @param power power to raise SetInterval to
#'
#' @details This does not calculate the exponentiation but
#'   is just a symbolic representation using unicode.
#'
#' @seealso \code{\link{product.SetInterval}}, \code{\link{union.SetInterval}},
#' \code{\link{complement.SetInterval}}
#'
#' @examples
#' PosNaturals$new() ^ 2
#' power.SetInterval(Reals$new(), 3)
#'
#' @export
power.SetInterval <- function(x, power){
  symbol = paste0(x$getSymbol(),"^",power)
  lower = rep(x$inf(),power)
  upper = rep(x$sup(),power)

  SetInterval$new(symbol = symbol, type = x$type(), lower = lower,
                  upper = upper, dimension = power)
}

#' @usage \method{^}{SetInterval}(x, power)
#' @rdname power.SetInterval
#' @export
`^.SetInterval` <- function(x, power){
  power.SetInterval(x, power)
}
