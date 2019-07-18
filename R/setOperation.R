#' @include RSmisc_helpers.R

#' @title Symbolic Operations for SetInterval
#'
#'
#' @description Operations for SetInterval objects and subclasses, symbolic only.
#' @return An R6 object of class SetInterval.
#' @name setOperation
#'
#' @param unicode unicode symbol for the setOperation.
#' @param sets list of sets and/or intervals to combine via the setOperation.
#' @param lower lower bound of new SetInterval
#' @param upper upper bound of new SetInterval
#' @param type type of new SetInterval
#' @param dim dimension of new SetInterval
#'
#' @details Generally not recommended to use this function directly but instead
#'   via one of the implemented operations.
#'
#' @seealso \code{\link{product.SetInterval}}, \code{\link{union.SetInterval}}, \code{\link{complement.SetInterval}},
#' \code{\link{power.SetInterval}}
#'
#' @export
setOperation <- function(unicode,sets,lower=NULL,upper=NULL,type=NULL,dim=NULL){
  symbols = lapply(sets,function(x){
    x <- x[["getSymbol"]]()
    if(!grepl("\\{.",x))
      x <- paste0("{", x)
    if(!grepl(".\\}",x))
      x <- paste0(x,"}")
    return(x)
  })

  if(is.null(lower)) lower = as.numeric(unlist(lapply(sets, function(x) x$inf())))
  if(is.null(upper)) upper = as.numeric(unlist(lapply(sets, function(x) x$sup())))
  if(is.null(dim)) dim = length(sets)
  if(is.null(type)) type = "{}"

  setSymbol <- paste(unlist(symbols), collapse = paste0(" ",unicode," "))
  return(SetInterval$new(symbol = setSymbol, type = type, lower = lower,
                         upper = upper, dimension = dim))
}
