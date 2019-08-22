#' @title Symbolic Cartesian Product for SetInterval
#'
#' @description Makes a symbolic representation for the cartesian product of sets/intervals.
#'
#' @param x SetInterval
#' @param y SetInterval
#' @param alpha If \code{FuzzySet}, where to \code{alphaCut}
#' @param tuple If TRUE returns \code{Tuple}, otherwise \code{Set}
#' @param ... SetIntervals to take the cartesian product of.
#'
#' @details This does not calculate the cartesian product of the arguments but
#'   is just a symbolic representation using unicode.
#'
#' @seealso \code{\link{union.SetInterval}}, \code{\link{setdiff}},
#' \code{\link{power.SetInterval}}
#'
#' @examples
#' PosNaturals$new() * Reals$new()
#' product.SetInterval(PosNaturals$new(), Reals$new())
#'
#' @export
product <- function(x, y, ...){
  UseMethod("product", x)
}

#' @rdname product
#' @export
product.SetInterval <- function(x, y){
  if(x$equals(y))
  if(length(unique(sapply(dots,function(x) x$strprint()))) == 1 & length(dots)>1)
    return(power.SetInterval(dots[[1]], length(dots)))
  else
    return(setOperation("\u00D7", sets = dots))
}

#' @rdname product
#' @export
product.Set <- function(x, y, alpha = 1, tuple = FALSE){
  if(testFuzzySet(y))
    y <- y$alphaCut(alpha, FALSE, TRUE)

  if(testSet(x)){
    if(tuple)
      return(Tuple$new(apply(expand.grid(x$elements(), y$elements()), 1,
                           function(z) Tuple$new(z)), dimension = 2))
    else
      return(Set$new(apply(expand.grid(x$elements(), y$elements()), 1,
                           function(z) Tuple$new(z)), dimension = 2))
  }
}

#' @rdname product
#' @export
product.FuzzySet <- function(x, y){
  stop("Product of fuzzy sets is currently unsupported.")
}

#' @rdname product
#' @export
`*.SetInterval` <- function(x, y){
  print("C")
  product.SetInterval(x, y)
}

#' @rdname product
#' @export
`*.Set` <- function(x, y){
  print("a")
  product.Set(x, y, 1)
}

#' @rdname product
#' @export
 `*.FuzzySet` <- function(x, y){
   print("b")
   product.FuzzySet(x, y)
 }
