#' @name product
#' @param x,y Set
#' @param simplify logical, if `TRUE` (default) returns the result in its simplest form, usually a `Set`
#' otherwise a `ProductSet`
#' @param ... additional arguments
#' @title Cartesian Product of Two Sets
#' @return An object inheriting from `Set` containing the cartesian product of elements in both `x` and `y`.
#' @description Returns the cartesian product of two objects inheriting from class `Set`.
#' @details The cartesian product of two sets, \eqn{X, Y}, is defined as the set of elements that exist
#' in one or both sets,
#' \deqn{P = \{(x, y) : x \epsilon X and y \epsilon Y\}}{U = {(x, y) : x \epsilon X and y \epsilon Y}}
#' where \eqn{(x, y)} is a tuple.
#'
#' The product of two [ConditionalSet]s is currently defined as the same as the intersection of two
#' [ConditionalSet]s, this may change in the future. See examples.
#'
#' @examples
#' # product of two sets
#'
#' Set$new(-2:4) * Set$new(2:5)
#' product(Set$new(1,4,"a"), Set$new("a", 6))
#' product(Set$new(1,4,"a"), Set$new("a", 6), simplify = FALSE)
#'
#' # product of two intervals
#'
#' Interval$new(1, 10) * Interval$new(5, 15)
#' Interval$new(1, 2, type = "()") * Interval$new(2, 3, type = "(]")
#' Interval$new(1, 5, class = "integer") *
#'     Interval$new(2, 7, class = "integer")
#'
#' # product of mixed set types
#'
#' Set$new(1:10) * Interval$new(5, 15)
#' Set$new(5,7) * Tuple$new(6, 8, 7)
#' FuzzySet$new(1,0.1) * Set$new(2)
#'
#' # product of FuzzySet
#' FuzzySet$new(1, 0.1, 2, 0.5) * Set$new(2:5)
#' # not the same when the order is reversed
#' Set$new(2:5) * FuzzySet$new(1, 0.1, 2, 0.5)
#'
#' # product of conditional sets
#'
#' ConditionalSet$new(function(x, y) x >= y) *
#'     ConditionalSet$new(function(x, y) x == y)
#'
#' # product of special sets
#' PosReals$new() * NegReals$new()
#'
#' @export
product <- function(..., nest = FALSE, simplify = FALSE){
  sets = operation_cleaner(list(...), "ProductSet", nest)

  if(length(sets) == 0)
    return(Set$new())
  else if(length(sets) == 1)
    return(sets[[1]])


  classes = sapply(sets, getR6Class)

  if(length(unique(rsapply(sets, strprint))) == 1 & !simplify)
    return(ExponentSet$new(sets[[1]], length(sets)))
  else if (any(grepl("ConditionalSet|Interval|SetWrapper", classes)) | !simplify)
    return(ProductSet$new(sets))
  else if (grepl("FuzzySet|FuzzyTuple", unique(classes)))
    return(.product_fuzzyset(sets))
  else (grepl("Set|Tuple", unique(classes)))
    return(.product_set(sets))
}

.product_set <- function(sets){
  Set$new(apply(expand.grid(rlapply(sets, elements,active = T)), 1, function(z) Tuple$new(z)))
}
.product_fuzzyset <- function(sets){
  mat = cbind(expand.grid(rsapply(sets, elements,active = T)),
              expand.grid(rsapply(sets, membership)))
  return(Set$new(apply(mat, 1, function(x) FuzzyTuple$new(elements = x[1:(ncol(mat)/2)],
                                           membership = x[((ncol(mat)/2)+1):(ncol(mat))]))))
}

#' @rdname product
#' @export
`*.Set` <- function(x, y){
  product(x, y)
}
