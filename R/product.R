#' @name product
#' @param ... [Set]s
#' @param x,y [Set]
#' @param simplify logical, if `TRUE` returns the result in its simplest (unwrapped) form, usually a `Set`
#' otherwise a `ProductSet`.
#' @param nest logical, if `FALSE` (default) then will treat any [ProductSet]s passed to `...` as unwrapped
#' [Set]s. See details and examples.
#' @title Cartesian Product of Sets
#' @return Either an object of class `ProductSet` or an unwrapped object inheriting from `Set`.
#' @description Returns the cartesian product of objects inheriting from class `Set`.
#'
#' @details The cartesian product of multiple sets is often implemented in programming languages as being
#' identical to the cartesian product of two sets being applied recursively. However, for sets \eqn{X, Y, Z},
#' \deqn{X × Y × Z \ne (X × Y) × Z}{X × Y × Z != (X × Y) × Z} [product] accomodates this by
#' adding the `nest` argument. If `nest == FALSE` then \eqn{X × Y × Z == (X × Y) × Z}, otherwise they
#' are identical. As it appears the former (`!nest`) is more common, `nest = FALSE` is the default and thus
#' `X * Y * Z` = \eqn{X × Y × Z}. Use `product(X, Y, Z)` for \eqn{(X × Y) × Z}. The cartesian
#' product of \eqn{N} sets, \eqn{X1,...,XN}, is defined as
#' \deqn{X1 × ... × XN = \\{(x1,...,xN) : x1 \epsilon X1 \cap ... \cap xN \epsilon XN\\}}{X1 × ... × XN = {(x1,...,xN) : x1 \epsilon X1 and ... and xN \epsilon xN\}}
#' where \eqn{(x1,...,xN)} is a tuple.
#'
#' @family operators
#' @examples
#' # difference between nesting
#' Set$new(1, 2) * Set$new(2, 3) * Set$new(4, 5)
#' product(Set$new(1, 2) * Set$new(2, 3), Set$new(4, 5), nest = FALSE) # same as above
#' product(Set$new(1, 2) * Set$new(2, 3), Set$new(4, 5), nest = TRUE)
#' unnest_set = product(Set$new(1, 2) * Set$new(2, 3), Set$new(4, 5), nest = FALSE)
#' nest_set = product(Set$new(1, 2) * Set$new(2, 3), Set$new(4, 5), nest = TRUE)
#' # note the difference when using contains
#' unnest_set$contains(Tuple$new(1,3,5))
#' nest_set$contains(Tuple$new(Tuple$new(1, 3), 5))
#'
#' # product of two sets
#' Set$new(-2:4) * Set$new(2:5)
#' product(Set$new(1,4,"a"), Set$new("a", 6))
#' product(Set$new(1,4,"a"), Set$new("a", 6), simplify = TRUE)
#'
#' # product of two intervals
#' Interval$new(1, 10) * Interval$new(5, 15)
#' Interval$new(1, 2, type = "()") * Interval$new(2, 3, type = "(]")
#' Interval$new(1, 5, class = "integer") *
#'     Interval$new(2, 7, class = "integer")
#'
#' # product of mixed set types
#' Set$new(1:10) * Interval$new(5, 15)
#' Set$new(5,7) * Tuple$new(6, 8, 7)
#' FuzzySet$new(1,0.1) * Set$new(2)
#'
#' # product of FuzzySet
#' FuzzySet$new(1, 0.1, 2, 0.5) * Set$new(2:5)
#'
#' # product of conditional sets
#' ConditionalSet$new(function(x, y) x >= y) *
#'     ConditionalSet$new(function(x, y) x == y)
#'
#' # product of special sets
#' PosReals$new() * NegReals$new()
#'
#' @export
product <- function(..., simplify = FALSE, nest = FALSE){
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
  mat = cbind(expand.grid(rsapply(sets, elements, active = T)),
              expand.grid(rsapply(sets, membership)))
  return(Set$new(apply(mat, 1, function(x) FuzzyTuple$new(elements = x[1:(ncol(mat)/2)],
                                           membership = x[((ncol(mat)/2)+1):(ncol(mat))]))))
}

#' @rdname product
#' @export
`*.Set` <- function(x, y){
  product(x, y)
}
