#' @name setproduct
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
#' @details The cartesian product of multiple sets, the 'n-ary Cartesian product', is often
#' implemented in programming languages as being identical to the cartesian product of two sets applied recursively.
#' However, for sets \eqn{X, Y, Z},
#' \deqn{XYZ \ne (XY)Z}{X × Y × Z != (X × Y) × Z}
#' This is accommodated with the `nest` argument. If `nest == TRUE` then \eqn{X*Y*Z == (X × Y) × Z}, i.e. the cartesian
#' product for two sets is applied recursively. If `nest == FALSE` then \eqn{X*Y*Z == (X × Y × Z)} and
#' the n-ary cartesian product is computed. As it appears the latter (n-ary product) is more common, `nest = FALSE`
#' is the default. The N-ary cartesian product of \eqn{N} sets, \eqn{X1,...,XN}, is defined as
#' \deqn{X1 × ... × XN = \\{(x1,...,xN) : x1 \epsilon X1 \cap ... \cap xN \epsilon XN\\}}{X1 × ... × XN = {(x1,...,xN) : x1 \epsilon X1 and ... and xN \epsilon xN}}
#' where \eqn{(x1,...,xN)} is a tuple.
#'
#' The product of fuzzy and crisp sets first coerces fuzzy sets to crisp sets by finding their support.
#'
#' @family operators
#' @examples
#' # difference between nesting
#' Set$new(1, 2) * Set$new(2, 3) * Set$new(4, 5)
#' setproduct(Set$new(1, 2) * Set$new(2, 3), Set$new(4, 5), nest = FALSE) # same as above
#' setproduct(Set$new(1, 2) * Set$new(2, 3), Set$new(4, 5), nest = TRUE)
#' unnest_set <- setproduct(Set$new(1, 2) * Set$new(2, 3), Set$new(4, 5), nest = FALSE)
#' nest_set <- setproduct(Set$new(1, 2) * Set$new(2, 3), Set$new(4, 5), nest = TRUE)
#' # note the difference when using contains
#' unnest_set$contains(Tuple$new(1, 3, 5))
#' nest_set$contains(Tuple$new(Tuple$new(1, 3), 5))
#'
#' # product of two sets
#' Set$new(-2:4) * Set$new(2:5)
#' setproduct(Set$new(1, 4, "a"), Set$new("a", 6))
#' setproduct(Set$new(1, 4, "a"), Set$new("a", 6), simplify = TRUE)
#'
#' # product of two intervals
#' Interval$new(1, 10) * Interval$new(5, 15)
#' Interval$new(1, 2, type = "()") * Interval$new(2, 3, type = "(]")
#' Interval$new(1, 5, class = "integer") *
#'   Interval$new(2, 7, class = "integer")
#'
#' # product of mixed set types
#' Set$new(1:10) * Interval$new(5, 15)
#' Set$new(5, 7) * Tuple$new(6, 8, 7)
#' FuzzySet$new(1, 0.1) * Set$new(2)
#'
#' # product of FuzzySet
#' FuzzySet$new(1, 0.1, 2, 0.5) * Set$new(2:5)
#'
#' # product of conditional sets
#' ConditionalSet$new(function(x, y) x >= y) *
#'   ConditionalSet$new(function(x, y) x == y)
#'
#' # product of special sets
#' PosReals$new() * NegReals$new()
#' @export
setproduct <- function(..., simplify = FALSE, nest = FALSE) {
  if (...length() == 0) {
    return(Set$new())
  } else if (...length() == 1) {
    return(assertSet(...elt(1)))
  }

  sets <- operation_cleaner(list(...), "ProductSet", nest, simplify = simplify)
  if (length(sets) == 1) {
    return(sets[[1]])
  } else if (length(sets) == 0) {
    return(Set$new())
  }

  classes <- sapply(sets, getR6Class)
  if ("ExponentSet" %in% classes) {
    varexp <- sapply(sets[classes == "ExponentSet"], function(x) x$power == "n")
    if (any(varexp)) {
      return(sets[classes == "ExponentSet"][which(varexp)[1]][[1]])
    }
  }

  if (length(unique(rsapply(sets, "strprint"))) == 1 & !simplify) {
    return(ExponentSet$new(sets[[1]], length(sets)))
  } else if (any(sapply(sets, function(x) inherits(x, "SetWrapper"))) |
    any(grepl("ConditionalSet|Interval", classes)) | !simplify) {
    return(ProductSet$new(sets))
  } else if (any(grepl("FuzzySet|FuzzyTuple", unique(classes)))) {
    return(.product_fuzzyset(sets))
  } else {
    (any(grepl("Set|Tuple", unique(classes))))
  }
  return(.product_set(sets, nest))
}

.product_set <- function(sets, nest) {
  if (!nest | length(sets) < 3) {
    return(Set$new(elements = apply(expand.grid(rlapply(sets, "elements", active = T)), 1, function(z) Tuple$new(elements = z))))
  } else {
    s <- Set$new(elements = apply(expand.grid(sets[[1]]$elements, sets[[2]]$elements), 1, function(z) Tuple$new(elements = z)))
    for (i in 3:length(sets)) {
      s <- Set$new(elements = apply(expand.grid(s$elements, sets[[3]]$elements), 1, function(z) Tuple$new(elements = z)))
    }
    return(s)
  }
}
.product_fuzzyset <- function(sets) {
  mat <- cbind(
    expand.grid(rlapply(sets, "elements", active = T)),
    expand.grid(rlapply(sets, "membership"))
  )
  return(Set$new(elements = apply(mat, 1, function(x) {
    FuzzyTuple$new(
      elements = x[1:(ncol(mat) / 2)],
      membership = x[((ncol(mat) / 2) + 1):(ncol(mat))]
    )
  })))
}

#' @rdname setproduct
#' @export
`*.Set` <- function(x, y) {
  setproduct(x, y)
}
