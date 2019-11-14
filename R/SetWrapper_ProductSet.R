#' @template SetWrapper
#' @templateVar operation product
#' @templateVar class ProductSet
#' @templateVar constructor ProductSet$new(setlist, lower = NULL, upper = NULL, type = NULL)
#' @templateVar arg1 `setlist` \tab list \tab List of sets to wrap. \cr
#'
#' @export
NULL
ProductSet <- R6::R6Class("ProductSet", inherit = SetWrapper)
ProductSet$set("public", "initialize", function(setlist, lower = NULL, upper = NULL, type = NULL){
  if(is.null(lower)) lower = sapply(setlist, function(x) x$lower)
  if(is.null(upper)) upper = sapply(setlist, function(x) x$upper)
  if(is.null(type)) type = "{}"
  super$initialize(setlist = setlist, lower = lower, upper = upper, type = type)
})
ProductSet$set("active", "length", function(){
  return(Tuple$new(sapply(self$wrappedSets, function(x) x$length)))
})
ProductSet$set("public","strprint",function(n = 2){
  str = lapply(self$wrappedSets, function(x) x$strprint(n))
  if(length(unique(str)) == 1)
    return(paste(unique(str), length(str), sep = "^"))
  else
    return(paste0("{",paste(str, collapse = " \u00D7 "),"}"))
})
ProductSet$set("public","liesInSet",function(x, all = FALSE, bound = FALSE){

  if(!testSetList(x))
    x = list(x)

  rets = sapply(x, function(y) ifelse(y$length == self$length$length, return(TRUE), return(FALSE)))

  rets[rets] = sapply(x[rets], function(el){
    ret = TRUE
    for (i in 1:el$length){
      if (!self$wrappedSets[[i]]$liesInSet(el$elements[i], bound = bound)){
        ret = FALSE
        break()
      }
    }
    return(ret)
  })

  if (all)
    return(all(rets))
  else
    return(unlist(rets))
})

#' @name product
#' @param x Set
#' @param y Set
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
product <- function(x, y){
  xl = x$length
  yl = y$length

  if(inherits(xl, "R6") | inherits(yl, "R6"))
    UseMethod("product", x)
  else{
    if(xl == 0 & yl == 0)
      return(Set$new())
    else if(xl == 0)
      return(y)
    else if(yl == 0)
      return(x)
    else
      UseMethod("product", x)
  }
}
#' @export
product.Set <- function(x, y){
  if(inherits(y, "SetWrapper"))
    return(ProductSet$new(c(list(x), y$wrappedSets)))

  if(testSet(y) & !testInterval(y) & !testConditionalSet(y))
    return(Set$new(apply(expand.grid(x$elements, y$elements), 1, function(z) Tuple$new(z))))
  else if(!testConditionalSet(y))
    return(ProductSet$new(list(x, y)))
  else {
    message(sprintf("Product of %s and %s not compatible.", x$strprint(), y$strprint()))
    return(Set$new())
  }
}
#' @export
product.Interval <- function(x, y){
  if(inherits(y, "SetWrapper"))
    return(ProductSet$new(c(list(x), y$wrappedSets)))

  if(all(x$length == 0) & all(y$length == 0)) return(Set$new())
  if(all(x$length == 0)) return(y)
  if(all(y$length == 0)) return(x)

  if (testConditionalSet(y)) {
    message(sprintf("Product of %s and %s not compatible.", x$strprint(), y$strprint()))
    return(Set$new())
  } else
    return(ProductSet$new(list(x, y)))
}
#' @export
product.FuzzySet <- function(x, y){
  if(inherits(y, "SetWrapper"))
    return(ProductSet$new(c(list(x), y$wrappedSets)))

  if(x$length == 0 & y$length == 0) return(Set$new())
  if(x$length == 0) return(y)
  if(y$length == 0) return(x)

  if (testConditionalSet(y) | testInterval(y)) {
    message(sprintf("Product of %s and %s not compatible.", x$strprint(), y$strprint()))
    return(Set$new())
  } else if (testFuzzyTuple(x)){
    return(ProductSet$new(list(x, as.FuzzyTuple(y))))
  } else {
    return(ProductSet$new(list(x, as.FuzzySet(y))))
  }
}
#' @export
product.ConditionalSet <- function(x, y){
  if(!inherits(y, "ConditionalSet"))
    return(Set$new())
  else {
    if(x$equals(y))
      return(x)
    else{
      condition = function(){}
      names = unique(names(c(formals(x$condition), formals(y$condition))))
      formals <- rep(list(bquote()), length(names))
      names(formals) = names
      formals(condition) = formals
      body(condition) = substitute(bx & by,
                                   list(bx = body(x$condition),
                                        by = body(y$condition)))
      # in future updates we can change this so the product of the argument classes is kept
      # not just the argclass of x
      class = c(x$class, y$class)[!duplicated(names(c(x$class, y$class)))]
      return(ConditionalSet$new(condition = condition, argclass = class))
    }
  }
}
#' @export
product.ProductSet <- function(x, y){
  ProductSet$new(c(x$wrappedSets, y))
}

#' @rdname product
#' @export
`*.Set` <- function(x, y){
  product(x, y)
}
