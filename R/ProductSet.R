ProductSet <- R6::R6Class("ProductSet", inherit = SetWrapper)
ProductSet$set("public", "initialize", function(setlist, lower = NULL, upper = NULL, type = NULL){
  if(is.null(lower)) lower = sapply(setlist, function(x) x$lower)
  if(is.null(upper)) upper = sapply(setlist, function(x) x$upper)
  if(is.null(type)) type = "{}"
  super$initialize(setlist = setlist, lower = lower, upper = upper, type = type)
})
ProductSet$set("active", "length", function(){
  return(sapply(self$wrappedSets, function(x) x$length))
})

ProductSet$set("public","strprint",function(n = 2){
  str = lapply(self$wrappedSets, function(x) x$strprint(n))
  if(length(unique(str)) == 1)
    return(paste(unique(str), length(str), sep = "^"))
  else
    return(paste0("{",paste(str, collapse = " \u00D7 "),"}"))
})

#' @name product
#' @rdname product
#' @export
product <- function(x, y){
  UseMethod("product", x)
}
#' @export
product.Set <- function(x, y){
  if(inherits(y, "SetWrapper"))
    return(ProductSet$new(c(list(x), y$wrappedSets)))

  if(x$length == 0 & y$length == 0) return(Set$new())
  if(x$length == 0) return(y)
  if(y$length == 0) return(x)

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
  if(!testConditionalSet(y)){
    message(sprintf("Product of %s and %s not compatible.", x$strprint(), y$strprint()))
    return(Set$new())
  }else{
    if(x$equals(y))
      return(x)
    else{
      if(all(names(formals(x$condition)) == names(formals(y$condition)))){
        condition = function(){}
        formals(condition) = formals(x$condition)
        body(condition) = substitute(bx & by,
                                     list(bx = body(x$condition),
                                          by = body(y$condition)))
        argclass = unique(c(x$class, y$class))
        names(argclass) = names(formals(condition))
        return(ConditionalSet$new(condition = condition, argclass = argclass))
      } else
        stop("Conditional set conditions must have the same formal arguments.")
    }
  }
}
#' @export
product.SetWrapper <- function(x, y){
  return(ProductSet$new(c(x$wrappedSets, list(y))))
}

#' @rdname product
#' @usage \method{*}{Set}(x, y)
#' @param x Set
#' @param y Set
#' @export
`*.Set` <- function(x, y){
  product(x, y)
}
