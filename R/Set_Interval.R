#---------------------------------------------
# Documentation
#---------------------------------------------
#' @name Interval
#' @title Mathematical Finite or Infinite Interval
#'
#' @description A general Interval object for mathematical intervals, inheriting from `Set`. Intervals
#' may be open, closed, or half-open; as well as bounded above, below, or not at all.
#' @return R6 object of class Interval inheriting from [Set].
#' @template Set
#' @templateVar constructor Interval$new(lower = -Inf, upper = Inf, type = "[]", class = "numeric", universe = Reals$new())
#' @templateVar arg1 `lower` \tab numeric \tab Lower limit of the interval. \cr
#' @templateVar arg2 `upper` \tab numeric \tab Upper limit of the interval. \cr
#' @templateVar arg3 `type` \tab character \tab One of: '()', '(]', '[)', '[]', which specifies if inteval is open, left-open, right-open, or closed. \cr
#' @templateVar arg4 `class` \tab character \tab One of: 'numeric', 'integer', which specifies if interval is over the Reals or Integers. \cr
#' @templateVar arg5 `universe` \tab Set \tab Optional universe that the interval lives in.
#' @templateVar constructorDets If defaults are used then the Real number line is constructed. The optional `universe` argument is useful for taking the complement of the `Set`. If a universe isn't given then [Reals] is assumed.
#' @templateVar meth1 **Interval Methods** \tab **Link** \cr
#' @templateVar meth2 isSubinterval(x, proper = FALSE, all = FALSE) \tab [isSubinterval] \cr
#' @templateVar meth3  \tab \cr \tab \cr \tab \cr
#'
#' @details
#' The Interval class can be used for finite or infinite intervals, but often Sets will be preferred for
#' integer intervals over a continuous range.
#'
#' @examples
#' # Set of Reals
#' Interval$new()
#'
#' # Set of Integers
#' Interval$new(class = "integer")
#'
#' # Half-open interval
#' i = Interval$new(1, 10, "(]")
#' i$contains(c(1, 10))
#' i$contains(c(1, 10), bound = TRUE)
#'
#' # Equivalent Set and Interval
#' Set$new(1:5) == Interval$new(1,5,class="integer")
#'
#' # SpecialSets can provide more efficient implementation
#' Interval$new() == ExtendedReals$new()
#' Interval$new(class = "integer", type = "()") == Integers$new()
#'
#' @export
NULL
#---------------------------------------------
# Definition and Construction
#---------------------------------------------
Interval <- R6::R6Class("Interval", inherit = Set)
Interval$set("public","initialize",function(lower = -Inf, upper = Inf, type = "[]", class = "numeric",
                                            universe = Reals$new()){

  checkmate::assertChoice(type, c("()","(]","[]","[)"))
  if(checkmate::testComplex(lower) | checkmate::testComplex(upper)){
    lower = as.complex(lower)
    upper = as.complex(upper)
    checkmate::assert(Re(upper) > Re(lower) | (Re(upper) == Re(lower) & Im(upper) > Im(lower)),
                      .var.name = sprintf("Assertion on '%s' failed. '%s' must be less than '%s'.",
                                          lower, lower, upper))
  } else {
    checkmate::assert(lower <= upper)
  }
  checkmate::assertChoice(class, c("numeric","integer"))
  if(!is.null(universe)){
    if(getR6Class(self) != "SpecialSet"){
      assertSet(universe)
      private$.universe <- universe
    }
  }

  if(lower == upper){
    private$.class <- "integer"
    private$.type <- "[]"
  } else {
    private$.class <- class
    private$.type <- type
  }

  private$.lower <- lower
  private$.upper <- upper

  if(private$.class == "numeric"){
    cardinality = "b1"
  } else {
    if (lower == -Inf | upper == Inf)
      cardinality = "a0"
    else
      cardinality = length(seq.int(lower, upper, 1))
  }

  closure = switch(type,
                   "[]" = "closed",
                   "()" = "open",
                   "half-open"
                   )

  private$.properties = Properties$new(closure, cardinality)

  invisible(self)
})
#---------------------------------------------
# Public Methods
#---------------------------------------------
Interval$set("public","strprint",function(...){

inf <- ifelse(self$lower==-Inf & use_unicode(), "-\u221E", self$lower)
sup <- ifelse(self$upper==Inf & use_unicode(), "+\u221E", self$upper)

if(self$class == "integer")
  return(paste0("{", inf, ",...,", sup, "}"))
else
  return(paste0(substr(self$type,1,1),inf,", ",sup,substr(self$type,2,2)))
})
Interval$set("public","equals",function(x, all = FALSE){
  if(!testMessage(as.Set(self)))
    return(super$equals(x, all))

  x <- listify(x)

  ret = sapply(x, function(el){
    if (!testInterval(el))
      return(FALSE)

    if (el$lower == self$lower & el$upper == self$upper & el$type == self$type & el$class == self$class)
      return(TRUE)
    else
      return(FALSE)
  })

  returner(ret, all)
})
Interval$set("public","contains",function(x, all = FALSE, bound = FALSE){
  if(!testMessage(as.Set(self)))
    return(super$contains(x, all, bound))

  x <- listify(x)

  ret = rep(FALSE, length(x))

  if(self$class == "integer")
    class_test = sapply(x, checkmate::testIntegerish)
  else if(self$class == "numeric")
    class_test = sapply(x, checkmate::testNumeric)

  if(bound)
    ret[class_test][(x >= self$lower & x <= self$upper)] = TRUE
  else if (!bound){
    index = rep(FALSE, length(ret))
    if(testClosedAbove(self))
      index[class_test] = x[class_test] <= self$max
    else
      index[class_test] = x[class_test] < self$max
    if(testClosedBelow(self))
      index[class_test] = index[class_test] & x[class_test] >= self$min
    else
      index[class_test] = index[class_test] & x[class_test] > self$min

    ret[index] = TRUE
  }

  returner(ret, all)
})
Interval$set("public", "isSubset", function(x, proper = FALSE, all = FALSE){
  if(!testMessage(as.Set(self)))
    return(super$isSubset(x, proper, all))

  x <- listify(x)

  ret = sapply(x, function(el){
    if(!testSet(el))
      return(FALSE)

    if(el$properties$empty)
      return(TRUE)

    if(!testInterval(el)){
      if(self$contains(el$elements, all = TRUE, bound = FALSE))
        return(TRUE)
      else
        return(FALSE)
    }

    if(el$class == "numeric" & self$class == "integer")
      return(FALSE)

    if(proper){
      if((el$lower > self$lower & el$upper <= self$upper) |
         (el$lower >= self$lower & el$upper < self$upper) |
         (el$lower >= self$lower & el$upper <= self$upper & el$class == "integer" & self$class == "numeric"))
        return(TRUE)
      else
        return(FALSE)
    } else {
      if(el$lower >= self$lower & el$upper <= self$upper)
        return(TRUE)
      else
        return(FALSE)
    }
  })

  returner(ret, all)
})
#' @name isSubinterval
#' @rdname isSubinterval
#' @title Test If Two Intervals Are Subintervals
#' @param x [Set] or `list`
#' @param proper If `TRUE` then tests if `x` is a proper subinterval (i.e. subinterval and not equal to)
#' of `self`, otherwise `FALSE` tests if `x` is a (non-proper) subinterval.
#' @param all If `TRUE` then returns `TRUE` if all `x` are subintervals, otherwise returns a vector of logicals.
#' @details If `x` is a [Set] then will be coerced to an [Interval] if possible. [isSubinterval] differs
#' from [isSubset] in that ordering and class are respected in [isSubinterval]. See examples for
#' a clearer illustration of the difference.
#' @return If `all` is `TRUE` then returns `TRUE` if all `x` are subsets of the Set, otherwise
#' `FALSE`. If `all` is `FALSE` then returns a vector of logicals corresponding to each individual
#' element of `x`.
#' @section R6 Usage: $isSubinterval(x, proper = FALSE, all = FALSE)
#' @seealso [isSubset]
#' @examples
#' Interval$new(1,3)$isSubset(Set$new(1,2)) # TRUE
#' Interval$new(1,3)$isSubset(Set$new(2, 1)) # TRUE
#' Interval$new(1,3, class = "integer")$isSubinterval(Set$new(1, 2)) # TRUE
#' Interval$new(1,3)$isSubinterval(Set$new(1, 2)) # FALSE
#' Interval$new(1,3)$isSubinterval(Set$new(2, 1)) # FALSE
#'
#' Reals$new()$isSubset(Integers$new()) # TRUE
#' Reals$new()$isSubinterval(Integers$new()) # FALSE
Interval$set("public", "isSubinterval", function(x, proper = FALSE, all = FALSE){
  if(!testMessage(as.Tuple(self)))
    return(as.Tuple(self)$isSubset(x, proper, all))

  x <- listify(x)

  ret = sapply(x, function(el){
    if(!testSet(el))
      return(FALSE)

    if(el$properties$empty)
      return(TRUE)

    if(testFuzzy(el) | testConditionalSet(el))
      return(FALSE)

    if(testMessage(as.Interval(el)))
      return(FALSE)
    else
      el = as.Interval(el)

    if(el$class != self$class)
      return(FALSE)

    if(proper){
      if((el$lower > self$lower & el$upper <= self$upper) | (el$lower >= self$lower & el$upper < self$upper))
        return(TRUE)
      else
        return(FALSE)
    } else {
      if(el$lower >= self$lower & el$upper <= self$upper)
        return(TRUE)
      else
        return(FALSE)
    }
  })

  returner(ret, all)
})

#---------------------------------------------
# Public Fields
#---------------------------------------------
Interval$set("active","length",function(){
  if(self$properties$countability == "countably finite")
    return(length(self$elements))
  else
    return(Inf)
})
Interval$set("active", "elements", function(){
  if(self$properties$countability == "countably finite")
    return(seq.int(self$min, self$max, 1))
  else
    return(NaN)
})

#---------------------------------------------
# Coercions
#---------------------------------------------
#' @template coercion1
#' @templateVar class1 Interval
#' @details
#' * `as.Interval.list/as.Interval.data.frame` - Assumes the `list`/`data.frame` has named items/columns:
#' `lower, upper, type, class`.
#' * `as.Interval.numeric` - If the `numeric` vector is a continuous interval with no breaks then coerces to an [Interval]
#' with `lower = min(object), upper = max(object), class = "integer"`, ignoring ordering.
#' * `as.Interval.matrix` - Tries coercion via [as.Interval.numeric] on the first column of the matrix.
#' * `as.Interval.Set` - First tries coercion via [as.Interval.numeric], if possible wraps result in a [Set].
#' * `as.Interval.FuzzySet` - Tries coecion via [as.Interval.Set] on the [support] of the [FuzzySet].
#' @export
as.Interval <- function(object){
  UseMethod("as.Interval",object)
}
#' @rdname as.Interval
#' @export
as.Interval.Set <- function(object){
  if(testFuzzy(object))
    object = object$support(create = TRUE)

  if(testMessage(as.Interval.numeric(object$elements))){
    message("Set cannot be coerced to Interval. Elements must be equally spaced with unit difference.")
    return(object)
  } else {
    return(as.Interval.numeric(object$elements))
  }
}
#' @rdname as.Interval
#' @export
as.Interval.Interval <- function(object){
  return(Interval$new(object$lower, object$upper, type = object$type, class = object$class))
}
#' @rdname as.Interval
#' @export
as.Interval.list <- function(object){
  return(Interval$new(object$lower, object$upper, type = object$type, class = object$class))
}
#' @rdname as.Interval
#' @export
as.Interval.data.frame <- function(object){
  return(Interval$new(object$lower, object$upper, type = object$type, class = object$class))
}
#' @rdname as.Interval
#' @export
as.Interval.matrix <- function(object){
  return(as.Interval.numeric(object[,1]))
}
#' @rdname as.Interval
#' @export
as.Interval.numeric <- function(object){
  if (length(object) == 1)
    return(Interval$new(object, object))
  else if (all(diff(object) == 1))
    return(Interval$new(min(object), max(object), class = "integer"))
  else {
    message("Numeric cannot be coerced to Interval. Elements must be equally spaced with unit difference.")
    return(object)
  }
}
#' @rdname as.Interval
#' @export
as.Interval.ConditionalSet <- function(object){
  message("ConditionalSet cannot be coerced to Interval.")
  return(object)
}


