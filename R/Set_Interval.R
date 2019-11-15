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
#'
#' @details
#' The Interval class can be used for finite or infinite intervals, but often Sets will be preferred for
#' integer intervals over a continuous range.
#'
#' @seealso
#' [Set]
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

  checkmate::assert(type %in% c("()","(]","[]","[)"))
  checkmate::assert(lower <= upper)
  checkmate::assert(class %in% c("numeric","integer"))
  if(getR6Class(self) != "SpecialSet"){
    assertSet(universe)
    private$.universe <- universe
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

  private$.properties$bounded = ifelse(lower == -Inf | upper == Inf, FALSE, TRUE)
  if(private$.class == "numeric"){
    private$.properties$countability = "uncountable"
    if(use_unicode())
      private$.properties$cardinality = "\u2136\u2081"
    else
      private$.properties$cardinality = "Beth1"
  } else {
    if(private$.properties$bounded){
      private$.properties$countability = "countably finite"
      private$.properties$cardinality = self$length
    } else {
      private$.properties$countability = "countably infinite"
      if(use_unicode())
        private$.properties$cardinality = "\u2135\u2080"
      else
        private$.properties$cardinality = "Aleph0"
    }
  }

  private$.properties$singleton = ifelse(self$length == 1, TRUE, FALSE)
  private$.properties$empty = ifelse(self$length == 0, TRUE, FALSE)
  private$.properties$closure = switch(type,
                                       "[]" = "closed",
                                       "()" = "open",
                                       "half-open"
                                       )

  private$.properties = private$.properties[match(c("empty","singleton","cardinality",
                                                    "countability","closure"),
                                                  names(private$.properties))]

  invisible(self)
})

Interval$set("public","equals",function(x){
  if (!testInterval(x))
    return(FALSE)
  if (x$type == self$type & x$class == self$class){
    if (is.null(x$lower) & is.null(self$lower) & is.null(x$upper) & is.null(self$upper))
      return(TRUE)
    else if (x$lower == self$lower & x$upper == self$upper)
      return(TRUE)
    else
      return(FALSE)
  } else
    return(FALSE)
})
Interval$set("public","strprint",function(...){

  inf <- ifelse(self$lower==-Inf & use_unicode(), "-\u221E", self$lower)
  sup <- ifelse(self$upper==Inf & use_unicode(), "+\u221E", self$upper)

  str <- paste0(substr(self$type,1,1),inf,", ",sup,substr(self$type,2,2))

  return(str)
})
Interval$set("public","contains",function(x, all = FALSE, bound = FALSE){
  if(testSet(x))
    x <- x$elements

  ret = rep(FALSE, length(x))

  if(self$class == "integer")
    class_test = sapply(x, checkmate::testIntegerish)
  else if(self$class == "numeric")
    class_test = sapply(x, checkmate::testNumeric)
  else if(self$class == "complex")
    class_test = sapply(x, checkmate::testComplex)

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

  if(all)
    return(all(ret))
  else
    return(ret)
})
Interval$set("public", "isSubset", function(x, proper = FALSE){
  if(x$properties$empty)
    return(TRUE)

  if(testSet(x) & !testInterval(x) & !testConditionalSet(x)){
    if(testFuzzy(x))
      return(FALSE)
    else{
      if(testMessage(as.Set(self))){
        if(self$contains(x, all = TRUE, bound = FALSE))
          return(TRUE)
        else
          return(FALSE)
      } else
        return(as.Set(self)$isSubset(x, proper = proper))
    }
  }
  if(self$equals(x)){
    if(proper)
      return(FALSE)
    else
      return(TRUE)
  } else{
    if(self == "numeric" & x == "integer")
      return(FALSE)

    if(x$lower >= self$lower & x$upper <= self$upper)
      return(TRUE)
    else
      return(FALSE)
  }
})
Interval$set("active","length",function(){
  if(self$lower == -Inf | self$upper == Inf)
    return(Inf)
  if(self$class == "numeric" & self$lower != self$upper)
    return(Inf)
  else if(self$class == "numeric" & self$lower == self$upper)
    return(1)

  return(length(self$elements))
})
Interval$set("active", "elements", function(){
  if(self$properties$countability == "countably finite")
    return(seq.int(self$min, self$max, 1))
  else
    return(NaN)
})

#' @export
as.double.Interval <- function(x,...){
  x$elements
}

#' @title Coercion to R6 Interval
#' @description Coerces objects to R6 Intervals
#' @param object object to coerce
#' @export
as.Interval <- function(object){
  UseMethod("as.Interval",object)
}
#' @rdname as.Interval
#' @export
as.Interval.Set <- function(object){
  if (object$length == 1)
    return(Interval$new(object$elements, object$elements))
  else if (all(diff(object$elements) == 1))
    return(Interval$new(min(object$elements), max(object$elements), class = "integer"))
  else {
    message("Set cannot be coerced to Interval. Elements must be equally spaced with unit difference.")
    return(object)
  }

}
#' @rdname as.Interval
#' @export
as.Interval.Interval <- function(object){
  return(Interval$new(object$lower, object$upper, type = object$type, class = object$class))
}
