#' @name Interval
#' @title Interval
#' @description Interval Object
NULL
#' @export
Interval <- R6::R6Class("Interval", inherit = Set)
Interval$set("public","initialize",function(lower = -Inf, upper = Inf, type = "[]", class = "numeric",
                                            dimension = 1, universe = Reals$new()){

  checkmate::assert(type %in% c("()","(]","[]","[)"))
  checkmate::assert(lower <= upper)
  checkmate::assert(class %in% c("numeric","integer"))
  checkmate::assertIntegerish(dimension)
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
  private$.dimension <- dimension

  private$.properties$bounded = ifelse(lower == -Inf | upper == Inf, FALSE, TRUE)
  if(private$.class == "numeric"){
    private$.properties$countability = "uncountable"
    private$.properties$cardinality = "\u2136\u2081"
  } else {
    if(private$.properties$bounded){
      private$.properties$countability = "countably finite"
      private$.properties$cardinality = self$length
    } else {
      private$.properties$countability = "countably infinite"
      private$.properties$cardinality = "\u2135\u2080"
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
  if (x$type == self$type & x$class == self$class & x$dimension == self$dimension){
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

  inf <- ifelse(self$lower==-Inf, "-\u221E", self$lower)
  sup <- ifelse(self$upper==Inf, "+\u221E", self$upper)

  str <- paste0(substr(self$type,1,1),inf,", ",sup,substr(self$type,2,2))

  if(self$dimension!=1)
    str <- paste(str,self$dimension,sep="^")

  return(str)
})
Interval$set("public","liesInSet",function(x, all = FALSE, bound = FALSE){
  if(testSet(x))
    x <- x$elements

  ret = rep(FALSE, length(x))

  if(self$class == "integer")
    class_test = sapply(x, checkmate::testIntegerish)
  else if(self$class == "numeric")
    class_test = sapply(x, checkmate::testNumeric)

  if(bound)
    ret[(x >= self$lower & x <= self$upper & class_test)] = TRUE
  else if (!bound){
    if(testClosedAbove(self))
      index = x <= self$max & class_test
    else
      index = x < self$max & class_test

    if(testClosedBelow(self))
      index = index & (x >= self$min & class_test)
    else
      index = index & (x > self$min & class_test)

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
        if(self$liesInSet(x, all = TRUE, bound = FALSE))
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
