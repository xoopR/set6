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
  assertSet(universe)

  private$.class <- class
  private$.type <- type
  private$.lower <- lower
  private$.upper <- upper
  private$.dimension <- dimension
  private$.universe <- universe

  invisible(self)
})

Interval$set("public","as.numeric", function(){
  if(self$class == "integer"){
    if(testBounded(self) & testFinite(self))
      return(seq.int(self$min, self$max, 1))
  }

  message("Interval is unbounded.")
  return(NaN)
})
Interval$set("public","equals",function(x){
  if(!testInterval(x))
    return(FALSE)
  if(x$type == self$type & x$lower == self$lower & x$upper == self$upper & x$class == self$class &
     self$dimension == x$dimension)
    return(TRUE)
  else
    return(FALSE)
})
Interval$set("public","strprint",function(){

  inf <- ifelse(self$lower==-Inf, "-\u221E", self$lower)
  sup <- ifelse(self$upper==Inf, "+\u221E", self$upper)

  str <- paste0(substr(self$type,1,1),inf,", ",sup,substr(self$type,2,2))

  if(self$dimension!=1)
    str <- paste(str,self$dimension,sep="^")

  return(str)
})
Interval$set("public","liesInSet",function(x, all = FALSE, bound = FALSE){
  ret = rep(FALSE, length(x))

  if(self$class == "integer")
    class_test = sapply(x, checkmate::testIntegerish)
  else if(self$class == "numeric")
    class_test = sapply(x, checkmate::testNumeric)

  if(bound)
    ret[(x >= self$lower & x <= self$upper & class_test)] = TRUE
  else(!bound)
  ret[(x >= self$min & x <= self$max & class_test)] = TRUE

  if(all)
    return(all(ret))
  else
    return(ret)
})

Interval$set("active","length",function(){
  if(self$lower == -Inf | self$upper == Inf)
    return(Inf)
  if(self$class == "numeric" & self$lower != self$upper)
    return(Inf)
  else if(self$class == "numeric" & self$lower == self$upper)
    return(1)

  return(length(self$as.numeric()))
})
