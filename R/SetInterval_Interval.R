#' @name Interval
#' @title Interval
NULL
#' @export
Interval <- R6::R6Class("Interval", inherit = SetInterval)
Interval$set("public","initialize",function(lower = -Inf, upper = Inf, type = "[]", class = "numeric",
                                            dimension = 1, universe = Reals$new()){

  checkmate::assert(type %in% c("()","(]","[]","[)"))
  checkmate::assert(lower <= upper)

  checkmate::assert(class %in% c("numeric","integer"))
  super$initialize(lower, upper, type, class, dimension, universe)
})

Interval$set("public","as.numeric", function(){
  if(self$class() == "integer"){
    if(testBounded(self) & testFinite(self))
      return(seq.int(self$min(),self$max(),1))
  }

  message("Interval is unbounded.")
  return(NaN)
})
Interval$set("public","length",function(){
  if(self$inf() == -Inf | self$sup() == Inf)
    return(Inf)
  if(self$class() == "numeric")
    return(Inf)

  return(length(self$as.numeric()))
})
Interval$set("public","equals",function(x){
  if(!testInterval(x))
    return(FALSE)
  if(x$type() == self$type() & x$inf() == self$inf() & x$sup() == self$sup() & x$class() == self$class() &
     self$dimension() == x$dimension())
    return(TRUE)
  else
    return(FALSE)
})
Interval$set("public","strprint",function(){

  inf <- ifelse(self$inf()==-Inf, "-\u221E", self$inf())
  sup <- ifelse(self$sup()==Inf, "+\u221E", self$sup())

  str <- paste0(substr(self$type(),1,1),inf,", ",sup,substr(self$type(),2,2))

  if(self$dimension()!=1)
    str <- paste(str,self$dimension(),sep="^")

  return(str)
})
Interval$set("public","liesInSetInterval",function(x, all = FALSE, bound = FALSE){
  ret = rep(FALSE, length(x))

  if(self$class() == "integer")
    class_test = sapply(x, checkmate::testIntegerish)
  else if(self$class() == "numeric")
    class_test = sapply(x, checkmate::testNumeric)

  if(bound)
    ret[(x >= self$inf() & x <= self$sup() & class_test)] = TRUE
  else(!bound)
  ret[(x >= self$min() & x <= self$max() & class_test)] = TRUE

  if(all)
    return(all(ret))
  else
    return(ret)
})
