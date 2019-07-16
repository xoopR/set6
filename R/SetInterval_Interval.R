Interval <- R6::R6Class("Interval", inherit = SetInterval)
Interval$set("public","initialize",function(lower = -Inf, upper = Inf, type = "[]", class = "numeric", dim = 1){
  types = c("()","(]","[]","[)")
  stopifnot(type %in% types)
  stopifnot(lower<=upper)
  private$.lower = lower
  private$.upper = upper
  private$.type = type
  if(lower == -Inf) lower = "-\u221E"
  if(upper == Inf) upper = "+\u221E"
  private$.setSymbol <- paste0(substr(type,1,1),lower,",", upper,substr(type,2,2))
  private$.class <- class
  if(dim != 1)
    private$.setSymbol <- paste0(private$.setSymbol,"^",dim)
  invisible(self)
})


Interval$set("public","as.numeric",function(){
  if(self$class() == "integer")
    return(seq.int(self$min(),self$max(),1))
})
Interval$set("public","length",function(){
  if(self$inf() == -Inf | self$sup() == Inf)
    return(Inf)
  if(self$class() == "numeric")
    return(Inf)

  return(length(self$as.numeric()))
})
Interval$set("private",".class",NULL)
