SetInterval <- R6::R6Class("SetInterval")
SetInterval$set("public","initialize",function(symbol, lower, upper, type, class = "numeric", dimension){
  if(getR6Class(self) == "SetInterval")
    stop(paste0(getR6Class(self), " is an abstract class that can't be initialized. Try Set, Interval or Tuple instead."))

  private$.lower = lower
  private$.upper = upper
  private$.type = type
  private$.dimension = as.integer(dimension)
  private$.setSymbol = symbol
  private$.class = class
  invisible(self)
})


SetInterval$set("public","type",function(){
  return(private$.type)
})
SetInterval$set("public","dimension",function(){
  return(private$.dimension)
})
SetInterval$set("public","max",function(){
  if(private$.type %in% c("()","[)"))
    return(self$sup()-1.1e-15)
  else
    return(self$sup())
})
SetInterval$set("public","min",function(){
  if(private$.type %in% c("()","(]"))
    return(self$inf()+1.1e-15)
  else
    return(self$inf())
})
SetInterval$set("public","sup",function(){
  return(private$.upper)
})
SetInterval$set("public","inf",function(){
  return(private$.lower)
})
SetInterval$set("public","getSymbol",function(){
  return(private$.setSymbol)
})
SetInterval$set("public","print",function(){
  print(self$strprint())
})
SetInterval$set("public","class",function(){
  return(private$.class)
})
SetInterval$set("public","liesInSetInterval",function(x, all = FALSE, bound = FALSE){
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

SetInterval$set("private",".lower",NULL)
SetInterval$set("private",".upper",NULL)
SetInterval$set("private",".type",NULL)
SetInterval$set("private",".class",NULL)
SetInterval$set("private",".dimension",NULL)
SetInterval$set("private",".setSymbol",NULL)
