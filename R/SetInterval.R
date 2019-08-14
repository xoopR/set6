SetInterval <- R6::R6Class("SetInterval")
SetInterval$set("public","initialize",function(lower, upper, type, class = "numeric", dimension,
                                               universe = Reals$new(), symbol = NULL){
  # if(getR6Class(self) == "SetInterval")
  #   stop(paste0(getR6Class(self), " is an abstract class that can't be initialized. Try Set, Interval or Tuple instead."))

  private$.lower = lower
  private$.upper = upper
  private$.type = type
  private$.dimension = as.integer(dimension)
  private$.class = class

  assertSetInterval(universe)
  private$.universe <- universe

  checkmate::assertIntegerish(dimension)
  private$.dimension <- dimension

  invisible(self)
})

# Accessors
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
SetInterval$set("public","class",function(){
  return(private$.class)
})
SetInterval$set("active","universe",function(x){
  if(missing(x))
    return(private$.universe)
  else{
    assertSetInterval(x)
    private$.universe <- x
  }
})

# Representation
SetInterval$set("public","print",function(){
  print(self$strprint())
})
SetInterval$set("public","strprint",function(){
  return(private$.setSymbol)
})
SetInterval$set("public","summary",function(){
  self$print()
})

# Methods
SetInterval$set("public","liesInSetInterval",function(x, all = FALSE, bound = FALSE){
  return(NULL)
})
SetInterval$set("public","isEmpty",function(){
  return(FALSE)
})
SetInterval$set("public","length",function(){
  return(NULL)
})
SetInterval$set("public","range",function(){
  if(private$.class %in% c("numeric", "integer"))
    return(self$sup() - self$inf())
  else
    return(NaN)
})

#' @title Test Equality of SetInterval
#' @name equals
#' @rdname equals
#' @param x SetInterval
#' @param y SetInterval
NULL
#' @export
SetInterval$set("public","equals",function(x){
  return(NULL)
})
#' @title Tests SetInterval Subsets
#' @name isSubset
#' @rdname isSubset
#' @param x SetInterval
#' @param y SetInterval
NULL
#' @export
SetInterval$set("public","isSubset",function(x, proper = FALSE){
  return(NULL)
})
SetInterval$set("public","powerSet",function(){
  return(NULL)
})
SetInterval$set("public","complement",function(){
  if(!is.null(self$universe))
    return(complement(self$universe, self))
})

#' # S3 Dispatch
#' #' method as.numeric SetInterval
#' #' export
#' as.numeric.SetInterval <- function(x){
#'   return(NaN)
#' }

#' @rdname isSubset
#' @export
'<.SetInterval' <- function(x, y){
  return(y$isSubset(x, proper = TRUE))
}
#' @rdname isSubset
#' @export
'<=.SetInterval' <- function(x, y){
  return(y$isSubset(x, proper = FALSE))
}
#' @rdname isSubset
#' @export
'>.SetInterval' <- function(x, y){
  return(x$isSubset(y, proper = TRUE))
}
#' @rdname isSubset
#' @export
'>=.SetInterval' <- function(x, y){
  return(x$isSubset(y, proper = FALSE))
}
#' @rdname equals
#' @export
'==.SetInterval' <- function(x, y){
  return(y$equals(x))
}
#' @rdname equals
#' @export
'!=.SetInterval' <- function(x, y){
  return(!y$equals(x))
}

# Private Variables
SetInterval$set("private",".lower",NaN)
SetInterval$set("private",".upper",NaN)
SetInterval$set("private",".type",NULL)
SetInterval$set("private",".class",NULL)
SetInterval$set("private",".dimension",NaN)
SetInterval$set("private",".universe",NULL)
