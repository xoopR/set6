#' @title setdiff for Set
#' @param x Set
#' @param y Set
#' @export
setdiff <- function(x, y){
  UseMethod("setdiff", x)
}

#' @rdname setdiff
#' @export
setdiff.Set <- function(x, y){

  if(getR6Class(x) == "Set" & getR6Class(y) == "Set"){
    if(y >= x)
      return(Empty$new())
    else
      return(Set$new(x$elements[!(x$elements %in% y$elements)]))
  }

  if(testSet(y) & y$length == 1)
    y <- Interval$new(y$elements,y$elements)

  if(testInterval(x) & testInterval(y)){
    if(y$upper >= x$upper & y$lower <= x$lower)
      return(Empty$new())
    else if(y$lower > x$upper | y$upper < x$lower)
      return(x)
    else if(y$upper >= x$upper & y$lower > x$lower & y$lower <= x$upper)
      return(Interval$new(lower = x$lower, upper = y$lower, type = paste0(substr(x$type,1,1),")"),
                          class = x$class))
    else if(y$upper < x$upper & y$lower <= x$lower & y$upper >= x$lower)
      return(Interval$new(lower = y$upper, upper = x$upper, type = paste0("(",substr(x$type,2,2)),
                          class = x$class))
    else if(y$upper <= x$upper & y$lower >= x$lower)
      return(Union$new(Interval$new(x$lower,y$lower,type=paste0(substr(x$type,1,1),")"),class = x$class),
                       Interval$new(y$upper,x$upper,type=paste0("(",substr(x$type,2,2)),class = x$class)))
  }

  # setOperation("/",lower = lower, upper = upper, type = type, dim = x$dimension,x,y)
}

#' @rdname setdiff
#' @export
`-.Set` <- function(x, y){
  setdiff.Set(x, y)
}
