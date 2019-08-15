#' @title setdiff for SetInterval
#' @param x SetInterval
#' @param y SetInterval
#' @export
setdiff <- function(x, y){
  UseMethod("setdiff", x)
}

#' @rdname setdiff
#' @export
setdiff.SetInterval <- function(x, y){

  # if(is.null(y)){
  #   y <- x$
  # }

  if(inherits(y,"Set")){
    if(y$length()==1)
      y <- Interval$new(y$elements(),y$elements())
  }

  if(inherits(x,"Interval") & inherits(y,"Interval")){
    if(y$sup() >= x$sup() & y$inf() <= x$inf())
      return(Empty$new())
    else if(y$inf() > x$sup() | y$sup() < x$inf())
      return(x)
    else if(y$sup() >= x$sup() & y$inf() > x$inf() & y$inf() <= x$sup())
      return(Interval$new(lower = x$inf(), upper = y$inf(), type = paste0(substr(x$type(),1,1),")"),
                          class = x$class()))
    else if(y$sup() < x$sup() & y$inf() <= x$inf() & y$sup() >= x$inf())
      return(Interval$new(lower = y$sup(), upper = x$sup(), type = paste0("(",substr(x$type(),2,2)),
                          class = x$class()))
    else if(y$sup() <= x$sup() & y$inf() >= x$inf())
      return(union.SetInterval(Interval$new(x$inf(),y$inf(),type=paste0(substr(x$type(),1,1),")"),class = x$class()),
                               Interval$new(y$sup(),x$sup(),type=paste0("(",substr(x$type(),2,2)),class = x$class()),
                               dim = x$dimension()))
  }

  # setOperation("/",lower = lower, upper = upper, type = type, dim = x$dimension(),x,y)
}

#' @rdname setdiff
#' @export
`-.SetInterval` <- function(x, y){
  setdiff.SetInterval(x, y)
}

#' @rdname setdiff
#' @export
setdiff.Set <- function(x, y){
  if(testSet(y)){
    if(y >= x)
      return(Empty$new())
    else
      return(Set$new(x$elements()[!(x$elements() %in% y$elements())]))
  }else
    return(setdiff.SetInterval(x, y))
}

#' @rdname setdiff
#' @export
`-.Set` <- function(x, y){
  setdiff.Set(x, y)
}
