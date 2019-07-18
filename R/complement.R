complement.SetInterval <- function(x, y = NULL){

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

`-.SetInterval` <- function(x, y){
  complement.SetInterval(x, y)
}

complement.Set <- function(x, y = NULL){

  assertSet(x)

  if(is.null(y)){
    y <- x$clone()
    x <- y$universe()
  }

  if(testSet(x)){
    if(sum(!(x$elements() %in% y$elements())) == 0)
      return(Empty$new())
    else
      return(Set$new(x$elements()[!(x$elements() %in% y$elements())]))
  }else
    return(complement.SetInterval(x, y))
}

`-.Set` <- function(x, y){
  complement.Set(x, y)
}
