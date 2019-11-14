SetWrapper <- R6::R6Class("SetWrapper", inherit = Set, lock_objects = FALSE)
SetWrapper$set("public","initialize",function(setlist, lower = NULL, upper = NULL, type = NULL,
                                              dimension = NULL, symbol = NULL, class = NULL){
  if(getR6Class(self) == "SetWrapper")
    stop(paste(getR6Class(self), "is an abstract class that can't be initialized."))

  assertSetList(setlist)

  private$.wrappedSets <- setlist

  if(!is.null(lower)) private$.lower <- lower
  if(!is.null(upper)) private$.upper <- upper
  if(!is.null(type)) private$.type <- type
  if(!is.null(class)) private$.class <- class
  if(!is.null(symbol)) private$.symbol <- symbol
  if(!is.null(dimension)) private$.dimension <- dimension
})

#' @name wrappedSets
#' @rdname wrappedSets
#' @title Get Sets in Wrapper
#' @description Gets the list of sets that are wrapped in the given wrapper.
#' @return List of `Set`s.
SetWrapper$set("active", "wrappedSets", function(){
  return(private$.wrappedSets)
})
SetWrapper$set("private", ".wrappedSets", list())
SetWrapper$set("private", ".symbol", character(0))
SetWrapper$set("public", "equals", function(x){
  if(getR6Class(x) != getR6Class(self))
    return(FALSE)
  else{
    if(all(sapply(self$active, getR6Class) == sapply(x$active, getR6Class)))
      return(TRUE)
    else
      return(FALSE)
  }
})
SetWrapper$set("public","liesInSet",function(x, all = FALSE, bound = FALSE){
  if(testSet(x))
    x <- list(x)
  else if(!testSetList(x))
    stop(sprintf("%s is not a Set or a list of Sets", substitute(x)))

  ret <- unlist(lapply(x, function(y){
    if(y$length != self$dimension)
      stop(paste("Set should be of dimension",self$dimension))
    ret <- numeric(self$dimension)
    for(i in 1:self$dimension){
      ret[[i]] <- self$wrappedSets[[i]]$liesInSet(y$elements[[i]], bound = bound) |
        self$wrappedSets[[i]]$liesInSet(y$elements[[i]], bound = bound)
    }

    if(all(as.logical(ret)))
      return(TRUE)
    else
      return(FALSE)
  }))

  if(all)
    return(all(ret))
  else
    return(ret)
})
