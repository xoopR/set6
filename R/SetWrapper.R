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
