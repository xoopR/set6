SetWrapper <- R6::R6Class("SetWrapper", inherit = Set, lock_objects = FALSE)
SetWrapper$set("public","initialize",function(setlist, lower = NULL, upper = NULL, type = NULL,
                                              class = NULL){
  if(getR6Class(self) == "SetWrapper")
    stop(paste(getR6Class(self), "is an abstract class that can't be initialized."))

  assertSetList(setlist)

  private$.wrappedSets <- setlist

  if(!is.null(lower)) private$.lower <- lower
  if(!is.null(upper)) private$.upper <- upper
  if(!is.null(type)) private$.type <- type
  if(!is.null(class))
    private$.class <- class
  else{
    class = sapply(setlist, function(x) x$class)
    if(length(unique(class)) == 1)
      private$.class <- unique(class)
    else
      private$.class <- "multiple"
  }
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
SetWrapper$set("public", "equals", function(x){
  if(getR6Class(x) != getR6Class(self))
    return(FALSE)

  if(length(self$wrappedSets) != length(x$wrappedSets))
    return(FALSE)

  ret = TRUE
  for(i in 1:length(self$wrappedSets)){
    if(self$wrappedSets[[i]] != x$wrappedSets[[i]]){
      ret = FALSE
      break()
    }
  }

  return(ret)
})
