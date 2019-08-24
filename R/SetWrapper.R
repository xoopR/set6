SetWrapper <- R6::R6Class("SetWrapper", inherit = Set, lock_objects = FALSE)
SetWrapper$set("public","initialize",function(..., setlist, lower = NULL, upper = NULL, type = NULL,
                                              dimension = NULL, symbol = NULL){
  if(getR6Class(self) == "SetWrapper")
    stop(paste(getR6Class(self), "is an abstract class that can't be initialized."))

  assertSetList(setlist)

  private$.wrappedSets <- setlist

  if(!is.null(lower)) private$.lower <- lower
  if(!is.null(upper)) private$.upper <- upper
  if(!is.null(type)) private$.type <- type
  if(!is.null(dimension)) private$.dimension <- dimension
  if(!is.null(symbol)) private$.symbol <- symbol

  invisible(self)
})

SetWrapper$set("public", "wrappedSets", function(model=NULL){
  return(private$.wrappedSets)
})
SetWrapper$set("private", ".wrappedSets", list())
SetWrapper$set("private", ".symbol", character(0))

