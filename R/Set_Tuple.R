Tuple <- R6::R6Class("Tuple", inherit = Set)
Tuple$set("public","initialize",function(..., dim = 1){
  if(length(list(...)) != 0){
    dots <- list(...)
    if(length(dots) == 1 & is.list(dots))
      dots <- dots[[1]]
    private$.elements <- unlist(dots)

    if(inherits(unlist(dots),"numeric") | inherits(unlist(dots),"integer")){
      private$.lower <- min(unlist(dots))
      private$.upper <- max(unlist(dots))
    } else{
      private$.lower <- dots[[1]]
      private$.upper <- dots[[length(dots)]]
    }
    private$.dimension <- dim
  }

  invisible(self)
})

Tuple$set("public","equals",function(x){
  assertSet(x)

  if(suppressWarnings(all(x$elements() == self$elements())))
    return(TRUE)
  else
    return(FALSE)
})

Tuple$set("private",".type","()")
