Set <- R6::R6Class("Set", inherit = SetInterval)

Set$set("public","initialize",function(..., dim = 1){
  if(length(list(...)) != 0){
    dots <- list(...)
    private$.elements <- unlist(dots)
    private$.lower <- dots[[1]]
    private$.upper <- dots[[length(dots)]]
    private$.dimension <- dim
  }

  invisible(self)
})

Set$set("public","length",function(){
  return(length(private$.elements))
})
Set$set("public","elements",function(){
  return(private$.elements)
})


Set$set("private",".class","integer")
Set$set("private",".type","{}")
Set$set("private",".elements",NULL)
Set$set("public","liesInSetInterval",function(x, all = FALSE, bound = NULL){
  ret = rep(FALSE, length(x))
  ret[x %in% self$elements()] = TRUE

  if(all)
    return(all(ret))
  else
    return(ret)
})
Set$set("public","equals",function(x){
  assertSet(x)
  if(all(x$elements() %in% self$elements()) & all(self$elements() %in% x$elements()))
    return(TRUE)
  else
    return(FALSE)
})
Set$set("public","strprint",function(n = 2){
  type <- private$.type
  if(self$length() <= n * 2)
    return(paste0(substr(type,1,1),paste0(self$elements(), collapse = ", "), substr(type,2,2)))
  else
    return(paste0(substr(type,1,1),paste0(self$elements()[1:n], collapse = ", "), ",...,",
                  paste0(self$elements()[(self$length()-n+1):self$length()],collapse=", "),
                  substr(type,2,2), collapse = ", "))
})
