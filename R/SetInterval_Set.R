Set <- R6::R6Class("Set", inherit = SetInterval)

Set$set("public","initialize",function(..., dim = 1){
  if(length(list(...)) != 0){
    dots <- list(...)
    if(length(dots) == 1 & is.list(dots))
      dots <- dots[[1]]
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
  elements <- sapply(self$elements(), function(x){
    y = try(x$strprint(), silent = T)
    if(inherits(y,"try-error"))
      return(x)
    else
      return(y)
  })
  if(self$length() <= n * 2)
    return(paste0(substr(type,1,1),paste0(elements, collapse = ", "), substr(type,2,2)))
  else
    return(paste0(substr(type,1,1),paste0(elements[1:n], collapse = ", "), ",...,",
                  paste0(elements[(self$length()-n+1):self$length()],collapse=", "),
                  substr(type,2,2), collapse = ", "))
})
Set$set("public","powerSet",function(){
  if(testSet(self))
    elements <- self$elements()
  else
    elements <- self$as.numeric()

  y = Vectorize(function(m) combn(elements, m),vectorize.args = c("m"))(1:(self$length()-1))
  return(Set$new(Set$new(), unlist(lapply(y, as.Set)), self))
})
Set$set("public","isSubset",function(x, proper = FALSE){
  assertSet(x)

  if(proper){
    if(all(x$elements() %in% self$elements()) & !all(self$elements() %in% x$elements()))
      return(TRUE)
    else
      return(FALSE)
  }else{
    if(all(x$elements() %in% self$elements()))
      return(TRUE)
    else
      return(FALSE)
  }

})

as.Set <- function(object){
  UseMethod("as.Set",object)
}
as.Set.list <- function(object){
  return(Set$new(unlist(object)))
}
as.Set.matrix <- function(object){
  return(apply(object,2,function(x) Set$new(x)))
}

'<.Set' <- function(x, y){
  y$isSubset(x, proper = TRUE)
}
'<=.Set' <- function(x, y){
  y$isSubset(x, proper = FALSE)
}
'==.Set' <- function(x, y){
  y$equals(x)
}


