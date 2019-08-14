Set <- R6::R6Class("Set", inherit = SetInterval)

Set$set("public","initialize",function(..., dim = 1){
  if(length(list(...)) != 0){
    dots <- list(...)
    private$.elements <- unlist(unique(dots), recursive = FALSE)

    if(inherits(unlist(dots, recursive = FALSE),"numeric") | inherits(unlist(dots, recursive = FALSE),"integer")){
      private$.lower <- min(unlist(dots, recursive = FALSE))
      private$.upper <- max(unlist(dots, recursive = FALSE))
    }
  }

  invisible(self)
})

Set$set("public","elements",function(){
  return(private$.elements)
})
Set$set("public","length",function(){
  return(length(self$elements()))
})
Set$set("public","isEmpty",function(){
  if(self$length()==0)
    return(TRUE)
  else
    return(FALSE)
})
Set$set("public","liesInSetInterval",function(x, all = FALSE, bound = NULL){
  ret = rep(FALSE, length(x))
  ret[x %in% self$elements()] = TRUE

  if(all)
    return(all(ret))
  else
    return(ret)
})
Set$set("public","equals",function(x){
  if(!testSet(x))
    return(FALSE)

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
  # if(testSet(self))
    elements <- self$elements()
  # else
  #   elements <- self$as.numeric()

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

Set$set("private",".class","integer")
Set$set("private",".type","{}")
Set$set("private",".elements",NULL)

as.Set <- function(object){
  UseMethod("as.Set",object)
}
as.Set.numeric <- function(object){
  Set$new(object)
}
as.Set.list <- function(object){
  return(Set$new(unlist(object)))
}
as.Set.matrix <- function(object){
  return(apply(object,2,function(x) Set$new(x)))
}

'<.Set' <- function(x, y){
  return(y$isSubset(x, proper = TRUE))
}
'<=.Set' <- function(x, y){
  return(y$isSubset(x, proper = FALSE))
}
'>.Set' <- function(x, y){
  return(x$isSubset(y, proper = TRUE))
}
'>=.Set' <- function(x, y){
  return(x$isSubset(y, proper = FALSE))
}
'==.Set' <- function(x, y){
  return(y$equals(x))
}
'!=.Set' <- function(x, y){
  return(!y$equals(x))
}
