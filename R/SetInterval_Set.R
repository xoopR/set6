#' @name Set
#' @title Set
#' @description Set Object
NULL
#' @export
Set <- R6::R6Class("Set", inherit = SetInterval)

Set$set("public","initialize",function(..., dimension = 1, universe = NULL){
  if(length(list(...)) != 0){
    if(!checkmate::testList(...))
      dots <- list(...)
    else
      dots <- unlist(list(...), recursive = FALSE)
    private$.elements <- unlist(unique(dots))
    class <- unique(sapply(dots,function(x) class(x)[[1]]))
    if(length(class)==1)
      private$.class <- class
    else if(length(class)==2 & "list" %in% class)
      private$.class <- class[!(class %in% "list")]
    else
      private$.class <- "multiple"

    if(private$.class %in% c("numeric", "integer")){
      private$.lower <- min(unlist(dots))
      private$.upper <- max(unlist(dots))
    }

    private$.dimension <- dimension

    if(!is.null(universe)){
      assertSetInterval(universe)
      private$.universe <- universe
    }
  }

  invisible(self)
})

# New Methods
Set$set("public","elements",function(){
  return(private$.elements)
})

# Overloaded methods
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
  if(!checkmate::testList(x)){
    if(inherits(x, "R6"))
      x <- list(x)
    else
      x <- as.list(x)
  }

  ret = rep(FALSE, length(x))

  r6.fil <- sapply(x, function(y) ifelse(inherits(y, "R6"), TRUE, FALSE))
  r6.match <- x[r6.fil]
  atom.match <- x[!r6.fil]

  if(length(atom.match) > 0)
    ret[!r6.fil][atom.match %in% self$elements()] = TRUE

  if(length(r6.match) > 0){
    r6.tr <- sapply(r6.match, function(y){
      cl <- getR6Class(y)
      fil <- sapply(self$elements(), function(z) ifelse(inherits(z, cl), TRUE, FALSE))
      els <- self$elements()[fil]
      any(sapply(els, function(z) y$equals(z)))
    })
    ret[r6.fil][r6.tr] = TRUE
  }

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

# Overloaded private variables
Set$set("private",".class","ANY")
Set$set("private",".type","{}")

# New private variables
Set$set("private",".elements",NULL)

#' @title Coercion to R6 Set
#' @description Coerces objects to R6 Sets
#' @param object object to coerce
#' @export
as.Set <- function(object){
  UseMethod("as.Set",object)
}
#' @rdname as.Set
#' @export
as.Set.numeric <- function(object){
  Set$new(object)
}
#' @rdname as.Set
#' @export
as.Set.list <- function(object){
  return(Set$new(unlist(object)))
}
#' @rdname as.Set
#' @export
as.Set.matrix <- function(object){
  return(apply(object,2,function(x) Set$new(x)))
}
