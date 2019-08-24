#---------------------------------------------
# Documentation
#---------------------------------------------
#' @name Set
#' @title Set
#' @description Set Object
#' @export
NULL

#---------------------------------------------
# Definition and Construction
#---------------------------------------------
Set <- R6::R6Class("Set")
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
      assertSet(universe)
      private$.universe <- universe
    }
  }

  invisible(self)
})

#---------------------------------------------
# Public methods - Representation
#---------------------------------------------
Set$set("public","print",function(){
print(self$strprint())
})
Set$set("public","strprint",function(n = 2){
  type <- private$.type
  elements <- sapply(self$elements, function(x){
    y = try(x$strprint(), silent = T)
    if(inherits(y,"try-error"))
      return(x)
    else
      return(y)
  })
  if(self$length <= n * 2)
    return(paste0(substr(type,1,1),paste0(elements, collapse = ", "), substr(type,2,2)))
  else
    return(paste0(substr(type,1,1),paste0(elements[1:n], collapse = ", "), ",...,",
                  paste0(elements[(self$length-n+1):self$length],collapse=", "),
                  substr(type,2,2), collapse = ", "))
})
Set$set("public","summary",function(){
  self$print()
})

#---------------------------------------------
# Public methods - Comparison
#---------------------------------------------
Set$set("public","liesInSet",function(x, all = FALSE, bound = NULL){
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
    ret[!r6.fil][atom.match %in% self$elements] = TRUE

  if(length(r6.match) > 0){
    r6.tr <- sapply(r6.match, function(y){
      cl <- getR6Class(y)
      fil <- sapply(self$elements, function(z) ifelse(inherits(z, cl), TRUE, FALSE))
      els <- self$elements[fil]
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

  if(all(x$elements %in% self$elements) & all(self$elements %in% x$elements))
    return(TRUE)
  else
    return(FALSE)
})
Set$set("public","isSubset",function(x, proper = FALSE){
  assertSet(x)

  if(proper){
    if(all(x$elements %in% self$elements) & !all(self$elements %in% x$elements))
      return(TRUE)
    else
      return(FALSE)
  }else{
    if(all(x$elements %in% self$elements))
      return(TRUE)
    else
      return(FALSE)
  }

})

#---------------------------------------------
# Public methods - isEmpty/complement/powerSet
#---------------------------------------------
Set$set("public","isEmpty",function(){
if(self$length==0)
  return(TRUE)
else
  return(FALSE)
})
Set$set("public","complement",function(){
  if(!is.null(self$universe))
    return(setdiff(self$universe, self))
})
Set$set("public","powerSet",function(){
  # if(testSet(self))
    elements <- self$elements
  # else
  #   elements <- self$as.numeric()

  y = Vectorize(function(m) combn(elements, m),vectorize.args = c("m"))(1:(self$length-1))
  return(Set$new(Set$new(), unlist(lapply(y, as.Set)), self))
})


#---------------------------------------------
# Accessors
#---------------------------------------------
Set$set("active","properties",function(){
  return(private$.properties)
})
Set$set("active","type",function(){
  return(private$.type)
})
Set$set("active","dimension",function(){
  return(private$.dimension)
})
Set$set("active","max",function(){
  if(private$.type %in% c("()","[)"))
    return(self$upper-1.1e-15)
  else
    return(self$upper)
})
Set$set("active","min",function(){
  if(private$.type %in% c("()","(]"))
    return(self$lower+1.1e-15)
  else
    return(self$lower)
})
Set$set("active","upper",function(){
  return(private$.upper)
})
Set$set("active","lower",function(){
  return(private$.lower)
})
Set$set("active","class",function(){
  return(private$.class)
})
Set$set("active","elements",function(){
  return(private$.elements)
})
Set$set("active","universe",function(x){
  if(missing(x))
    return(private$.universe)
  else{
    assertSet(x)
    private$.universe <- x
  }
})
Set$set("active","range",function(){
  if(self$class %in% c("numeric", "integer"))
    return(self$upper - self$lower)
  else
    return(numeric(0))
})
Set$set("active","length",function(){
  return(length(self$elements))
})

#---------------------------------------------
# Private variables
#---------------------------------------------
Set$set("private",".class","ANY")
Set$set("private",".type","{}")
Set$set("private",".lower",numeric(0))
Set$set("private",".upper",numeric(0))
Set$set("private",".dimension",numeric(0))
Set$set("private",".universe",NULL)
Set$set("private",".elements",list())
Set$set("private",".properties",list())

#---------------------------------------------
# as.Set
#---------------------------------------------
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
#' @rdname as.Set
#' @export
as.Set.FuzzySet <- function(object){
  return(object$support(create = TRUE))
}
#' @rdname as.Set
#' @export
as.Set.Set <- function(object){
  return(Set$new(object$elements()))
}

#---------------------------------------------
# Overloaded operators
#---------------------------------------------
#' @rdname isSubset
#' @export
'<.Set' <- function(x, y){
  return(y$isSubset(x, proper = TRUE))
}
#' @rdname isSubset
#' @export
'<=.Set' <- function(x, y){
  return(y$isSubset(x, proper = FALSE))
}
#' @rdname isSubset
#' @export
'>.Set' <- function(x, y){
  return(x$isSubset(y, proper = TRUE))
}
#' @rdname isSubset
#' @export
'>=.Set' <- function(x, y){
  return(x$isSubset(y, proper = FALSE))
}
#' @rdname equals
#' @export
'==.Set' <- function(x, y){
  return(x$equals(y))
}
#' @rdname equals
#' @export
'!=.Set' <- function(x, y){
  return(!x$equals(y))
}
