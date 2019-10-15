#' @name ConditionalSet
#' @title ConditionalSet
#' @description ConditionalSet Object
NULL
#' @export
ConditionalSet <- R6::R6Class("ConditionalSet", inherit = Set)

ConditionalSet$set("public","initialize",function(condition, argclass = NULL){
  if(!is.function(condition))
    stop("'condition' must be a function.")
  else{
    lst <- as.list(1:length(formals(condition)))
    names(lst) <- names(formals(condition))
    if(!checkmate::testLogical(do.call(condition, lst)))
      stop("'condition' should result in a logical.")
  }

  private$.condition <- condition
  private$.dimension <- length(formals(condition))

  if(is.null(argclass)){
    argclass <- rep(list(Reals$new()), private$.dimension)
    names(argclass) <- names(formals(condition))
  }

  private$.argclass <- argclass

  invisible(self)
})

ConditionalSet$set("public","liesInSet",function(x, all = FALSE, bound = NULL){
  # x <- list(...)
  # assertSetList(x)
  if(inherits(x, "Set"))
    x <- list(x)
  else if(inherits(x, "list")){
    if(!all(sapply(x, testSet)))
      stop("x should inherit from Set")
  } else
    stop("x should be a Set, Tuple or list of Sets/Tuples")

  ret <- sapply(1:length(x), function(i){
    els <- as.list(x[[i]]$elements)
    names(els) <- names(private$.argclass)
    do.call(self$condition, els)
  })

  if(all)
    return(all(ret))
  else
    return(ret)
})
ConditionalSet$set("public","equals",function(x){
  if(!testConditionalSet(x))
    return(FALSE)

  if(all(names(formals(x$condition)) == names(formals(self$condition))) &
     all(body(x$condition) == body(self$condition)) &
     all(unlist(lapply(x$class, getR6Class)) == unlist(lapply(self$class, getR6Class))))
    return(TRUE)
  else
    return(FALSE)
})
ConditionalSet$set("public","strprint",function(n = NULL){
  return(paste0("{",paste0(deparse(body(self$condition))," : ",
                           paste(names(self$class), sapply(self$class, strprint),
                                 sep = " \u03B5 ", collapse = ", "),"}")))
})
ConditionalSet$set("public","summary",function(n = NULL){
  self$print()
})


ConditionalSet$set("active","condition", function(){
  return(private$.condition)
})
ConditionalSet$set("active","class", function(){
  return(private$.argclass)
})

ConditionalSet$set("private",".condition", NULL)
ConditionalSet$set("private",".argclass", NULL)
ConditionalSet$set("private",".traits", list())
ConditionalSet$set("private",".properties", list())
