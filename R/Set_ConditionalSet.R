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

ConditionalSet$set("public","liesInSetInterval",function(x, all = FALSE, bound = NULL){
  if(inherits(x, "Set"))
    x <- list(x)
  else if(inherits(x, "list")){
    if(!all(sapply(x, testSet)))
      stop("x should inherit from Set")
  } else
    stop("x should be a Set, Tuple or list of Sets/Tuples")

  ret <- sapply(1:length(x), function(i){
    els <- as.list(x[[i]]$elements())
    names(els) <- names(formals(condition))
    do.call(condition, els)
  })

  if(all)
    return(all(ret))
  else
    return(ret)
})
ConditionalSet$set("public","equals",function(x){
  if(!ConditionalSet(x))
    return(FALSE)

  if(self$condition() == x$condition() & self$class == x$class)
    return(TRUE)
  else
    return(FALSE)
})
ConditionalSet$set("public","strprint",function(){
  return(paste0("{",paste0(deparse(body(self$condition()))," | ",
                           paste(names(self$class), sapply(self$class, strprint),
                                 sep = " \u03B5 ", collapse = ", "),"}")))
})
ConditionalSet$set("public","condition", function(){
  return(private$.condition)
})
ConditionalSet$set("private",".condition", NULL)
ConditionalSet$set("active","class", function(){
  return(private$.argclass)
})
ConditionalSet$set("private",".argclass", NULL)
