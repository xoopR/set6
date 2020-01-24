#---------------------------------------------
# Documentation
#---------------------------------------------
#' @name ConditionalSet
#' @title Mathematical Set of Conditions
#'
#' @description A mathematical set defined by one or more logical conditions.
#' @return R6 object of class ConditionalSet inheriting from [Set].
#' @template Set
#' @templateVar constructor ConditionalSet$new(condition, argclass = NULL)
#' @templateVar arg1 `condition` \tab function \tab Function for defining the set. See constructor details. \cr
#' @templateVar arg2 `argclass` \tab list \tab Optional list of sets that the function arguments live in. See constructor details.
#' @templateVar constructorDets The `condition` should be given as a function that when evaluated returns either TRUE or FALSE. Further constraints can be given by providing the universe of the function arguments as [Set]s, if these are not given then the [Reals] is assumed. See examples.
#' @templateVar field1 `condition` \tab [condition] \cr
#'
#' @details
#' Conditional sets are a useful tool for symbolically defining possibly infinite sets. They can be combined
#' using standard 'and', `&`, and 'or', `|`, operators.
#'
#' @examples
#' # Set of positives
#' s = ConditionalSet$new(function(x) x > 0)
#' s$contains(list(1,-1))
#'
#' # Set via equality
#' s = ConditionalSet$new(function(x, y) x + y == 2)
#' s$contains(list(Set$new(2, 0), Set$new(0, 2)))
#'
#' # Tuples are recommended when using contains as they allow non-unique elements
#' s = ConditionalSet$new(function(x, y) x + y == 4)
#' \dontrun{
#' s$contains(Set$new(2, 2)) # Errors as Set$new(2,2) == Set$new(2)
#' }
#' s$contains(Tuple$new(2, 2))
#'
#' # Set of Positive Naturals
#' s = ConditionalSet$new(function(x) TRUE, argclass = list(x = PosNaturals$new()))
#' s$contains(list(-2, 2))
#'
#' @export
NULL
#---------------------------------------------
# Definition and Construction
#---------------------------------------------
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

  if (!is.null(argclass))
    assertSetList(argclass)
  else {
    argclass <- rep(list(Reals$new()), private$.dimension)
    names(argclass) <- names(formals(condition))
  }

  private$.argclass <- argclass

  private$.properties <- Properties$new()

  invisible(self)
})
#---------------------------------------------
# Public Methods
#---------------------------------------------
ConditionalSet$set("public","contains",function(x, all = FALSE, bound = NULL){
  x <- lapply(listify(x), function(y) ifelse(testSet(y), return(y), return(Set$new(y))))

  ret <- sapply(1:length(x), function(i){
    els <- as.list(x[[i]]$elements)
    if(length(els) != length(self$class))
      stop(sprintf("Set is of length %s, length %s expected.", length(els), length(self$class)))
    names(els) <- names(self$class)
    do.call(self$condition, els) & all(mapply(function(x, y) x$contains(y), self$class, els))
  })

  returner(ret, all)
})
ConditionalSet$set("public","equals",function(x, all = FALSE){
  x <- listify(x)

  ret = sapply(x, function(el){
    if(!testConditionalSet(el))
      return(FALSE)

    if(all(names(formals(el$condition)) == names(formals(self$condition))) &
       all(body(el$condition) == body(self$condition)) &
       all(unlist(lapply(el$class, getR6Class)) == unlist(lapply(self$class, getR6Class))))
      return(TRUE)


    if(!all(rsapply(self$class, "strprint") == rsapply(el$class, "strprint")))
      return(FALSE)
    else{
      sclass = self$class
      elclass = el$class
      if(length(sclass) < length(elclass))
        sclass = rep(sclass, length(elclass))[1:length(elclass)]
      if(length(elclass) < length(sclass))
        elclass = rep(elclass, length(sclass))[1:length(sclass)]

      elcond = body(el$condition)
      if(!all(names(sclass) == names(elclass))){
        for(i in 1:length(names(elclass)))
          elcond = gsub(names(elclass)[[i]], names(sclass)[[i]], elcond, fixed = TRUE)
      }
      if(all(elcond == as.character(body(self$condition))))
        return(TRUE)
      else
        return(FALSE)
    }
  })

  returner(ret, all)
})
ConditionalSet$set("public","strprint",function(n = NULL){
  if(useUnicode())
    sep = " \u2208 "
  else
    sep = " in "

  return(paste0("{",paste0(deparse(body(self$condition))," : ",
                           paste(names(self$class), sapply(self$class, function(x) x$strprint()),
                                 sep = sep, collapse = ", "),"}")))
})
ConditionalSet$set("public","summary",function(n = NULL){
  self$print()
})
ConditionalSet$set("public", "isSubset", function(x, proper = FALSE, all = FALSE){
  message("isSubset is currently undefined for conditional sets.")
  return(FALSE)
})

#---------------------------------------------
# Public Fields
#---------------------------------------------
#' @name condition
#' @title ConditionalSet Condition
#' @rdname condition
#' @section R6 Usage: $condition
#' @description Return the condition defining the ConditionalSet.
#' @return A function that evaluates to `TRUE` or `FALSE`.
#' @seealso [ConditionalSet]
ConditionalSet$set("active","condition", function(){
  return(private$.condition)
})
ConditionalSet$set("active","class", function(){
  return(private$.argclass)
})
ConditionalSet$set("active","elements", function(){
  return(NaN)
})

#---------------------------------------------
# Private Fields
#---------------------------------------------
ConditionalSet$set("private",".condition", NULL)
ConditionalSet$set("private",".argclass", NULL)
