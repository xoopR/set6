#' @name Powerset
#' @template SetWrapper
#' @templateVar operation exponentiation
#' @templateVar class Powerset
#' @templateVar constructor Powerset$new(set)
#' @templateVar arg1 `set` \tab list \tab Set to wrap. \cr
#'
#' @export
NULL
Powerset <- R6::R6Class("Powerset", inherit = ProductSet)
Powerset$set("public", "initialize", function(set){
  #
  # private$.properties$cardinality = Tuple$new(rep(set$properties$cardinality, power))
  # private$.properties$countability = Tuple$new(rep(set$properties$countability, power))
  # private$.properties$closure = Tuple$new(rep(set$properties$closure, power))

  super$initialize(setlist = list(set), lower = Set$new(), upper = set, type = "{}")
})
Powerset$set("public", "strprint", function(n = 2){
  if(use_unicode())
    paste0("\U2118(", self$wrappedSets[[1]]$strprint(n),")")
  else
    paste0("2^", self$wrappedSets[[1]]$strprint(n))
})
Powerset$set("public", "contains", function(x, all = FALSE, bound = NULL){
  x = self$wrappedSets[[1]]$isSubset(x, proper = FALSE)
  if(all)
    return(all(x))
  else
    return(x)
})
Powerset$set("public", "isSubset", function(x, proper = FALSE){
  if(self$equals(x)){
    if(proper)
      return(FALSE)
    else
      return(TRUE)
  }

  if(getR6Class(x) == "Powerset")
    return(self$wrappedSets[[1]]$isSubset(x$wrappedSets[[1]], proper = proper))

  all(self$wrappedSets[[1]]$isSubset(x$elements, proper = FALSE))
})
