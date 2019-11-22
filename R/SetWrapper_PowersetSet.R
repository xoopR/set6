#' @name PowersetSet
#' @template SetWrapper
#' @templateVar operation exponentiation
#' @templateVar class PowersetSet
#' @templateVar constructor PowersetSet$new(set)
#' @templateVar arg1 `set` \tab list \tab Set to wrap. \cr
#'
#' @export
NULL
PowersetSet <- R6::R6Class("PowersetSet", inherit = ProductSet)
PowersetSet$set("public", "initialize", function(set){
  #
  # private$.properties$cardinality = Tuple$new(rep(set$properties$cardinality, power))
  # private$.properties$countability = Tuple$new(rep(set$properties$countability, power))
  # private$.properties$closure = Tuple$new(rep(set$properties$closure, power))

  super$initialize(setlist = list(set), lower = Set$new(), upper = set, type = "{}")
})
PowersetSet$set("public", "strprint", function(n = 2){
  if(use_unicode())
    paste0("\U2118(", self$wrappedSets[[1]]$strprint(n),")")
  else
    paste0("2^", self$wrappedSets[[1]]$strprint(n))
})
PowersetSet$set("public", "contains", function(x, all = FALSE, bound = NULL){
  x = self$wrappedSets[[1]]$isSubset(x, proper = FALSE)
  if(all)
    return(all(x))
  else
    return(x)
})
PowersetSet$set("public", "isSubset", function(x, proper = FALSE, all = FALSE){

  if(!checkmate::testList(x)){
    if(inherits(x, "R6"))
      x <- list(x)
    else
      x <- as.list(x)
  }

  assertSetList(x)

  ret = sapply(x, function(el){
    if(self$equals(el)){
      if(proper)
        return(FALSE)
      else
        return(TRUE)
    }

    if(getR6Class(el) == "PowersetSet")
      return(self$wrappedSets[[1]]$isSubset(el$wrappedSets[[1]], proper = proper))

    if(!(getR6Class(el) %in% c("Tuple","Set")))
      return(FALSE)

    if(!testSet(el$elements[[1]]))
      return(FALSE)

    all(self$wrappedSets[[1]]$isSubset(el$elements[[1]], proper = FALSE))
  })

  if(all)
    return(all(ret))
  else
    return(ret)
})
