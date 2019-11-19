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
