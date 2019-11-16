#' @template SetWrapper
#' @templateVar operation exponentiation
#' @templateVar class ExponentSet
#' @templateVar constructor ExponentSet$new(setlist, lower = NULL, upper = NULL, type = NULL)
#' @templateVar arg1 `setlist` \tab list \tab List of sets to wrap. \cr
#'
#' @export
NULL
ExponentSet <- R6::R6Class("ExponentSet", inherit = ProductSet)
ExponentSet$set("private", ".power", 1)
ExponentSet$set("active", "power", function() return(private$.power))
ExponentSet$set("public", "initialize", function(set, power){
  lower = Tuple$new(rep(set$lower, power))
  upper = Tuple$new(rep(set$upper, power))
  type = "{}"
  setlist = rep(list(set), power)
  private$.power = power

  private$.properties$cardinality = Tuple$new(rep(set$properties$cardinality, power))
  private$.properties$countability = Tuple$new(rep(set$properties$countability, power))
  private$.properties$closure = Tuple$new(rep(set$properties$closure, power))

  super$initialize(setlist = setlist, lower = lower, upper = upper, type = type)
})
ExponentSet$set("public", "strprint", function(n = 2){
  paste(self$wrappedSets[[1]]$strprint(n=n), self$power, sep = "^")
})
