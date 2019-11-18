#' @template SetWrapper
#' @templateVar operation exponentiation
#' @templateVar class Powerset
#' @templateVar constructor Powerset$new(setlist)
#' @templateVar arg1 `setlist` \tab list \tab List of sets to wrap. \cr
#'
#' @export
NULL
Powerset <- R6::R6Class("Powerset", inherit = ProductSet)
Powerset$set("public", "initialize", function(setlist){
  #
  # private$.properties$cardinality = Tuple$new(rep(set$properties$cardinality, power))
  # private$.properties$countability = Tuple$new(rep(set$properties$countability, power))
  # private$.properties$closure = Tuple$new(rep(set$properties$closure, power))

  super$initialize(setlist = setlist, lower = Set$new(), upper = set, type = "{}")
})
Powerset$set("public", "strprint", function(n = 2){
  if(use_unicode())
    paste0("\U2118(", self$wrappedSets[[1]]$strprint(n),")")
  else
    paste0("2^", self$wrappedSets[[1]]$strprint(n))
})

powerset <- function(set, simplify = TRUE){
  if(set$properties$empty)
    return(Set$new(Set$new()))
  else{
    if(simplify){
      elements <- set$elements
      y = Vectorize(function(m) combn(elements, m),vectorize.args = c("m"))(1:(set$length-1))
      return(Set$new(Set$new(), unlist(lapply(y, as.Set)), set))
    } else {
      Powerset$new(list(set))
    }
  }
}
