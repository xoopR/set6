#' @name ExponentSet
#' @template SetWrapper
#' @templateVar operation exponentiation
#' @templateVar class ExponentSet
#' @templateVar constructor ExponentSet$new(set, power)
#' @templateVar arg1 `set` \tab list \tab Set to wrap. \cr
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
  if(inherits(self$wrappedSets[[1]], "SetWrapper"))
    paste0("(",self$wrappedSets[[1]]$strprint(n=n),")^",self$power)
  else
    paste(self$wrappedSets[[1]]$strprint(n=n), self$power, sep = "^")
})
ExponentSet$set("public","contains",function(x, all = FALSE, bound = FALSE){

  if(class(x)[[1]] != "list")
    x = list(x)

  x = sapply(x, function(y) ifelse(testSet(y), return(y), return(as.Set(y))))

  rets = sapply(x, function(y) ifelse(y$length == self$power, return(TRUE), return(FALSE)))

  rets[rets] = sapply(x[rets], function(el) all(self$wrappedSets[[1]]$contains(el$elements, bound = bound)))

  if (all)
    return(all(unlist(rets)))
  else
    return(unlist(rets))
})
