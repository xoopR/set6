#---------------------------------------------
# Documentation
#---------------------------------------------
#' @name ExponentSet
#' @template SetWrapper
#' @templateVar operation exponentiation
#' @templateVar class ExponentSet
#' @templateVar constructor ExponentSet$new(set, power)
#' @templateVar arg1 `set` \tab list \tab Set to wrap. \cr
#'
#' @export
NULL
#---------------------------------------------
# Definition and Construction
#---------------------------------------------
ExponentSet <- R6::R6Class("ExponentSet", inherit = ProductSet)
ExponentSet$set("public", "initialize", function(set, power){
  lower = Tuple$new(rep(set$lower, power))
  upper = Tuple$new(rep(set$upper, power))
  type = "{}"
  setlist = rep(list(set), power)
  private$.power = power

  if(is.null(set$properties$cardinality))
    cardinality = NULL
  else if(grepl("Beth|Aleph", set$properties$cardinality))
    cardinality = set$properties$cardinality
  else
    cardinality = set$properties$cardinality^power

  super$initialize(setlist = setlist, lower = lower, upper = upper, type = type, cardinality = cardinality)
})
#---------------------------------------------
# Public Methods
#---------------------------------------------
ExponentSet$set("public", "strprint", function(n = 2){
  if(inherits(self$wrappedSets[[1]], "SetWrapper"))
    paste0("(",self$wrappedSets[[1]]$strprint(n=n),")^",self$power)
  else
    paste(self$wrappedSets[[1]]$strprint(n=n), self$power, sep = "^")
})
ExponentSet$set("public","contains",function(x, all = FALSE, bound = FALSE){
  x <- listify(x)

  ret = sapply(x, function(el){
    if(!testSet(el))
      el = as.Set(el)

    if(el$length != self$power)
      return(FALSE)

    all(self$wrappedSets[[1]]$contains(el$elements, bound = bound))
  })

  returner(ret, all)
})
#---------------------------------------------
# Public Fields
#---------------------------------------------
ExponentSet$set("active", "power", function() return(private$.power))
#---------------------------------------------
# Private Fields
#---------------------------------------------
ExponentSet$set("private", ".power", 1)
