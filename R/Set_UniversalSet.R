#---------------------------------------------
# Documentation
#---------------------------------------------
#' @name UniversalSet
#' @title Mathematical UniversalSet
#'
#' @description The UniversalSet is defined as the [Set] containing all possible elements.
#' @return R6 object of class UniversalSet inheriting from [Set].
#' @template Set
#' @templateVar constructor UniversalSet$new(...)
#' @templateVar arg1 `...` \tab ANY \tab Added for consistency, ignored.
#' @templateVar constructorDets The UniversalSet takes no arguments in construction.
#'
#' @details
#' The Universal set is the default universe to all sets, and is the largest possible set.
#' The Universal set contains every single possible element. We denote the Universal set with `V`
#' instead of `U` to avoid confusion with the union symbol. The Universal set is also responsible
#' for a few set paradoxes, to resolve these we use the following results:
#' Let \eqn{V} be the universal set, \eqn{S} be any non-universal set, and \eqn{0} the empty set, then
#'
#' \deqn{V \cup S = V}{V or S = V}
#' \deqn{V \cap S = S}{V and S = S}
#' \deqn{S - V = 0}
#' \deqn{V^n = V}
#' \deqn{P(V) = V}
#'
#' @examples
#' u = UniversalSet$new()
#' print(u)
#' u$contains(c(1, letters, TRUE, Set$new()), all = TRUE)
#'
#' @export
NULL
#---------------------------------------------
# Definition and Construction
#---------------------------------------------
UniversalSet <- R6::R6Class("UniversalSet", inherit = Set)

#---------------------------------------------
# Public Methods
#---------------------------------------------
UniversalSet$set("public","equals",function(x, all = FALSE){
  x <- listify(x)
  ret <- rep(FALSE, length(x))
  ret[sapply(x, getR6Class) %in% "UniversalSet"] = TRUE

  returner(ret, all)
})
UniversalSet$set("public","isSubset",function(x, proper = FALSE, all = FALSE){
  x <- listify(x)

  ret = sapply(x, function(el){
    if(!inherits(el, "Set"))
      return(FALSE)
    else if(proper & getR6Class(el) == "UniversalSet")
      return(FALSE)
    else
      return(TRUE)
  })

  returner(ret, all)
})
UniversalSet$set("public","contains",function(x, all = FALSE, bound = NULL){
  returner(rep(TRUE, length(x)), all)
})
UniversalSet$set("public","strprint",function(n = 2){
  return("V")
})

UniversalSet$set("private",".elements",NA)
