#---------------------------------------------
# Documentation
#---------------------------------------------
#' @name UnionSet
#' @template SetWrapper
#' @templateVar operation union
#' @templateVar class UnionSet
#' @templateVar constructor UnionSet$new(setlist, lower = NULL, upper = NULL, type = NULL)
#' @templateVar arg1 `setlist` \tab list \tab List of sets to wrap. \cr
#'
#' @export
NULL
#---------------------------------------------
# Definition and Construction
#---------------------------------------------
UnionSet <- R6::R6Class("UnionSet", inherit = SetWrapper)
UnionSet$set("public", "initialize", function(setlist, lower = NULL, upper = NULL, type = NULL){
  checkmate::assertList(setlist)

  if(is.null(lower)){
    lower = try(min(unlist(sapply(setlist, function(x) x$lower))), silent = T)
    if(class(lower) == "try-error")
      lower = NaN
  }
  if(is.null(upper)){
    upper = try(max(unlist(sapply(setlist, function(x) x$upper))), silent = T)
    if(class(upper) == "try-error")
      upper = NaN
  }

  if(is.null(type)) type = "{}"

  cardinality = sapply(setlist, function(x) x$properties$cardinality)
  if(any(grepl("Beth", cardinality))){
    cardinality = paste0("Beth",
                         max(as.numeric(sapply(cardinality[grepl("Beth", cardinality)],
                                               substr, start = 5, stop = 100))))
  } else if(any(grepl("Aleph", cardinality))) {
    cardinality = "Aleph0"
  } else {
    if(any(unlist(sapply(cardinality, is.null))))
      cardinality = NULL
    else
      cardinality = sum(cardinality)
  }

  super$initialize(setlist = setlist, lower = lower, upper = upper, type = type,
                   cardinality = cardinality)
})
#---------------------------------------------
# Public Methods
#---------------------------------------------
UnionSet$set("public","strprint",function(n = 2){
  if(useUnicode())
    collapse = " \u222A "
  else
    collapse = " U "

  str = lapply(self$wrappedSets, function(x){
    if(inherits(x, "SetWrapper"))
      paste0("(",x$strprint(n),")")
    else
      x$strprint(n)
  })

  paste0(str, collapse = collapse)
})
UnionSet$set("public","contains",function(x, all = FALSE, bound = FALSE){
  apply(do.call(rbind,
                lapply(self$wrappedSets, function(y) y$contains(x, all = all, bound = bound))),
        2, any)
})
#---------------------------------------------
# Public Fields
#---------------------------------------------
UnionSet$set("active","elements",function(){
  els = unlist(unique(as.vector(rsapply(self$wrappedSets, "elements", active = TRUE))))
  if("NaN" %in% els | any(is.nan(els)))
    return(NaN)
  else
    return(els)
})
UnionSet$set("active","length",function(){
  len = rsapply(self$wrappedSets, "length", active = TRUE)

  sum(unlist(len))
})
