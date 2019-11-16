#' @template SetWrapper
#' @templateVar operation difference
#' @templateVar class DifferenceSet
#' @templateVar constructor DifferenceSet$new(addset, subtractset, lower = NULL, upper = NULL, type = NULL)
#' @templateVar arg1 `addset` \tab list \tab Sets to add. \cr
#' @templateVar arg2 `subtractset` \tab list \tab Sets to subtract. \cr
#' @templateVar field1 `addedSet` \tab [addedSet] \cr
#' @templateVar field1 `subtractedSet` \tab [subtractedSet] \cr
#'
#' @export
NULL
DifferenceSet <- R6::R6Class("DifferenceSet", inherit = SetWrapper)
DifferenceSet$set("public", "initialize", function(addset, subtractset, lower = NULL, upper = NULL, type = NULL){
  if(is.null(lower)) lower = min(sapply(setlist, function(x) x$lower))
  if(is.null(upper)) upper = max(sapply(setlist, function(x) x$upper))
  if(is.null(type)) type = "{}"
  private$.addedSet = addset
  private$.subtractedSet = subtractset
  super$initialize(setlist = c(addset, subtractset), lower = lower, upper = upper, type = type)
})
DifferenceSet$set("public","strprint",function(n = 2){
  if(inherits(self$addedSet, "SetWrapper"))
    add = paste0("{",self$addedSet$strprint(n),"}")
  else
    add = self$addedSet$strprint(n)

  if(inherits(self$subtractedSet, "SetWrapper"))
    sub = paste0("{",self$subtractedSet$strprint(n),"}")
  else
    sub = self$subtractedSet$strprint(n)

 paste0("{", add, " \\ ", sub, "}")
})
DifferenceSet$set("public","contains",function(x, all = FALSE, bound = FALSE){
  add = apply(do.call(rbind,
                lapply(self$addedSet, function(y) y$contains(x, all = all, bound = bound))),
        2, any)
  diff = apply(do.call(rbind,
                      lapply(self$subtractedSet, function(y) y$contains(x, all = all, bound = bound))),
              2, any)

  return(add & !diff)
})
DifferenceSet$set("active","elements",function(){
  add_els = unlist(sapply(self$addedSet, function(x) x$elements))
  if(any(is.nan(add_els)))
    return(NaN)

  sub_els = unlist(sapply(self$subtractedSet, function(x) x$elements))
  if(any(is.nan(sub_els)))
    return(NaN)

  add_els[!(add_els %in% sub_els)]
})

#' @name addedSet
#' @rdname addedSet
#' @title Get Added Sets in Wrapper
#' @description For the `DifferenceSet` wrapper, `X-Y`, gets the set `X`.
#' @return `Set`.
#' @seealso [DifferenceSet], [subtractedSet]
DifferenceSet$set("active","addedSet", function() return(private$.addedSet))
#' @name subtractedSet
#' @rdname subtractedSet
#' @title Get Subtracted Sets in Wrapper
#' @description For the `DifferenceSet` wrapper, `X-Y`, gets the set `Y`.
#' @return `Set`
#' @seealso [DifferenceSet], [addedSet]
DifferenceSet$set("active","subtractedSet",function() return(private$.subtractedSet))
DifferenceSet$set("private",".addedSet",Set$new())
DifferenceSet$set("private",".subtractedSet",Set$new())
