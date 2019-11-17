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
  if(is.null(lower)){
    if(!any(subtractset$contains(addset$lower, bound = TRUE)))
      lower = addset$lower
    else{
      if(testInterval(addset) & class(addset)[1] == "numeric")
        lower = addset$lower + .Machine$double.xmin
      else if(testInterval(addset) & class(addset)[1] == "integer")
        lower = lower + 1
      else
        lower = addset$elements[2]
    }
  }

  if(is.null(upper)){
    if(!any(subtractset$contains(addset$upper, bound = TRUE)))
      upper = addset$upper
    else{
      if(testInterval(addset) & class(addset)[1] == "numeric")
        upper = addset$upper - .Machine$double.xmin
      else if(testInterval(addset) & class(addset)[1] == "integer")
        upper = upper + 1
      else
        upper = addset$elements[length(addset$elements)-1]
    }
  }

  if(is.null(type)) type = "{}"
  private$.addedSet = addset
  private$.subtractedSet = subtractset
  super$initialize(setlist = c(addset, subtractset), lower = lower, upper = upper, type = type)
})
DifferenceSet$set("public","strprint",function(n = 2){
  if(inherits(self$addedSet, "SetWrapper"))
    add = paste0("(",self$addedSet$strprint(n),")")
  else
    add = self$addedSet$strprint(n)

  if(inherits(self$subtractedSet, "SetWrapper"))
    sub = paste0("(",self$subtractedSet$strprint(n),")")
  else
    sub = self$subtractedSet$strprint(n)

 paste0(add, " \\ ", sub)
})
DifferenceSet$set("public","contains",function(x, all = FALSE, bound = FALSE){
  add = self$addedSet$contains(x, all = all, bound = bound)
  diff = self$subtractedSet$contains(x, all = all, bound = bound)

  add & !diff
})
DifferenceSet$set("active","elements",function(){
  add_els = self$addedSet$elements
  if(any(is.nan(add_els)))
    return(NaN)

  sub_els = self$subtractedSet$elements
  if(any(is.nan(sub_els)))
    return(NaN)

  add_els[!(add_els %in% sub_els)]
})
DifferenceSet$set("active","length",function(){
  if(self$addedSet$length == Inf)
    return(Inf)
  else
    return(self$addedSet$length - self$subtractedSet$length)
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
