#---------------------------------------------
# Documentation
#---------------------------------------------
#' @name ComplementSet
#' @template SetWrapper
#' @templateVar operation complement
#' @templateVar class ComplementSet
#' @templateVar constructor ComplementSet$new(addset, subtractset, lower = NULL, upper = NULL, type = NULL)
#' @templateVar arg1 `addset` \tab [Set] \tab Set to add. \cr
#' @templateVar arg2 `subtractset` \tab [Set] \tab Set to subtract. \cr
#' @templateVar field1 `addedSet` \tab [addedSet] \cr
#' @templateVar field2 `subtractedSet` \tab [subtractedSet] \cr
#'
#' @export
NULL
#---------------------------------------------
# Definition and Construction
#---------------------------------------------
ComplementSet <- R6::R6Class("ComplementSet", inherit = SetWrapper)
ComplementSet$set("public", "initialize", function(addset, subtractset, lower = NULL, upper = NULL, type = NULL){
  assertSet(addset)
  assertSet(subtractset)

  if(is.null(lower)){
    if(!any(subtractset$contains(addset$lower, bound = TRUE)))
      lower = addset$lower
    else{
      if(testInterval(addset) & addset$class == "numeric")
        lower = addset$lower + .Machine$double.xmin
      else if(testInterval(addset) & addset$class == "integer")
        lower = addset$lower + 1
      else
        lower = addset$elements[!subtractset$contains(addset$elements, bound = TRUE)][1]
    }
  }

  if(is.null(upper)){
    if(!any(subtractset$contains(addset$upper, bound = TRUE)))
      upper = addset$upper
    else{
      if(testInterval(addset) & addset$class == "numeric")
        upper = addset$upper - .Machine$double.xmin
      else if(testInterval(addset) & addset$class == "integer")
        upper = addset$upper - 1
      else
        upper = addset$elements[!subtractset$contains(addset$elements, bound = TRUE)][sum(!subtractset$contains(addset$elements, bound = TRUE))]
    }
  }

  if(is.null(type)) type = "{}"

  private$.addedSet = addset
  private$.subtractedSet = subtractset
  super$initialize(setlist = c(addset, subtractset), lower = lower, upper = upper, type = type,
                   cardinality = addset$properties$cardinality)
})
#---------------------------------------------
# Public Methods
#---------------------------------------------
ComplementSet$set("public","strprint",function(n = 2){
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
ComplementSet$set("public","contains",function(x, all = FALSE, bound = FALSE){
  add = self$addedSet$contains(x, all = all, bound = bound)
  diff = self$subtractedSet$contains(x, all = all, bound = bound)

  add & !diff
})
#---------------------------------------------
# Public Fields
#---------------------------------------------
ComplementSet$set("active","elements",function(){
  add_els = self$addedSet$elements
  if(any(is.na(add_els)))
    return(NaN)

  sub_els = self$subtractedSet$elements
  if(any(is.na(sub_els)))
    return(NaN)

  add_els[!(add_els %in% sub_els)]
})
ComplementSet$set("active","length",function(){
  if(self$addedSet$length == Inf)
    return(Inf)
  else
    return(self$addedSet$length - self$subtractedSet$length)
})
#' @name addedSet
#' @rdname addedSet
#' @title Get Added Set in Wrapper
#' @description For the `ComplementSet` wrapper, `X-Y`, gets the set `X`.
#' @return `Set`.
#' @seealso [ComplementSet], [subtractedSet]
ComplementSet$set("active","addedSet", function() return(private$.addedSet))
#' @name subtractedSet
#' @rdname subtractedSet
#' @title Get Subtracted Set in Wrapper
#' @description For the `ComplementSet` wrapper, `X-Y`, gets the set `Y`.
#' @return `Set`
#' @seealso [ComplementSet], [addedSet]
ComplementSet$set("active","subtractedSet",function() return(private$.subtractedSet))
#---------------------------------------------
# Private Fields
#---------------------------------------------
ComplementSet$set("private",".addedSet",Set$new())
ComplementSet$set("private",".subtractedSet",Set$new())
