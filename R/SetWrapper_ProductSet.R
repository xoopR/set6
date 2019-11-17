#' @template SetWrapper
#' @templateVar operation product
#' @templateVar class ProductSet
#' @templateVar constructor ProductSet$new(setlist, lower = NULL, upper = NULL, type = NULL)
#' @templateVar arg1 `setlist` \tab list \tab List of sets to wrap. \cr
#'
#' @export
NULL
ProductSet <- R6::R6Class("ProductSet", inherit = SetWrapper)
ProductSet$set("public", "initialize", function(setlist, lower = NULL, upper = NULL, type = NULL){
  if(is.null(lower)) lower = Tuple$new(rsapply(setlist, lower, active = TRUE))
  if(is.null(upper)) upper = Tuple$new(rsapply(setlist, upper, active = TRUE))
  if(is.null(type)) type = "{}"

  private$.properties$cardinality = Tuple$new(sapply(setlist, function(x) x$properties$cardinality))
  private$.properties$countability = Tuple$new(sapply(setlist, function(x) x$properties$countability))
  private$.properties$closure = Tuple$new(sapply(setlist, function(x) x$properties$closure))

  super$initialize(setlist = setlist, lower = lower, upper = upper, type = type)
})
ProductSet$set("active", "length", function(){
  return(Tuple$new(sapply(self$wrappedSets, function(x) x$length)))
})
ProductSet$set("public","strprint",function(n = 2){
  str = lapply(self$wrappedSets, function(x){
    if(inherits(x, "SetWrapper"))
      paste0("(",x$strprint(n),")")
    else
      x$strprint(n)
  })

  if(use_unicode())
    collapse = " \u00D7 "
  else
    collapse = " X "

  if(length(unique(str)) == 1)
    return(paste(unique(str), length(str), sep = "^"))
  else
    return(paste(str, collapse = collapse))
})
ProductSet$set("public","contains",function(x, all = FALSE, bound = FALSE){

  if(!inherits(x, "R6") & !inherits(x, "list"))
    x = Set$new(x)

  if(!testSetList(x))
    x = list(x)

  rets = sapply(x, function(y) ifelse(y$length == self$length$length, return(TRUE), return(FALSE)))

  rets[rets] = sapply(x[rets], function(el){
    ret = TRUE
    for (i in 1:el$length){
      if (!self$wrappedSets[[i]]$contains(el$elements[i], bound = bound)){
        ret = FALSE
        break()
      }
    }
    return(ret)
  })

  if (all)
    return(all(rets))
  else
    return(unlist(rets))
})
