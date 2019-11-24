#---------------------------------------------
# Documentation
#---------------------------------------------
#' @name ProductSet
#' @template SetWrapper
#' @templateVar operation product
#' @templateVar class ProductSet
#' @templateVar constructor ProductSet$new(setlist, lower = NULL, upper = NULL, type = NULL)
#' @templateVar arg1 `setlist` \tab list \tab List of sets to wrap. \cr
#'
#' @export
NULL
ProductSet <- R6::R6Class("ProductSet", inherit = SetWrapper)
#---------------------------------------------
# Definition and Construction
#---------------------------------------------
ProductSet$set("public", "initialize", function(setlist, lower = NULL, upper = NULL, type = NULL,
                                                cardinality = NULL){
  if(is.null(lower)) lower = Tuple$new(rsapply(setlist, lower, active = TRUE))
  if(is.null(upper)) upper = Tuple$new(rsapply(setlist, upper, active = TRUE))
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
      cardinality = prod(cardinality)
  }

  super$initialize(setlist = setlist, lower = lower, upper = upper, type = type,
                   cardinality = cardinality)
})
#---------------------------------------------
# Public Methods
#---------------------------------------------
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

  paste(str, collapse = collapse)
})
ProductSet$set("public","contains",function(x, all = FALSE, bound = FALSE){
  x <- listify(x)

  rets = sapply(x, function(el){
    if(!inherits(el, "R6"))
      return(FALSE)

    if(el$length != self$length$length)
      return(FALSE)

    ret = TRUE
    for (i in 1:el$length){
      if (!self$wrappedSets[[i]]$contains(el$elements[i], bound = bound)){
        ret = FALSE
        break()
      }
    }
    return(ret)
  })

  returner(rets, all)
})
#---------------------------------------------
# Public Fields
#---------------------------------------------
ProductSet$set("active", "length", function(){
  return(Tuple$new(rsapply(self$wrappedSets, length, active = TRUE)))
})
