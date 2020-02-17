#' @template SetWrapper
#' @templateVar operation union
#' @templateVar class UnionSet
#' @export
UnionSet <- R6Class("UnionSet", inherit = SetWrapper,
  public = list(
    #' @description Create a new `UnionSet` object. It is not recommended to construct this class directly.
    #' @template param_wrapinit
    #' @return A new `UnionSet` object.
    initialize = function(setlist, lower = NULL, upper = NULL, type = NULL){
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
    },

    #' @template param_strprint
    #' @description Creates a printable representation of the object.
    #' @return A character string representing the object.
    strprint = function(n = 2){
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
    },

    #' @description Tests if elements `x` are contained in `self`.
    #' @template param_contains
    #' @return If `all == TRUE` then returns `TRUE` if all `x` are contained in `self`, otherwise `FALSE`.
    #' If `all == FALSE` returns a vector of logicals corresponding to the length of `x`, representing
    #' if each is contained in `self`. If `bound == TRUE` then an element is contained in `self` if it
    #' is on or within the (possibly-open) bounds of `self`, otherwise `TRUE` only if the element is within
    #' `self` or the bounds are closed.
    contains = function(x, all = FALSE, bound = FALSE){
      apply(do.call(rbind,
                    lapply(self$wrappedSets, function(y) y$contains(x, all = all, bound = bound))),
            2, any)
    }
  ),

  active = list(
    #' @field elements
    #' Returns the elements in the object.
    elements = function(){
      els = unlist(unique(as.vector(rsapply(self$wrappedSets, "elements", active = TRUE))))
      if(any(is.na(els)))
        return(NA)
      else
        return(els)
    },

    #' @field length
    #' Returns the number of elements in the object.
    length = function(){
      len = rsapply(self$wrappedSets, "length", active = TRUE)

      sum(unlist(len))
    }
  )
)
