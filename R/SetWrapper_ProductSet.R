#' @name ProductSet
#' @template SetWrapper
#' @templateVar operation product
#' @templateVar class ProductSet
#' @export
ProductSet <- R6Class("ProductSet", inherit = SetWrapper,
  public = list(
    #' @description Create a new `ProductSet` object. It is not recommended to construct this class directly.
    #' @template param_wrapinit
    #' @param cardinality Either an integer, "Aleph0", or a beth number. If `NULL` then calculated automatically (recommended).
    #' @return A new `ProductSet` object.
    initialize = function(setlist, lower = NULL, upper = NULL, type = NULL,
                          cardinality = NULL){
      if(is.null(lower)) lower = Tuple$new(elements = rsapply(setlist, "lower", active = TRUE))
      if(is.null(upper)) upper = Tuple$new(elements = rsapply(setlist, "upper", active = TRUE))
      if(is.null(type)) type = "{}"

      if(is.null(cardinality)){
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
      }

      super$initialize(setlist = setlist, lower = lower, upper = upper, type = type,
                       cardinality = cardinality)
    },

    #' @template param_strprint
    #' @description Creates a printable representation of the object.
    #' @return A character string representing the object.
    strprint = function(n = 2){
      str = lapply(self$wrappedSets, function(x){
        if(inherits(x, "SetWrapper"))
          paste0("(",x$strprint(n),")")
        else
          x$strprint(n)
      })

      if(useUnicode())
        collapse = " \u00D7 "
      else
        collapse = " X "

      paste(str, collapse = collapse)
    },

    #' @description Tests if elements `x` are contained in `self`.
    #' @template param_xall
    #' @param bound logical
    #' @return If `all == TRUE` then returns `TRUE` if all `x` are contained in `self`, otherwise `FALSE`.
    #' If `all == FALSE` returns a vector of logicals corresponding to the length of `x`, representing
    #' if each is contained in `self`. If `bound == TRUE` then an element is contained in `self` if it
    #' is on or within the (possibly-open) bounds of `self`, otherwise `TRUE` only if the element is within
    #' `self` or the bounds are closed.
    contains = function(x, all = FALSE, bound = FALSE){
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
    }
  ),

  active = list(
    #' @field length
    #' Returns the number of elements in the object.
    length = function(){
      return(Tuple$new(elements = rsapply(self$wrappedSets, "length", active = TRUE)))
    }
  )
)
