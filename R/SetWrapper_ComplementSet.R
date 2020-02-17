#' @name ComplementSet
#' @template SetWrapper
#' @templateVar operation complement
#' @templateVar class ComplementSet
#' @export
ComplementSet <- R6Class("ComplementSet", inherit = SetWrapper,
  public = list(
    #' @description Create a new `ComplementSet` object. It is not recommended to construct this class directly.
    #' @param addset [Set] to be subtracted from.
    #' @param subtractset [Set] to subtract.
    #' @param lower lower bound of new object.
    #' @param upper upper bound of new object.
    #' @param type closure type of new object.
    #' @return A new `ComplementSet` object.
    initialize = function(addset, subtractset, lower = NULL, upper = NULL, type = NULL){
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
    },

    #' @template param_strprint
    #' @description Creates a printable representation of the object.
    #' @return A character string representing the object.
    strprint = function(n = 2){
      if(inherits(self$addedSet, "SetWrapper"))
        add = paste0("(",self$addedSet$strprint(n),")")
      else
        add = self$addedSet$strprint(n)

      if(inherits(self$subtractedSet, "SetWrapper"))
        sub = paste0("(",self$subtractedSet$strprint(n),")")
      else
        sub = self$subtractedSet$strprint(n)

      paste0(add, " \\ ", sub)
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
      add = self$addedSet$contains(x, all = all, bound = bound)
      diff = self$subtractedSet$contains(x, all = all, bound = bound)

      add & !diff
    }
  ),

  active = list(
    #' @field elements
    #' Returns the elements in the object.
    elements = function(){
      add_els = self$addedSet$elements
      if(any(is.na(add_els)))
        return(NA)

      sub_els = self$subtractedSet$elements
      if(any(is.na(sub_els)))
        return(NA)

      add_els[!(add_els %in% sub_els)]
    },

    #' @field length
    #' Returns the number of elements in the object.
    length = function(){
      if(self$addedSet$length == Inf)
        return(Inf)
      else
        return(self$addedSet$length - self$subtractedSet$length)
    },

    #' @field addedSet
    #' For the `ComplementSet` wrapper, `X-Y`, returns the set `X`.
    addedSet = function(){
      return(private$.addedSet)
    },

    #' @field subtractedSet
    #' For the `ComplementSet` wrapper, `X-Y`, returns the set `Y`.
    subtractedSet = function(){
      return(private$.subtractedSet)
    }
  ),

  private = list(
    .addedSet = NULL,
    .subtractedSet = NULL
  ))

