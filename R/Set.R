#' @name Set
#' @title Mathematical Set
#' @description A general Set object for mathematical sets. This also serves as the parent class to
#' intervals, tuples, and fuzzy variants.
#' @family sets
#'
#' @details
#' Mathematical sets can loosely be thought of as a collection of objects of any kind. The Set class
#' is used for sets of finite elements, for infinite sets use [Interval]. These can be
#' expanded for fuzzy logic by using [FuzzySet]s. Elements in a set cannot be duplicated and ordering
#' of elements does not matter, [Tuple]s can be used if duplicates or ordering are required.
#'
#' @examples
#' # Set of integers
#' Set$new(1:5)
#'
#' # Set of multiple types
#' Set$new("a", 5, Set$new(1))
#'
#' # Each Set has properties and traits
#' s = Set$new(1,2,3)
#' s$traits
#' s$properties
#'
#' # Elements cannot be duplicated
#' Set$new(2, 2) == Set$new(2)
#'
#' # Ordering does not matter
#' Set$new(1, 2) == Set$new(2, 1)
#'
#' @export
Set <- R6Class("Set",
  public = list(
    #' @description Create a new `Set` object.
    #' @details Sets are constructed by elements of any types (including R6 classes), excluding lists.
    #' `Set`s should be used within `Set`s instead of lists. The `universe` argument is useful for taking the absolute complement
    #' of the `Set`. If a universe isn't given then [UniversalSet] is assumed. If the `class` argument is non-NULL, then all elements
    #' will be coerced to the given class in construction, and if elements of a different class are added these will either be rejected
    #' or coerced.
    #' @param ... any. Elements in the set.
    #' @param universe Set. Universe that the Set lives in, i.e. elements that could be added to the Set. Default is the [UniversalSet].
    #' @param elements list. Alternative constructor that may be more efficient if passing objects of multiple classes.
    #' @param class character. Optional string naming a class that if supplied gives the set the `typed` property.
    #' @return A new `Set` object.
    initialize = function(..., universe = UniversalSet$new(), elements = NULL, class = NULL){

      private$.universe <- assertSet(universe)

      if(is.null(elements)) {
        elements = list(...)
      }

      if(length(elements)){
        if(!checkmate::testList(elements)){
          elements = as.list(elements)
        }

        if (!is.null(class)){
          private$.class <- class
          elements <- as.list(as(unlist(elements), class))
          if(class %in% c("numeric", "integer")) {
            private$.lower <- min(unlist(elements))
            private$.upper <- max(unlist(elements))
          } else if (class == "complex") {
            abs_els = Vectorize(abs)(unlist(elements))
            private$.lower <- unlist(elements[which.min(abs_els)])
            private$.upper <- unlist(elements[which.max(abs_els)])
          }
        }

        if(getR6Class(universe) != "UniversalSet"){
          assertContains(universe, elements, errormsg = "elements are not contained in the given universe")
        }

        private$.str_elements = sapply(elements, as.character)
        private$.elements <- elements


        if(!testTuple(self) & !testFuzzyTuple(self)) {
          if(private$.class != "ANY") {
            private$.elements <- unique(elements)
          } else {
            dup = duplicated(private$.str_elements)
            private$.elements <- elements[!dup]
            private$.str_elements <- private$.str_elements[!dup]
          }
        }

        if(!(private$.class %in% c("numeric","integer","complex"))){
          private$.lower <- private$.elements[[1]]
          private$.upper <- private$.elements[[length(private$.elements)]]
        }
      }

      private$.properties = Properties$new(closure = "closed", cardinality = self$length)

      invisible(self)
    },

    #' @description Prints a symbolic representation of the `Set`.
    #' @param n numeric. Number of elements to display on either side of ellipsis when printing.
    #' @details The function [useUnicode()] can be used to determine if unicode should be used when
    #' printing the `Set`. Internally `print` first calls `strprint` to create a printable representation
    #' of the Set.
    print = function(n = 2){
      cat(self$strprint(n),"\n")
      invisible(self)
    },

    #' @description
    #' Creates a printable representation of the object.
    #' @param n numeric. Number of elements to display on either side of ellipsis when printing.
    #' @return A character string representing the object.
    strprint = function(n = 2){
      if (self$properties$empty) {
        if(useUnicode())
          return("\u2205")
        else
          return("{}")
      } else {
        type <- private$.type
        elements <- sapply(private$.elements, as.character, n = n)
        if(self$length <= n * 2)
          return(paste0(substr(type,1,1),paste0(elements, collapse = ", "), substr(type,2,2)))
        else
          return(paste0(substr(type,1,1),paste0(elements[1:n], collapse = ", "), ",...,",
                        paste0(elements[(self$length-n+1):self$length],collapse=", "),
                        substr(type,2,2), collapse = ", "))
      }
    },

    #' @description Summarises the `Set`.
    #' @param n numeric. Number of elements to display on either side of ellipsis when printing.
    #' @details The function [useUnicode()] can be used to determine if unicode should be used when
    #' printing the `Set`. Summarised details include the `Set` class, properties, and traits.
    summary = function(n = 2){
      prop = self$properties
      cat(getR6Class(self),"\n\t",self$strprint(n),"\n",sep="")
      cat("Traits:\n\t")
      cat(ifelse(testCrisp(self), "Crisp", "Fuzzy"),"\n")
      cat("Properties:\n")
      if(prop$empty) cat("\tEmpty\n")
      if(prop$singleton) cat("\tSingleton\n")
      cat("\tCardinality =",prop$cardinality," - ",prop$countability,"\n")
      cat("\t",toproper(prop$closure),"\n",sep="")
    },

    #' @description Tests to see if \code{x} is contained in the Set.
    #'
    #' @param x any. Object or vector of objects to test.
    #' @param all logical. If `FALSE` tests each `x` separately. Otherwise returns `TRUE` only if all `x` pass test.
    #' @param bound ignored, added for consistency.
    #'
    #' @details \code{x} can be of any type, including a Set itself. \code{x} should be a tuple if
    #' checking to see if it lies within a set of dimension greater than one. To test for multiple \code{x}
    #' at the same time, then provide these as a list.
    #'
    #' If `all = TRUE` then returns `TRUE` if all `x` are contained in the `Set`, otherwise
    #' returns a vector of logicals.
    #'
    #' @return If \code{all} is `TRUE` then returns `TRUE` if all elements of \code{x} are contained in the `Set`, otherwise
    #' `FALSE.` If \code{all} is `FALSE` then returns a vector of logicals corresponding to each individual
    #' element of \code{x}.
    #'
    #' The infix operator `%inset%` is available to test if `x` is an element in the `Set`,
    #' see examples.
    #'
    #' @examples
    #' s = Set$new(elements = 1:5)
    #'
    #' # Simplest case
    #' s$contains(4)
    #' 8 %inset% s
    #'
    #' # Test if multiple elements lie in the set
    #' s$contains(4:6, all = FALSE)
    #' s$contains(4:6, all = TRUE)
    #'
    #' # Check if a tuple lies in a Set of higher dimension
    #' s2 = s * s
    #' s2$contains(Tuple$new(2,1))
    #' c(Tuple$new(2,1), Tuple$new(1,7), 2) %inset% s2
    contains = function(x, all = FALSE, bound = NULL){
      returner(x = sapply(listify(x), as.character) %in% private$.str_elements,
               all = all)
    },

    #' @description Tests if two sets are equal.
    #' @details Two sets are equal if they contain the same elements. Infix operators can be used for:
    #' \tabular{ll}{
    #' Equal \tab `==` \cr
    #' Not equal \tab `!=` \cr
    #' }
    #' @param x [Set] or vector of [Set]s.
    #' @param all logical. If `FALSE` tests each `x` separately. Otherwise returns `TRUE` only if all `x` pass test.
    #' @return If `all` is `TRUE` then returns `TRUE` if all `x` are equal to the Set, otherwise
    #' `FALSE`. If `all` is `FALSE` then returns a vector of logicals corresponding to each individual
    #' element of `x`.
    #'
    #' @examples
    #' # Equals
    #' Set$new(1,2)$equals(Set$new(5,6))
    #' Set$new(1,2)$equals(Interval$new(1,2))
    #' Set$new(1,2) == Interval$new(1,2, class = "integer")
    #'
    #' # Not equal
    #' !Set$new(1,2)$equals(Set$new(1,2))
    #' Set$new(1,2) != Set$new(1,5)
    equals = function(x, all = FALSE){
      x = listify(x)
      ret = sapply(x, function(y){
        if(!testSet(y)){
          return(FALSE)
        }

        if(testFuzzy(y)){
          if(!all(y$membership() == 1)){
            return(FALSE)
          }
        }

        if(testConditionalSet(y)){
          return(FALSE)
        } else if(testInterval(y)){
          if(testCountablyFinite(y)){
            return(all(suppressWarnings(y$elements %in% self$elements &
                         self$elements %in% y$elements)))
          } else {
            return(FALSE)
          }
        } else {
          return(all(suppressWarnings(y$.__enclos_env__$private$.str_elements %in% private$.str_elements &
                       private$.str_elements %in% y$.__enclos_env__$private$.str_elements)))
        }
      })

      returner(ret, all)
    },

    #' @description  Test if one set is a (proper) subset of another
    #' @param x any. Object or vector of objects to test.
    #' @param proper logical. If `TRUE` tests for proper subsets.
    #' @param all logical. If `FALSE` tests each `x` separately. Otherwise returns `TRUE` only if all `x` pass test.
    #' @details If using the method directly, and not via one of the operators then the additional boolean
    #' argument `proper` can be used to specify testing of subsets or proper subsets. A Set is a proper
    #' subset of another if it is fully contained by the other Set (i.e. not equal to) whereas a Set is a
    #' (non-proper) subset if it is fully contained by, or equal to, the other Set.
    #'
    #' Infix operators can be used for:
    #' \tabular{ll}{
    #' Subset \tab `<` \cr
    #' Proper Subset \tab `<=` \cr
    #' Superset \tab `>` \cr
    #' Proper Superset \tab `>=`
    #' }
    #'
    #' @return If `all` is `TRUE` then returns `TRUE` if all `x` are subsets of the Set, otherwise
    #' `FALSE`. If `all` is `FALSE` then returns a vector of logicals corresponding to each individual
    #' element of `x`.
    #' @examples
    #' Set$new(1,2,3)$isSubset(Set$new(1,2), proper = TRUE)
    #' Set$new(1,2) < Set$new(1,2,3) # proper subset
    #'
    #' c(Set$new(1,2,3), Set$new(1)) < Set$new(1,2,3) # not proper
    #' Set$new(1,2,3) <= Set$new(1,2,3) # proper
    isSubset = function(x, proper = FALSE, all = FALSE){
      x = listify(x)
      ret = sapply(x, function(y){
        if(!testSet(y)){
          return(FALSE)
        }

        if(testFuzzy(y)){
          if(!all(y$membership() == 1)){
            return(FALSE)
          }
        }

        if(getR6Class(y) %in% c("ConditionalSet", "UniversalSet")) {
          return(FALSE)
        } else if(testInterval(y)){
          if(testFinite(y)){
            if(proper){
              return(all(suppressWarnings(y$elements %in% self$elements)) &
                       !all(suppressWarnings(self$elements %in% y$elements)))
            } else {
              return(all(suppressWarnings(y$elements %in% self$elements)))
            }
          } else {
            return(FALSE)
          }
        } else {
          if(proper){
            return(all(suppressWarnings(y$.__enclos_env__$private$.str_elements %in% private$.str_elements)) &
                     !all(suppressWarnings(private$.str_elements %in% y$.__enclos_env__$private$.str_elements)))
          } else {
            return(all(suppressWarnings(y$.__enclos_env__$private$.str_elements %in% private$.str_elements)))
          }
        }
      })

      returner(ret, all)
    },

    #' @description Add elements to a set.
    #' @param ... elements to add
    #' @details `$add` is a wrapper around the `setunion` method with `setunion(self, Set$new(...))`.
    #' Note a key difference is that any elements passed to `...` are first converted to a `Set`, this
    #' important difference is illustrated in the examples by adding an [Interval] to a `Set`.
    #'
    #' Additionally, `$add` first coerces `...` to `$class` if `self` is a typed-set (i.e. `$class != "ANY"`),
    #' and `$add` checks if elements in `...` live in the universe of `self`.
    #' @return An object inheriting from [Set].
    #' @examples
    #' Set$new(1,2)$add(3)$print()
    #' Set$new(1,2,universe = Interval$new(1,3))$add(3)$print()
    #' \dontrun{
    #' # errors as 4 is not in [1,3]
    #' Set$new(1,2,universe = Interval$new(1,3))$add(4)$print()
    #' }
    #' # coerced to complex
    #' Set$new(0+1i, 2i, class = "complex")$add(4)$print()
    #'
    #' # setunion vs. add
    #' Set$new(1,2)$add(Interval$new(5,6))$print()
    #' Set$new(1,2) + Interval$new(5,6)
    add = function(...){
      assertContains(self$universe, list(...),
                     errormsg = sprintf("some added elements are not contained in the set universe: %s",
                                        self$universe$strprint()))

      if (self$class == "ANY") {
        els = setunion(self, Set$new(elements = list(...)))
      } else {
        els = setunion(self, Set$new(elements = list(...), class = self$class))
      }

      private$.elements <- els$elements
      private$.str_elements <- els$.__enclos_env__$private$.str_elements
      private$.lower <- els$lower
      private$.upper <- els$upper
      private$.properties <- els$properties
      private$.type <- els$type

      invisible(self)
    },

    #' @description Remove elements from a set.
    #' @param ... elements to remove
    #' @details `$remove` is a wrapper around the `setcomplement` method with
    #' `setcomplement(self, Set$new(...))`. Note a key difference is that any elements passed to `...`
    #' are first converted to a `Set`, this important difference is illustrated in the examples by
    #' removing an [Interval] from a `Set`.
    #' @return If the complement cannot be simplified to a `Set` then a [ComplementSet] is returned
    #' otherwise an object inheriting from [Set] is returned.
    #' @examples
    #' Set$new(1,2,3)$remove(1,2)$print()
    #' Set$new(1,Set$new(1),2)$remove(Set$new(1))$print()
    #' Interval$new(1,5)$remove(5)$print()
    #' Interval$new(1,5)$remove(4)$print()
    #'
    #' # setcomplement vs. remove
    #' Set$new(1,2,3)$remove(Interval$new(5,7))$print()
    #' Set$new(1,2,3) - Interval$new(5,7)
    remove = function(...){
      els = setcomplement(self, Set$new(elements = list(...)))
      if(inherits(els, "SetWrapper")) {
        return(els)
      } else {
        private$.elements <- els$elements
        private$.str_elements <- els$.__enclos_env__$private$.str_elements
        private$.lower <- els$lower
        private$.upper <- els$upper
        private$.properties <- els$properties
        private$.type <- els$type
        invisible(self)
      }
    }
  ),

  active = list(
    #' @field properties
    #' Returns an object of class `Properties`, which lists the properties of the Set. Set
    #' properties include:
    #' \itemize{
    #'  \item \code{empty} - is the Set empty or does it contain elements?
    #'  \item \code{singleton} - is the Set a singleton? i.e. Does it contain only one element?
    #'  \item \code{cardinality} - number of elements in the Set
    #'  \item \code{countability} - One of: countably finite, countably infinite, uncountable
    #'  \item \code{closure} - One of: closed, open, half-open
    #' }
    properties = function(){
      return(private$.properties)
    },

    #' @field traits
    #' List the traits of the Set. Set traits include:
    #' \itemize{
    #'  \item \code{crisp} - is the Set crisp or fuzzy?
    #' }
    traits = function(){
      return(private$.traits)
    },

    #' @field type
    #' Returns the type of the Set. One of: (), (], [), [], \{\}
    type = function(){
      return(private$.type)
    },

    #' @field max
    #' If the Set consists of numerics only then returns the maximum element in the Set. For open
    #' or half-open sets, then the maximum is defined by
    #' \deqn{upper - 1e-15}
    max = function(){
      if(self$class %in% c("numeric","integer","complex")) {
        if(self$type %in% c("()","[)"))
          return(self$upper - 1e-15)
        else
          return(self$upper)
      } else {
        return(NaN)
      }
    },

    #' @field min
    #' If the Set consists of numerics only then returns the minimum element in the Set. For open
    #' or half-open sets, then the minimum is defined by
    #' \deqn{lower + 1e-15}
    min = function(){
      if(self$class %in% c("numeric","integer","complex")) {
        if(self$type %in% c("()","(]"))
          return(self$lower + 1e-15)
        else
          return(self$lower)
      } else {
        return(NaN)
      }
    },

    #' @field upper
    #' If the Set consists of numerics only then returns the upper bound of the Set.
    upper = function(){
      return(private$.upper)
    },

    #' @field lower
    #' If the Set consists of numerics only then returns the lower bound of the Set.
    lower = function(){
      return(private$.lower)
    },

    #' @field class
    #' If all elements in the Set are the same class then returns that class, otherwise "ANY".
    class = function(){
      return(private$.class)
    },

    #' @field elements
    #' If the Set is finite then returns all elements in the Set as a `list`, otherwise "NA".
    elements = function(){
      return(private$.elements)
    },

    #' @field universe
    #' Returns the universe of the Set, i.e. the set of values that can be added to the Set.
    universe = function(){
      return(private$.universe)
    },

    #' @field range
    #' If the Set consists of numerics only then returns the range of the Set defined by
    #' \deqn{upper - lower}
    range = function(){
      if(self$class %in% c("numeric", "integer","complex"))
        return(self$upper - self$lower)
      else
        return(NaN)
    },

    #' @field length
    #' If the Set is finite then returns the number of elements in the Set, otherwise Inf. See
    #' the cardinality property for the type of infinity.
    length = function(){
      if (class(self$elements) == "logical") {
        if (is.na(self$elements)) {
          return(Inf)
        }
      } else {
        return(length(self$elements))
      }
    }
  ),

  private = list(
    .class = "ANY",
    .type = "{}",
    .lower = NA,
    .upper = NA,
    .universe = NULL,
    .elements = list(),
    .str_elements = c(),
    .properties = NULL,
    .traits = list(crisp = TRUE),
    .dimension = integer()
  )
)

#' @export
summary.Set = function(object, n, ...) object$summary(n = 2)
