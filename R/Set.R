#---------------------------------------------
# Documentation
#---------------------------------------------
#' @name Set
#' @title Mathematical Set
#'
#' @description A general Set object for mathematical sets. This also serves as the parent class to
#' intervals, tuples, and fuzzy variants.
#' @return R6 object of class Set.
#' @template Set
#' @templateVar constructor Set$new(..., universe = NULL)
#' @templateVar arg1 `...` \tab ANY \tab Elements in the set. \cr
#' @templateVar arg2 `universe` \tab Set \tab Optional universe that the Set lives in.
#' @templateVar constructorDets Sets are constructed by elements of any types (including R6 classes), excluding lists. `Set`s should be used within `Set`s instead of lists. The optional `universe` argument is useful for taking the complement of the `Set`. If a universe isn't given then [Reals] is assumed.
#'
#' @details
#' Mathematical sets can loosely be thought of as a collection of objects of any kind. The Set class
#' is used for sets of finite elements, for infinite sets use [Interval]. These can be
#' expanded for fuzzy logic by using [FuzzySet]s. Elements in a set cannot be duplicated
#' but [Tuple]s can be used if this is required.
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
NULL
#---------------------------------------------
# Definition and Construction
#---------------------------------------------
Set <- R6::R6Class("Set")
Set$set("public","initialize",function(..., universe = NULL){

  dots = list(...)
  if(any(grepl("list", lapply(dots, class))))
    dots <- unlist(dots, recursive = FALSE)

  if(length(dots) != 0 & length(unlist(dots))!=0){
    class <- unique(sapply(dots,function(x) class(x)[[1]]))
    if(length(class)==1)
      private$.class <- class
    else if(length(class)==2 & "list" %in% class)
      private$.class <- class[!(class %in% "list")]
    else
      private$.class <- "multiple"

    if(private$.class == "multiple")
      elements = dots
    else
      elements = unlist(dots)

    if(testTuple(self) | testFuzzyTuple(self))
      private$.elements <- elements
    else
      private$.elements <- elements[!duplicated(lapply(elements, function(x){
        y = try(x$strprint(), silent = TRUE)
        if(inherits(y, "try-error"))
          return(x)
        else
          return(y)
      }))]

    if(private$.class %in% c("numeric", "integer")){
      private$.lower <- min(unlist(dots))
      private$.upper <- max(unlist(dots))
    }

    if(!is.null(universe)){
      assertSet(universe)
      private$.universe <- universe
    }
  }

  private$.properties$singleton = ifelse(self$length == 1, TRUE, FALSE)
  private$.properties$empty = ifelse(self$length == 0, TRUE, FALSE)
  private$.properties$cardinality = self$length
  private$.properties$countability = "countably finite"
  private$.properties$closure = "closed"
  private$.properties = private$.properties[match(c("empty","singleton","cardinality",
                                                          "countability","closure"),
                                            names(private$.properties))]

  invisible(self)
})

#---------------------------------------------
# Public methods - Representation
#---------------------------------------------
Set$set("public","print",function(n = 2){
  cat(self$strprint(n),"\n")
  invisible(self)
})
#' @title String Representation For Print
#' @name strprint
#' @description Parsable string to be supplied to \code{print}, \code{data.frame}, etc.
#' @details `strprint` is a suggested method that should be included in all R6 classes to be passed to
#' methods such as \code{cat}, \code{summary} and \code{print}. Additionally can be used to easily
#' parse R6 objects into data-frames, see examples.
#'
#' It is often not required to call this directly and the print method is recommended for printing
#' strings to the console.
#'
#' @param object R6 object
#' @param n Number of elements to display before & after ellipsis
#' @section R6 Usage: $strprint(object, n = 2)
#' @return String representation of the set.
#'
#' @examples
#' Set$new(1:10)$strprint(n = 2)
#' Set$new(1:10)$strprint()
Set$set("public","strprint",function(n = 2){
  if (self$properties$empty) {
    if(use_unicode())
      return("\u2205")
    else
      return("{}")
  } else {
    type <- private$.type
    elements <- sapply(self$elements, function(x){
      y = try(x$strprint(), silent = T)
      if(inherits(y,"try-error"))
        return(x)
      else
        return(y)
    })
    if(self$length <= n * 2)
      return(paste0(substr(type,1,1),paste0(elements, collapse = ", "), substr(type,2,2)))
    else
      return(paste0(substr(type,1,1),paste0(elements[1:n], collapse = ", "), ",...,",
                    paste0(elements[(self$length-n+1):self$length],collapse=", "),
                    substr(type,2,2), collapse = ", "))
  }
})
Set$set("public","summary",function(n = 2){
  prop = self$properties
  cat(getR6Class(self),"\n\t",self$strprint(n),"\n",sep="")
  cat("Traits:\n\t")
  cat(ifelse(testCrisp(self), "Crisp", "Fuzzy"),"\n")
  cat("Properties:\n")
  if(prop$empty) cat("\tEmpty\n")
  if(prop$singleton) cat("\tSingleton\n")
  cat("\tCardinality =",prop$cardinality," - ",prop$countability,"\n")
  cat("\t",toproper(prop$closure),"\n",sep="")
})

#---------------------------------------------
# Public methods - Comparison
#---------------------------------------------
#' @name contains
#' @rdname contains
#' @title Are Elements Contained in the Set?
#' @description Tests to see if \code{x} is contained in the Set.
#'
#' @param x ANY
#' @param y [Set]
#'
#' @details \code{x} can be of any type, including a Set itself. \code{x} should be a tuple if
#' checking to see if it lies within a set of dimension greater than one. To test for multiple \code{x}
#' at the same time, then provide these as a list.
#'
#' If using the method directly, and not via one of the operators then the additional boolean
#' arguments `all` and `bound`. If `all = TRUE` then returns `TRUE` if all `x` are contained in the `Set``, otherwise
#' returns a vector of logicals. If `bound = TRUE` then returns `TRUE` for elements of `x` if they are
#' in or on the boundaries of the Set.
#'
#' can be used to specify testing of subsets or proper subsets. A Set is a proper
#' subset of another if it is fully contained by the other Set (i.e. not equal to) whereas a Set is a
#' (non-proper) subset if it is fully contained by, or equal to, the other Set.
#'
#' @return If \code{all} is TRUE then returns TRUE if all elements of \code{x} are contained in the Set, otherwise
#' FALSE. If \code{all} is FALSE then returns a vector of logicals corresponding to each individual
#' element of \code{x}.
#' @section R6 Usage: $contains(x, all = FALSE, bound = NULL)
#' @examples
#' s = Set$new(1:5)
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
Set$set("public","contains",function(x, all = FALSE, bound = NULL){
  x = listify(x)

  ret = rep(FALSE, length(x))

  # determine which elements are R6 and need a special equals method
  r6.fil <- sapply(x, function(y) ifelse(inherits(y, "R6"), TRUE, FALSE))
  r6.match <- x[r6.fil]
  atom.match <- x[!r6.fil]

  # for base classes simply use %in% for containedness
  if(length(atom.match) > 0)
    ret[!r6.fil][atom.match %in% self$elements] = TRUE

  # for R6 classes a
  if(length(r6.match) > 0){
    r6.tr <- sapply(r6.match, function(y){
      if(Set$new()$equals(y))
        y <- Set$new()
      cl <- getR6Class(y)
      # first check to see if they are same class
      fil <- sapply(self$elements, function(z) ifelse(inherits(z, cl), TRUE, FALSE))
      # if they are then check if any of elements of self are equal to x
      any(sapply(self$elements[fil], function(z) y$equals(z)))
    })
    ret[r6.fil][r6.tr] = TRUE
  }

  returner(ret, all)
})
#' @name equals
#' @rdname equals
#' @param x Set
#' @param y Set
#' @title Are Two Sets Equal?
#' @return If `all` is `TRUE` then returns `TRUE` if all `x` are equal to the Set, otherwise
#' `FALSE`. If `all` is `FALSE` then returns a vector of logicals corresponding to each individual
#' element of `x`.
#' @section R6 Usage: $equals(x)
#' @examples
#' # Equals
#' Set$new(1,2)$equals(Set$new(5,6))
#' Set$new(1,2)$equals(Interval$new(1,2))
#' Set$new(1,2) == Interval$new(1,2, class = "integer")
#'
#' # Not equal
#' !Set$new(1,2)$equals(Set$new(1,2))
#' Set$new(1,2) != Set$new(1,5)
Set$set("public","equals",function(x, all = FALSE){
  x <- listify(x)

  ret = sapply(x, function(el){
    if(testFuzzy(el)){
      if(all(el$membership() == 1))
        el = as.Set(el)
      else
        return(FALSE)
    }

    if(!testSet(el))
      return(FALSE)

    elel = lapply(el$elements, function(x) ifelse(testSet(x), x$strprint(), x))
    selel = lapply(self$elements, function(x) ifelse(testSet(x), x$strprint(), x))

    suppressWarnings(all(elel %in% selel) & all(selel %in% elel))
  })

  returner(ret, all)
})
#' @name isSubset
#' @rdname isSubset
#' @title Test If Two Sets Are Subsets
#' @param x,y [Set]
#' @details If using the method directly, and not via one of the operators then the additional boolean
#' argument `proper` can be used to specify testing of subsets or proper subsets. A Set is a proper
#' subset of another if it is fully contained by the other Set (i.e. not equal to) whereas a Set is a
#' (non-proper) subset if it is fully contained by, or equal to, the other Set.
#'
#' When calling [isSubset] on objects inheriting from [Interval], the method treats the interval as if
#' it is a [Set], i.e. ordering and class are ignored. Use [isSubinterval] to test if one interval
#' is a subinterval of another.
#'
#' @return If `all` is `TRUE` then returns `TRUE` if all `x` are subsets of the Set, otherwise
#' `FALSE`. If `all` is `FALSE` then returns a vector of logicals corresponding to each individual
#' element of `x`.
#' @section R6 Usage: $isSubset(x, proper = FALSE, all = FALSE)
#' @seealso [isSubinterval]
#' @examples
#' Set$new(1,2,3)$isSubset(Set$new(1,2), proper = TRUE)
#' Set$new(1,2) < Set$new(1,2,3) # proper subset
#'
#' c(Set$new(1,2,3), Set$new(1)) < Set$new(1,2,3) # not proper
#' Set$new(1,2,3) <= Set$new(1,2,3) # proper
Set$set("public","isSubset",function(x, proper = FALSE, all = FALSE){
  x = listify(x)

  ret = sapply(x, function(el){
    if(!inherits(el, "R6"))
      return(FALSE)

    if(testFuzzy(el)){
      if(all(el$membership() == 1))
        el = as.Set(el)
      else
        return(FALSE)
    }

    if(!testSet(el))
      return(FALSE)

    elel = lapply(el$elements, function(x) ifelse(testSet(x), x$strprint(), x))
    selel = lapply(self$elements, function(x) ifelse(testSet(x), x$strprint(), x))

    if(proper){
      if(all(elel %in% selel) & !all(selel %in% elel))
        return(TRUE)
      else
        return(FALSE)
    }else{
      if(all(elel %in% selel))
        return(TRUE)
      else
        return(FALSE)
    }
  })

  returner(ret, all)
})
#---------------------------------------------
# Public methods - absComplement
#---------------------------------------------
#' @name absComplement
#' @rdname absComplement
#' @title Absolute Complement of a Set
#' @description Calculates and returns the absolute complement of a Set, which is the relative
#' complement of a set from its universe.
#' @details If a universe is not provided then the method has no effect. To find the relative difference
#' between sets use [setcomplement].
#' @section R6 Usage: $absComplement()
#' @return Set
Set$set("public","absComplement",function(){
  if(!is.null(self$universe))
    return(setcomplement(self$universe, self))
  else{
    message("Universe not provided, returning self.")
    invisible(self)
  }
})
#---------------------------------------------
# Accessors
#---------------------------------------------
#' @name properties
#' @title Set Properties
#' @rdname properties
#' @section R6 Usage: $properties
#' @description List the properties of the Set.
#' @details Set properties include:
#' \itemize{
#'  \item \code{empty} - is the Set empty or does it contain elements?
#'  \item \code{singleton} - is the Set a singleton? i.e. Does it contain only one element?
#'  \item \code{cardinality} - number of elements in the Set
#'  \item \code{countability} - One of countably finite, countably infinite, uncountable
#'  \item \code{closure} - One of closed, open, half-open
#' }
Set$set("active","properties",function(){
  return(private$.properties)
})
#' @name traits
#' @title Set Traits
#' @rdname traits
#' @section R6 Usage: $traits
#' @description List the traits of the Set.
#' @details Set traits include:
#' \itemize{
#'  \item \code{crisp} - is the Set crisp or fuzzy?
#' }
Set$set("active","traits",function(){
  return(private$.traits)
})
#' @name type
#' @title Set Type
#' @rdname type
#' @section R6 Usage: $type
#' @description Returns the type of the Set.
#' @details Set type is one of: (), (], [), [], \{\}
Set$set("active","type",function(){
  return(private$.type)
})
#' @name max
#' @title Set Maximum
#' @rdname max
#' @section R6 Usage: $max
#' @description Returns the maximum of the Set.
#' @details If the Set consists of numerics only then return the maximum element in the Set. For
#' open or half-open sets, then the maximum is defined by
#' \deqn{upper - .Machine\$double.xmin}
Set$set("active","max",function(){
  if(private$.type %in% c("()","[)"))
    return(self$upper-.Machine$double.xmin)
  else
    return(self$upper)
})
#' @name min
#' @title Set Minimum
#' @rdname min
#' @section R6 Usage: $min
#' @description Returns the minimum of the Set.
#' @details If the Set consists of numerics only then return the minimum element in the Set. For
#' open or half-open sets, then the minimum is defined by
#' \deqn{lower + .Machine\$double.xmin}
Set$set("active","min",function(){
  if(private$.type %in% c("()","(]"))
    return(self$lower+.Machine$double.xmin)
  else
    return(self$lower)
})
#' @name upper
#' @title Upper Limit of Set
#' @rdname upper
#' @section R6 Usage: $upper
#' @description Returns the upper limit or last element in the Set.
#' @details If the Set consists of numerics only then returns the upper limit, or supremum, of the Set.
#' Otherwise assumes that the elements were supplied in a particular order and returns the last
#' element.
Set$set("active","upper",function(){
  return(private$.upper)
})
#' @name lower
#' @title Lower Limit of Set
#' @rdname lower
#' @section R6 Usage: $lower
#' @description Returns the lower limit or first element in the Set.
#' @details If the Set consists of numerics only then returns the lower limit, or infimum, of the Set.
#' Otherwise assumes that the elements were supplied in a particular order and returns the first
#' element.
Set$set("active","lower",function(){
  return(private$.lower)
})
#' @name class
#' @title Class of Set
#' @rdname class
#' @section R6 Usage: $class
#' @description Returns the class of the Set.
#' @details If all elements in the Set are of the same class, then returns the class. Otherwise
#' returns "multiple".
Set$set("active","class",function(){
  return(private$.class)
})
#' @name elements
#' @title Set Elements
#' @rdname elements
#' @section R6 Usage: $elements
#' @description Returns the elements in the Set.
#' @details If the Set is countably finite then the elements in the Set are returned, otherwise NaN.
Set$set("active","elements",function(){
  return(private$.elements)
})
#' @name universe
#' @title Universe of a Set
#' @rdname universe
#' @section R6 Usage: $universe
#' @description Returns the universe of the Set.
#' @details The universe is an optional Set that specifies where the given Set lives. This is useful
#' for taking the complement of a Set.
Set$set("active","universe",function(){
  return(private$.universe)
})
#' @name range
#' @title Numeric Range of Set
#' @rdname range
#' @section R6 Usage: $range
#' @description Returns the range of the Set.
#' @details If the Set consists of numerics only then returns
#' \deqn{max - min}.
Set$set("active","range",function(){
  if(self$class %in% c("numeric", "integer"))
    return(self$upper - self$lower)
  else
    return(numeric(0))
})
#' @name length
#' @title Number of Elements in the Set
#' @rdname length
#' @section R6 Usage: $length
#' @description Returns the number of elements in the Set.
#' @details Returns either the number of elements in the Set, or \code{Inf} for infinite intervals.
#' See \code{cardinality} in \code{\link{properties}} for the type of infinity.
Set$set("active","length",function(){
  return(length(self$elements))
})

#---------------------------------------------
# Private variables
#---------------------------------------------
Set$set("private",".class","multiple")
Set$set("private",".type","{}")
Set$set("private",".lower", NaN)
Set$set("private",".upper", NaN)
Set$set("private",".universe",NULL)
Set$set("private",".elements",list())
Set$set("private",".properties",list(empty = logical(0), singleton = logical(0),
                                     cardinality = character(0), countability = character(0),
                                     closure = character(0)))
Set$set("private",".traits",list(crisp = TRUE))
Set$set("private",".dimension", numeric(0))
#---------------------------------------------
# summary
#---------------------------------------------
#' @export
summary.Set <- function(object, n = 2, ...){
  object$summary(n)
}
#---------------------------------------------
# as.Set
#---------------------------------------------
#' @title Coercion to R6 Set
#' @description Coerces objects to R6 Sets
#' @param object object to coerce
#' @export
as.Set <- function(object){
  UseMethod("as.Set",object)
}
#' @rdname as.Set
#' @export
as.Set.numeric <- function(object){
  Set$new(object)
}
#' @rdname as.Set
#' @export
as.Set.list <- function(object){
  return(Set$new(unlist(object)))
}
#' @rdname as.Set
#' @export
as.Set.matrix <- function(object){
  return(apply(object,2,function(x) Set$new(x)))
}
#' @rdname as.Set
#' @export
as.Set.Set <- function(object){
  return(Set$new(object$elements))
}
#' @rdname as.Set
#' @export
as.Set.Interval <- function(object){
  if(any(is.nan(object$elements))){
    message("Interval cannot be coerced to Set.")
    return(object)
  } else {
    return(Set$new(object$elements))
  }
}
#' @rdname as.Set
#' @export
as.Set.ConditionalSet <- function(object){
  message("ConditionalSet cannot be coerced to Set.")
  return(object)
}
#---------------------------------------------
# Overloaded operators
#---------------------------------------------
#' @rdname isSubset
#' @export
'<.Set' <- function(x, y){
  return(y$isSubset(x, proper = TRUE))
}
#' @rdname isSubset
#' @export
'<=.Set' <- function(x, y){
  return(y$isSubset(x, proper = FALSE))
}
#' @rdname isSubset
#' @export
'>.Set' <- function(x, y){
  return(x$isSubset(y, proper = TRUE))
}
#' @rdname isSubset
#' @export
'>=.Set' <- function(x, y){
  return(x$isSubset(y, proper = FALSE))
}
#' @rdname equals
#' @export
'==.Set' <- function(x, y){
  return(x$equals(y))
}
#' @rdname equals
#' @export
'!=.Set' <- function(x, y){
  return(!x$equals(y))
}
#' @rdname contains
#' @export
'%inset%' <- function(x, y){
  return(y$contains(x, bound = TRUE))
}
