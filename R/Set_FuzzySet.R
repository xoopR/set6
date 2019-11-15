#---------------------------------------------
# Documentation
#---------------------------------------------
#' @name FuzzySet
#' @title Mathematical Fuzzy Set
#'
#' @description A general FuzzySet object for mathematical fuzzy sets, inheriting from `Set`.
#' @return R6 object of class FuzzySet inheriting from [Set].
#' @template Set
#' @templateVar constructor FuzzySet$new(..., elements = NULL, membership = rep(1, length(elements)))
#' @templateVar arg1 `...` \tab ANY \tab Alternating elements and membership, see constructor details. \cr
#' @templateVar arg2 `elements` \tab ANY \tab Elements in the set, see constructor details. \cr
#' @templateVar arg3 `membership` \tab numeric \tab Corresponding membership of the elements, see constructor details. \cr
#' @templateVar constructorDets `FuzzySet`s can be constructed in one of two ways, either by supplying the elements and their membership in alternate order, or by providing a list of elements to `elements` and a list of respective memberships to `membership`, see examples.
#' @templateVar meth1 **Fuzzy Methods** \tab **Link** \cr
#' @templateVar meth2 `membership(element = NULL)` \tab [membership] \cr
#' @templateVar meth3 `alphaCut(alpha, strong = FALSE, create = FALSE)` \tab [alphaCut] \cr
#' @templateVar meth4 `support(create = FALSE)` \tab [support] \cr
#' @templateVar meth5 `core(create = FALSE)` \tab [core] \cr
#' @templateVar meth6 `inclusion(element)` \tab [inclusion] \cr
#' @templateVar meth7  \tab \cr \tab \cr \tab \cr
#'
#' @details
#' Fuzzy sets generalise standard mathematical sets to allow for fuzzy relationships. Whereas a
#' standard, or crisp, set assumes that an element is either in a set or not, a fuzzy set allows
#' an element to be in a set to a particular degree, known as the membership function, which
#' quantifies the inclusion of an element by a number in \[0, 1\]. Thus a (crisp) set is a
#' fuzzy set where all elements have a membership equal to \eqn{1}. Similarly to [Set]s, elements
#' must be unique and the ordering does not matter, to establish order and non-unique elements,
#' [FuzzyTuple]s can be used.
#'
#' @seealso
#' [Set], [FuzzyTuple]
#'
#' @examples
#' # Different constructors
#' FuzzySet$new(1, 0.5, 2, 1, 3, 0)
#' FuzzySet$new(elements = 1:3, membership = c(0.5, 1, 0))
#'
#' # Crisp sets are a special case FuzzySet
#' # Note membership defaults to full membership
#' FuzzySet$new(elements = 1:5) == Set$new(1:5)
#'
#' f = FuzzySet$new(1, 0.2, 2, 1, 3, 0)
#' f$membership()
#' f$alphaCut(0.3)
#' f$core()
#' f$inclusion(0)
#' f$membership(0)
#' f$membership(1)
#'
#' @export
NULL
#---------------------------------------------
# Definition and Construction
#---------------------------------------------
FuzzySet <- R6::R6Class("FuzzySet", inherit = Set)
FuzzySet$set("public","initialize",function(..., elements = NULL, membership = rep(1, length(elements))){
  if(!is.null(elements) & !is.null(membership)){
    membership <- as.numeric(membership)
    if(length(membership) == 1)
      membership <- rep(membership, length(elements))
  } else if(length(list(...)) != 0){
    dots <- list(...)
    if(length(dots)%%2)
      stop("Every element needs a corresponding membership.")
    elements <- dots[seq.int(1,length(dots),2)]
    membership <- as.numeric(dots[seq.int(2,length(dots),2)])
  }

  if(any(duplicated(elements)) & !testFuzzyTuple(self)){
    message("Duplicated elements dedicated, only the first element-membership pair is included.")
    membership <- membership[!duplicated(elements)]
    elements <- elements[!duplicated(elements)]
  }

  checkmate::assertNumeric(membership, lower = 0, upper = 1, any.missing = FALSE)
  private$.membership <- membership

  super$initialize(elements)
  invisible(self)
})

FuzzySet$set("public","strprint",function(n = 2){
  if(self$properties$empty) {
    if(use_unicode())
      return("\u2205")
    else
      return("{}")
  } else {
    elements <- sapply(self$elements, function(x){
      y = try(x$strprint(), silent = T)
      if(inherits(y,"try-error"))
        return(x)
      else
        return(y)
    })
    members <- self$membership()

    if(self$length <= n * 2)
      return(paste0(substr(self$type,1,1),paste0(elements,"(",members,")", collapse = ", "),
                    substr(self$type,2,2)))
    else
      return(paste0(substr(self$type,1,1),paste0(elements[1:n],"(",members[1:n],")",collapse = ", "), ",...,",
                    paste0(elements[(self$length-n+1):self$length],"(",
                           members[(self$length-n+1):self$length],")",collapse=", "),
                    substr(self$type,2,2), collapse = ", "))
  }
})

#' @name membership
#' @rdname membership
#' @title Get Membership of Element in FuzzySet
#' @param element element in the set, if NULL returns membership of all elements
#' @description Returns the membership, i.e. value in \[0, 1\], of either the given element
#' or all elements in the fuzzy set.
#' @details For `FuzzySet`s this is straightforward and returns the membership of the given element,
#' however in `FuzzyTuple`s when an element may be duplicated, the function returns the membership of
#' the first instance of the element.
#' @return Value, or vector of values, in \[0, 1\]
#' @section R6 Usage: $membership(element = NULL)
#' @examples
#' f = FuzzySet$new(1, 0.1, 2, 0.5, 3, 1)
#' f$membership()
#' f$membership(2)
FuzzySet$set("public","membership",function(element = NULL){
  if(is.null(element))
    return(private$.membership)
  else{
    x <-  match(element, self$elements)
    if(is.na(x)){
      message(sprintf("%s is not in this fuzzy set.", element))
      return(NA)
    }else
      return(private$.membership[match(element, self$elements)])
  }
})

#' @name alphaCut
#' @rdname alphaCut
#' @title Get Elements in FuzzySet with Membership Greater than Alpha
#' @param alpha numeric in \[0, 1\] to determine which elements to return
#' @param strong logical, if `FALSE` (default) then only includes elements strictly greater than alpha, otherwise greater than or equal
#' @param create logical, if `FALSE` (default) returns the elements in the alpha cut, otherwise returns a crisp set of the elements
#' @description The alpha-cut of a fuzzy set is defined as the set
#' \deqn{A_\alpha = \{x \epsilon F | m \ge \alpha\}}{A_\alpha = {x \epsilon F | m \ge \alpha}}
#' where \eqn{x} is an element in the fuzzy set, \eqn{F}, and \eqn{m} is the corresponding membership.
#' @return Elements in fuzzy set or a [Set] of the elements.
#' @section R6 Usage: $alphaCut(alpha, strong = FALSE, create = FALSE)
#' @examples
#' f = FuzzySet$new(1, 0.1, 2, 0.5, 3, 1)
#' # Alpha-cut
#' f$alphaCut(0.5)
#'
#' # Strong alpha-cut
#' f$alphaCut(0.5, strong = TRUE)
#'
#' # Create a set from the alpha-cut
#' f$alphaCut(0.5, create = TRUE)
FuzzySet$set("public","alphaCut",function(alpha, strong = FALSE, create = FALSE){
  if(strong)
    els <- self$elements[self$membership() > alpha]
  else
    els <- self$elements[self$membership() >= alpha]

  if(create){
    if(length(els) == 0)
      return(Set$new())
    else
      return(Set$new(els))
  } else{
    if(length(els) == 0)
      return(NULL)
    else
      return(unname(els))
  }
})

#' @name support
#' @rdname support
#' @title Get Support of FuzzySet
#' @param create logical, if `FALSE` (default) returns the support elements, otherwise returns a set of the support elements
#' @description The support of a fuzzy set is defined as the set of elements whose membership is greater than zero, or the strong
#' alpha-cut with \eqn{\alpha = 0},
#' \deqn{A_\alpha = \{x \epsilon F | m > 0\}}{A_\alpha = {x \epsilon F | m > 0}}
#' where \eqn{x} is an element in the fuzzy set, \eqn{F}, and \eqn{m} is the corresponding membership.
#' @return Support elements in fuzzy set or a [Set] of the support elements.
#' @section R6 Usage: $support(create = FALSE)
#' @examples
#' f = FuzzySet$new(0.1, 0, 1, 0.1, 2, 0.5, 3, 1)
#' f$support()
#' f$support(TRUE)
FuzzySet$set("public","support",function(create = FALSE){
  self$alphaCut(0, TRUE, create)
})

#' @name core
#' @rdname core
#' @title Get Core of FuzzySet
#' @param create logical, if `FALSE` (default) returns the core elements, otherwise returns a set of the core elements
#' @description The core of a fuzzy set is defined as the set of elements whose membership is equal to one,
#' or the alpha-cut with \eqn{\alpha = 1},
#' \deqn{A_\alpha = \{x \epsilon F | m \ge 1\}}{A_\alpha = {x \epsilon F | m \ge 1}}
#' where \eqn{x} is an element in the fuzzy set, \eqn{F}, and \eqn{m} is the corresponding membership.
#' @return Core elements in fuzzy set or a [Set] of the core elements.
#' @section R6 Usage: $core(create = FALSE)
#' @examples
#' f = FuzzySet$new(0.1, 0, 1, 0.1, 2, 0.5, 3, 1)
#' f$core()
#' f$core(TRUE)
FuzzySet$set("public","core",function(create = FALSE){
  self$alphaCut(1, FALSE, create)
})

#' @name inclusion
#' @rdname inclusion
#' @title Get Inclusion Level of Element In FuzzySet
#' @param element element in fuzzy set for which to get the inclusion level
#' @description An element in a fuzzy set is:
#' * Included - If m = 1
#' * Partially Included - If 0 < m < 1
#' * Not Included - If m = 0
#' @return One of: "Included", "Partially Included", "Not Included"
#' @section R6 Usage: $inclusion(element)
#' @details For `FuzzySet`s this is straightforward and returns the inclusion level of the given element,
#' however in `FuzzyTuple`s when an element may be duplicated, the function returns the inclusion level of
#' the first instance of the element.
#' @examples
#' f = FuzzySet$new(0.1, 0, 1, 0.1, 2, 0.5, 3, 1)
#' f$inclusion(0.1)
#' f$inclusion(1)
#' f$inclusion(3)
FuzzySet$set("public","inclusion",function(element){
  member <- self$membership()[self$elements %in% element]
  if(length(member) == 0)
    return("Not Included")

  if(member == 1)
    return("Fully Included")
  else if(member == 0)
    return("Not Included")
  else
    return("Partially Included")
})
FuzzySet$set("public","equals",function(x){
  if(all(self$membership() == 1))
    return(self$core(create = T)$equals(x))

  if(!testFuzzySet(x))
    return(FALSE)

  x_mat = matrix(c(x$elements,x$membership()),ncol=2)[order(x$elements),]
  self_mat = matrix(c(self$elements,self$membership()),ncol=2)[order(self$elements),]

  if(any(dim(x_mat) != dim(self_mat)))
    return(FALSE)

  if(all(x_mat == self_mat))
    return(TRUE)
  else
    return(FALSE)
})
FuzzySet$set("public","isSubset",function(x, proper = FALSE){
  if(all(self$membership() == 1))
    return(self$core(create = T)$isSubset(x, proper = proper))

  if(!testFuzzySet(x))
    return(FALSE)

  self_comp <- paste(self$elements, self$membership(), sep=";")
  x_comp <- paste(x$elements, x$membership(), sep=";")

  if(proper){
    if(all(x_comp %in% self_comp) & !all(self_comp %in% x_comp))
      return(TRUE)
    else
      return(FALSE)
  }else{
    if(all(x_comp %in% self_comp))
      return(TRUE)
    else
      return(FALSE)
  }
})
FuzzySet$set("public","complement",function(){
  FuzzySet$new(elements = self$elements, membership = 1 - self$membership())
})
FuzzySet$set("public","powerset",function(){
  y = Vectorize(function(m) combn(self$elements, m),vectorize.args = c("m"))(1:(self$length-1))
  if(checkmate::testList(y))
    y = lapply(y, function(z) apply(z, 2, function(x){
      FuzzySet$new(elements = x, membership = self$membership(x))
    }))
  else
    y = apply(y, 1, function(x){
      FuzzySet$new(elements = x, membership = self$membership(x))
    })
  return(Set$new(Set$new(), y, self))
})

FuzzySet$set("private",".type","{}")
FuzzySet$set("private",".membership", 0)
FuzzySet$set("private",".properties",list())
FuzzySet$set("private",".traits",list(crisp = FALSE))


#' @title Coercion to R6 FuzzySet
#' @description Coerces objects to R6 FuzzySet
#' @param object object to coerce
#' @export
as.FuzzySet <- function(object){
  UseMethod("as.FuzzySet",object)
}
#' @rdname as.FuzzySet
#' @export
as.FuzzySet.numeric <- function(object){
  return(FuzzySet$new(elements = as.numeric(object), membership = names(object)))
}
#' @rdname as.FuzzySet
#' @export
as.FuzzySet.list <- function(object){
  return(FuzzySet$new(elements = unlist(object, use.names = FALSE), membership = names(object)))
}
#' @rdname as.FuzzySet
#' @export
as.FuzzySet.matrix <- function(object){
  return(FuzzySet$new(elements = object[,1], membership = object[,2]))
}
#' @rdname as.FuzzySet
#' @export
as.FuzzySet.data.table <- function(object){
  return(as.FuzzySet(as.matrix(object)))
}
#' @rdname as.FuzzySet
#' @export
as.FuzzySet.data.frame <- function(object){
  return(as.FuzzySet(as.matrix(object)))
}
#' @rdname as.FuzzySet
#' @export
as.FuzzySet.Set <- function(object){
  return(FuzzySet$new(elements = object$elements))
}
#' @rdname as.FuzzySet
#' @export
as.FuzzySet.FuzzySet <- function(object){
  return(object)
}
#' @rdname as.FuzzySet
#' @export
as.FuzzySet.FuzzySet <- function(object){
  return(FuzzySet$new(elements = object$elements, membership = object$membership()))
}
