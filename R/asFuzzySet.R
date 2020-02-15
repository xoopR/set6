#' @template coercion2
#' @templateVar class1 FuzzySet
#' @templateVar class2 FuzzyTuple
#' @details
#' * `as.FuzzySet.list` - Assumes `list` has two items, named `elements` and `membership`,
#' and that they are ordered to be corresponding.
#' * `as.FuzzySet.matrix` - Assumes first column corresponds to `elements` and second column corresponds
#' to their respective `membership`.
#' * `as.FuzzySet.data.frame` - First checks to see if one column is called `elements` and the other is called `membership`.
#' If not then uses `as.FuzzySet.matrix`.
#' * `as.FuzzySet.Set` - Creates a [FuzzySet] by assuming [Set] elements all have `membership` equal to \eqn{1}.
#' * `as.FuzzySet.Interval` - First tries coercion via [as.Set.Interval] then uses [as.FuzzySet.Set].
#' @export
as.FuzzySet <- function(object){
  UseMethod("as.FuzzySet",object)
}
#' @rdname as.FuzzySet
#' @export
as.FuzzySet.numeric <- function(object){
  FuzzySet$new(elements = object[seq.int(1,length(object),2)],
               membership = as.numeric(object[seq.int(2,length(object),2)]))
}
#' @rdname as.FuzzySet
#' @export
as.FuzzySet.list <- function(object){
  return(FuzzySet$new(elements = object$elements, membership = object$membership))
}
#' @rdname as.FuzzySet
#' @export
as.FuzzySet.matrix <- function(object){
  return(FuzzySet$new(elements = object[,1], membership = object[,2]))
}
#' @rdname as.FuzzySet
#' @export
as.FuzzySet.data.frame <- function(object){
  if(all(c("elements", "membership") %in% colnames(object)))
    return(FuzzySet$new(elements = object$elements, membership = object$membership))
  else
    return(FuzzySet$new(elements = object[,1], membership = object[,2]))
}
#' @rdname as.FuzzySet
#' @export
as.FuzzySet.Set <- function(object){
  return(FuzzySet$new(elements = object$elements))
}
#' @rdname as.FuzzySet
#' @export
as.FuzzySet.FuzzySet <- function(object){
  return(FuzzySet$new(elements = object$elements, membership = object$membership()))
}
#' @rdname as.FuzzySet
#' @export
as.FuzzySet.Interval <- function(object){
  ifnerror(as.Set.Interval(object), error = "stop", errormsg = "Interval cannot be coerced to FuzzySet.")
}
#' @rdname as.FuzzySet
#' @export
as.FuzzySet.ConditionalSet <- function(object){
  stop("ConditionalSet cannot be coerced to FuzzySet.")
}
#-----------------------------
# FuzzyTuple
#-----------------------------
#' @rdname as.FuzzySet
#' @aliases as.FuzzyTuple
#' @export
as.FuzzyTuple <- function(object){
  UseMethod("as.FuzzyTuple",object)
}
#' @rdname as.FuzzySet
#' @export
as.FuzzyTuple.numeric <- function(object){
  FuzzyTuple$new(elements = object[seq.int(1,length(object),2)],
                 membership = as.numeric(object[seq.int(2,length(object),2)]))
}
#' @rdname as.FuzzySet
#' @export
as.FuzzyTuple.list <- function(object){
  return(FuzzyTuple$new(elements = object$elements, membership = object$membership))
}
#' @rdname as.FuzzySet
#' @export
as.FuzzyTuple.matrix <- function(object){
  return(FuzzyTuple$new(elements = object[,1], membership = object[,2]))
}
#' @rdname as.FuzzySet
#' @export
as.FuzzyTuple.data.frame <- function(object){
  if(all(c("elements", "membership") %in% colnames(object)))
    return(FuzzyTuple$new(elements = object$elements, membership = object$membership))
  else
    return(FuzzyTuple$new(elements = object[,1], membership = object[,2]))
}
#' @rdname as.FuzzySet
#' @export
as.FuzzyTuple.Set <- function(object){
  return(FuzzyTuple$new(elements = object$elements))
}
#' @rdname as.FuzzySet
#' @export
as.FuzzyTuple.FuzzySet <- function(object){
  return(FuzzyTuple$new(elements = object$elements, membership = object$membership()))
}
#' @rdname as.FuzzySet
#' @export
as.FuzzyTuple.Interval <- function(object){
  ifnerror(as.Set.Interval(object), error = "stop", errormsg = "Interval cannot be coerced to FuzzyTuple.")
}
#' @rdname as.FuzzySet
#' @export
as.FuzzyTuple.ConditionalSet <- function(object){
  stop("ConditionalSet cannot be coerced to FuzzyTuple.")
}
