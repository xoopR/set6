#' @template coercion2
#' @templateVar class1 Set
#' @templateVar class2 Tuple
#' @details
#' * `as.Set.default` - Creates a [Set] using the object as the elements.
#' * `as.Set.list` - Creates a [Set] for each element in `list`.
#' * `as.Set.matrix/as.Set.data.frame` - Creates a [Set] for each column in `matrix/data.frame`.
#' * `as.Set.FuzzySet` - Creates a [Set] from the support of the [FuzzySet].
#' * `as.Set.Interval` - If the interval has finite cardinality then creates a [Set] from the
#' [Interval] elements.
#' @export
as.Set <- function(object) {
  UseMethod("as.Set", object)
}
#' @rdname as.Set
#' @export
as.Set.default <- function(object) {
  Set$new(elements = object)
}
#' @rdname as.Set
#' @export
as.Set.numeric <- function(object) {
  Set$new(elements = object, class = "numeric")
}
#' @rdname as.Set
#' @export
as.Set.list <- function(object) {
  return(lapply(object, function(x) Set$new(elements = x)))
}
#' @rdname as.Set
#' @export
as.Set.matrix <- function(object) {
  return(apply(object, 2, function(x) Set$new(elements = x)))
}
#' @rdname as.Set
#' @export
as.Set.data.frame <- as.Set.matrix
#' @rdname as.Set
#' @export
as.Set.Set <- function(object) {
  Set$new(elements = object$elements)
}
#' @rdname as.Set
#' @export
as.Set.FuzzySet <- function(object) {
  return(object$support(create = TRUE))
}
#' @rdname as.Set
#' @export
as.Set.Interval <- function(object) {
  if (anyNA(object$elements)) {
    stop("Interval cannot be coerced to Set.")
  } else {
    return(Set$new(elements = object$elements, class = object$class))
  }
}
#' @rdname as.Set
#' @export
as.Set.ConditionalSet <- function(object) {
  stop("ConditionalSet cannot be coerced to Set.")
}
#--------------------------
# as.Tuple
#--------------------------
#' @rdname as.Set
#' @aliases as.Tuple
#' @export
as.Tuple <- function(object) {
  UseMethod("as.Tuple", object)
}
#' @rdname as.Set
#' @export
as.Tuple.default <- function(object) {
  Tuple$new(elements = object)
}
#' @rdname as.Set
#' @export
as.Tuple.numeric <- function(object) {
  Tuple$new(elements = object, class = "numeric")
}
#' @rdname as.Set
#' @export
as.Tuple.list <- function(object) {
  return(lapply(object, function(x) Tuple$new(elements = x)))
}
#' @rdname as.Set
#' @export
as.Tuple.matrix <- function(object) {
  return(apply(object, 2, function(x) Tuple$new(elements = x)))
}
#' @rdname as.Set
#' @export
as.Tuple.data.frame <- as.Tuple.matrix
#' @rdname as.Set
#' @export
as.Tuple.FuzzySet <- function(object) {
  return(Tuple$new(elements = object$support()))
}
#' @rdname as.Set
#' @export
as.Tuple.Set <- function(object) {
  return(Tuple$new(elements = object$elements))
}
#' @rdname as.Set
#' @export
as.Tuple.Interval <- function(object) {
  if (anyNA(object$elements)) {
    stop("Interval cannot be coerced to Tuple.")
  } else {
    return(Tuple$new(elements = object$elements))
  }
}
#' @rdname as.Set
#' @export
as.Tuple.ConditionalSet <- function(object) {
  stop("ConditionalSet cannot be coerced to Tuple.")
}
#--------------------------
# as.Multiset
#--------------------------
#' @rdname as.Set
#' @aliases as.Multiset
#' @export
as.Multiset <- function(object) {
  UseMethod("as.Multiset", object)
}
#' @rdname as.Set
#' @export
as.Multiset.default <- function(object) {
  Multiset$new(elements = object)
}
#' @rdname as.Set
#' @export
as.Multiset.numeric <- function(object) {
  Multiset$new(elements = object, class = "numeric")
}
#' @rdname as.Set
#' @export
as.Multiset.list <- function(object) {
  return(lapply(object, function(x) Multiset$new(elements = x)))
}
#' @rdname as.Set
#' @export
as.Multiset.matrix <- function(object) {
  return(apply(object, 2, function(x) Multiset$new(elements = x)))
}
#' @rdname as.Set
#' @export
as.Multiset.data.frame <- as.Multiset.matrix
#' @rdname as.Set
#' @export
as.Multiset.FuzzySet <- function(object) {
  return(Multiset$new(elements = object$support()))
}
#' @rdname as.Set
#' @export
as.Multiset.Set <- function(object) {
  return(Multiset$new(elements = object$elements))
}
#' @rdname as.Set
#' @export
as.Multiset.Interval <- function(object) {
  if (anyNA(object$elements)) {
    stop("Interval cannot be coerced to Multiset.")
  } else {
    return(Multiset$new(elements = object$elements))
  }
}
#' @rdname as.Set
#' @export
as.Multiset.ConditionalSet <- function(object) {
  stop("ConditionalSet cannot be coerced to Multiset.")
}
