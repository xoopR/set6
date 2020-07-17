#' @template coercion1
#' @templateVar class1 Interval
#' @details
#' * `as.Interval.list/as.Interval.data.frame` - Assumes the `list`/`data.frame` has
#' named items/columns: `lower, upper, type, class`.
#' * `as.Interval.numeric` - If the `numeric` vector is a continuous interval with no breaks then
#' coerces to an [Interval] with: `lower = min(object), upper = max(object), class = "integer"`.
#' Ordering is ignored.
#' * `as.Interval.matrix` - Tries coercion via [as.Interval.numeric] on the first column of the
#' matrix.
#' * `as.Interval.Set` - First tries coercion via [as.Interval.numeric], if possible wraps result
#' in a [Set].
#' * `as.Interval.FuzzySet` - Tries coercion via [as.Interval.Set] on the support of the [FuzzySet].
#' @export
as.Interval <- function(object) {
  UseMethod("as.Interval", object)
}
#' @rdname as.Interval
#' @export
as.Interval.Set <- function(object) {
  if (testFuzzy(object)) {
    object <- object$support(create = TRUE)
  }

  as.Interval.numeric(unlist(object$elements))
}
#' @rdname as.Interval
#' @export
as.Interval.Interval <- function(object) {
  return(Interval$new(object$lower, object$upper, type = object$type, class = object$class))
}
#' @rdname as.Interval
#' @export
as.Interval.list <- function(object) {
  return(Interval$new(object$lower, object$upper, type = object$type, class = object$class))
}
#' @rdname as.Interval
#' @export
as.Interval.data.frame <- function(object) {
  return(Interval$new(object$lower, object$upper, type = object$type, class = object$class))
}
#' @rdname as.Interval
#' @export
as.Interval.matrix <- function(object) {
  return(as.Interval.numeric(object[, 1]))
}
#' @rdname as.Interval
#' @export
as.Interval.numeric <- function(object) {
  if (length(object) == 1) {
    return(Interval$new(object, object))
  } else if (all(diff(object) == 1)) {
    return(Interval$new(min(object), max(object), class = "integer"))
  } else {
    stop("Cannot be coerced to Interval. Elements must be equally spaced with unit difference.")
  }
}
#' @rdname as.Interval
#' @export
as.Interval.ConditionalSet <- function(object) {
  stop("ConditionalSet cannot be coerced to Interval.")
}
