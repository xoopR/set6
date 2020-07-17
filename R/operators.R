#' @template operators
#' @templateVar name isSubset
#' @templateVar op1 Subset \tab `x` is a subset of `y` \tab `x <= y` \cr
#' @templateVar op2 Proper Subset \tab `x` is a proper subset of `y` \tab `x < y` \cr
#' @templateVar op3 Superset \tab `x` is a superset of `y` \tab `x >= y` \cr
#' @templateVar op4 Proper Superset \tab `x` is a proper superset of `y` \tab `x > y` \cr
NULL

#' @rdname isSubset
#' @export
"<.Set" <- function(x, y) {
  return(y$isSubset(x, proper = TRUE))
}

#' @rdname isSubset
#' @export
"<=.Set" <- function(x, y) {
  return(y$isSubset(x, proper = FALSE))
}
#' @rdname isSubset
#' @export
">.Set" <- function(x, y) {
  return(x$isSubset(y, proper = TRUE))
}
#' @rdname isSubset
#' @export
">=.Set" <- function(x, y) {
  return(x$isSubset(y, proper = FALSE))
}

#' @template operators
#' @templateVar name equals
#' @templateVar op1 Equal \tab `x` equals `y` \tab `==` \cr
#' @templateVar op2 Not Equal \tab `x` does not equal `y` \tab `!=` \cr
NULL
#' @rdname equals
#' @export
"==.Set" <- function(x, y) {
  return(x$equals(y))
}
#' @rdname equals
#' @export
"!=.Set" <- function(x, y) {
  return(!x$equals(y))
}

#' @template operators
#' @templateVar name contains
#' @templateVar op1 Contains \tab `x` contains `y` \tab `y $inset$ x` \cr
NULL
#' @rdname contains
#' @export
"%inset%" <- function(x, y) {
  return(y$contains(x))
}
