#' @include Set.R

#' @rdname Set
#' @export
'<.Set' <- function(x, y){
  return(y$isSubset(x, proper = TRUE))
}
#' @rdname Set
#' @export
'<=.Set' <- function(x, y){
  return(y$isSubset(x, proper = FALSE))
}
#' @rdname Set
#' @export
'>.Set' <- function(x, y){
  return(x$isSubset(y, proper = TRUE))
}
#' @rdname Set
#' @export
'>=.Set' <- function(x, y){
  return(x$isSubset(y, proper = FALSE))
}
#' @rdname Set
#' @export
'==.Set' <- function(x, y){
  return(x$equals(y))
}
#' @rdname Set
#' @export
'!=.Set' <- function(x, y){
  return(!x$equals(y))
}
#' @rdname Set
#' @export
'%inset%' <- function(x, y){
  return(y$contains(x))
}
