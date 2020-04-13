#' @export
as.double.Set <- function(x,...) {
  if(testFuzzy(x))
    return(unlist(x$support()))
  else
    return(unlist(x$elements))
}

#' @export
as.character.Set <- function(x, n = 1e10, ...){
  x$strprint(n = n)
}
