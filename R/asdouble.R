#' @export
as.double.Set <- function(x,...) {
  if(testFuzzy(x))
    return(unlist(x$support()))
  else
    return(unlist(x$elements))
}
