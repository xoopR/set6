#' @title Intersection
#' @description Intersection of two SetInterval objects
#' @param x SetInterval
#' @param y SetInterval
#' @export
intersection <- function(x, y){
  if(testSet(x) & testSet(y))
    return(Set$new(intersect(x$elements(), y$elements())))
}
