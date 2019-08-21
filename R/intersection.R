#' @title Intersection
#' @description Intersection of two SetInterval objects
#' @param x SetInterval
#' @param y SetInterval
#' @export
intersection <- function(x, y){
  if(testSet(x) & testSet(y)){
    int <- intersect(x$elements(), y$elements())
    if(length(int) == 0)
      return(Empty$new())
    else
      return(Set$new(intersect(x$elements(), y$elements())))
  }

}

#' @rdname intersection.Set
#' @export
'&.Set' <- function(x, y){
  intersection(x, y)
}
