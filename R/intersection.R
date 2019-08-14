intersection <- function(x, y){
  if(testSet(x) & testSet(y))
    return(Set$new(intersect(x$elements(), y$elements())))
}
