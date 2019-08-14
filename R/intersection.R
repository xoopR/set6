intersection <- function(x, y){
  if(testSet(x) & testSet(y))
    return(Set$new(intersect(self$elements(), x$elements())))
}
