ProductSet <- R6::R6Class("ProductSet", inherit = SetWrapper)
ProductSet$set("public", "initialize", function(x, y, tuple = FALSE){
  if(testConditionalSet(x) | testConditionalSet(y))
    stop("Product with conditional sets is currently unsupported.")

  if(x$length == 0 & y$length == 0) return(Empty$new())
  if(x$length == 0) return(y)
  if(y$length == 0) return(x)

  if(testFuzzy(x)){
    message("Only the support of the FuzzySet is kept, use alphaCut() to change this.")
    x <- x$support(create = TRUE)
  }
  if(testFuzzy(y)){
    message("Only the support of the FuzzySet is kept, use alphaCut() to change this.")
    y <- y$support(create = TRUE)
  }

  if(testInterval(x) | testInterval(y)){

    lower <- Tuple$new(x$lower, y$lower)
    upper <- Tuple$new(x$upper, y$upper)
    super$initialize(setlist = list(x,y),
                     lower = lower,
                     upper = upper,
                     type = '{}',
                     dimension = 2,
                     symbol = paste(x$strprint(), y$strprint(),sep=" \u00D7 ")
    )
  } else if(getR6Class(x) %in% c("Set", "Tuple")){

    # Set/Tuple X Set/Tuple
    if(getR6Class(y) %in% c("Set", "Tuple")){
      super$initialize(apply(expand.grid(x$elements, y$elements), 1,
                             function(z) Tuple$new(z)), setlist = list(x,y), dimension = 2)
    }
  }

  invisible(self)
})

ProductSet$set("public", "strprint", function(){
  if(length(private$.symbol) != 0)
    return(paste0(substr(self$type,1,1), private$.symbol, substr(self$type,2,2)))
  else
    super$strprint()
})
ProductSet$set("private", "symbol", character(0))

#' @export
`*.Set` <- function(x, y){
  ProductSet$new(x, y)
}
