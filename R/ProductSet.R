ProductSet <- R6::R6Class("ProductSet", inherit = SetWrapper)
ProductSet$set("public", "initialize", function(x, y, tuple = FALSE){
  if(testConditionalSet(x) | testConditionalSet(y))
    stop("Product with conditional sets is currently unsupported.")


  if(getR6Class(x) %in% c("Set", "Tuple")){
    if(testFuzzy(y)){
      message("Only the support of the fuzzy set is kept, use alphaCut() to change this.")
      y <- y$support(create = TRUE)
    }

    # Set/Tuple X Set/Tuple
    if(getR6Class(y) %in% c("Set", "Tuple")){
      super$initialize(apply(expand.grid(x$elements, y$elements), 1,
                             function(z) Tuple$new(z)), setlist = list(x,y))
      # if(tuple)
      #   return(Tuple$new(apply(expand.grid(x$elements, y$elements), 1,
      #                          function(z) Tuple$new(z)), dimension = 2))
    }
  } else if(testInterval(y)){
    if(testInterval(y)){
      super$initialize(lower = Set$new(x$lower, y$lower),
                       upper = Set$new(x$upper, y$upper),
                       type = '{}',
                       dimension = 2,
                       symbol = paste(x$strprint(), y$strprint(),sep=" \u00D7 "),
                       setlist = list(x,y))
    }
  }


})

ProductSet$set("public", "strprint", function(){
  if(length(private$.symbol) != 0)
    return(paste0(substr(self$type,1,1), private$.symbol, substr(self$type,2,2)))
  else
    super$strprint()
})
ProductSet$set("private", "symbol", character(0))

`*.Set` <- function(x, y){
  ProductSet$new(x, y)
}
