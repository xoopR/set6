Tuple <- R6::R6Class("Tuple", inherit = Set)

Tuple$set("public","equals",function(x){
  assertSet(x)

  if(suppressWarnings(all(x$elements() == self$elements())))
    return(TRUE)
  else
    return(FALSE)
})

Tuple$set("private",".type","()")
