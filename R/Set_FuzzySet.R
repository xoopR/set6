FuzzySet <- R6::R6Class("FuzzySet", inherit = Set)
FuzzySet$set("public","initialize",function(..., dim = 1){
  if(length(list(...)) != 0){
    dots <- list(...)
    if(length(dots) == 1 & is.list(dots))
      dots <- dots[[1]]
    x <- unlist(dots)
    elements <- x[seq.int(1,length(x),2)]
    private$.elements <- elements
    members <- x[seq.int(2,length(x),2)]
    checkmate::assertNumeric(members, lower = 0, upper = 1)
    private$.members <- members

    if(inherits(elements,"numeric") | inherits(elements,"integer")){
      private$.lower <- min(elements)
      private$.upper <- max(elements)
    } else{
      private$.lower <- elements[[1]]
      private$.upper <- elements[[length(elements)]]
    }
    private$.dimension <- dim
  }

  invisible(self)
})

FuzzySet$set("public","strprint",function(){
  return(paste0("{",paste0(self$elements(),"(",self$members(),")", collapse = ", "),"}"))
})
FuzzySet$set("public","isEmpty",function(){
  if(all(self$members() == 0))
    return(TRUE)
  else
    return(FALSE)
})
FuzzySet$set("public","alphaCut",function(alpha, strong = FALSE){
  if(strong)
    return(self$elements()[self$members() > alpha])
  else
    return(self$elements()[self$members() >= alpha])
})
FuzzySet$set("public","support",function(){
  return(self$elements()[self$members() > 0])
})
FuzzySet$set("public","core",function(){
  return(self$elements()[self$members() == 1])
})
FuzzySet$set("public","inclusion",function(x){
  member <- self$members()[self$elements() %in% x]
  if(length(member) == 0)
    return("Not Included")

  if(member == 1)
    return("Fully Included")
  else if(member == 0)
    return("Not Included")
  else
    return("Partially Included")
})

FuzzySet$set("public","equals",function(x){
  if(!testFuzzySet(x))
    return(FALSE)

  x_mat = matrix(c(x$elements(),x$members()),ncol=2)[order(x$elements()),]
  self_mat = matrix(c(self$elements(),self$members()),ncol=2)[order(self$elements()),]

  if(any(dim(x_mat) != dim(self_mat)))
    return(FALSE)

  if(all(x_mat == self_mat))
    return(TRUE)
  else
    return(FALSE)
})
FuzzySet$set("public","complement",function(){
  private$.members <- 1 - self$members()
  return(self)
})

FuzzySet$set("private",".type","{}")
FuzzySet$set("private",".members", 0)
FuzzySet$set("public","members",function(){
  return(private$.members)
})


