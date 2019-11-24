#---------------------------------------------
# Definition and Construction
#---------------------------------------------
Properties <- R6::R6Class("Properties")
Properties$set("public","initialize",function(closure = character(0), cardinality = NULL){

  if(length(closure) != 0){
    checkmate::assertChoice(closure, c("open", "half-open", "closed"))
    private$.closure = closure
  }

  if(!is.null(cardinality)){
    if (!checkmate::testIntegerish(cardinality)){
      if (grepl("^a(leph){0,1}0$", cardinality, ignore.case = TRUE)) {
        private$.cardinality = "Aleph0"
        private$.countability = "countably infinite"
      } else if (grepl("^b(eth){0,1}[0-9]+$", cardinality, ignore.case = TRUE)) {
        if(nchar(cardinality) < 5)
          private$.cardinality = paste0("Beth", substr(cardinality, 2, 100))
        else
          private$.cardinality = paste0("Beth", substr(cardinality, 5, 100))
        private$.countability = "uncountable"
      } else
        stop("Cardinality must either be a number or in {'Aleph0', 'A0', 'BethX', 'BX'} for some number X (case-insensitive)")
    } else {
      private$.cardinality = cardinality
      private$.countability = "countably finite"
    }

    private$.empty = (cardinality == 0)
    private$.singleton = (cardinality == 1)
  }

  invisible(self)
})
#---------------------------------------------
# Public methods
#---------------------------------------------
Properties$set("public","print",function(){
  print(self$strprint())
})
Properties$set("public","strprint",function(){
  if(useUnicode() & length(self$cardinality) != 0){
    if(self$cardinality == "Aleph0")
      cardinality = "\u2135\u2080"
    else if(class(self$cardinality) != "character")
      cardinality = self$cardinality
    else{
      .beths = data.frame(X = 0:9, U = c("\u2080", "\u2081", "\u2082", "\u2083", "\u2084", "\u2085",
                    "\u2086", "\u2087", "\u2088", "\u2089"))

      cardinality = paste0("\u2136", paste0(.beths$U[match(unlist(strsplit(substr(self$cardinality,
                                                                                  5, 100), split = "")),
                                                           .beths$X)], collapse = ""))
    }
  } else
    cardinality = self$cardinality

  list(empty = self$empty,
       singleton = self$singleton,
       cardinality = cardinality,
       countability = self$countability,
       closure = self$closure
       )
})
#---------------------------------------------
# Public Fields
#---------------------------------------------
Properties$set("active", "closure", function() return(private$.closure))
Properties$set("active", "countability", function() return(private$.countability))
Properties$set("active", "cardinality", function() return(private$.cardinality))
Properties$set("active", "empty", function() return(private$.empty))
Properties$set("active", "singleton", function() return(private$.singleton))
#---------------------------------------------
# Private Fields
#---------------------------------------------
Properties$set("private", ".closure", character(0))
Properties$set("private", ".countability", character(0))
Properties$set("private", ".cardinality", NULL)
Properties$set("private", ".empty", logical(0))
Properties$set("private", ".singleton", logical(0))
