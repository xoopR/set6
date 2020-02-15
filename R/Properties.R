#' @title Set Properties Class
#' @name Properties
#' @rdname Properties
#' @description Used to store the properties of a [Set]. Though this is not an abstract class,
#' it should never be constructed outside of the [Set] constructor.
Properties <- R6Class("Properties",
  public = list(
    #' @description Creates a new `Properties` object.
    #' @param closure One of "open", "half-open", or "closed."
    #' @param cardinality If non-`NULL` then either an integer, "Aleph0", or a Beth number.
    #' @return A new `Properties` object.
    initialize = function(closure = character(0), cardinality = NULL){

      if(length(closure) != 0){
        checkmate::assertChoice(closure, c("open", "half-open", "closed"))
        private$.closure = closure
      }

      if(!is.null(cardinality)){
        if (!(class(cardinality) %in% c("integer","numeric"))){
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
    },

    #' @description
    #' Prints the `Properties` list.
    #' @return Prints `Properties` list to console.
    print = function(){
      print(self$strprint())
    },

    #' @description
    #' Creates a printable representation of the `Properties`.
    #' @return A `list` of properties.
    strprint = function(){
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
    }
  ),

  active = list(
    #' @field closure
    #' Returns the closure of the `Set`. One of "open", "half-open", or "closed."
    closure = function() return(private$.closure),

    #' @field countability
    #' Returns the countability of the `Set`. One of "countably finite", "countably infinite", or "uncountable".
    countability = function() return(private$.countability),

    #' @field cardinality
    #' Returns the cardinality of the `Set`. Either an integer if the `Set` is countably finite, Aleph0 if countably infinite, or a Beth number.
    cardinality = function() return(private$.cardinality),

    #' @field empty
    #' Returns if the `Set` is empty or not. `TRUE` if the Set cardinality is `0`, `FALSE` otherwise.
    empty = function() return(private$.empty),

    #' @field singleton
    #' Returns if the `Set` is a singleton or not. `TRUE` if the Set cardinality is `1`, `FALSE` otherwise.
    singleton = function() return(private$.singleton)
  ),

  private = list(
    .closure = character(0),
    .countability = character(0),
    .cardinality = NULL,
    .empty = logical(0),
    .singleton = logical(0)
  )
)
