#' @name LogicalSet
#' @title Set of Logicals
#' @description The `LogicalSet` is defined as the [Set] containing the elements `TRUE` and `FALSE`.
#'
#' @examples
#' l <- LogicalSet$new()
#' print(l)
#' l$contains(list(TRUE, 1, FALSE))
#' @export
LogicalSet <- R6::R6Class("LogicalSet",
  inherit = Set,
  public = list(
    #' @description Create a new `LogicalSet` object.
    #' @details The Logical set is the set containing `TRUE` and `FALSE`.
    #' @return A new `LogicalSet` object.
    initialize = function() {
      warning("Deprecated. In the future please use Logicals$new(). This will be removed in v0.4.0.")
      private$.elements <- list(TRUE, FALSE)
      private$.str_elements <- c("TRUE", "FALSE")
      private$.multiplicity <- list("TRUE" = 1, "FALSE" = 1)
      private$.class <- "logical"
      private$.properties <- Properties$new(closure = "closed", cardinality = 2)
      private$.lower <- TRUE
      private$.upper <- FALSE
      private$.universe <- self
      invisible(self)
    }
  )
)
