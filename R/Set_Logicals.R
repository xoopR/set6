#' @name Logicals
#' @title Set of Logicals
#' @description The `Logicals` is defined as the [Set] containing the elements `TRUE` and `FALSE`.
#' @family special sets
#'
#' @examples
#' l <- Logicals$new()
#' print(l)
#' l$contains(list(TRUE, 1, FALSE))
#' @export
Logicals <- R6::R6Class("Logicals",
  inherit = Set,
  public = list(
    #' @description Create a new `Logicals` object.
    #' @details The Logical set is the set containing `TRUE` and `FALSE`.
    #' @return A new `Logicals` object.
    initialize = function() {
      private$.elements <- list(TRUE, FALSE)
      private$.str_elements <- c("TRUE", "FALSE")
      private$.multiplicity <- list("TRUE" = 1, "FALSE" = 1)
      private$.class <- "logical"
      private$.properties <- Properties$new(closure = "closed", cardinality = 2)
      private$.lower <- TRUE
      private$.upper <- FALSE
      private$.universe <- Set$new(TRUE, FALSE)
      invisible(self)
    }
  )
)
