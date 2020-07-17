#' @title Get/Set Unicode Printing Method
#' @description Change whether unicode symbols should be used when printing sets.
#' @param use logical, if `TRUE` unicode will be used in printing, otherwise simpler character strings. If missing the current setting is returned.
#' @details Using unicode symbols makes the printing of sets and properties 'prettier', however may not
#' work on all machines or versions of `R`. Therefore this function is used to decide whether
#' unicode representations should be used, or standard alpha-numeric and special characters.
#'
#' By default `set6` starts with unicode printing turned on.
#'
#' @examples
#' current <- useUnicode()
#' useUnicode(TRUE)
#' useUnicode()
#' useUnicode(current)
#' @export
useUnicode <- function(use) {
  if (missing(use)) {
    return(getOption("set6.unicode"))
  } else {
    checkmate::assertFlag(use)
    options(set6.unicode = use)
  }
}
