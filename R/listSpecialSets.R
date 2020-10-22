#' @title Lists Implemented R6 Special Sets
#' @description Lists special sets that can be used in Set.
#' @param simplify logical. If `FALSE` (default) returns data.frame of set name and symbol,
#' otherwise set names as characters.
#' @return Either a list of characters (if `simplify` is `TRUE`) or a `data.frame` of `SpecialSet`s
#' and their traits.
#' @examples
#' listSpecialSets()
#' listSpecialSets(TRUE)
#' @export
listSpecialSets <- function(simplify = FALSE) {
  y <- c(
    "Universal", "Logicals", "Naturals", "PosNaturals", "Integers", "PosIntegers", "NegIntegers",
    "Rationals", "PosRationals", "NegRationals", "Reals", "PosReals", "NegReals", "ExtendedReals",
    "Complex"
  )
  if (simplify) {
    return(as.character(y))
  } else {
    return(data.frame(
      ClassName = y,
      Symbol = unname(sapply(y, setSymbol)),
      Infimum = c("NA", "FALSE", "0", "0/1", "-Inf", "0/1", "-Inf", "-Inf", "0/1", "-Inf", "-Inf", "0/1",
                  rep("-Inf", 2), "NA"),
      Supremum = c("NA", "TRUE", rep("Inf", 4), "-1/0", "Inf", "Inf", "-1/0", "Inf", "Inf", "-1/0", "Inf", "NA")
    ))
  }
}
