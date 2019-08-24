#' @title Lists Implemented R6 Special Sets
#' @description Lists special sets that can be used in Set.
#' @param simplify logical. If FALSE (default) returns data.table of set name and symbol, otherwise set names as characters.
#' @seealso \code{\link{SpecialSet}}
#' @return Either a list of characters (if \code{simplify} is TRUE) or a data.table of \code{SpecialSet}s and their traits.
#' @examples
#' listSpecialSets()
#' listSpecialSets(TRUE)
#' @export
listSpecialSets <- function(simplify = FALSE){
  y = c("Empty","Naturals","PosNaturals","Integers","PosIntegers","NegIntegers","Rationals",
        "PosRationals","NegRationals","Reals","PosReals","NegReals","ExtendedReals",
        "Complex")
  if(simplify)
    return(as.character(y))
  else{
    symbols = do.call(rbind.data.frame,lapply(y, function(x){
      x = get(x)
      zero = "zero" %in% names(formals(PosReals$public_methods$initialize))
      ClassName = x$classname
      x = x$new()
      Symbol = x$strprint()
      if(zero & grepl("Pos",ClassName))
        Lower = "0/1"
      else
        Lower = x$lower
      if(is.null(Lower)) Lower = "NULL"
      if(zero & grepl("Neg",ClassName))
        Upper = "-1/0"
      else
        Upper = x$upper
      if(is.null(Upper)) Upper = "NULL"
      return(cbind(ClassName, Symbol, Lower, Upper))
    }))
    row.names(symbols) = NULL

    return(data.table::data.table(symbols))
  }
}
