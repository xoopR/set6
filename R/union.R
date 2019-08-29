#' @name Union
#' @title Union
#' @export
NULL
Union <- R6::R6Class("Union", inherit = SetWrapper)
Union$set("public", "initialize", function(x, y){
  if(x$isSubset(y, proper = FALSE)){
    super$initialize(setlist = list(x,y),
                     lower = x$lower,
                     upper = x$upper,
                     type = x$type,
                     dimension = x$dimension,
                     symbol = x$strprint())
  } else if(y$isSubset(x, proper = FALSE)){
    super$initialize(setlist = list(x,y),
                     lower = y$lower,
                     upper = y$upper,
                     type = y$type,
                     dimension = y$dimension,
                     symbol = y$strprint())
  } else{
    if(testInterval(x) & testInterval(y)){
      if(getR6Class(x) == "PosReals" & getR6Class(y) == "NegReals" |
         getR6Class(y) == "PosReals" & getR6Class(x) == "NegReals")
        return(super$initialize(setlist = list(x,y),
                         lower = -Inf,
                         upper = Inf,
                         type = "()",
                         dimension = x$dimension,
                         symbol = setSymbol(Reals)))
      else if(getR6Class(x) == "PosRationals" & getR6Class(y) == "NegRationals" |
         getR6Class(y) == "PosRationals" & getR6Class(x) == "NegRationals")
        return(super$initialize(setlist = list(x,y),
                         lower = -Inf,
                         upper = Inf,
                         type = "()",
                         dimension = x$dimension,
                         symbol = setSymbol(Rationals)))
      else if(getR6Class(x) == "PosIntegers" & getR6Class(y) == "NegIntegers" |
              getR6Class(y) == "PosIntegers" & getR6Class(x) == "NegIntegers")
        return(super$initialize(setlist = list(x,y),
                         lower = -Inf,
                         upper = Inf,
                         type = "()",
                         class = "integer",
                         dimension = x$dimension,
                         symbol = setSymbol(Integers)))

      if(x$upper > y$lower & x$lower < y$lower){
        return(super$initialize(setlist = list(x,y),
                         lower = x$lower,
                         upper = y$upper,
                         type = paste0(substr(x$type,1,1),substr(y$type,2,2)),
                         dimension = y$dimension))
      } else if(y$upper > x$lower & y$lower < x$lower){
        return(super$initialize(setlist = list(x,y),
                         lower = y$lower,
                         upper = x$upper,
                         type = paste0(substr(y$type,1,1),substr(x$type,2,2)),
                         dimension = x$dimension))
      } else if(x$upper < y$lower){
        return(super$initialize(setlist = list(x,y),
                         lower = x$lower,
                         upper = y$upper,
                         type = paste0(substr(x$type,1,1),substr(y$type,2,2)),
                         dimension = y$dimension,
                         symbol = paste(x$strprint(), y$strprint(),sep=" \u222A ")))
      } else if(y$upper < x$lower){
        return(super$initialize(setlist = list(x,y),
                         lower = y$lower,
                         upper = x$upper,
                         type = paste0(substr(y$type,1,1),substr(x$type,2,2)),
                         dimension = x$dimension,
                         symbol = paste(y$strprint(), x$strprint(),sep=" \u222A ")))
      } else if(y$upper == x$lower){
        if(!testClosedAbove(y) & !testClosedBelow(x))
          return(super$initialize(setlist = list(x,y),
                           lower = y$lower,
                           upper = x$upper,
                           type = paste0(substr(y$type,1,1),substr(x$type,2,2)),
                           dimension = y$dimension,
                           symbol = paste(y$strprint(), x$strprint(),sep=" \u222A ")))
       else
         return(super$initialize(setlist = list(x,y),
                          lower = y$lower,
                          upper = x$upper,
                          type = paste0(substr(y$type,1,1),substr(x$type,2,2)),
                          dimension = x$dimension,
                          symbol = paste0(substr(y$type,1,1),y$lower,", ",x$upper,substr(x$type,2,2))))
      } else if(x$upper == y$lower){
        if(!testClosedAbove(x) & !testClosedBelow(y))
          return(super$initialize(setlist = list(x,y),
                           lower = x$lower,
                           upper = y$upper,
                           type = paste0(substr(x$type,1,1),substr(y$type,2,2)),
                           dimension = x$dimension,
                           symbol = paste(x$strprint(), y$strprint(),sep=" \u222A ")))
        else
          return(super$initialize(setlist = list(x,y),
                           lower = x$lower,
                           upper = y$upper,
                           type = paste0(substr(x$type,1,1),substr(y$type,2,2)),
                           dimension = y$dimension,
                           symbol = paste0(substr(x$type,1,1),x$lower,", ",y$upper,substr(y$type,2,2))))
      }
    }else if(testInterval(x) & !testInterval(y) | testInterval(y) & !testInterval(x)){
      return(super$initialize(setlist = list(x,y),
                              lower = min(x$lower, y$lower),
                              upper = max(x$upper, y$upper),
                              type = "{}",
                              dimension = x$dimension,
                              symbol = paste0("{",x$strprint(), " \u222A ", y$strprint(),"}")))
    } else if(getR6Class(x) %in% c("Set","Tuple") & getR6Class(y) %in% c("Set","Tuple")){
      return(super$initialize(c(x$elements, y$elements),
                              setlist = list(x,y),
                              dimension = x$dimension,
                              symbol = paste0("{",paste0(unique(c(x$elements,y$elements)), collapse = ", "),"}")))
    }
  }
})


Union$set("public", "strprint", function(){
  if(length(private$.symbol) != 0)
    return(private$.symbol)
  else
    super$strprint()
})
Union$set("private", "symbol", character(0))

#' @rdname Union
#' @usage \method{+}{Set}(x, y)
#' @param x Set
#' @param y Set
#' @export
`+.Set` <- function(x, y){
  Union$new(x, y)
}
